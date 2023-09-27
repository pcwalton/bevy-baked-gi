// bevy-baked-gi/Crates/bevy-baked-gi/src/lightmaps.rs

//! Textures storing baked light.

use crate::{GiPbrMaterial, GltfGiSettings};
use bevy::asset::{AssetLoader, AssetPath, Error as AnyhowError, LoadContext, LoadedAsset};
use bevy::gltf::{GltfError, GltfLoader};
use bevy::math::vec4;
use bevy::prelude::{
    error, info, warn, AssetEvent, AssetServer, Assets, Commands, Component, Deref, DerefMut,
    Entity, EventReader, Handle, Image, Mesh, Query, Res, ResMut, Resource, StandardMaterial, Vec4,
};
use bevy::reflect::{Reflect, TypePath, TypeUuid};
use bevy::render::extract_component::ExtractComponent;
use bevy::render::mesh::MeshVertexAttribute;
use bevy::render::render_resource::{AsBindGroup, VertexFormat, ShaderType};
use bevy::utils::{BoxedFuture, HashSet};
use gltf::buffer::Source;
use gltf::{Gltf as GGltf, Mesh as GMesh, Primitive, Semantic};
use std::sync::{Arc, Mutex};

/// The mesh vertex attribute used for lightmap UVs.
///
/// This is named `LightmapUv` and is a `vec2f`.
pub static LIGHTMAP_UV_ATTRIBUTE: MeshVertexAttribute =
    MeshVertexAttribute::new("LightmapUv", 0xbe293e1f, VertexFormat::Float32x2);

/// Stores information about the lightmap on this mesh.
///
/// Entities with this component are unaffected by irradiance volumes and
/// reflection probes, as `bevy-baked-gi` assumes that the lightmap provides
/// more accurate global illumination than either of these.
#[derive(Clone, Component, ExtractComponent, AsBindGroup, Reflect, Debug)]
pub struct Lightmap {
    /// The lightmap applied to this mesh.
    #[texture(0)]
    #[sampler(1)]
    pub image: Handle<Image>,

    /// Various settings relating to the lightmap.
    #[uniform(2)]
    pub settings: LightmapSettings,
}

/// Various settings relating to the lightmap.
#[derive(Clone, Reflect, ShaderType, Debug)]
pub struct LightmapSettings {
    /// The subrectangle of the lightmap texture that the lightmap UVs are relative to.
    ///
    /// The presence of this field allows the same mesh, including lightmap UVs, to be instantiated
    /// multiple times in the scene with separate lightmaps.
    ///
    /// The *x* and *y* components of this Vec4 are the minimum *u* and *v* coordinates of the
    /// rectangle respectively, and the *z* and *w* components are the maximum *u* and *v*
    /// coordinates of the rectangle. All components are between 0 and 1 inclusive.
    pub uv_rect: Vec4,

    /// A factor that the lightmap lumels will be multiplied with.
    ///
    /// Higher values are brighter.
    ///
    /// The default value is 0.00075.
    pub exposure: f32,
}

/// Temporarily stores the lightmap UVs for a mesh.
///
/// This is essentially a workaround for the fact that Bevy currently ignores
/// the UV1 channel in glTF. When `bevy-baked-gi` loads a glTF file with a
/// `.gi.glb` extension, the lightmap UVs are parsed and transiently stored
/// inside this asset before being merged into the Mesh asset itself.
#[derive(TypeUuid, Reflect)]
#[uuid = "df95f00d-3deb-40b3-a3fd-7c4bfc788228"]
pub struct LightmapUvs {
    /// A handle to the mesh that the UVs are to be attached to.
    pub mesh_handle: Handle<Mesh>,

    /// The UVs themselves.
    pub uvs: Vec<[f32; 2]>,
}

/// An asset loader for lightmapped glTF scenes ending in `.gi.gltf` or
/// `.gi.glb`.
///
/// Asset files containing a `.gi.gltf`/`.gi.glb` extension are processed for
/// lightmaps, in addition to being loaded with the standard Bevy glTF loader.
pub struct LightmappedGltfAssetLoader {
    /// The underlying Bevy glTF loader.
    pub gltf_loader: GltfLoader,

    /// Stores strong references to the lightmap UVs, to prevent them from
    /// getting deleted before they're added to the mesh.
    pub(crate) kung_fu_death_grip: LightmapUvKungFuDeathGrip,
}

/// Holds references to all lightmap UVs so that they don't get freed.
#[derive(Clone, Default, TypePath, Deref, DerefMut, Resource)]
pub(crate) struct LightmapUvKungFuDeathGrip(Arc<Mutex<HashSet<Handle<LightmapUvs>>>>);

/// A system that applies the lightmap settings that the
/// [crate::parse_gltf_gi_settings] system added to this entity.
pub fn apply_gltf_lightmap_settings(
    mut commands: Commands,
    standard_material_query: Query<(Entity, &Handle<StandardMaterial>, &GltfGiSettings)>,
    asset_server: ResMut<AssetServer>,
    standard_material_assets: Res<Assets<StandardMaterial>>,
    mut gi_pbr_assets: ResMut<Assets<GiPbrMaterial>>,
) {
    for (entity, standard_material_handle, gi_settings) in standard_material_query.iter() {
        if gi_settings.disable_gi {
            continue;
        }

        let Some(ref lightmap_settings) = gi_settings.lightmap else { continue };
        let Some(standard_material) =
            standard_material_assets.get(standard_material_handle) else { continue };

        let lightmap_rect = lightmap_settings.uv_rect;
        let lightmap_rect = Vec4::from_array([
            lightmap_rect.min.x,
            lightmap_rect.min.y,
            lightmap_rect.max.x,
            lightmap_rect.max.y,
        ]);
        let lightmap = asset_server.load(&*lightmap_settings.path);

        // TODO: Cache and reuse these!
        let new_lightmapped_material =
            gi_pbr_assets.add(GiPbrMaterial((*standard_material).clone()));

        commands
            .entity(entity)
            .remove::<Handle<StandardMaterial>>()
            .insert(new_lightmapped_material)
            .insert(Lightmap {
                image: lightmap.clone(),
                settings: LightmapSettings {
                    uv_rect: lightmap_rect,
                    ..LightmapSettings::default()
                },
            });

        info!(
            "Instantiated standard material with lightmap: {} and uv rect: {:?}",
            lightmap_settings.path.display(),
            lightmap_rect
        );
    }
}

impl AssetLoader for LightmappedGltfAssetLoader {
    fn load<'a>(
        &'a self,
        bytes: &'a [u8],
        load_context: &'a mut LoadContext,
    ) -> BoxedFuture<'a, Result<(), AnyhowError>> {
        Box::pin(async move {
            self.gltf_loader.load(bytes, load_context).await?;

            let gltf = GGltf::from_slice(bytes)?;
            let buffer_data = load_buffers(&gltf).await?;

            for gltf_mesh in gltf.meshes() {
                for gltf_primitive in gltf_mesh.primitives() {
                    let primitive_label = primitive_label(&gltf_mesh, &gltf_primitive);
                    let mesh_handle = load_context.get_handle(AssetPath::new_ref(
                        load_context.path(),
                        Some(&*primitive_label),
                    ));
                    for (semantic, _) in gltf_primitive.attributes() {
                        if semantic != Semantic::TexCoords(1) {
                            continue;
                        }

                        let Some(tex_coords) = gltf_primitive.reader(|buffer| {
                            Some(&buffer_data[buffer.index()])
                        }).read_tex_coords(1) else {
                            warn!("Failed to read texture coordinates");
                            continue;
                        };

                        let lightmap_uv_handle = load_context.set_labeled_asset(
                            &format!("{}/LightmapUV", primitive_label),
                            LoadedAsset::new(LightmapUvs {
                                mesh_handle: mesh_handle.clone(),
                                uvs: tex_coords.into_f32().collect::<Vec<_>>(),
                            }),
                        );

                        info!(
                            "Loaded lightmap UVs: {}: {}: {:?}",
                            load_context.path().display(),
                            primitive_label,
                            lightmap_uv_handle.id()
                        );

                        self.kung_fu_death_grip
                            .lock()
                            .unwrap()
                            .insert(lightmap_uv_handle);
                    }
                }
            }

            Ok(())
        })
    }

    fn extensions(&self) -> &[&str] {
        static EXTENSIONS: [&str; 2] = ["gi.gltf", "gi.glb"];
        &EXTENSIONS
    }
}

/// A system that receives lightmap UVs and attaches them to meshes.
pub(crate) fn handle_lightmap_uv_asset_events(
    mut lightmap_uv_events: EventReader<AssetEvent<LightmapUvs>>,
    lightmap_uvs_assets: Res<Assets<LightmapUvs>>,
    mut mesh_assets: ResMut<Assets<Mesh>>,
    kung_fu_death_grip: Res<LightmapUvKungFuDeathGrip>,
) {
    for asset_event in lightmap_uv_events.into_iter() {
        let lightmap_uvs_handle = match asset_event {
            AssetEvent::Created { handle } | AssetEvent::Modified { handle } => handle.clone(),
            AssetEvent::Removed { .. } => continue,
        };

        let Some(lightmap_uvs) = lightmap_uvs_assets.get(&lightmap_uvs_handle) else {
            panic!(
                "Failed to find lightmap UVs for {:?}, did they get freed?",
                lightmap_uvs_handle.id()
            );
        };

        let Some(mesh) = mesh_assets.get_mut(&lightmap_uvs.mesh_handle) else {
            error!("Didn't find the mesh referenced by lightmap UVs {:?}", lightmap_uvs_handle);
            continue;
        };

        mesh.insert_attribute(LIGHTMAP_UV_ATTRIBUTE.clone(), lightmap_uvs.uvs.clone());
        info!("Copied lightmap UVs {:?}", lightmap_uvs_handle);

        // It's OK to drop the lightmap UVs now, since they've been added to the mesh.
        kung_fu_death_grip
            .lock()
            .unwrap()
            .remove(&lightmap_uvs_handle);
    }
}

// Copy-and-pasted from `bevy_gltf::loader`.
fn primitive_label(mesh: &GMesh, primitive: &Primitive) -> String {
    format!("Mesh{}/Primitive{}", mesh.index(), primitive.index())
}

/// Loads the raw glTF buffer data for a specific glTF file.
async fn load_buffers(gltf: &GGltf) -> Result<Vec<Vec<u8>>, GltfError> {
    let mut buffer_data = Vec::new();
    for buffer in gltf.buffers() {
        match buffer.source() {
            Source::Uri(_) => {
                warn!("URIs for lightmap UVs are currently unsupported");
                return Err(GltfError::MissingBlob);
            }
            Source::Bin => {
                if let Some(blob) = gltf.blob.as_deref() {
                    buffer_data.push(blob.into());
                } else {
                    return Err(GltfError::MissingBlob);
                }
            }
        }
    }

    Ok(buffer_data)
}

impl Default for LightmapSettings {
    fn default() -> Self {
        LightmapSettings {
            uv_rect: vec4(0.0, 0.0, 1.0, 1.0),
            exposure: 0.00075,
        }
    }
}
