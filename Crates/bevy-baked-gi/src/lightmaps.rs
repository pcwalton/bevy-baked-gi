// bevy-baked-gi/Crates/bevy-baked-gi/src/lightmaps.rs

use crate::irradiance_volumes::GiPbrMaterial;
use arrayvec::ArrayVec;
use bevy::asset::{AssetLoader, AssetPath, Error as AnyhowError, LoadContext, LoadedAsset};
use bevy::gltf::{GltfError, GltfExtras, GltfLoader};
use bevy::math::vec4;
use bevy::prelude::{
    error, info, warn, AssetEvent, AssetServer, Assets, Children, Commands, Component, EventReader,
    Handle, Image, Mesh, Query, Res, ResMut, StandardMaterial, Vec4, Without,
};
use bevy::reflect::{Reflect, TypeUuid};
use bevy::render::extract_component::ExtractComponent;
use bevy::render::mesh::MeshVertexAttribute;
use bevy::render::render_resource::{AsBindGroup, VertexFormat};
use bevy::utils::BoxedFuture;
use gltf::buffer::Source;
use gltf::{Gltf as GGltf, Mesh as GMesh, Primitive, Semantic};
use serde_json::Value;

pub static LIGHTMAP_UV_ATTRIBUTE: MeshVertexAttribute =
    MeshVertexAttribute::new("LightmapUv", 0xbe293e1f, VertexFormat::Float32x2);

/// Stores information about the currently-active lightmap on this entity.
///
/// This component does nothing unless the Lightmapped component is present on the same entity.
#[derive(Clone, Component, ExtractComponent, AsBindGroup, Reflect)]
pub struct Lightmap {
    #[texture(0)]
    #[sampler(1)]
    pub image: Handle<Image>,

    /// The subrectangle of the lightmap texture that the lightmap UVs are relative to.
    ///
    /// The presence of this field allows the same mesh, including lightmap UVs, to be instantiated
    /// multiple times in the scene with separate lightmaps.
    ///
    /// The *x* and *y* components of this Vec4 are the minimum *u* and *v* coordinates of the
    /// rectangle respectively, and the *z* and *w* components are the maximum *u* and *v*
    /// coordinates of the rectangle. All components are between 0 and 1 inclusive.
    #[uniform(2)]
    pub uv_rect: Vec4,
}

#[derive(TypeUuid, Reflect)]
#[uuid = "df95f00d-3deb-40b3-a3fd-7c4bfc788228"]
pub struct LightmapUvs {
    pub mesh_handle: Handle<Mesh>,
    pub uvs: Vec<[f32; 2]>,
}

pub struct LightmappedGltfAssetLoader {
    pub gltf_loader: GltfLoader,
}

/// Attach this component to an entity with a Mesh in order to use lightmaps.
///
/// When this component is present, the real-time rendering of the mesh will be unaffected by lights
/// containing the StaticLight component, as it's assumed that the lightmap for this mesh includes
/// all the radiance coming from that light.
///
/// Entities with this component should have a Lightmap component as well, unless the lightmaps
/// haven't been generated yet. Lightmappers that operate on Bevy scenes should look for entities
/// with this component, compute lightmaps for them, and then attach a Lightmap component containing
/// information about the resulting baked lighting.
///
/// To ensure proper lighting, the transform attached to the entity with this component should not
/// change at runtime.
#[derive(Reflect, Default, Clone, Component, ExtractComponent)]
pub struct Lightmapped;

/// Attach this component to a light to instruct Bevy to take this light into account when
/// generating lightmaps and irradiance volumes.
///
/// Any light with StaticLight attached will be ignored when rendering meshes with the Lightmapped
/// component, as it's expected that the lightmap includes all radiance coming from such lights.
#[derive(Reflect, Default, Clone)]
pub struct StaticLight;

pub fn instantiate_lightmaps_in_gltf(
    mut commands: Commands,
    mut gltf_extras_query: Query<(&Children, &mut GltfExtras), Without<Handle<GiPbrMaterial>>>,
    standard_material_query: Query<&Handle<StandardMaterial>>,
    asset_server: ResMut<AssetServer>,
    standard_material_assets: Res<Assets<StandardMaterial>>,
    mut gi_pbr_assets: ResMut<Assets<GiPbrMaterial>>,
) {
    for (children, gltf_extras) in gltf_extras_query.iter_mut() {
        let Ok(Value::Object(gltf_extras)) =
            serde_json::from_str(&gltf_extras.value) else { continue };

        let Some(Value::String(lightmap_path)) = gltf_extras.get("Lightmap") else { continue };

        // Gather lightmap coordinates.
        let lightmap_coords = [
            "LightmapMinX",
            "LightmapMinY",
            "LightmapMaxX",
            "LightmapMaxY",
        ]
        .iter()
        .map(|name| match gltf_extras.get(*name) {
            Some(Value::Number(number)) => match number.as_f64() {
                Some(number) => Ok(number as f32),
                None => Err(()),
            },
            _ => Err(()),
        })
        .collect::<Result<ArrayVec<f32, 4>, ()>>();

        let lightmap_rect = lightmap_coords.map(|coords| Vec4::from_slice(&coords));
        let lightmap = asset_server.load(lightmap_path);

        for &kid in children.iter() {
            let Ok(standard_material_handle) = standard_material_query.get(kid) else { continue };
            let Some(standard_material) =
                standard_material_assets.get(standard_material_handle) else { continue };

            // TODO: Cache and reuse these!
            let new_lightmapped_material = gi_pbr_assets.add(GiPbrMaterial {
                base_color: standard_material.base_color.as_rgba_f32().into(),
                base_color_texture: standard_material.base_color_texture.clone(),
            });

            commands
                .entity(kid)
                .remove::<Handle<StandardMaterial>>()
                .insert(new_lightmapped_material)
                .insert(Lightmap {
                    image: lightmap.clone(),
                    uv_rect: lightmap_rect.unwrap_or(vec4(0.0, 0.0, 1.0, 1.0)),
                })
                .insert(Lightmapped);

            info!(
                "Instantiated standard material with lightmap: {} and uv rect: {:?}",
                lightmap_path, lightmap_rect
            );
        }
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

                        load_context.set_labeled_asset(
                            &format!("{}/LightmapUV", primitive_label),
                            LoadedAsset::new(LightmapUvs {
                                mesh_handle: mesh_handle.clone(),
                                uvs: tex_coords.into_f32().collect::<Vec<_>>(),
                            }),
                        );

                        info!(
                            "Loaded lightmap UVs: {}: {}",
                            load_context.path().display(),
                            primitive_label
                        );
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

pub fn handle_newly_added_lightmap_uvs(
    mut lightmap_uv_events: EventReader<AssetEvent<LightmapUvs>>,
    lightmap_uvs_assets: Res<Assets<LightmapUvs>>,
    mut mesh_assets: ResMut<Assets<Mesh>>,
) {
    for asset_event in lightmap_uv_events.into_iter() {
        let lightmap_uvs_handle = match asset_event {
            AssetEvent::Created { handle } | AssetEvent::Modified { handle } => (*handle).clone(),
            AssetEvent::Removed { .. } => continue,
        };

        let Some(lightmap_uvs) = lightmap_uvs_assets.get(&lightmap_uvs_handle) else { continue };

        let Some(mesh) = mesh_assets.get_mut(&lightmap_uvs.mesh_handle) else {
            error!("Didn't find the mesh");
            continue;
        };

        mesh.insert_attribute(LIGHTMAP_UV_ATTRIBUTE.clone(), lightmap_uvs.uvs.clone());
        info!("Copied lightmap UVs");
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
