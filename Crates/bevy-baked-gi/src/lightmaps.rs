// bevy-baked-gi/Crates/bevy-baked-gi/src/lightmaps.rs

use arrayvec::ArrayVec;
use bevy::asset::{AssetLoader, AssetPath, Error as AnyhowError, LoadContext, LoadedAsset};
use bevy::core_pipeline::core_3d::{AlphaMask3d, Opaque3d, Transparent3d};
use bevy::core_pipeline::experimental::taa::TemporalAntiAliasSettings;
use bevy::core_pipeline::prepass::NormalPrepass;
use bevy::core_pipeline::tonemapping::{DebandDither, Tonemapping};
use bevy::ecs::query::ROQueryItem;
use bevy::ecs::system::lifetimeless::Read;
use bevy::ecs::system::SystemParamItem;
use bevy::gltf::{GltfError, GltfExtras, GltfLoader};
use bevy::math::vec4;
use bevy::pbr::{
    DrawMesh, MaterialPipeline, MaterialPipelineKey, MeshPipelineKey, MeshUniform, RenderMaterials,
    ScreenSpaceAmbientOcclusionSettings, SetMaterialBindGroup, SetMeshBindGroup,
    SetMeshViewBindGroup,
};
use bevy::prelude::{
    error, info, warn, AlphaMode, AssetEvent, AssetServer, Assets, Children, Commands, Component,
    Entity, EnvironmentMapLight, EventReader, FromWorld, Handle, Image, Material, Mesh, Msaa,
    Query, Res, ResMut, Resource, StandardMaterial, Vec4, Without, World,
};
use bevy::reflect::{Reflect, TypeUuid};
use bevy::render::extract_component::ExtractComponent;
use bevy::render::mesh::{MeshVertexAttribute, MeshVertexBufferLayout};
use bevy::render::render_asset::RenderAssets;
use bevy::render::render_phase::{
    DrawFunctions, PhaseItem, RenderCommand, RenderCommandResult, RenderPhase, SetItemPipeline,
    TrackedRenderPass,
};
use bevy::render::render_resource::{
    AsBindGroup, BindGroupLayout, PipelineCache, PreparedBindGroup, RenderPipelineDescriptor,
    ShaderRef, SpecializedMeshPipeline, SpecializedMeshPipelineError, SpecializedMeshPipelines,
    VertexFormat,
};
use bevy::render::renderer::RenderDevice;
use bevy::render::texture::FallbackImage;
use bevy::render::view::{ExtractedView, VisibleEntities};
use bevy::utils::BoxedFuture;
use gltf::buffer::Source;
use gltf::{Gltf as GGltf, Mesh as GMesh, Primitive, Semantic};
use serde_json::Value;

use crate::irradiance_volumes::IrradianceVolumePbrMaterial;

pub static LIGHTMAP_UV_ATTRIBUTE: MeshVertexAttribute =
    MeshVertexAttribute::new("LightmapUv", 0xbe293e1f, VertexFormat::Float32x2);

#[derive(Clone, Default, Reflect, AsBindGroup, TypeUuid, Debug)]
#[uuid = "e5e6e265-c03b-4f99-b751-5ef736179742"]
pub struct LightmappedPbrMaterial {
    #[uniform(0)]
    pub base_color: Vec4,
    #[texture(1)]
    #[sampler(2)]
    pub base_color_texture: Option<Handle<Image>>,
}

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

pub struct SetLightmapDataBindGroup<const I: usize>;

pub type DrawLightmappedPbrMaterial = (
    SetItemPipeline,
    SetMeshViewBindGroup<0>,
    SetMaterialBindGroup<LightmappedPbrMaterial, 1>,
    SetMeshBindGroup<2>,
    SetLightmapDataBindGroup<3>,
    DrawMesh,
);

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

#[derive(Resource)]
pub struct LightmappedPbrPipeline {
    material_pipeline: MaterialPipeline<LightmappedPbrMaterial>,
    lightmap_bind_group_layout: BindGroupLayout,
}

#[derive(Component)]
pub struct RenderLightmap(pub PreparedBindGroup<()>);

impl Material for LightmappedPbrMaterial {
    fn vertex_shader() -> ShaderRef {
        // TODO: Use `include_bytes!` instead.
        "LightmappedPBR.wgsl".into()
    }

    fn fragment_shader() -> ShaderRef {
        // TODO: Use `include_bytes!` instead.
        "LightmappedPBR.wgsl".into()
    }
}

pub fn instantiate_lightmaps_in_gltf(
    mut commands: Commands,
    mut gltf_extras_query: Query<
        (&Children, &mut GltfExtras),
        Without<Handle<IrradianceVolumePbrMaterial>>,
    >,
    standard_material_query: Query<&Handle<StandardMaterial>>,
    asset_server: ResMut<AssetServer>,
    standard_material_assets: Res<Assets<StandardMaterial>>,
    mut pbr_gi_material_assets: ResMut<Assets<IrradianceVolumePbrMaterial>>,
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
            let new_pbr_gi_material = pbr_gi_material_assets.add(IrradianceVolumePbrMaterial {
                base_color: standard_material.base_color.as_rgba_f32().into(),
                base_color_texture: standard_material.base_color_texture.clone(),
            });

            commands
                .entity(kid)
                .remove::<Handle<StandardMaterial>>()
                .insert(new_pbr_gi_material)
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

impl SpecializedMeshPipeline for LightmappedPbrPipeline {
    type Key = MaterialPipelineKey<LightmappedPbrMaterial>;

    fn specialize(
        &self,
        key: Self::Key,
        layout: &MeshVertexBufferLayout,
    ) -> Result<RenderPipelineDescriptor, SpecializedMeshPipelineError> {
        let mut descriptor = self.material_pipeline.specialize(key, layout)?;

        // This is copy-and-pasted from `bevy_pbr::render::mesh::MeshPipeline::specialize`.
        let mut vertex_attributes = vec![];
        if layout.contains(Mesh::ATTRIBUTE_POSITION) {
            vertex_attributes.push(Mesh::ATTRIBUTE_POSITION.at_shader_location(0));
        }
        if layout.contains(Mesh::ATTRIBUTE_NORMAL) {
            vertex_attributes.push(Mesh::ATTRIBUTE_NORMAL.at_shader_location(1));
        }
        if layout.contains(Mesh::ATTRIBUTE_UV_0) {
            vertex_attributes.push(Mesh::ATTRIBUTE_UV_0.at_shader_location(2));
        }
        if layout.contains(Mesh::ATTRIBUTE_TANGENT) {
            vertex_attributes.push(Mesh::ATTRIBUTE_TANGENT.at_shader_location(3));
        }
        if layout.contains(Mesh::ATTRIBUTE_COLOR) {
            vertex_attributes.push(Mesh::ATTRIBUTE_COLOR.at_shader_location(4));
        }
        if layout.contains(LIGHTMAP_UV_ATTRIBUTE.clone()) {
            descriptor
                .vertex
                .shader_defs
                .push("VERTEX_LIGHTMAP_UVS".into());
            if let Some(ref mut fragment) = descriptor.fragment {
                fragment.shader_defs.push("VERTEX_LIGHTMAP_UVS".into());
            }

            vertex_attributes.push(LIGHTMAP_UV_ATTRIBUTE.at_shader_location(5));
        }

        let vertex_layout = layout.get_layout(&vertex_attributes).unwrap();
        descriptor.vertex.buffers = vec![vertex_layout];

        debug_assert_eq!(descriptor.layout.len(), 3);
        descriptor
            .layout
            .push(self.lightmap_bind_group_layout.clone());

        Ok(descriptor)
    }
}

impl FromWorld for LightmappedPbrPipeline {
    fn from_world(world: &mut World) -> Self {
        let render_device = world.resource::<RenderDevice>();
        let lightmap_bind_group_layout = Lightmap::bind_group_layout(render_device);
        LightmappedPbrPipeline {
            material_pipeline: MaterialPipeline::from_world(world),
            lightmap_bind_group_layout,
        }
    }
}

impl<P, const I: usize> RenderCommand<P> for SetLightmapDataBindGroup<I>
where
    P: PhaseItem,
{
    type Param = ();
    type ViewWorldQuery = ();
    type ItemWorldQuery = Option<Read<RenderLightmap>>;

    #[inline]
    fn render<'w>(
        _: &P,
        _: ROQueryItem<'w, Self::ViewWorldQuery>,
        entity: ROQueryItem<'w, Self::ItemWorldQuery>,
        _: SystemParamItem<'w, '_, Self::Param>,
        pass: &mut TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        match entity {
            None => RenderCommandResult::Failure,
            Some(irradiance_data) => {
                pass.set_bind_group(I, &irradiance_data.0.bind_group, &[]);
                RenderCommandResult::Success
            }
        }
    }
}

pub fn prepare_lightmaps(
    mut commands: Commands,
    query: Query<(Entity, &Lightmap)>,
    pipeline: Res<LightmappedPbrPipeline>,
    render_device: Res<RenderDevice>,
    images: Res<RenderAssets<Image>>,
    fallback_image: Res<FallbackImage>,
) {
    for (entity, data) in query.into_iter() {
        let maybe_bind_group = data.as_bind_group(
            &pipeline.lightmap_bind_group_layout,
            &render_device,
            &images,
            &fallback_image,
        );
        match maybe_bind_group {
            Ok(bind_group) => {
                commands.entity(entity).insert(RenderLightmap(bind_group));
            }
            Err(_) => {
                warn!("Failed to create bind group for lightmapped PBR material");
            }
        }
    }
}

/// Copied from `bevy_pbr::material::queue_material_meshes`.
///
/// When this goes upstream, this can either be refactored to avoid duplication or else just merged
/// into `queue_material_meshes`.
#[allow(clippy::too_many_arguments)]
pub fn queue_lightmapped_pbr_material_meshes(
    opaque_draw_functions: Res<DrawFunctions<Opaque3d>>,
    alpha_mask_draw_functions: Res<DrawFunctions<AlphaMask3d>>,
    transparent_draw_functions: Res<DrawFunctions<Transparent3d>>,
    material_pipeline: Res<LightmappedPbrPipeline>,
    mut pipelines: ResMut<SpecializedMeshPipelines<LightmappedPbrPipeline>>,
    pipeline_cache: Res<PipelineCache>,
    msaa: Res<Msaa>,
    render_meshes: Res<RenderAssets<Mesh>>,
    render_materials: Res<RenderMaterials<LightmappedPbrMaterial>>,
    material_meshes: Query<(&Handle<LightmappedPbrMaterial>, &Handle<Mesh>, &MeshUniform)>,
    images: Res<RenderAssets<Image>>,
    mut views: Query<(
        &ExtractedView,
        &VisibleEntities,
        Option<&Tonemapping>,
        Option<&DebandDither>,
        Option<&EnvironmentMapLight>,
        Option<&ScreenSpaceAmbientOcclusionSettings>,
        Option<&NormalPrepass>,
        Option<&TemporalAntiAliasSettings>,
        &mut RenderPhase<Opaque3d>,
        &mut RenderPhase<AlphaMask3d>,
        &mut RenderPhase<Transparent3d>,
    )>,
) {
    for (
        view,
        visible_entities,
        tonemapping,
        dither,
        environment_map,
        ssao,
        normal_prepass,
        taa_settings,
        mut opaque_phase,
        mut alpha_mask_phase,
        mut transparent_phase,
    ) in &mut views
    {
        let draw_opaque_pbr = opaque_draw_functions
            .read()
            .id::<DrawLightmappedPbrMaterial>();
        let draw_alpha_mask_pbr = alpha_mask_draw_functions
            .read()
            .id::<DrawLightmappedPbrMaterial>();
        let draw_transparent_pbr = transparent_draw_functions
            .read()
            .id::<DrawLightmappedPbrMaterial>();

        let mut view_key = MeshPipelineKey::from_msaa_samples(msaa.samples())
            | MeshPipelineKey::from_hdr(view.hdr);

        if normal_prepass.is_some() {
            view_key |= MeshPipelineKey::NORMAL_PREPASS;
        }

        if taa_settings.is_some() {
            view_key |= MeshPipelineKey::TAA;
        }

        let environment_map_loaded = match environment_map {
            Some(environment_map) => environment_map.is_loaded(&images),
            None => false,
        };
        if environment_map_loaded {
            view_key |= MeshPipelineKey::ENVIRONMENT_MAP;
        }

        if !view.hdr {
            if let Some(tonemapping) = tonemapping {
                view_key |= MeshPipelineKey::TONEMAP_IN_SHADER;
                view_key |= match tonemapping {
                    Tonemapping::None => MeshPipelineKey::TONEMAP_METHOD_NONE,
                    Tonemapping::Reinhard => MeshPipelineKey::TONEMAP_METHOD_REINHARD,
                    Tonemapping::ReinhardLuminance => {
                        MeshPipelineKey::TONEMAP_METHOD_REINHARD_LUMINANCE
                    }
                    Tonemapping::AcesFitted => MeshPipelineKey::TONEMAP_METHOD_ACES_FITTED,
                    Tonemapping::AgX => MeshPipelineKey::TONEMAP_METHOD_AGX,
                    Tonemapping::SomewhatBoringDisplayTransform => {
                        MeshPipelineKey::TONEMAP_METHOD_SOMEWHAT_BORING_DISPLAY_TRANSFORM
                    }
                    Tonemapping::TonyMcMapface => MeshPipelineKey::TONEMAP_METHOD_TONY_MC_MAPFACE,
                    Tonemapping::BlenderFilmic => MeshPipelineKey::TONEMAP_METHOD_BLENDER_FILMIC,
                };
            }
            if let Some(DebandDither::Enabled) = dither {
                view_key |= MeshPipelineKey::DEBAND_DITHER;
            }
        }

        if ssao.is_some() {
            view_key |= MeshPipelineKey::SCREEN_SPACE_AMBIENT_OCCLUSION;
        }

        let rangefinder = view.rangefinder3d();
        for visible_entity in &visible_entities.entities {
            if let Ok((material_handle, mesh_handle, mesh_uniform)) =
                material_meshes.get(*visible_entity)
            {
                if let (Some(mesh), Some(material)) = (
                    render_meshes.get(mesh_handle),
                    render_materials.get(material_handle),
                ) {
                    let mut mesh_key =
                        MeshPipelineKey::from_primitive_topology(mesh.primitive_topology)
                            | view_key;
                    if mesh.morph_targets.is_some() {
                        mesh_key |= MeshPipelineKey::MORPH_TARGETS;
                    }
                    match material.properties.alpha_mode {
                        AlphaMode::Blend => {
                            mesh_key |= MeshPipelineKey::BLEND_ALPHA;
                        }
                        AlphaMode::Premultiplied | AlphaMode::Add => {
                            // Premultiplied and Add share the same pipeline key
                            // They're made distinct in the PBR shader, via `premultiply_alpha()`
                            mesh_key |= MeshPipelineKey::BLEND_PREMULTIPLIED_ALPHA;
                        }
                        AlphaMode::Multiply => {
                            mesh_key |= MeshPipelineKey::BLEND_MULTIPLY;
                        }
                        AlphaMode::Mask(_) => {
                            mesh_key |= MeshPipelineKey::MAY_DISCARD;
                        }
                        _ => (),
                    }

                    let pipeline_id = pipelines.specialize(
                        &pipeline_cache,
                        &material_pipeline,
                        MaterialPipelineKey {
                            mesh_key,
                            bind_group_data: material.key,
                        },
                        &mesh.layout,
                    );
                    let pipeline_id = match pipeline_id {
                        Ok(id) => id,
                        Err(err) => {
                            error!("{}", err);
                            continue;
                        }
                    };

                    let distance = rangefinder.distance(&mesh_uniform.transform)
                        + material.properties.depth_bias;
                    match material.properties.alpha_mode {
                        AlphaMode::Opaque => {
                            opaque_phase.add(Opaque3d {
                                entity: *visible_entity,
                                draw_function: draw_opaque_pbr,
                                pipeline: pipeline_id,
                                distance,
                            });
                        }
                        AlphaMode::Mask(_) => {
                            alpha_mask_phase.add(AlphaMask3d {
                                entity: *visible_entity,
                                draw_function: draw_alpha_mask_pbr,
                                pipeline: pipeline_id,
                                distance,
                            });
                        }
                        AlphaMode::Blend
                        | AlphaMode::Premultiplied
                        | AlphaMode::Add
                        | AlphaMode::Multiply => {
                            transparent_phase.add(Transparent3d {
                                distance,
                                pipeline: pipeline_id,
                                entity: *visible_entity,
                                draw_function: draw_transparent_pbr,
                            });
                        }
                    }
                }
            }
        }
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
