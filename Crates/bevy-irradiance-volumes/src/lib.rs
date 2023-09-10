// bevy-irradiance-volumes/Crates/bevy-irradiance-volumes/src/lib.rs

#![allow(clippy::type_complexity)]

use bevy::asset::{AssetLoader, Error, LoadContext, LoadedAsset};
use bevy::core_pipeline::core_3d::{AlphaMask3d, Opaque3d, Transparent3d};
use bevy::core_pipeline::experimental::taa::TemporalAntiAliasSettings;
use bevy::core_pipeline::prepass::NormalPrepass;
use bevy::core_pipeline::tonemapping::{DebandDither, Tonemapping};
use bevy::ecs::query::ROQueryItem;
use bevy::ecs::system::lifetimeless::Read;
use bevy::ecs::system::SystemParamItem;
use bevy::math::ivec2;
use bevy::pbr::{
    self, DrawMesh, DrawPrepass, ExtractedMaterials, MaterialPipeline, MaterialPipelineKey,
    MeshPipelineKey, MeshUniform, PrepassPipelinePlugin, PrepassPlugin, RenderLightSystems,
    RenderMaterials, ScreenSpaceAmbientOcclusionSettings, SetMaterialBindGroup, SetMeshBindGroup,
    SetMeshViewBindGroup, Shadow,
};
use bevy::prelude::{
    error, info, warn, AddAsset, AlphaMode, App, AssetEvent, Assets, Changed, Commands, Component,
    Entity, EnvironmentMapLight, EventReader, FromWorld, GlobalTransform, Handle, IVec2, IVec3,
    Image, IntoSystemConfigs, Mat4, Material, Mesh, Msaa, Or, Plugin, PostUpdate, Query, Res,
    ResMut, Resource, Vec3, Vec4, With, World,
};
use bevy::reflect::{Reflect, TypeUuid};
use bevy::render::extract_component::{ExtractComponent, ExtractComponentPlugin};
use bevy::render::mesh::MeshVertexBufferLayout;
use bevy::render::render_asset::{PrepareAssetSet, RenderAssets};
use bevy::render::render_phase::{
    AddRenderCommand, DrawFunctions, PhaseItem, RenderCommand, RenderCommandResult, RenderPhase,
    SetItemPipeline, TrackedRenderPass,
};
use bevy::render::render_resource::{
    AsBindGroup, BindGroupLayout, PipelineCache, PreparedBindGroup, RenderPipelineDescriptor,
    ShaderRef, ShaderType, SpecializedMeshPipeline, SpecializedMeshPipelineError,
    SpecializedMeshPipelines,
};
use bevy::render::renderer::RenderDevice;
use bevy::render::texture::FallbackImage;
use bevy::render::view::{ExtractedView, VisibleEntities};
use bevy::render::{ExtractSchedule, Render, RenderApp, RenderSet};
use bevy::transform::TransformSystem;
use bevy::utils::BoxedFuture;
use image::{DynamicImage, ImageBuffer};
use serde::{Deserialize, Serialize};

#[doc(hidden)]
pub const IRRADIANCE_GRID_BYTES_PER_SAMPLE: usize = 4;
#[doc(hidden)]
pub const IRRADIANCE_GRID_SAMPLES_PER_CELL: usize = 6;
#[doc(hidden)]
pub const IRRADIANCE_GRID_BYTES_PER_CELL: usize =
    IRRADIANCE_GRID_BYTES_PER_SAMPLE * IRRADIANCE_GRID_SAMPLES_PER_CELL;

pub struct IrradianceVolumesPlugin;

/// The asset that defines an irradiance volume.
#[derive(Clone, Default, Reflect, Debug, TypeUuid, Serialize, Deserialize)]
#[uuid = "0adffc01-d268-4441-a9ea-a236679590b0"]
pub struct IrradianceVolume {
    pub meta: IrradianceVolumeMetadata,
    pub data: Vec<u8>,
}

#[derive(Clone, Default, Reflect, Debug, ShaderType, Serialize, Deserialize)]
pub struct IrradianceVolumeMetadata {
    pub resolution: IVec3,
    pub corner: Vec3,
    pub increment_x: Vec3,
    pub increment_y: Vec3,
    pub increment_z: Vec3,
    pub level_bias: f32,
}

pub struct IrradianceVolumeAssetLoader;

#[derive(Clone, Default, Reflect, AsBindGroup, TypeUuid, Debug)]
#[uuid = "d18d9aa6-5053-4cb4-8b59-a1b2d1e6b6db"]
pub struct PbrGiMaterial {
    #[uniform(0)]
    pub base_color: Vec4,
}

#[derive(Clone, Component, ExtractComponent, AsBindGroup)]
pub struct ComputedIrradianceVolumeData {
    #[uniform(0)]
    pub grid_data: IrradianceVolumeGpuData,
    #[texture(1)]
    pub irradiance_grid: Handle<Image>,
}

#[derive(Clone, Default, Reflect, Debug, ShaderType)]
pub struct IrradianceVolumeGpuData {
    pub meta: IrradianceVolumeMetadata,
    pub transform: Mat4,
    pub offset: i32,
}

#[derive(Resource, Clone)]
pub struct IrradianceGrid {
    texture: Option<Handle<Image>>,
    gpu_data: Vec<IrradianceVolumeGpuData>,
}

pub struct SetIrradianceVolumeDataBindGroup<const I: usize>;

type DrawPbrGiMaterial = (
    SetItemPipeline,
    SetMeshViewBindGroup<0>,
    SetMaterialBindGroup<PbrGiMaterial, 1>,
    SetMeshBindGroup<2>,
    SetIrradianceVolumeDataBindGroup<3>,
    DrawMesh,
);

#[derive(Component)]
pub struct RenderIrradianceVolumeData(pub PreparedBindGroup<()>);

#[derive(Resource)]
pub struct PbrGiMaterialPipeline {
    material_pipeline: MaterialPipeline<PbrGiMaterial>,
    irradiance_volume_data_bind_group_layout: BindGroupLayout,
}

impl Plugin for IrradianceVolumesPlugin {
    fn build(&self, app: &mut App) {
        app.register_type::<IrradianceVolume>()
            .register_type::<PbrGiMaterial>()
            .register_type::<IrradianceVolumeMetadata>()
            .add_asset::<IrradianceVolume>()
            .add_asset::<PbrGiMaterial>()
            .add_asset_loader(IrradianceVolumeAssetLoader)
            .init_resource::<IrradianceGrid>()
            .add_plugins(ExtractComponentPlugin::<Handle<PbrGiMaterial>>::extract_visible())
            .add_plugins(ExtractComponentPlugin::<ComputedIrradianceVolumeData>::extract_visible())
            .add_plugins(PrepassPipelinePlugin::<PbrGiMaterial>::default())
            .add_plugins(PrepassPlugin::<PbrGiMaterial>::default())
            .add_systems(
                PostUpdate,
                update_irradiance_grid.after(TransformSystem::TransformPropagate),
            )
            .add_systems(
                PostUpdate,
                write_irradiance_grid_to_materials.after(update_irradiance_grid),
            );

        let Ok(render_app) = app.get_sub_app_mut(RenderApp) else { return };
        render_app
            .init_resource::<DrawFunctions<Shadow>>()
            .add_render_command::<Shadow, DrawPrepass<PbrGiMaterial>>()
            .add_render_command::<Transparent3d, DrawPbrGiMaterial>()
            .add_render_command::<Opaque3d, DrawPbrGiMaterial>()
            .add_render_command::<AlphaMask3d, DrawPbrGiMaterial>()
            .init_resource::<ExtractedMaterials<PbrGiMaterial>>()
            .init_resource::<RenderMaterials<PbrGiMaterial>>()
            .init_resource::<SpecializedMeshPipelines<PbrGiMaterialPipeline>>()
            .add_systems(ExtractSchedule, pbr::extract_materials::<PbrGiMaterial>)
            .add_systems(
                Render,
                (
                    pbr::prepare_materials::<PbrGiMaterial>
                        .in_set(RenderSet::Prepare)
                        .after(PrepareAssetSet::PreAssetPrepare),
                    pbr::queue_shadows::<PbrGiMaterial>.in_set(RenderLightSystems::QueueShadows),
                    queue_pbr_gi_material_meshes.in_set(RenderSet::Queue),
                    prepare_irradiance_volumes
                        .in_set(RenderSet::Prepare)
                        .after(PrepareAssetSet::PreAssetPrepare),
                ),
            );
    }

    fn finish(&self, app: &mut App) {
        if let Ok(render_app) = app.get_sub_app_mut(RenderApp) {
            // The plain `MaterialPipeline<PbrGiMaterial>` is just for the prepass.
            render_app
                .init_resource::<MaterialPipeline<PbrGiMaterial>>()
                .init_resource::<PbrGiMaterialPipeline>();
        }
    }
}

impl Material for PbrGiMaterial {
    fn fragment_shader() -> ShaderRef {
        // TODO: Use `include_bytes!` instead.
        "IrradianceVolume.wgsl".into()
    }
}

impl SpecializedMeshPipeline for PbrGiMaterialPipeline {
    type Key = MaterialPipelineKey<PbrGiMaterial>;

    fn specialize(
        &self,
        key: Self::Key,
        layout: &MeshVertexBufferLayout,
    ) -> Result<RenderPipelineDescriptor, SpecializedMeshPipelineError> {
        let mut descriptor = self.material_pipeline.specialize(key, layout)?;

        debug_assert_eq!(descriptor.layout.len(), 3);
        descriptor
            .layout
            .push(self.irradiance_volume_data_bind_group_layout.clone());

        Ok(descriptor)
    }
}

impl AssetLoader for IrradianceVolumeAssetLoader {
    fn load<'a>(
        &'a self,
        bytes: &'a [u8],
        load_context: &'a mut LoadContext,
    ) -> BoxedFuture<'a, Result<(), Error>> {
        Box::pin(async move {
            let irradiance_volume: IrradianceVolume = bincode::deserialize(bytes)?;
            load_context.set_default_asset(LoadedAsset::new(irradiance_volume));
            Ok(())
        })
    }

    fn extensions(&self) -> &[&str] {
        static EXTENSIONS: [&str; 1] = ["voxelgi.bincode"];
        &EXTENSIONS
    }
}

impl FromWorld for IrradianceGrid {
    fn from_world(_: &mut World) -> Self {
        IrradianceGrid {
            texture: None,
            gpu_data: vec![],
        }
    }
}

fn update_irradiance_grid(
    mut irradiance_grid: ResMut<IrradianceGrid>,
    irradiance_volume_assets: Res<Assets<IrradianceVolume>>,
    mut image_assets: ResMut<Assets<Image>>,
    mut irradiance_volume_asset_events: EventReader<AssetEvent<IrradianceVolume>>,
    mut irradiance_volumes_query: Query<
        (&Handle<IrradianceVolume>, &GlobalTransform),
        Or<(Changed<Handle<IrradianceVolume>>, Changed<GlobalTransform>)>,
    >,
) {
    if irradiance_volume_asset_events.into_iter().next().is_none()
        && irradiance_volumes_query.into_iter().next().is_none()
    {
        // Nothing to do this frame.
        return;
    }

    info!("Regenerating irradiance grid");

    let (mut current_offset, mut new_gpu_data, mut irradiance_volume_handles) = (0, vec![], vec![]);
    for (irradiance_volume_handle, transform) in irradiance_volumes_query.iter_mut() {
        let Some(irradiance_volume) =
            irradiance_volume_assets.get(irradiance_volume_handle) else { continue };

        new_gpu_data.push(IrradianceVolumeGpuData {
            meta: irradiance_volume.meta.clone(),
            transform: transform.compute_matrix(),
            offset: current_offset,
        });

        irradiance_volume_handles.push(irradiance_volume_handle.clone());

        let voxel_count = irradiance_volume.data.len() / IRRADIANCE_GRID_BYTES_PER_CELL;
        current_offset += voxel_count as i32;
        debug_assert_eq!(voxel_count, irradiance_volume.meta.sample_count());
    }

    // Nothing has been loaded yet; do nothing.
    if current_offset == 0 {
        return;
    }

    // FIXME: Pick this dynamically.
    let dest_width = 768;
    let dest_cells_per_row = dest_width as usize / 3;
    let dest_height = div_ceil(current_offset as _, dest_cells_per_row as _) * 2;

    let mut dest_grid_data =
        vec![0; dest_width as usize * dest_height as usize * IRRADIANCE_GRID_BYTES_PER_SAMPLE];
    let dest_stride = dest_width as usize * IRRADIANCE_GRID_BYTES_PER_SAMPLE;

    for (irradiance_volume_handle, irradiance_volume_gpu_data) in
        irradiance_volume_handles.iter().zip(new_gpu_data.iter())
    {
        let src_volume = irradiance_volume_assets
            .get(irradiance_volume_handle)
            .unwrap();

        for src_cell_index in 0..src_volume.meta.sample_count() {
            let dest_cell_index = irradiance_volume_gpu_data.offset as usize + src_cell_index;
            let dest_origin = ivec2(
                (dest_cell_index % dest_cells_per_row * 3) as i32,
                (dest_cell_index / dest_cells_per_row * 2) as i32,
            );

            for y in 0..2 {
                for x in 0..3 {
                    let dest_offset = ivec2(x, y);
                    let src_sample_index = dest_offset.y as usize * 3
                        + dest_offset.x as usize
                        + src_cell_index * IRRADIANCE_GRID_SAMPLES_PER_CELL;
                    let src_byte_offset = src_sample_index * IRRADIANCE_GRID_BYTES_PER_SAMPLE;

                    let texel = &src_volume.data
                        [src_byte_offset..(src_byte_offset + IRRADIANCE_GRID_BYTES_PER_SAMPLE)];
                    put_texel(
                        &mut dest_grid_data,
                        texel.try_into().unwrap(),
                        dest_origin + dest_offset,
                        dest_stride,
                    );
                }
            }
        }
    }

    let grid_image = Image::from_dynamic(
        DynamicImage::ImageRgba8(
            ImageBuffer::from_vec(dest_width, dest_height, dest_grid_data).unwrap(),
        ),
        /*is_srgb=*/ false,
    );

    let grid_image_handle = image_assets.add(grid_image);

    *irradiance_grid = IrradianceGrid {
        texture: Some(grid_image_handle),
        gpu_data: new_gpu_data,
    }
}

fn write_irradiance_grid_to_materials(
    mut commands: Commands,
    irradiance_grid: Res<IrradianceGrid>,
    mut entities_query: Query<Entity, With<Handle<PbrGiMaterial>>>,
) {
    let Some(ref texture) = irradiance_grid.texture else { return };
    // FIXME: Check distance, fill in appropriately.
    let Some(gpu_data) = irradiance_grid.gpu_data.get(0) else { return };

    for entity in entities_query.iter_mut() {
        commands
            .entity(entity)
            .insert(ComputedIrradianceVolumeData {
                grid_data: (*gpu_data).clone(),
                irradiance_grid: (*texture).clone(),
            });
    }
}

fn div_ceil(a: u32, b: u32) -> u32 {
    (a + b - 1) / b
}

impl IrradianceVolumeMetadata {
    pub fn sample_count(&self) -> usize {
        self.resolution.x as usize * self.resolution.y as usize * self.resolution.z as usize
    }
}

/// `stride` is in bytes.
fn put_texel(buffer: &mut [u8], texel: [u8; 4], p: IVec2, stride: usize) {
    let offset = p.y as usize * stride + p.x as usize * IRRADIANCE_GRID_BYTES_PER_SAMPLE;
    buffer[offset..offset + 4].copy_from_slice(&texel)
}

impl<P, const I: usize> RenderCommand<P> for SetIrradianceVolumeDataBindGroup<I>
where
    P: PhaseItem,
{
    type Param = ();
    type ViewWorldQuery = ();
    type ItemWorldQuery = Read<RenderIrradianceVolumeData>;

    #[inline]
    fn render<'w>(
        _: &P,
        _: ROQueryItem<'w, Self::ViewWorldQuery>,
        entity: ROQueryItem<'w, Self::ItemWorldQuery>,
        _: SystemParamItem<'w, '_, Self::Param>,
        pass: &mut TrackedRenderPass<'w>,
    ) -> RenderCommandResult {
        pass.set_bind_group(I, &entity.0.bind_group, &[]);
        RenderCommandResult::Success
    }
}

pub fn prepare_irradiance_volumes(
    mut commands: Commands,
    query: Query<(Entity, &ComputedIrradianceVolumeData)>,
    pipeline: Res<PbrGiMaterialPipeline>,
    render_device: Res<RenderDevice>,
    images: Res<RenderAssets<Image>>,
    fallback_image: Res<FallbackImage>,
) {
    for (entity, data) in query.into_iter() {
        let maybe_bind_group = data.as_bind_group(
            &pipeline.irradiance_volume_data_bind_group_layout,
            &render_device,
            &images,
            &fallback_image,
        );
        match maybe_bind_group {
            Ok(bind_group) => {
                commands
                    .entity(entity)
                    .insert(RenderIrradianceVolumeData(bind_group));
            }
            Err(_) => {
                warn!("Failed to create bind group for PBR GI material");
            }
        }
    }
}

/// Copied from `bevy_pbr::material::queue_material_meshes`.
///
/// When this goes upstream, this can either be refactored to avoid duplication or else just merged
/// into `queue_material_meshes`.
#[allow(clippy::too_many_arguments)]
pub fn queue_pbr_gi_material_meshes(
    opaque_draw_functions: Res<DrawFunctions<Opaque3d>>,
    alpha_mask_draw_functions: Res<DrawFunctions<AlphaMask3d>>,
    transparent_draw_functions: Res<DrawFunctions<Transparent3d>>,
    material_pipeline: Res<PbrGiMaterialPipeline>,
    mut pipelines: ResMut<SpecializedMeshPipelines<PbrGiMaterialPipeline>>,
    pipeline_cache: Res<PipelineCache>,
    msaa: Res<Msaa>,
    render_meshes: Res<RenderAssets<Mesh>>,
    render_materials: Res<RenderMaterials<PbrGiMaterial>>,
    material_meshes: Query<(&Handle<PbrGiMaterial>, &Handle<Mesh>, &MeshUniform)>,
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
        let draw_opaque_pbr = opaque_draw_functions.read().id::<DrawPbrGiMaterial>();
        let draw_alpha_mask_pbr = alpha_mask_draw_functions.read().id::<DrawPbrGiMaterial>();
        let draw_transparent_pbr = transparent_draw_functions.read().id::<DrawPbrGiMaterial>();

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
                                entity: *visible_entity,
                                draw_function: draw_transparent_pbr,
                                pipeline: pipeline_id,
                                distance,
                            });
                        }
                    }
                }
            }
        }
    }
}

impl FromWorld for PbrGiMaterialPipeline {
    fn from_world(world: &mut World) -> Self {
        let render_device = world.resource::<RenderDevice>();
        let irradiance_volume_data_bind_group_layout =
            ComputedIrradianceVolumeData::bind_group_layout(render_device);
        PbrGiMaterialPipeline {
            material_pipeline: MaterialPipeline::from_world(world),
            irradiance_volume_data_bind_group_layout,
        }
    }
}
