// bevy-baked-gi/Crates/bevy-baked-gi/src/lib.rs

#![doc = include_str!("../../../README.md")]
#![allow(clippy::type_complexity)]

use crate::irradiance_volumes::{
    ComputedIrradianceVolumeInfo, IrradianceGrid, IrradianceVolume, IrradianceVolumeAssetLoader,
    IrradianceVolumeMetadata,
};
use crate::lightmaps::{
    Lightmap, LightmapUvKungFuDeathGrip, LightmapUvs, Lightmapped, LightmappedGltfAssetLoader,
    LIGHTMAP_UV_ATTRIBUTE,
};
use crate::reflection_probes::AppliedReflectionProbe;
use arrayvec::ArrayVec;
use bevy::asset::{load_internal_asset, HandleId};
use bevy::core_pipeline::core_3d::{AlphaMask3d, Opaque3d, Transparent3d};
use bevy::core_pipeline::experimental::taa::TemporalAntiAliasSettings;
use bevy::core_pipeline::prepass::NormalPrepass;
use bevy::core_pipeline::tonemapping::{DebandDither, Tonemapping};
use bevy::ecs::query::ROQueryItem;
use bevy::ecs::system::lifetimeless::Read;
use bevy::ecs::system::SystemParamItem;
use bevy::gltf::{GltfExtras, GltfLoader};
use bevy::math::{vec2, Vec3A};
use bevy::pbr::{
    self, DrawMesh, DrawPrepass, ExtractedMaterials, MaterialPipeline, MaterialPipelineKey,
    MeshPipelineKey, MeshUniform, PrepassPipelinePlugin, PrepassPlugin, RenderLightSystems,
    RenderMaterials, ScreenSpaceAmbientOcclusionSettings, SetMaterialBindGroup, SetMeshBindGroup,
    SetMeshViewBindGroup, Shadow, PBR_PREPASS_SHADER_HANDLE,
};
use bevy::prelude::{
    error, info, warn, AddAsset, AlphaMode, App, AssetEvent, AssetServer, Assets, Children,
    Commands, Component, Deref, DerefMut, Entity, EnvironmentMapLight, EventReader, FromWorld,
    Handle, HandleUntyped, Image, IntoSystemConfigs, Material, Mesh, Msaa, Name, Plugin,
    PostUpdate, Query, Rect, Res, ResMut, Resource, Shader, StandardMaterial, Update, Vec2, With,
    Without, World,
};
use bevy::reflect::{Reflect, TypeUuid};
use bevy::render::extract_component::ExtractComponentPlugin;
use bevy::render::mesh::{MeshVertexAttribute, MeshVertexBufferLayout};
use bevy::render::primitives::Aabb;
use bevy::render::render_asset::{PrepareAssetSet, RenderAssets};
use bevy::render::render_phase::{
    AddRenderCommand, DrawFunctions, PhaseItem, RenderCommand, RenderCommandResult, RenderPhase,
    SetItemPipeline, TrackedRenderPass,
};
use bevy::render::render_resource::{
    AsBindGroup, AsBindGroupError, BindGroupLayout, PipelineCache, PreparedBindGroup,
    RenderPipelineDescriptor, ShaderRef, SpecializedMeshPipeline, SpecializedMeshPipelineError,
    SpecializedMeshPipelines,
};
use bevy::render::renderer::RenderDevice;
use bevy::render::texture::{CompressedImageFormats, FallbackImage};
use bevy::render::view::{ExtractedView, VisibleEntities};
use bevy::render::{ExtractSchedule, Render, RenderApp, RenderSet};
use bevy::scene::Scene;
use bevy::transform::TransformSystem;
use bevy::utils::HashMap;
use reflection_probes::ReflectionProbe;
use serde::{Deserialize, Serialize};
use serde_json::{Error as SerdeJsonError, Value};
use std::collections::BTreeMap;
use std::path::PathBuf;
use thiserror::Error as Thiserror;

pub mod irradiance_volumes;
pub mod lightmaps;
pub mod reflection_probes;

/// Add this plugin to your App in order to enable baked global illumination.
#[derive(Default)]
pub struct BakedGiPlugin {
    /// Extra application-specific vertex attributes that will be parsed in glTF meshes.
    pub custom_vertex_attributes: HashMap<String, MeshVertexAttribute>,
}

/// The GPU mesh pipeline for physically-based rendering with baked global
/// illumination.
///
/// This is part of the Bevy render app.
#[derive(Resource)]
pub struct GiPbrPipeline {
    material_pipeline: MaterialPipeline<GiPbrMaterial>,
    irradiance_volume_bind_group_layout: BindGroupLayout,
    lightmap_bind_group_layout: BindGroupLayout,
    reflection_probe_bind_group_layout: BindGroupLayout,
}

/// Like [StandardMaterial], but takes baked global illumination into account.
///
/// You must apply this material to a mesh in order for lightmaps, irradiance
/// volumes, and/or reflection probes to have an effect on its rendering.
///
/// By default, the [upgrade_pbr_materials] system replaces all
/// [StandardMaterial]s with this material. This allows you to use glTF scenes
/// as usual without having to manually replace the materials within them.
#[derive(Clone, Default, Reflect, TypeUuid, Debug)]
#[uuid = "d18d9aa6-5053-4cb4-8b59-a1b2d1e6b6db"]
pub struct GiPbrMaterial(pub StandardMaterial);

#[derive(Component)]
pub struct RenderGiPbrData {
    pub lighting: PreparedBindGroup<()>,
}

/// A [RenderCommand] that sets the bind group necessary to render with a reflection probe or
/// irradiance volume.
pub struct SetGiPbrBindGroup<const I: usize>;

/// The type of global illumination that is to be applied to a mesh.
#[derive(Clone, Copy, Debug, Reflect, PartialEq, Eq, Hash)]
pub enum GiType {
    /// No global illumination is to be applied, other than possibly an environment map.
    NoGi,
    /// The mesh has a lightmap that captures the indirect and baked lights.
    Lightmapped,
    /// The mesh has a baked reflection probe that captures the surrounding scene.
    ReflectionProbe,
    /// The mesh is located within an irradiance volume that captures the surrounding light.
    IrradianceVolume,
}

/// All render commands necessary to draw a mesh with a physically-based
/// material and baked global illumination.
pub type DrawGiPbrMaterial = (
    SetItemPipeline,
    SetMeshViewBindGroup<0>,
    SetMaterialBindGroup<GiPbrMaterial, 1>,
    SetMeshBindGroup<2>,
    SetGiPbrBindGroup<3>,
    DrawMesh,
);

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct GiPbrPipelineKey {
    pub material: MaterialPipelineKey<GiPbrMaterial>,
    pub lighting: GiType,
}

/// Settings that can be present in glTF files as glTF extras.
#[derive(Clone, Component, Reflect, Default, Debug)]
pub struct GltfGiSettings {
    /// No baked global illumination will be applied to this object (other than
    /// an environment map, if applicable).
    ///
    /// This corresponds to the DisableGi setting.
    pub disable_gi: bool,

    /// If a lightmap is attached to this object, then this contains the
    /// settings of that lightmap.
    pub lightmap: Option<GltfLightmapSettings>,
}

/// Lightmap settings embedded in glTF extras.
#[derive(Clone, Component, Reflect, Default, Debug)]
pub struct GltfLightmapSettings {
    /// The path to the lightmap texture. This is relative to the glTF file.
    ///
    /// This corresponds to the Lightmap setting.
    ///
    /// FIXME: Make sure it actually is relative to that!
    pub path: PathBuf,
    /// The UV rectangle within the lightmap texture that contains the baked
    /// lumels for this object.
    ///
    /// This corresponds to the LightmapMinU, LightmapMinV, LightmapMaxU, and
    /// LightmapMaxV settings.
    pub uv_rect: Rect,
}

/// The errors that can occur when parsing global-illumination-related fields in
/// glTF extras.
#[derive(Thiserror, Debug)]
pub enum GltfGiSettingsParseError {
    #[error("The glTF extras weren't valid JSON")]
    Serde(#[from] SerdeJsonError),
    #[error("The glTF extras were malformed")]
    MalformedExtras,
    #[error("The DisableGi field wasn't a boolean")]
    MalformedDisableGi,
    #[error("The Lightmap field wasn't a valid path")]
    MalformedLightmap,
    #[error(
        "The LightmapMinU/LightmapMinV/LightmapMaxU/LightmapMaxV fields weren't all present and \
numeric"
    )]
    MalformedLightmapCoords,
}

/// Maps handle IDs to paths of assets within the assets directory.
///
/// `export-blender-gi` exports a serialized instance of this type alongside the
/// scene in RON format.  Bevy `.scn.ron` files refer to assets by their
/// [HandleId]s, not by their pathnames. Consequently, in order to load the
/// scene containing the baked global illumination data at runtime, this table
/// mapping handle IDs to paths within the assets folder is necessary. The
/// manifest is written to a file in RON format with the extension
/// `.manifest.ron`.
///
/// Typically, an application loading a scene exported from Blender via
/// `export-blender-gi` will load the manifest from the `.manifest.ron` file and
/// direct the [AssetServer] to load every asset in this table with the
/// [Manifest::load_all] method.
#[derive(Serialize, Deserialize, Deref, DerefMut)]
pub struct Manifest(pub BTreeMap<HandleId, PathBuf>);

/// Extension methods for axis-aligned bounding boxes.
trait AabbExt {
    /// Returns true if this AABB contains the given point.
    fn contains_point(&self, point: Vec3A) -> bool;

    /// Returns an AABB with corners at (-1, -1, -1) and (1, 1, 1).
    ///
    /// That is, the resulting AABB has a center at the origin, and every side
    /// has length of 2 units.
    fn centered_unit_cube() -> Self;
}

pub const SHADER_HANDLE: HandleUntyped =
    HandleUntyped::weak_from_u64(Shader::TYPE_UUID, 0xa68b654e530a1882);

impl Plugin for BakedGiPlugin {
    fn build(&self, app: &mut App) {
        load_internal_asset!(
            app,
            SHADER_HANDLE,
            "../assets/BakedGIPBR.wgsl",
            Shader::from_wgsl
        );

        // TODO: glTF instantiation should be optional.
        app.register_type::<IrradianceVolume>()
            .register_type::<GiPbrMaterial>()
            .register_type::<IrradianceVolumeMetadata>()
            .register_type::<ComputedIrradianceVolumeInfo>()
            .register_type::<Lightmap>()
            .register_type::<Lightmapped>()
            .register_type::<GltfGiSettings>()
            .register_type::<GltfLightmapSettings>()
            .register_type::<ReflectionProbe>()
            .add_asset::<IrradianceVolume>()
            .add_asset::<GiPbrMaterial>()
            .add_asset::<LightmapUvs>()
            .add_asset_loader(IrradianceVolumeAssetLoader)
            .preregister_asset_loader(&["gi.gltf", "gi.glb"])
            .init_resource::<IrradianceGrid>()
            .init_resource::<LightmapUvKungFuDeathGrip>()
            .add_plugins(ExtractComponentPlugin::<Handle<GiPbrMaterial>>::extract_visible())
            .add_plugins(ExtractComponentPlugin::<ComputedIrradianceVolumeInfo>::extract_visible())
            .add_plugins(ExtractComponentPlugin::<Lightmap>::extract_visible())
            .add_plugins(ExtractComponentPlugin::<Lightmapped>::extract_visible())
            .add_plugins(ExtractComponentPlugin::<AppliedReflectionProbe>::extract_visible())
            .add_plugins(PrepassPipelinePlugin::<GiPbrMaterial>::default())
            .add_plugins(PrepassPlugin::<GiPbrMaterial>::default())
            .add_systems(
                PostUpdate,
                irradiance_volumes::update_irradiance_grid
                    .after(TransformSystem::TransformPropagate),
            )
            .add_systems(
                PostUpdate,
                irradiance_volumes::apply_irradiance_volumes
                    .after(irradiance_volumes::update_irradiance_grid),
            )
            .add_systems(PostUpdate, reflection_probes::apply_reflection_probes)
            .add_systems(Update, parse_gltf_gi_settings)
            .add_systems(
                Update,
                lightmaps::apply_gltf_lightmap_settings.after(parse_gltf_gi_settings),
            )
            .add_systems(Update, lightmaps::handle_lightmap_uv_asset_events)
            .add_systems(
                Update,
                upgrade_pbr_materials.after(lightmaps::apply_gltf_lightmap_settings),
            );

        let Ok(render_app) = app.get_sub_app_mut(RenderApp) else { return };
        render_app
            .init_resource::<DrawFunctions<Shadow>>()
            .add_render_command::<Shadow, DrawPrepass<GiPbrMaterial>>()
            .add_render_command::<Transparent3d, DrawGiPbrMaterial>()
            .add_render_command::<Opaque3d, DrawGiPbrMaterial>()
            .add_render_command::<AlphaMask3d, DrawGiPbrMaterial>()
            .init_resource::<ExtractedMaterials<GiPbrMaterial>>()
            .init_resource::<RenderMaterials<GiPbrMaterial>>()
            .init_resource::<SpecializedMeshPipelines<GiPbrPipeline>>()
            .add_systems(ExtractSchedule, pbr::extract_materials::<GiPbrMaterial>)
            .add_systems(
                Render,
                (
                    pbr::prepare_materials::<GiPbrMaterial>
                        .in_set(RenderSet::Prepare)
                        .after(PrepareAssetSet::PreAssetPrepare),
                    pbr::queue_shadows::<GiPbrMaterial>.in_set(RenderLightSystems::QueueShadows),
                    queue_gi_pbr_material_meshes.in_set(RenderSet::Queue),
                    prepare_gi_pbr_meshes
                        .in_set(RenderSet::Prepare)
                        .after(PrepareAssetSet::PreAssetPrepare),
                ),
            );
    }

    fn finish(&self, app: &mut App) {
        if let Ok(render_app) = app.get_sub_app_mut(RenderApp) {
            // The plain MaterialPipelines are just for the prepass.
            render_app
                .init_resource::<MaterialPipeline<GiPbrMaterial>>()
                .init_resource::<GiPbrPipeline>();
        }

        // This is copy-and-pasted from `GltfPlugin::finish()`.

        let supported_compressed_formats = match app.world.get_resource::<RenderDevice>() {
            Some(render_device) => CompressedImageFormats::from_features(render_device.features()),
            None => CompressedImageFormats::NONE,
        };

        app.add_asset_loader(LightmappedGltfAssetLoader {
            gltf_loader: GltfLoader {
                supported_compressed_formats,
                custom_vertex_attributes: self.custom_vertex_attributes.clone(),
            },
            kung_fu_death_grip: (*app.world.resource::<LightmapUvKungFuDeathGrip>()).clone(),
        });
    }
}

/// A system, part of the render app, that creates the bind group corresponding to the baked global
/// illumination in use and builds the [RenderGiPbrData].
pub fn prepare_gi_pbr_meshes(
    mut commands: Commands,
    query: Query<
        (
            Entity,
            &Handle<Mesh>,
            Option<&ComputedIrradianceVolumeInfo>,
            Option<&Lightmap>,
            Option<&AppliedReflectionProbe>,
        ),
        With<Handle<GiPbrMaterial>>,
    >,
    pipeline: Res<GiPbrPipeline>,
    render_device: Res<RenderDevice>,
    images: Res<RenderAssets<Image>>,
    fallback_image: Res<FallbackImage>,
    meshes: Res<RenderAssets<Mesh>>,
) {
    for (
        entity,
        mesh_handle,
        maybe_irradiance_volume_info,
        maybe_lightmap,
        maybe_reflection_probe,
    ) in query.into_iter()
    {
        let Some(mesh) = meshes.get(mesh_handle) else {
            warn!("Failed to find mesh for entity {:?}", entity);
            continue;
        };

        let maybe_lighting_bind_group = match (maybe_lightmap, maybe_reflection_probe) {
            (Some(lightmap), _) if mesh.layout.contains(LIGHTMAP_UV_ATTRIBUTE.clone()) => lightmap
                .as_bind_group(
                    &pipeline.lightmap_bind_group_layout,
                    &render_device,
                    &images,
                    &fallback_image,
                ),
            (_, Some(reflection_probe)) => reflection_probe.as_bind_group(
                &pipeline.reflection_probe_bind_group_layout,
                &render_device,
                &images,
                &fallback_image,
            ),
            _ => maybe_irradiance_volume_info
                .cloned()
                .unwrap_or_default()
                .as_bind_group(
                    &pipeline.irradiance_volume_bind_group_layout,
                    &render_device,
                    &images,
                    &fallback_image,
                ),
        };

        match maybe_lighting_bind_group {
            Ok(lighting) => {
                commands.entity(entity).insert(RenderGiPbrData { lighting });
            }
            _ => {
                warn!("Failed to create bind group for globally-illuminated PBR material");
            }
        }
    }
}

/// A system, part of the render app, that adds meshes with PBR materials and baked GI to the
/// appropriate queue, specializing any pipelines if necessary.
///
/// This is copied from `bevy_pbr::material::queue_material_meshes` and modified
/// to support baked global illumination.
///
/// When this goes upstream, this can either be refactored to avoid duplication or else just merged
/// into `queue_material_meshes`.
#[allow(clippy::too_many_arguments)]
pub fn queue_gi_pbr_material_meshes(
    opaque_draw_functions: Res<DrawFunctions<Opaque3d>>,
    alpha_mask_draw_functions: Res<DrawFunctions<AlphaMask3d>>,
    transparent_draw_functions: Res<DrawFunctions<Transparent3d>>,
    material_pipeline: Res<GiPbrPipeline>,
    mut pipelines: ResMut<SpecializedMeshPipelines<GiPbrPipeline>>,
    pipeline_cache: Res<PipelineCache>,
    msaa: Res<Msaa>,
    render_meshes: Res<RenderAssets<Mesh>>,
    render_materials: Res<RenderMaterials<GiPbrMaterial>>,
    material_meshes: Query<(
        &Handle<GiPbrMaterial>,
        &Handle<Mesh>,
        &MeshUniform,
        Option<&Lightmap>,
        Option<&AppliedReflectionProbe>,
    )>,
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
        let draw_opaque_pbr = opaque_draw_functions.read().id::<DrawGiPbrMaterial>();
        let draw_alpha_mask_pbr = alpha_mask_draw_functions.read().id::<DrawGiPbrMaterial>();
        let draw_transparent_pbr = transparent_draw_functions.read().id::<DrawGiPbrMaterial>();

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
            if let Ok((
                material_handle,
                mesh_handle,
                mesh_uniform,
                maybe_lightmap,
                maybe_reflection_probe,
            )) = material_meshes.get(*visible_entity)
            {
                if let (Some(mesh), Some(material)) = (
                    render_meshes.get(mesh_handle),
                    render_materials.get(material_handle),
                ) {
                    // Determine the mesh key.

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

                    let lighting_type = if maybe_lightmap.is_some()
                        && mesh.layout.contains(LIGHTMAP_UV_ATTRIBUTE.clone())
                    {
                        GiType::Lightmapped
                    } else if maybe_reflection_probe.is_some() {
                        GiType::ReflectionProbe
                    } else {
                        GiType::IrradianceVolume
                    };

                    // Specialize the pipeline.

                    let pipeline_id = pipelines.specialize(
                        &pipeline_cache,
                        &material_pipeline,
                        GiPbrPipelineKey {
                            material: MaterialPipelineKey {
                                mesh_key,
                                bind_group_data: material.key.clone(),
                            },
                            lighting: lighting_type,
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

impl FromWorld for GiPbrPipeline {
    fn from_world(world: &mut World) -> Self {
        let render_device = world.resource::<RenderDevice>();
        let irradiance_volume_bind_group_layout =
            ComputedIrradianceVolumeInfo::bind_group_layout(render_device);
        let lightmap_bind_group_layout = Lightmap::bind_group_layout(render_device);
        let reflection_probe_bind_group_layout =
            AppliedReflectionProbe::bind_group_layout(render_device);
        GiPbrPipeline {
            material_pipeline: MaterialPipeline::from_world(world),
            irradiance_volume_bind_group_layout,
            lightmap_bind_group_layout,
            reflection_probe_bind_group_layout,
        }
    }
}

impl SpecializedMeshPipeline for GiPbrPipeline {
    type Key = GiPbrPipelineKey;

    // Copied from `bevy_pbr::render::mesh`.
    fn specialize(
        &self,
        key: Self::Key,
        layout: &MeshVertexBufferLayout,
    ) -> Result<RenderPipelineDescriptor, SpecializedMeshPipelineError> {
        let mut descriptor = self.material_pipeline.specialize(key.material, layout)?;

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

        let maybe_define = match key.lighting {
            GiType::NoGi => None,
            GiType::Lightmapped => {
                vertex_attributes.push(LIGHTMAP_UV_ATTRIBUTE.at_shader_location(7));
                Some("VERTEX_LIGHTMAP_UVS")
            }
            GiType::ReflectionProbe => Some("FRAGMENT_REFLECTION_PROBE"),
            GiType::IrradianceVolume => Some("FRAGMENT_IRRADIANCE_VOLUME"),
        };

        if let Some(define) = maybe_define {
            descriptor.vertex.shader_defs.push(define.into());
            if let Some(ref mut fragment) = descriptor.fragment {
                fragment.shader_defs.push(define.into());
            }
        }

        let vertex_layout = layout.get_layout(&vertex_attributes).unwrap();
        descriptor.vertex.buffers = vec![vertex_layout];

        // Push the appropriate bind group.
        let gi_bind_group_layout = match key.lighting {
            GiType::NoGi => None,
            GiType::ReflectionProbe => Some(self.reflection_probe_bind_group_layout.clone()),
            GiType::IrradianceVolume => Some(self.irradiance_volume_bind_group_layout.clone()),
            GiType::Lightmapped => Some(self.lightmap_bind_group_layout.clone()),
        };

        if let Some(gi_bind_group_layout) = gi_bind_group_layout {
            debug_assert_eq!(descriptor.layout.len(), 3);
            descriptor.layout.push(gi_bind_group_layout);
        }

        Ok(descriptor)
    }
}

impl<P, const I: usize> RenderCommand<P> for SetGiPbrBindGroup<I>
where
    P: PhaseItem,
{
    type Param = ();
    type ViewWorldQuery = ();
    type ItemWorldQuery = Option<Read<RenderGiPbrData>>;

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
            Some(gi_pbr_data) => {
                pass.set_bind_group(I, &gi_pbr_data.lighting.bind_group, &[]);
                RenderCommandResult::Success
            }
        }
    }
}

/// A system that upgrades non-lightmapped StandardMaterials to [GiPbrMaterial]s if they aren't
/// lightmapped.
pub fn upgrade_pbr_materials(
    mut commands: Commands,
    standard_material_query: Query<(
        Entity,
        Option<&Name>,
        &Handle<StandardMaterial>,
        &GltfGiSettings,
    )>,
    standard_material_assets: Res<Assets<StandardMaterial>>,
    mut pbr_gi_material_assets: ResMut<Assets<GiPbrMaterial>>,
) {
    for (entity, name, standard_material_handle, gi_settings) in standard_material_query.iter() {
        if gi_settings.disable_gi || gi_settings.lightmap.is_some() {
            continue;
        }

        let Some(pbr_material) =
            standard_material_assets.get(standard_material_handle) else { continue };

        let new_pbr_gi_material =
            pbr_gi_material_assets.add(GiPbrMaterial((*pbr_material).clone()));

        commands
            .entity(entity)
            .remove::<Handle<StandardMaterial>>()
            .insert(new_pbr_gi_material);

        info!(
            "Upgraded standard material on {:?} to a GI PBR material",
            name
        );
    }
}

/// A system that parses and applies the settings relating to baked global
/// illumination specified via glTF extras.
///
/// See `docs/Lightmaps.md` for more information on the glTF extras available
/// and how to specify them.
pub fn parse_gltf_gi_settings(
    mut commands: Commands,
    mut scene_asset_events: EventReader<AssetEvent<Scene>>,
    gltf_roots: Query<(Entity, &Handle<Scene>), Without<GltfGiSettings>>,
    gltf_extras_query: Query<(Option<&Children>, Option<&GltfExtras>), Without<GltfGiSettings>>,
) {
    for asset_event in scene_asset_events.iter() {
        let scene_handle = match asset_event {
            AssetEvent::Created { handle } | AssetEvent::Modified { handle } => handle.clone(),
            AssetEvent::Removed { .. } => continue,
        };

        let Some(gltf_root) = gltf_roots.iter().find_map(|(entity, this_scene_handle)| {
            if scene_handle.id() == this_scene_handle.id() {
                Some(entity)
            } else {
                None
            }
        }) else { continue };

        let mut worklist = vec![(gltf_root, GltfGiSettings::default())];
        while let Some((entity, mut settings)) = worklist.pop() {
            if let Ok((maybe_children, maybe_extras)) = gltf_extras_query.get(entity) {
                if let Some(extras) = maybe_extras {
                    if let Err(error) = settings.apply(extras) {
                        error!("Failed to parse glTF GI settings: {:?}", error);
                        continue;
                    }
                }

                if let Some(children) = maybe_children {
                    for &kid in children.iter() {
                        worklist.push((kid, settings.clone()));
                    }
                }
            }

            commands.entity(entity).insert(settings);
            info!("Applied glTF GI settings");
        }
    }
}

impl GltfGiSettings {
    fn apply(&mut self, extras: &GltfExtras) -> Result<(), GltfGiSettingsParseError> {
        let json_value = serde_json::from_str(&extras.value)?;

        let Value::Object(json_extras) = json_value else {
            return Err(GltfGiSettingsParseError::MalformedExtras)
        };

        match json_extras.get("DisableGI") {
            None => {}
            Some(Value::Bool(on)) => self.disable_gi = *on,
            Some(_) => return Err(GltfGiSettingsParseError::MalformedDisableGi),
        };

        match json_extras.get("Lightmap") {
            Some(Value::String(lightmap_path)) => {
                let path = PathBuf::from(lightmap_path);

                let lightmap_coords = [
                    "LightmapMinU",
                    "LightmapMinV",
                    "LightmapMaxU",
                    "LightmapMaxV",
                ]
                .iter()
                .filter_map(|name| match json_extras.get(*name) {
                    Some(Value::Number(number)) => number.as_f64(),
                    _ => None,
                })
                .collect::<ArrayVec<_, 4>>();

                let uv_rect = if lightmap_coords.is_empty() {
                    Rect::from_corners(Vec2::ZERO, Vec2::ONE)
                } else {
                    if lightmap_coords.len() != 4 {
                        return Err(GltfGiSettingsParseError::MalformedLightmapCoords);
                    }

                    Rect::from_corners(
                        vec2(lightmap_coords[0] as f32, lightmap_coords[1] as f32),
                        vec2(lightmap_coords[2] as f32, lightmap_coords[3] as f32),
                    )
                };

                self.lightmap = Some(GltfLightmapSettings { path, uv_rect });
            }
            Some(_) => return Err(GltfGiSettingsParseError::MalformedLightmap),
            None => {}
        }

        Ok(())
    }
}

impl Manifest {
    /// Creates a new empty manifest.
    pub fn new() -> Manifest {
        Manifest(BTreeMap::new())
    }

    /// Queues all the assets in the manifest for loading and returns a map from
    /// the handle IDs to the actual loaded handles.
    pub fn load_all(&self, asset_server: &mut AssetServer) -> HashMap<HandleId, HandleUntyped> {
        self.iter()
            .map(|(handle_id, path)| {
                let handle = asset_server.load_untyped(&**path);
                debug_assert_eq!(handle.id(), *handle_id);
                (*handle_id, handle)
            })
            .collect()
    }
}

impl Default for Manifest {
    fn default() -> Self {
        Self::new()
    }
}

impl AabbExt for Aabb {
    fn contains_point(&self, point: Vec3A) -> bool {
        (point.cmpge(self.min()) & (point.cmple(self.max()))).all()
    }

    fn centered_unit_cube() -> Aabb {
        Aabb {
            center: Vec3A::ZERO,
            half_extents: Vec3A::ONE,
        }
    }
}

impl AsBindGroup for GiPbrMaterial {
    type Data = <StandardMaterial as AsBindGroup>::Data;

    fn as_bind_group(
        &self,
        layout: &BindGroupLayout,
        render_device: &RenderDevice,
        images: &RenderAssets<Image>,
        fallback_image: &FallbackImage,
    ) -> Result<PreparedBindGroup<Self::Data>, AsBindGroupError> {
        self.0
            .as_bind_group(layout, render_device, images, fallback_image)
    }

    fn bind_group_layout(render_device: &RenderDevice) -> BindGroupLayout
    where
        Self: Sized,
    {
        StandardMaterial::bind_group_layout(render_device)
    }
}

impl Material for GiPbrMaterial {
    fn vertex_shader() -> ShaderRef {
        SHADER_HANDLE.typed().into()
    }

    fn fragment_shader() -> ShaderRef {
        SHADER_HANDLE.typed().into()
    }

    fn prepass_fragment_shader() -> ShaderRef {
        PBR_PREPASS_SHADER_HANDLE.typed().into()
    }
}
