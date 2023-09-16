// bevy-baked-gi/Crates/bevy-baked-gi/src/lib.rs

#![doc = include_str!("../../../README.md")]
#![allow(clippy::type_complexity)]

use crate::irradiance_volumes::IrradianceVolumeMetadata;
use crate::irradiance_volumes::{
    ComputedIrradianceVolumeInfo, DrawIrradianceVolumePbrMaterial, IrradianceGrid,
    IrradianceVolume, IrradianceVolumeAssetLoader, IrradianceVolumePbrMaterial,
    IrradianceVolumePbrPipeline,
};
use crate::lightmaps::DrawLightmappedPbrMaterial;
use crate::lightmaps::{
    Lightmap, LightmapUvs, Lightmapped, LightmappedGltfAssetLoader, LightmappedPbrMaterial,
    LightmappedPbrPipeline,
};
use bevy::core_pipeline::core_3d::{AlphaMask3d, Opaque3d, Transparent3d};
use bevy::gltf::GltfLoader;
use bevy::pbr::{
    self, DrawPrepass, ExtractedMaterials, MaterialPipeline, PrepassPipelinePlugin, PrepassPlugin,
    RenderLightSystems, RenderMaterials, Shadow,
};
use bevy::prelude::{AddAsset, App, Handle, IntoSystemConfigs, Plugin, PostUpdate, Update};
use bevy::render::extract_component::ExtractComponentPlugin;
use bevy::render::mesh::MeshVertexAttribute;
use bevy::render::render_asset::PrepareAssetSet;
use bevy::render::render_phase::{AddRenderCommand, DrawFunctions};
use bevy::render::render_resource::SpecializedMeshPipelines;
use bevy::render::renderer::RenderDevice;
use bevy::render::texture::CompressedImageFormats;
use bevy::render::{ExtractSchedule, Render, RenderApp, RenderSet};
use bevy::transform::TransformSystem;
use bevy::utils::HashMap;

pub mod irradiance_volumes;
pub mod lightmaps;

/// Add this plugin to your App in order to enable baked global illumination.
#[derive(Default)]
pub struct BakedGiPlugin {
    pub custom_vertex_attributes: HashMap<String, MeshVertexAttribute>,
}

impl Plugin for BakedGiPlugin {
    fn build(&self, app: &mut App) {
        // TODO: glTF instantiation should be optional.
        app.register_type::<IrradianceVolume>()
            .register_type::<IrradianceVolumePbrMaterial>()
            .register_type::<IrradianceVolumeMetadata>()
            .register_type::<ComputedIrradianceVolumeInfo>()
            .register_type::<LightmappedPbrMaterial>()
            .register_type::<Lightmap>()
            .register_type::<Lightmapped>()
            .add_asset::<IrradianceVolume>()
            .add_asset::<IrradianceVolumePbrMaterial>()
            .add_asset::<LightmapUvs>()
            .add_asset_loader(IrradianceVolumeAssetLoader)
            .preregister_asset_loader(&["gi.gltf", "gi.glb"])
            .init_resource::<IrradianceGrid>()
            .add_plugins(
                ExtractComponentPlugin::<Handle<IrradianceVolumePbrMaterial>>::extract_visible(),
            )
            .add_plugins(ExtractComponentPlugin::<ComputedIrradianceVolumeInfo>::extract_visible())
            .add_plugins(ExtractComponentPlugin::<Lightmap>::extract_visible())
            .add_plugins(ExtractComponentPlugin::<Lightmapped>::extract_visible())
            .add_plugins(PrepassPipelinePlugin::<IrradianceVolumePbrMaterial>::default())
            .add_plugins(PrepassPlugin::<IrradianceVolumePbrMaterial>::default())
            .add_plugins(PrepassPipelinePlugin::<LightmappedPbrMaterial>::default())
            .add_plugins(PrepassPlugin::<LightmappedPbrMaterial>::default())
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
            .add_systems(Update, lightmaps::instantiate_lightmaps_in_gltf)
            .add_systems(Update, lightmaps::handle_newly_added_lightmap_uvs)
            .add_systems(
                Update,
                irradiance_volumes::upgrade_standard_materials_for_irradiance_volumes
                    .after(lightmaps::instantiate_lightmaps_in_gltf),
            );

        let Ok(render_app) = app.get_sub_app_mut(RenderApp) else { return };
        render_app
            .init_resource::<DrawFunctions<Shadow>>()
            .add_render_command::<Shadow, DrawPrepass<IrradianceVolumePbrMaterial>>()
            .add_render_command::<Transparent3d, DrawIrradianceVolumePbrMaterial>()
            .add_render_command::<Opaque3d, DrawIrradianceVolumePbrMaterial>()
            .add_render_command::<AlphaMask3d, DrawIrradianceVolumePbrMaterial>()
            .add_render_command::<Transparent3d, DrawLightmappedPbrMaterial>()
            .add_render_command::<Opaque3d, DrawLightmappedPbrMaterial>()
            .add_render_command::<AlphaMask3d, DrawLightmappedPbrMaterial>()
            .init_resource::<ExtractedMaterials<IrradianceVolumePbrMaterial>>()
            .init_resource::<RenderMaterials<IrradianceVolumePbrMaterial>>()
            .init_resource::<SpecializedMeshPipelines<IrradianceVolumePbrPipeline>>()
            .add_systems(
                ExtractSchedule,
                pbr::extract_materials::<IrradianceVolumePbrMaterial>,
            )
            .add_systems(
                ExtractSchedule,
                pbr::extract_materials::<LightmappedPbrMaterial>,
            )
            .add_systems(
                Render,
                (
                    pbr::prepare_materials::<IrradianceVolumePbrMaterial>
                        .in_set(RenderSet::Prepare)
                        .after(PrepareAssetSet::PreAssetPrepare),
                    pbr::queue_shadows::<IrradianceVolumePbrMaterial>
                        .in_set(RenderLightSystems::QueueShadows),
                    irradiance_volumes::queue_irradiance_volume_pbr_material_meshes
                        .in_set(RenderSet::Queue),
                    irradiance_volumes::prepare_irradiance_volumes
                        .in_set(RenderSet::Prepare)
                        .after(PrepareAssetSet::PreAssetPrepare),
                ),
            )
            .add_systems(
                Render,
                (
                    pbr::prepare_materials::<LightmappedPbrMaterial>
                        .in_set(RenderSet::Prepare)
                        .after(PrepareAssetSet::PreAssetPrepare),
                    pbr::queue_shadows::<LightmappedPbrMaterial>
                        .in_set(RenderLightSystems::QueueShadows),
                    lightmaps::queue_lightmapped_pbr_material_meshes.in_set(RenderSet::Queue),
                    lightmaps::prepare_lightmaps
                        .in_set(RenderSet::Prepare)
                        .after(PrepareAssetSet::PreAssetPrepare),
                ),
            );
    }

    fn finish(&self, app: &mut App) {
        if let Ok(render_app) = app.get_sub_app_mut(RenderApp) {
            // The plain MaterialPipelines are just for the prepass.
            render_app
                .init_resource::<MaterialPipeline<IrradianceVolumePbrMaterial>>()
                .init_resource::<MaterialPipeline<LightmappedPbrMaterial>>()
                .init_resource::<IrradianceVolumePbrPipeline>()
                .init_resource::<LightmappedPbrPipeline>();
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
        });
    }
}
