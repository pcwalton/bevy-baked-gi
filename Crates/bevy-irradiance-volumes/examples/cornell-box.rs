// bevy-irradiance-volumes/Crates/bevy-irradiance-volumes/examples/cornell-box.rs

use bevy::asset::FileAssetIo;
use bevy::math::Vec3A;
use bevy::prelude::{
    AmbientLight, App, AssetPlugin, AssetServer, Assets, Camera3dBundle, Color, Commands, Entity,
    Handle, Name, Plugin, PluginGroup, Query, Res, ResMut, SpatialBundle, StandardMaterial,
    Startup, Transform, Update, Vec3, Vec4,
};
use bevy::scene::SceneBundle;
use bevy::DefaultPlugins;
use bevy_egui::EguiPlugin;
use bevy_irradiance_volumes::{
    IrradianceVolume, IrradianceVolumeGpuData, IrradianceVolumesPlugin, PbrGiMaterial,
};
use bevy_view_controls_egui::{ControllableCamera, ViewControlsPlugin};
use std::env;
use std::path::PathBuf;

struct ExampleAssetIoPlugin;

fn main() {
    App::new()
        .insert_resource(AmbientLight {
            color: Color::WHITE,
            brightness: 0.2,
        })
        .add_plugins(
            DefaultPlugins
                .build()
                .add_before::<AssetPlugin, _>(ExampleAssetIoPlugin),
        )
        .add_plugins(EguiPlugin)
        .add_plugins(ViewControlsPlugin)
        .add_plugins(IrradianceVolumesPlugin)
        .add_systems(Startup, setup)
        .add_systems(Startup, bevy_view_controls_egui::simple_setup)
        .add_systems(Update, bevy_view_controls_egui::simple_view_controls)
        .add_systems(Update, replace_standard_materials)
        .run();
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands
        .spawn(Camera3dBundle {
            transform: Transform::from_xyz(0.0, 0.0, 5.0).looking_at(Vec3::ZERO, Vec3::Y),
            ..Camera3dBundle::default()
        })
        .insert(ControllableCamera {
            target: Vec3A::ZERO,
            ..ControllableCamera::default()
        });

    commands.spawn(SceneBundle {
        scene: asset_server.load("CornellBox.glb#Scene0"),
        transform: Transform::from_scale(Vec3::splat(0.2294848)),
        ..SceneBundle::default()
    });

    // TODO: Add irradiance volume data.
    commands
        .spawn(SpatialBundle {
            transform: Transform::from_scale(Vec3::splat(0.2294848)),
            ..SpatialBundle::default()
        })
        .insert(Name::new("IrradianceVolume"))
        .insert(asset_server.load::<IrradianceVolume, _>("CornellBox.voxelgi.bincode"));
}

fn replace_standard_materials(
    mut commands: Commands,
    query: Query<(Entity, &Handle<StandardMaterial>)>,
    standard_materials: ResMut<Assets<StandardMaterial>>,
    mut irradiance_volume_materials: ResMut<Assets<PbrGiMaterial>>,
) {
    for (entity, standard_material) in query.iter() {
        let Some(standard_material) = standard_materials.get(standard_material) else { continue };

        let base_color = Vec4::from_slice(&standard_material.base_color.as_rgba_f32());

        commands
            .entity(entity)
            .remove::<Handle<StandardMaterial>>()
            .insert(irradiance_volume_materials.add(PbrGiMaterial { base_color }));
    }
}

impl Plugin for ExampleAssetIoPlugin {
    fn build(&self, app: &mut App) {
        let assets_root = match env::var("BEVY_ASSET_ROOT") {
            Ok(dir_str) => PathBuf::from(dir_str),
            Err(_) => PathBuf::from("../../Assets"),
        };
        app.insert_resource(AssetServer::new(FileAssetIo::new(assets_root, &None)));
    }
}
