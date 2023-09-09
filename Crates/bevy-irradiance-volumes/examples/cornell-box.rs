// bevy-irradiance-volumes/Crates/bevy-irradiance-volumes/examples/cornell-box.rs

use bevy::asset::FileAssetIo;
use bevy::math::Vec3A;
use bevy::prelude::{
    Added, AmbientLight, App, AssetPlugin, AssetServer, Assets, Camera3dBundle, Color, Commands,
    Entity, Handle, Plugin, PluginGroup, Query, Res, ResMut, StandardMaterial, Startup, Transform,
    Update, Vec3, Mat4, IVec3,
};
use bevy::scene::SceneBundle;
use bevy::DefaultPlugins;
use bevy_egui::EguiPlugin;
use bevy_irradiance_volumes::{IrradianceVolumeMaterial, IrradianceVolumesPlugin, GridData};
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
            transform: Transform::from_xyz(0.0, 0.0, 3.0).looking_at(Vec3::ZERO, Vec3::Y),
            ..Camera3dBundle::default()
        })
        .insert(ControllableCamera {
            target: Vec3A::ZERO,
            ..ControllableCamera::default()
        });

    commands.spawn(SceneBundle {
        scene: asset_server.load("CornellBox.glb#Scene0"),
        transform: Transform::from_scale(Vec3::splat(0.0038554997)),
        ..SceneBundle::default()
    });
}

fn replace_standard_materials(
    mut commands: Commands,
    query: Query<Entity, Added<Handle<StandardMaterial>>>,
    asset_server: Res<AssetServer>,
    mut materials: ResMut<Assets<IrradianceVolumeMaterial>>,
) {
    for entity in query.iter() {
        commands
            .entity(entity)
            .remove::<Handle<StandardMaterial>>()
            .insert(materials.add(IrradianceVolumeMaterial {
                grid_data: GridData {
                    matrix: Mat4::from_scale(Vec3::splat(0.0038554997)),
                    resolution: IVec3::splat(4),
                    offset: 1,
                    corner: Vec3::splat(-194.52731),
                    increment_x: Vec3::new(129.68488, 0.0, 0.0),
                    increment_y: Vec3::new(0.0, 129.68488, 0.0),
                    level_bias: 1.0,
                    increment_z: Vec3::new(0.0, 0.0, 129.68488),
                },
                irradiance_grid: asset_server.load("CornellBoxGrid.ktx2"),
            }));
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
