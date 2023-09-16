// bevy-irradiance-volumes/Crates/bevy-irradiance-volumes/examples/scene.rs

use bevy::asset::FileAssetIo;
use bevy::math::{vec3, Vec3A};
use bevy::prelude::{
    AmbientLight, App, AssetPlugin, AssetServer, Camera3dBundle, Color, Commands, Name, Plugin,
    PluginGroup, Res, SpatialBundle, Startup, Transform, Update, Vec3,
};
use bevy::scene::SceneBundle;
use bevy::DefaultPlugins;
use bevy_baked_gi::irradiance_volumes::IrradianceVolume;
use bevy_baked_gi::BakedGiPlugin;
use bevy_egui::EguiPlugin;
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
        .add_plugins(BakedGiPlugin::default())
        .add_systems(Startup, setup)
        .add_systems(Startup, bevy_view_controls_egui::simple_setup)
        .add_systems(Update, bevy_view_controls_egui::simple_view_controls)
        .run();
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    let scene_name = env::args()
        .nth(1)
        .unwrap_or_else(|| "CornellBox.glb".to_owned());

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
        scene: asset_server.load(format!("{}#Scene0", scene_name)),
        transform: Transform::IDENTITY,
        ..SceneBundle::default()
    });

    // FIXME: Export a `scn.ron` from `export-blender-gi`.
    commands
        .spawn(SpatialBundle {
            transform: Transform::from_scale(vec3(0.012931285, 0.008930373, 0.012931285))
                .with_translation(vec3(0.0, -0.041710883, -0.81668377)),
            ..SpatialBundle::default()
        })
        .insert(Name::new("IrradianceVolume"))
        .insert(asset_server.load::<IrradianceVolume, _>("Sponza.voxelgi.bincode"));
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
