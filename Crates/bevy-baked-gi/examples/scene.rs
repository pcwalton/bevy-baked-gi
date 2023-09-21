// bevy-irradiance-volumes/Crates/bevy-irradiance-volumes/examples/scene.rs

use bevy::asset::FileAssetIo;
use bevy::math::{vec3, Vec3A};
use bevy::prelude::{
    AmbientLight, App, AssetPlugin, AssetServer, Camera3dBundle, Color, Commands, Image, Name,
    Plugin, PluginGroup, Query, Res, SpatialBundle, Startup, Transform, Update, Vec3,
};
use bevy::scene::SceneBundle;
use bevy::DefaultPlugins;
use bevy_baked_gi::irradiance_volumes::IrradianceVolume;
use bevy_baked_gi::reflection_probes::ReflectionProbe;
use bevy_baked_gi::BakedGiPlugin;
use bevy_egui::EguiPlugin;
use bevy_view_controls_egui::{ControllableCamera, ViewControlsPlugin};
use std::env;
use std::path::PathBuf;

const FERRIS_ROTATION_SPEED: f32 = 0.01;

struct ExampleAssetIoPlugin;

fn main() {
    App::new()
        .insert_resource(AmbientLight {
            color: Color::WHITE,
            brightness: 1.0,
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
        .add_systems(Update, rotate_ferris)
        .run();
}

fn setup(mut commands: Commands, asset_server: Res<AssetServer>) {
    let specified_scene = env::args().nth(1);
    let scene_name = match specified_scene {
        None => "Sponza.gi.glb",
        Some(ref scene_name) => scene_name,
    };

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

    commands
        .spawn(SceneBundle {
            scene: asset_server.load("Ferris.glb#Scene0"),
            transform: Transform::from_scale(Vec3::splat(12.5)),
            ..SceneBundle::default()
        })
        .insert(Name::new("Ferris"));

    // FIXME: Export a `scn.ron` from `export-blender-gi`.

    /*
    commands
        .spawn(SpatialBundle {
            transform: Transform::from_scale(vec3(0.012931285, 0.008930373, 0.012931285))
                .with_translation(vec3(0.0, -0.041710883, -0.81668377)),
            ..SpatialBundle::default()
        })
        .insert(Name::new("IrradianceVolume"))
        .insert(asset_server.load::<IrradianceVolume, _>("Sponza.voxelgi.bincode"));
    */

    let diffuse_reflection_probe_map = asset_server.load::<Image, _>("Sponza.diffuse.001.ktx2");
    let specular_reflection_probe_map = asset_server.load::<Image, _>("Sponza.specular.001.ktx2");
    commands
        .spawn(SpatialBundle::default())
        .insert(Name::new("ReflectionProbe"))
        .insert(ReflectionProbe {
            diffuse_map: diffuse_reflection_probe_map,
            specular_map: specular_reflection_probe_map,
        });
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

fn rotate_ferris(mut query: Query<(&Name, &mut Transform)>) {
    for (name, mut transform) in query.iter_mut() {
        if &**name == "Ferris" {
            transform.rotate_axis(Vec3::Y, FERRIS_ROTATION_SPEED);
        }
    }
}
