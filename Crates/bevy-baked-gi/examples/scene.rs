// bevy-irradiance-volumes/Crates/bevy-irradiance-volumes/examples/scene.rs

use bevy::asset::FileAssetIo;
use bevy::math::Vec3A;
use bevy::prelude::{
    AmbientLight, App, AssetPlugin, AssetServer, Camera3dBundle, Color, Commands, Name,
    Plugin, PluginGroup, Query, Res, ResMut, Resource, Startup, Transform, Update, Vec3,
};
use bevy::scene::{DynamicSceneBundle, SceneBundle};
use bevy::DefaultPlugins;
use bevy_baked_gi::{BakedGiPlugin, Manifest};
use bevy_egui::EguiPlugin;
use bevy_view_controls_egui::{ControllableCamera, ViewControlsPlugin};
use clap::Parser;
use std::ffi::OsStr;
use std::fs::File;
use std::path::PathBuf;
use std::{env, mem};

const FERRIS_ROTATION_SPEED: f32 = 0.01;

struct ExampleAssetIoPlugin;

#[derive(Parser, Resource)]
#[command(author, version, about)]
struct Args {
    #[arg()]
    scene: Vec<PathBuf>,

    #[arg(short, long)]
    manifest: Vec<PathBuf>,

    #[arg(short, long)]
    assets_dir: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();

    App::new()
        .insert_resource(AmbientLight {
            color: Color::WHITE,
            brightness: 1.0,
        })
        .add_plugins(DefaultPlugins.set(AssetPlugin {
            asset_folder: args.assets_dir().to_string_lossy().into_owned(),
            watch_for_changes: None,
        }))
        .add_plugins(EguiPlugin)
        .add_plugins(ViewControlsPlugin)
        .add_plugins(BakedGiPlugin::default())
        .add_systems(Startup, setup)
        .add_systems(Startup, bevy_view_controls_egui::simple_setup)
        .add_systems(Update, bevy_view_controls_egui::simple_view_controls)
        .add_systems(Update, rotate_ferris)
        .insert_resource(args)
        .run();
}

fn setup(mut commands: Commands, mut asset_server: ResMut<AssetServer>, args: Res<Args>) {
    for manifest in &args.manifest {
        let manifest: Manifest = ron::de::from_reader(&File::open(manifest).unwrap()).unwrap();
        // FIXME: Don't forget.
        let handles = manifest.load_all(&mut asset_server);
        println!("{:#?}", handles);
        mem::forget(handles);
    }

    commands
        .spawn(Camera3dBundle {
            transform: Transform::from_xyz(0.0, 0.0, 3.0).looking_at(Vec3::ZERO, Vec3::Y),
            ..Camera3dBundle::default()
        })
        .insert(ControllableCamera {
            target: Vec3A::ZERO,
            ..ControllableCamera::default()
        });

    for scene_path in &args.scene {
        if ["gltf", "glb"]
            .iter()
            .any(|extension| scene_path.extension() == Some(OsStr::new(extension)))
        {
            commands.spawn(SceneBundle {
                scene: asset_server.load(format!("{}#Scene0", scene_path.display())),
                transform: Transform::IDENTITY,
                ..SceneBundle::default()
            });
        } else {
            commands.spawn(DynamicSceneBundle {
                scene: asset_server.load(scene_path.to_string_lossy().into_owned()),
                transform: Transform::IDENTITY,
                ..DynamicSceneBundle::default()
            });
        };
    }

    // TODO: Make this configurable.
    commands
        .spawn(SceneBundle {
            scene: asset_server.load("Ferris.glb#Scene0"),
            transform: Transform::from_scale(Vec3::splat(12.5)),
            ..SceneBundle::default()
        })
        .insert(Name::new("Ferris"));

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

    /*
    let diffuse_reflection_probe_map = asset_server.load::<Image, _>("Sponza.diffuse.001.ktx2");
    let specular_reflection_probe_map = asset_server.load::<Image, _>("Sponza.specular.001.ktx2");
    commands
        .spawn(SpatialBundle::default())
        .insert(Name::new("ReflectionProbe"))
        .insert(ReflectionProbe {
            diffuse_map: diffuse_reflection_probe_map,
            specular_map: specular_reflection_probe_map,
        });
    */
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

impl Args {
    fn assets_dir(&self) -> PathBuf {
        match self.assets_dir {
            Some(ref assets_dir) => (*assets_dir).clone(),
            // FIXME: Should be the scene path root.
            None => PathBuf::from("Assets"),
        }
    }
}
