// bevy-irradiance-volumes/Crates/bevy-irradiance-volumes/examples/scene.rs

use bevy::asset::FileAssetIo;
use bevy::core_pipeline::bloom::BloomSettings;
use bevy::core_pipeline::experimental::taa::TemporalAntiAliasBundle;
use bevy::core_pipeline::tonemapping::Tonemapping;
use bevy::math::Vec3A;
use bevy::pbr::ScreenSpaceAmbientOcclusionBundle;
use bevy::prelude::{
    AmbientLight, App, AssetPlugin, AssetServer, Camera, Camera3dBundle, Changed, Color, Commands,
    DirectionalLight, Msaa, Name, Plugin, PluginGroup, PointLight, Query, Res, ResMut, Resource,
    Startup, Transform, Update, Vec3,
};
use bevy::scene::{DynamicSceneBundle, SceneBundle};
use bevy::DefaultPlugins;
use bevy_baked_gi::{BakedGiPlugin, Manifest};
use bevy_egui::EguiPlugin;
use bevy_view_controls_egui::{ControllableCamera, ViewControlsPlugin};
use clap::Parser;
use std::env;
use std::ffi::OsStr;
use std::fs::File;
use std::mem;
use std::path::PathBuf;

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
        .add_systems(Update, apply_shadows_to_lights)
        .insert_resource(args)
        .insert_resource(AmbientLight {
            brightness: 0.0,
            ..AmbientLight::default()
        })
        .insert_resource(Msaa::Off)
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
            camera: Camera {
                hdr: true,
                ..Camera::default()
            },
            tonemapping: Tonemapping::TonyMcMapface,
            ..Camera3dBundle::default()
        })
        .insert(ControllableCamera {
            target: Vec3A::ZERO,
            ..ControllableCamera::default()
        })
        .insert(ScreenSpaceAmbientOcclusionBundle::default())
        .insert(TemporalAntiAliasBundle::default())
        .insert(BloomSettings::default());

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

fn apply_shadows_to_lights(
    mut directional_lights: Query<&mut DirectionalLight, Changed<DirectionalLight>>,
    mut point_lights: Query<&mut PointLight, Changed<PointLight>>,
) {
    for mut directional_light in directional_lights.iter_mut() {
        directional_light.shadows_enabled = true;
    }

    for mut point_light in point_lights.iter_mut() {
        point_light.shadows_enabled = true;
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
