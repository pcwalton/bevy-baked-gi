// bevy-irradiance-volumes/Crates/bevy-irradiance-volumes/examples/cornell-box.rs

use bevy::asset::FileAssetIo;
use bevy::math::Vec3A;
use bevy::prelude::{
    Added, AmbientLight, App, AssetPlugin, AssetServer, Assets, Camera3dBundle, Color, Commands,
    Entity, Handle, IVec3, Image, Mat4, Plugin, PluginGroup, Query, Res, ResMut, StandardMaterial,
    Startup, Transform, Update, Vec3, Vec4,
};
use bevy::render::texture::{CompressedImageFormats, ImageType};
use bevy::scene::SceneBundle;
use bevy::DefaultPlugins;
use bevy_egui::EguiPlugin;
use bevy_irradiance_volumes::{GridData, IrradianceVolumeMaterial, IrradianceVolumesPlugin};
use bevy_view_controls_egui::{ControllableCamera, ViewControlsPlugin};
use futures;
use std::env;
use std::fs::File;
use std::path::{Path, PathBuf};

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
}

fn replace_standard_materials(
    mut commands: Commands,
    query: Query<(Entity, &Handle<StandardMaterial>)>,
    asset_server: Res<AssetServer>,
    standard_materials: ResMut<Assets<StandardMaterial>>,
    mut irradiance_volume_materials: ResMut<Assets<IrradianceVolumeMaterial>>,
    mut images: ResMut<Assets<Image>>,
) {
    for (entity, standard_material) in query.iter() {
        let Some(standard_material) = standard_materials.get(standard_material) else { continue };

        let base_color = Vec4::from_slice(&standard_material.base_color.as_rgba_f32());

        let data = futures::executor::block_on(
            asset_server
                .asset_io()
                .load_path(Path::new("CornellBoxGrid.ktx2")),
        )
        .unwrap();
        let image = Image::from_buffer(
            &data,
            ImageType::Extension("ktx2"),
            CompressedImageFormats::NONE,
            false,
        )
        .unwrap();
        let irradiance_grid = images.add(image);

        commands
            .entity(entity)
            .remove::<Handle<StandardMaterial>>()
            .insert(irradiance_volume_materials.add(IrradianceVolumeMaterial {
                grid_data: GridData {
                    matrix: Mat4::from_scale(Vec3::splat(0.2294848)),
                    resolution: IVec3::splat(8),
                    offset: 1,
                    corner: Vec3::splat(-3.8128886),
                    increment_x: Vec3::new(1.089397, 0.0, 0.0),
                    increment_y: Vec3::new(0.0, 1.089397, 0.0),
                    level_bias: 1.0,
                    increment_z: Vec3::new(0.0, 0.0, 1.089397),
                },
                irradiance_grid,
                base_color,
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
