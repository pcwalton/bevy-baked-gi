// bevy-baked-gi/Crates/bevy-baked-gi/examples/scene.rs

//! Renders one or more Bevy scenes in glTF or RON format, with baked global illumination applied.

use bevy::asset::{AssetPath, HandleId};
use bevy::core_pipeline::bloom::BloomSettings;
use bevy::core_pipeline::experimental::taa::TemporalAntiAliasBundle;
use bevy::core_pipeline::tonemapping::Tonemapping;
use bevy::math::Vec3A;
use bevy::pbr::ScreenSpaceAmbientOcclusionBundle;
use bevy::prelude::{
    info, warn, Added, AmbientLight, App, AppTypeRegistry, AssetPlugin, AssetServer, Camera,
    Camera3dBundle, Changed, Children, Color, Commands, Deref, DerefMut, DirectionalLight, Entity,
    Msaa, Name, PluginGroup, PointLight, Query, ReflectComponent, Res, ResMut, Resource, Startup,
    Transform, Update, Vec3, World,
};
use bevy::reflect::ReflectRef;
use bevy::scene::{DynamicSceneBundle, SceneBundle, SceneInstance};
use bevy::DefaultPlugins;
use bevy_baked_gi::{BakedGiPlugin, Manifest};
use bevy_egui::EguiPlugin;
use bevy_view_controls_egui::{ControllableCamera, ViewControlsPlugin};
use clap::Parser;
use std::collections::HashMap;
use std::ffi::OsStr;
use std::fs::File;
use std::mem;
use std::path::PathBuf;
use walkdir::WalkDir;

const FERRIS_ROTATION_SPEED: f32 = 0.01;

/// Displays one or more Bevy scenes with global illumination applied.
#[derive(Parser, Resource)]
#[command(author, version, about)]
struct Args {
    /// The scenes to display.
    ///
    /// Multiple scenes can be provided, and if so then all scenes will be
    /// merged together and displayed at once.
    #[arg()]
    scene: Vec<PathBuf>,

    /// The path to the manifest emitted by `export-blender-gi`.
    ///
    /// All assets within the manifest will be loaded.
    #[arg(short, long)]
    manifest: Vec<PathBuf>,

    /// The directory that all assets are relative to.
    ///
    /// If loading a scene emitted with `export-blender-gi`, this must be the
    /// same assets directory that was supplied to that tool.
    #[arg(short, long)]
    assets_dir: Option<PathBuf>,
}

#[derive(Resource, Default, Deref, DerefMut)]
struct AssetIds(HashMap<HandleId, PathBuf>);

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
        .add_systems(Update, get_handle_ids_from_new_entities)
        .init_resource::<AssetIds>()
        .insert_resource(args)
        .insert_resource(AmbientLight {
            brightness: 0.0,
            ..AmbientLight::default()
        })
        .insert_resource(Msaa::Off)
        .run();
}

fn setup(
    mut commands: Commands,
    mut asset_server: ResMut<AssetServer>,
    args: Res<Args>,
    mut asset_ids: ResMut<AssetIds>,
) {
    for maybe_dirent in WalkDir::new(args.assets_dir()) {
        let Ok(dirent) = maybe_dirent else { continue };
        let asset_path = PathBuf::from_iter(dirent.path().components().skip(1));
        let handle_id = HandleId::AssetPathId(AssetPath::new(asset_path.clone(), None).into());
        asset_ids.insert(handle_id, asset_path);
    }

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

fn rotate_ferris(mut query: Query<(&Name, &mut Transform)>) {
    for (name, mut transform) in query.iter_mut() {
        if &**name == "Ferris" {
            transform.rotate_axis(Vec3::Y, FERRIS_ROTATION_SPEED);
        }
    }
}

/// Applies shadow maps to every directional and point light in the scene.
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

fn get_handle_ids_from_new_entities(
    mut commands: Commands,
    query: Query<Entity, Added<SceneInstance>>,
) {
    for root in query.iter() {
        commands.add(move |world: &mut World| {
            let type_registry = (*world.resource::<AppTypeRegistry>()).clone();

            let (mut worklist, mut to_load) = (vec![root], vec![]);
            while let Some(parent) = worklist.pop() {
                if let Some(kids) = world.entity(parent).get::<Children>() {
                    worklist.extend(kids.iter().cloned());
                }

                let parent = world.entity(parent);
                for component_id in parent.archetype().components() {
                    let Some(component_info) =
                        world.components().get_info(component_id) else { continue };
                    let Some(component_type_id) = component_info.type_id() else { continue };

                    let type_registry = type_registry.read();
                    let Some(component_type_registration) =
                        type_registry.get(component_type_id) else { continue };
                    let Some(reflect_component) =
                        component_type_registration.data::<ReflectComponent>() else { continue };
                    let Some(reflect) = reflect_component.reflect(parent) else { continue };

                    let mut worklist = vec![reflect];
                    while let Some(reflect) = worklist.pop() {
                        if let Some(handle_id) = reflect.downcast_ref::<HandleId>() {
                            to_load.push(*handle_id);
                            continue;
                        }

                        match reflect.reflect_ref() {
                            ReflectRef::Struct(reflect_struct) => {
                                worklist.extend(reflect_struct.iter_fields());
                            }
                            ReflectRef::TupleStruct(reflect_tuple_struct) => {
                                worklist.extend(reflect_tuple_struct.iter_fields());
                            }
                            ReflectRef::Tuple(reflect_tuple) => {
                                worklist.extend(reflect_tuple.iter_fields());
                            }
                            ReflectRef::List(reflect_list) => {
                                worklist.extend(reflect_list.iter());
                            }
                            ReflectRef::Array(reflect_array) => {
                                worklist.extend(reflect_array.iter());
                            }
                            ReflectRef::Map(reflect_map) => {
                                worklist.extend(reflect_map.iter().map(|(_, value)| value));
                            }
                            ReflectRef::Enum(reflect_enum) => {
                                worklist
                                    .extend(reflect_enum.iter_fields().map(|field| field.value()));
                            }
                            ReflectRef::Value(_) => {}
                        }
                    }
                }
            }

            for handle_id in to_load {
                let asset_ids = world.resource::<AssetIds>();
                let asset_path = match asset_ids.get(&handle_id) {
                    Some(asset_path) => (*asset_path).clone(),
                    None => {
                        warn!("Asset with ID {:?} not found in assets directory", handle_id);
                        continue;
                    }
                };

                let asset_server = world.resource::<AssetServer>();
                let strong_handle = asset_server.load_untyped(&*asset_path);

                // FIXME: Is this the right thing to do?
                mem::forget(strong_handle);

                info!(
                    "Loading asset {:?} at path {}",
                    handle_id,
                    asset_path.display()
                );
            }
        });
    }
}
