// bevy-baked-gi/Crates/bevy-baked-gi/examples/scene.rs

//! Renders one or more Bevy scenes in glTF or RON format, with baked global illumination applied.

use bevy::asset::{AssetPath, HandleId};
use bevy::core_pipeline::bloom::BloomSettings;
use bevy::core_pipeline::experimental::taa::TemporalAntiAliasBundle;
use bevy::core_pipeline::tonemapping::Tonemapping;
use bevy::ecs::system::EntityCommands;
use bevy::pbr::ScreenSpaceAmbientOcclusionBundle;
use bevy::prelude::{
    info, warn, Added, AmbientLight, App, AppTypeRegistry, AssetPlugin, AssetServer, Camera,
    Camera3dBundle, Changed, Children, Color, Commands, Component, Deref, DerefMut,
    DirectionalLight, Entity, Msaa, PluginGroup, PointLight, Query, ReflectComponent, Res, ResMut,
    Resource, Startup, Transform, Update, Vec3, With, World,
};
use bevy::reflect::ReflectRef;
use bevy::scene::{DynamicSceneBundle, SceneBundle, SceneInstance};
use bevy::DefaultPlugins;
use bevy_baked_gi::BakedGiPlugin;
use clap::Parser;
use std::collections::HashMap;
use std::ffi::OsStr;
use std::fs;
use std::mem;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

const SCENE_ROTATION_SPEED: f32 = 0.01;

/// Displays one or more Bevy scenes with global illumination applied.
#[derive(Parser, Resource)]
#[command(author, version, about)]
struct Args {
    /// The scenes to display, as `.glb`, `.gltf`, or `.scn.ron`.
    ///
    /// Multiple scenes can be provided, and if so then all scenes will be
    /// merged together and displayed at once.
    #[arg()]
    scene: Vec<PathBuf>,

    /// Scenes that will rotate.
    ///
    /// This is useful for testing reflection probes.
    #[arg(long)]
    rotating_scene: Vec<PathBuf>,

    /// The directory that all assets are relative to.
    ///
    /// If loading a scene emitted with `export-blender-gi`, this must be the
    /// same assets directory that was supplied to that tool.
    #[arg(short, long)]
    assets_dir: Option<PathBuf>,
}

#[derive(Resource, Default, Deref, DerefMut)]
struct AssetIds(HashMap<HandleId, PathBuf>);

#[derive(Component)]
struct Rotating;

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
        .add_plugins(BakedGiPlugin::default())
        .add_systems(Startup, setup)
        .add_systems(Update, rotate_scenes)
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
    let assets_dir = args.assets_dir();
    for maybe_dirent in WalkDir::new(&*assets_dir) {
        let Ok(dirent) = maybe_dirent else { continue };
        let Some(asset_path) = pathdiff::diff_paths(dirent.path(), &assets_dir) else { continue };
        let handle_id = HandleId::AssetPathId(AssetPath::new(asset_path.clone(), None).into());
        asset_ids.insert(handle_id, asset_path);
    }

    /*
    commands
        .spawn(Camera3dBundle {
            transform: Transform::from_xyz(80.0, 20.0, -3.0).looking_at(Vec3::ZERO, Vec3::Y),
            camera: Camera {
                hdr: true,
                ..Camera::default()
            },
            tonemapping: Tonemapping::TonyMcMapface,
            ..Camera3dBundle::default()
        })
        .insert(ScreenSpaceAmbientOcclusionBundle::default())
        .insert(TemporalAntiAliasBundle::default())
        .insert(BloomSettings::default());
    */

    for absolute_scene_path in &args.scene {
        let relative_scene_path = pathdiff::diff_paths(
            fs::canonicalize(absolute_scene_path).unwrap_or_else(|_| absolute_scene_path.clone()),
            &assets_dir,
        )
        .unwrap_or_else(|| (*absolute_scene_path).clone());
        spawn_scene(&mut commands, &mut asset_server, &relative_scene_path);
    }

    for absolute_scene_path in &args.rotating_scene {
        let relative_scene_path = pathdiff::diff_paths(
            fs::canonicalize(absolute_scene_path).unwrap_or_else(|_| absolute_scene_path.clone()),
            &assets_dir,
        )
        .unwrap_or_else(|| (*absolute_scene_path).clone());
        spawn_scene(&mut commands, &mut asset_server, &relative_scene_path).insert(Rotating);
    }
}

fn spawn_scene<'w, 's, 'a>(
    commands: &'a mut Commands<'w, 's>,
    asset_server: &mut AssetServer,
    scene_path: &Path,
) -> EntityCommands<'w, 's, 'a> {
    if ["gltf", "glb"]
        .iter()
        .any(|extension| scene_path.extension() == Some(OsStr::new(extension)))
    {
        commands.spawn(SceneBundle {
            scene: asset_server.load(format!("{}#Scene0", scene_path.display())),
            transform: Transform::IDENTITY,
            ..SceneBundle::default()
        })
    } else {
        commands.spawn(DynamicSceneBundle {
            scene: asset_server.load(scene_path.to_string_lossy().into_owned()),
            transform: Transform::IDENTITY,
            ..DynamicSceneBundle::default()
        })
    }
}

fn rotate_scenes(mut query: Query<&mut Transform, With<Rotating>>) {
    for mut transform in query.iter_mut() {
        transform.rotate_axis(Vec3::Y, SCENE_ROTATION_SPEED);
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
        let relative_assets_dir = match self.assets_dir {
            Some(ref assets_dir) => assets_dir,
            // FIXME: Should be the scene path root.
            None => match self.scene.get(0).and_then(|path| path.parent()) {
                Some(assets_dir) => assets_dir,
                None => Path::new("Assets"),
            },
        };
        match fs::canonicalize(relative_assets_dir) {
            Err(_) => PathBuf::from("Assets"),
            Ok(assets_dir) => assets_dir,
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
                        warn!(
                            "Asset with ID {:?} not found in assets directory",
                            handle_id
                        );
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
