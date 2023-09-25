// bevy-baked-gi/Crates/bevy-baked-gi/src/reflection_probes.rs

//! Baked cubemap reflections.

use crate::{AabbExt, GiPbrMaterial};
use bevy::math::Vec3A;
use bevy::prelude::{
    Commands, Component, Entity, GlobalTransform, Handle, Image, Query, ReflectComponent, With,
};
use bevy::reflect::{Reflect, TypeUuid};
use bevy::render::extract_component::ExtractComponent;
use bevy::render::primitives::Aabb;
use bevy::render::render_resource::AsBindGroup;

/// The component that defines a baked reflection probe.
/// 
/// Any mesh with a GI PBR material located within the boundaries of a
/// reflection probe uses the diffuse and specular cubemaps stored here for
/// diffuse and specular reflections, respectively. These reflections reflect
/// static geometry only, so they won't reflect objects that move. The benefit
/// is that, because these reflections are precomputed, they're cheap at
/// runtime.
#[derive(Component, Clone, Default, Reflect, Debug, TypeUuid)]
#[uuid = "0fb4528e-7992-41cf-a4d9-445e1fcf055e"]
#[reflect(Component)]
pub struct ReflectionProbe {
    pub diffuse_map: Handle<Image>,
    pub specular_map: Handle<Image>,
}

/// Which reflection probe is to be applied to an entity this frame.
/// 
/// You don't need to manage these manually; the [apply_reflection_probe] system
/// automatically handles these.
#[derive(Component, Clone, Default, AsBindGroup, Reflect, Debug, ExtractComponent)]
#[reflect(Component)]
pub struct AppliedReflectionProbe {
    /// The diffuse map of the surrounding reflection probe, if applicable.
    #[texture(0, dimension = "cube")]
    #[sampler(2)]
    pub diffuse_map: Option<Handle<Image>>,
    /// The specular map of the surrounding reflection probe, if applicable.
    #[texture(1, dimension = "cube")]
    pub specular_map: Option<Handle<Image>>,
}

/// A system that determines which reflection probe to apply to each mesh.
pub fn apply_reflection_probes(
    mut commands: Commands,
    reflection_probes_query: Query<(&ReflectionProbe, &GlobalTransform)>,
    mut targets_query: Query<(Entity, &GlobalTransform), With<Handle<GiPbrMaterial>>>,
) {
    'outer: for (target, target_transform) in targets_query.iter_mut() {
        for (reflection_probe, reflection_probe_transform) in &reflection_probes_query {
            let point = Vec3A::from(
                reflection_probe_transform.compute_matrix().inverse()
                    * target_transform.translation().extend(1.0),
            );
            if Aabb::centered_unit_cube().contains_point(point) {
                commands.entity(target).insert(AppliedReflectionProbe {
                    diffuse_map: Some(reflection_probe.diffuse_map.clone()),
                    specular_map: Some(reflection_probe.specular_map.clone()),
                });
                continue 'outer;
            }
        }

        commands.entity(target).remove::<AppliedReflectionProbe>();
    }
}
