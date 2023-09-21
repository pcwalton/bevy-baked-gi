// bevy-baked-gi/Crates/bevy-baked-gi/src/reflection_probes.rs

use crate::irradiance_volumes::GiPbrMaterial;
use bevy::prelude::{Commands, Component, Entity, GlobalTransform, Handle, Image, Query, With};
use bevy::reflect::Reflect;
use bevy::render::extract_component::ExtractComponent;
use bevy::render::render_resource::AsBindGroup;

/// The component that defines a reflection probe.
#[derive(Component, Clone, Default, Reflect, Debug)]
pub struct ReflectionProbe {
    pub diffuse_map: Handle<Image>,
    pub specular_map: Handle<Image>,
}

/// Which reflection probe is to be applied to this entity this frame.
#[derive(Component, Clone, Default, AsBindGroup, Reflect, Debug, ExtractComponent)]
pub struct AppliedReflectionProbe {
    #[texture(0, dimension = "cube")]
    #[sampler(2)]
    pub diffuse_map: Option<Handle<Image>>,
    #[texture(1, dimension = "cube")]
    pub specular_map: Option<Handle<Image>>,
}

pub fn apply_reflection_probes(
    mut commands: Commands,
    reflection_probes_query: Query<(&ReflectionProbe, &GlobalTransform)>,
    mut targets_query: Query<(Entity, &GlobalTransform), With<Handle<GiPbrMaterial>>>,
) {
    // FIXME: Check distance, fill in appropriately.
    let Some((reflection_probe, _)) = reflection_probes_query.iter().next() else { return };

    // TODO: It would probably be nice to switch to building an AABB tree or something if there are
    // a *lot* of reflection probes, to avoid worst-case O(n²) behavior.
    for (target, _) in targets_query.iter_mut() {
        commands.entity(target).insert(AppliedReflectionProbe {
            diffuse_map: Some(reflection_probe.diffuse_map.clone()),
            specular_map: Some(reflection_probe.specular_map.clone()),
        });
    }
}
