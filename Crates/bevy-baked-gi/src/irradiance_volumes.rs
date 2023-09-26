// bevy-baked-gi/Crates/bevy-baked-gi/src/irradiance_volumes.rs

//! Baked voxel grids storing indirect light.

use crate::lightmaps::Lightmap;
use crate::{AabbExt, GiPbrMaterial};
use bevy::math::Vec3A;
use bevy::prelude::{
    Commands, Component, Entity, GlobalTransform, Handle, IVec3, Image, Mat4, Query,
    ReflectComponent, With, Without,
};
use bevy::reflect::{Reflect, TypeUuid};
use bevy::render::extract_component::ExtractComponent;
use bevy::render::primitives::Aabb;
use bevy::render::render_resource::{AsBindGroup, ShaderType};
use serde::{Deserialize, Serialize};

#[doc(hidden)]
pub const IRRADIANCE_GRID_BYTES_PER_SAMPLE: usize = 4;
#[doc(hidden)]
pub const IRRADIANCE_GRID_SAMPLES_PER_CELL: usize = 6;
#[doc(hidden)]
pub const IRRADIANCE_GRID_BYTES_PER_CELL: usize =
    IRRADIANCE_GRID_BYTES_PER_SAMPLE * IRRADIANCE_GRID_SAMPLES_PER_CELL;

/// The component that defines an irradiance volume.
#[derive(Clone, Component, Default, Reflect, Debug, TypeUuid)]
#[uuid = "0adffc01-d268-4441-a9ea-a236679590b0"]
#[reflect(Component)]
pub struct IrradianceVolume {
    pub meta: IrradianceVolumeMetadata,
    pub image: Handle<Image>,
}

#[derive(Clone, Default, Reflect, Debug, ShaderType, Serialize, Deserialize)]
pub struct IrradianceVolumeMetadata {
    /// Transforms a canonical voxel cube with corners at (0, 0, 0) and (1, 1,
    /// 1) to world space.  In other words, this matrix specifies a
    /// transformation from a cube whose side length is 1 and centered at (0.5,
    /// 0.5, 0.5), representing a *single* voxel (not the entire voxel grid), to
    /// the scene's world space.
    pub transform: Mat4,

    /// The size of the voxel grid, in voxels.
    pub resolution: IVec3,

    /// The LOD bias.
    pub level_bias: f32,
}

/// Stores information about the irradiance volume on this entity.
#[derive(Clone, Component, ExtractComponent, AsBindGroup, Default, Reflect)]
pub struct AppliedIrradianceVolume {
    #[uniform(0)]
    pub irradiance_volume_descriptor: IrradianceVolumeDescriptor,
    #[texture(1)]
    pub irradiance_volume_texture: Option<Handle<Image>>,
}

/// Information about the irradiance volume that will be used to illuminate this
/// entity.
///
/// This is stored on the [AppliedIrradianceVolume] component.
#[derive(Clone, Default, Reflect, Debug, ShaderType)]
pub struct IrradianceVolumeDescriptor {
    /// Metadata about the irradiance volume.
    pub meta: IrradianceVolumeMetadata,
    /// The position and size of the irradiance volume in the scene.
    pub transform: Mat4,
}

pub fn apply_irradiance_volumes(
    mut commands: Commands,
    irradiance_volume_query: Query<(&IrradianceVolume, &GlobalTransform)>,
    mut targets_query: Query<
        (Entity, &GlobalTransform, Option<&Aabb>),
        (With<Handle<GiPbrMaterial>>, Without<Lightmap>),
    >,
) {
    'outer: for (target, target_transform, maybe_aabb) in targets_query.iter_mut() {
        let center = match maybe_aabb {
            None => target_transform.translation(),
            Some(aabb) => target_transform.transform_point(aabb.center.into()),
        };

        for (irradiance_volume, irradiance_volume_transform) in &irradiance_volume_query {
            // FIXME: Cache the matrix inverse operation…
            let irradiance_volume_transform = irradiance_volume_transform.compute_matrix();
            let point = Vec3A::from(irradiance_volume_transform.inverse() * center.extend(1.0));
            if Aabb::centered_unit_cube().contains_point(point) {
                commands.entity(target).insert(AppliedIrradianceVolume {
                    irradiance_volume_descriptor: IrradianceVolumeDescriptor {
                        meta: irradiance_volume.meta.clone(),
                        transform: irradiance_volume_transform,
                    },
                    irradiance_volume_texture: Some(irradiance_volume.image.clone()),
                });
                continue 'outer;
            }
        }

        commands.entity(target).remove::<AppliedIrradianceVolume>();
    }
}

impl IrradianceVolumeMetadata {
    pub fn sample_count(&self) -> usize {
        self.resolution.x as usize * self.resolution.y as usize * self.resolution.z as usize
    }
}
