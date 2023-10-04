// bevy-baked-gi/Crates/bevy-baked-gi/src/irradiance_volumes.rs

//! Baked voxel grids storing indirect light.

use crate::lightmaps::Lightmap;
use crate::{AabbExt, GiPbrMaterial};
use bevy::asset::{AssetLoader, Error as AnyhowError, LoadContext, LoadedAsset};
use bevy::math::Vec3A;
use bevy::prelude::{
    Commands, Component, Entity, GlobalTransform, Handle, IVec3, Image, Mat4, Query,
    ReflectComponent, With, Without,
};
use bevy::reflect::{Reflect, TypeUuid};
use bevy::render::extract_component::ExtractComponent;
use bevy::render::primitives::Aabb;
use bevy::render::render_resource::{AsBindGroup, ShaderType};
use bevy::render::texture::{CompressedImageFormats, ImageType};
use bevy::utils::BoxedFuture;
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
    /// General information about this irradiance volume, such as its
    /// resolution.
    pub meta: IrradianceVolumeMetadata,

    /// The actual irradiance volume data.
    ///
    /// You can generate this with `export-blender-gi`.
    pub image: Handle<Image>,
}

/// General information about each irradiance volume, such as its resolution.
#[derive(Clone, Default, Reflect, Debug, ShaderType, Serialize, Deserialize)]
pub struct IrradianceVolumeMetadata {
    /// Transforms a canonical voxel cube with corners at (0, 0, 0) and (1, 1,
    /// 1) to world space.  In other words, this matrix specifies a
    /// transformation from a cube whose side length is 1 and centered at (0.5,
    /// 0.5, 0.5), representing a *single* voxel (not the entire voxel grid), to
    /// the scene's world space.
    pub transform: Mat4,

    /// The above matrix, inverted. That is, a transformation from world space
    /// to voxel space.
    pub inverse_transform: Mat4,

    /// The size of the voxel grid, in voxels.
    pub resolution: IVec3,
}

/// Stores information about the irradiance volume on this entity so that the
/// shader can access it.
///
/// You don't need to add this component yourself; the
/// [apply_irradiance_volumes] system automatically detects and applies it to
/// entities that contain GI PBR materials as appropriate.
#[derive(Clone, Component, ExtractComponent, AsBindGroup, Default, Reflect)]
pub struct AppliedIrradianceVolume {
    /// Metadata about the irradiance volume.
    #[uniform(0)]
    pub irradiance_volume_descriptor: IrradianceVolumeDescriptor,

    /// A handle to the actual irradiance volume texture.
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

/// An asset loader for the data texture ending in `.voxelgi.ktx2`.
///
/// This is necessary because Bevy's built-in texture loader loads in sRGB, which will corrupt the
/// irradiance volume.
pub struct IrradianceVolumeTextureLoader;

/// A system that determines which irradiance volumes apply to each object and assigns
/// [AppliedIrradianceVolume] components to affected objects.
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
    /// The total number of voxels present in the irradiance volume.
    pub fn sample_count(&self) -> usize {
        self.resolution.x as usize * self.resolution.y as usize * self.resolution.z as usize
    }
}

impl AssetLoader for IrradianceVolumeTextureLoader {
    fn load<'a>(
        &'a self,
        bytes: &'a [u8],
        load_context: &'a mut LoadContext,
    ) -> BoxedFuture<'a, Result<(), AnyhowError>> {
        Box::pin(async move {
            let dyn_img = Image::from_buffer(
                bytes,
                ImageType::Extension("ktx2"),
                CompressedImageFormats::empty(),
                /*is_srgb=*/ false,
            )?;

            load_context.set_default_asset(LoadedAsset::new(dyn_img));
            Ok(())
        })
    }

    fn extensions(&self) -> &[&str] {
        &["voxelgi.ktx2"]
    }
}
