// bevy-irradiance-volumes/Crates/bevy-irradiance-volumes/src/lib.rs

#![allow(clippy::type_complexity)]

use bevy::asset::{AssetLoader, Error, LoadContext, LoadedAsset};
use bevy::math::ivec2;
use bevy::prelude::{
    info, AddAsset, App, AssetEvent, Assets, Changed, EventReader, FromWorld, GlobalTransform,
    Handle, IVec2, IVec3, Image, IntoSystemConfigs, Mat4, Material, MaterialPlugin, Or, Plugin,
    PostUpdate, Query, Res, ResMut, Resource, Vec3, Vec4, World,
};
use bevy::reflect::{Reflect, TypeUuid};
use bevy::render::render_resource::{AsBindGroup, ShaderRef, ShaderType};
use bevy::transform::TransformSystem;
use bevy::utils::BoxedFuture;
use image::{DynamicImage, ImageBuffer};
use serde::{Deserialize, Serialize};

#[doc(hidden)]
pub const IRRADIANCE_GRID_BYTES_PER_SAMPLE: usize = 4;
#[doc(hidden)]
pub const IRRADIANCE_GRID_SAMPLES_PER_CELL: usize = 6;
#[doc(hidden)]
pub const IRRADIANCE_GRID_BYTES_PER_CELL: usize =
    IRRADIANCE_GRID_BYTES_PER_SAMPLE * IRRADIANCE_GRID_SAMPLES_PER_CELL;

pub struct IrradianceVolumesPlugin;

/// The asset that defines an irradiance volume.
#[derive(Clone, Default, Reflect, Debug, TypeUuid, Serialize, Deserialize)]
#[uuid = "0adffc01-d268-4441-a9ea-a236679590b0"]
pub struct IrradianceVolume {
    pub meta: IrradianceVolumeMetadata,
    pub data: Vec<u8>,
}

#[derive(Clone, Default, Reflect, Debug, ShaderType, Serialize, Deserialize)]
pub struct IrradianceVolumeMetadata {
    pub resolution: IVec3,
    pub corner: Vec3,
    pub increment_x: Vec3,
    pub increment_y: Vec3,
    pub increment_z: Vec3,
    pub level_bias: f32,
}

pub struct IrradianceVolumeAssetLoader;

#[derive(Clone, Default, Reflect, AsBindGroup, TypeUuid, Debug)]
#[uuid = "d18d9aa6-5053-4cb4-8b59-a1b2d1e6b6db"]
pub struct StandardGiMaterial {
    // FIXME: This needs to be a separate component bridged to the shader, but I'm not sure how to
    // do that yet.
    #[uniform(0)]
    pub grid_data: IrradianceVolumeGpuData,
    #[texture(1, dimension = "2d")]
    pub irradiance_grid: Handle<Image>,
    #[uniform(2)]
    pub base_color: Vec4,
}

#[derive(Clone, Default, Reflect, Debug, ShaderType)]
pub struct IrradianceVolumeGpuData {
    pub meta: IrradianceVolumeMetadata,
    pub transform: Mat4,
    pub offset: i32,
}

#[derive(Resource, Clone)]
pub struct IrradianceGrid {
    texture: Option<Handle<Image>>,
    gpu_data: Vec<IrradianceVolumeGpuData>,
}

impl Plugin for IrradianceVolumesPlugin {
    fn build(&self, app: &mut App) {
        app.register_type::<IrradianceVolume>()
            .register_type::<StandardGiMaterial>()
            .register_type::<IrradianceVolumeMetadata>()
            .add_asset::<IrradianceVolume>()
            .add_asset_loader(IrradianceVolumeAssetLoader)
            .init_resource::<IrradianceGrid>()
            .add_plugins(MaterialPlugin::<StandardGiMaterial>::default())
            .add_systems(
                PostUpdate,
                update_irradiance_grid.after(TransformSystem::TransformPropagate),
            )
            .add_systems(
                PostUpdate,
                write_irradiance_grid_to_materials.after(update_irradiance_grid),
            );
    }
}

impl Material for StandardGiMaterial {
    fn fragment_shader() -> ShaderRef {
        // TODO: Use `include_bytes!` instead.
        "IrradianceVolume.wgsl".into()
    }
}

impl AssetLoader for IrradianceVolumeAssetLoader {
    fn load<'a>(
        &'a self,
        bytes: &'a [u8],
        load_context: &'a mut LoadContext,
    ) -> BoxedFuture<'a, Result<(), Error>> {
        Box::pin(async move {
            let irradiance_volume: IrradianceVolume = bincode::deserialize(bytes)?;
            load_context.set_default_asset(LoadedAsset::new(irradiance_volume));
            Ok(())
        })
    }

    fn extensions(&self) -> &[&str] {
        static EXTENSIONS: [&str; 1] = ["voxelgi.bincode"];
        &EXTENSIONS
    }
}

impl FromWorld for IrradianceGrid {
    fn from_world(_: &mut World) -> Self {
        IrradianceGrid {
            texture: None,
            gpu_data: vec![],
        }
    }
}

fn update_irradiance_grid(
    mut irradiance_grid: ResMut<IrradianceGrid>,
    irradiance_volume_assets: Res<Assets<IrradianceVolume>>,
    mut image_assets: ResMut<Assets<Image>>,
    mut irradiance_volume_asset_events: EventReader<AssetEvent<IrradianceVolume>>,
    mut irradiance_volumes_query: Query<
        (&Handle<IrradianceVolume>, &GlobalTransform),
        Or<(Changed<Handle<IrradianceVolume>>, Changed<GlobalTransform>)>,
    >,
) {
    if irradiance_volume_asset_events.into_iter().next().is_none()
        && irradiance_volumes_query.into_iter().next().is_none()
    {
        // Nothing to do this frame.
        return;
    }

    info!("Regenerating irradiance grid");

    let (mut current_offset, mut new_gpu_data, mut irradiance_volume_handles) = (0, vec![], vec![]);
    for (irradiance_volume_handle, transform) in irradiance_volumes_query.iter_mut() {
        let Some(irradiance_volume) =
            irradiance_volume_assets.get(irradiance_volume_handle) else { continue };

        new_gpu_data.push(IrradianceVolumeGpuData {
            meta: irradiance_volume.meta.clone(),
            transform: transform.compute_matrix(),
            offset: current_offset,
        });

        irradiance_volume_handles.push(irradiance_volume_handle.clone());

        let voxel_count = irradiance_volume.data.len() / IRRADIANCE_GRID_BYTES_PER_CELL;
        current_offset += voxel_count as i32;
        debug_assert_eq!(voxel_count, irradiance_volume.meta.sample_count());
    }

    // Nothing has been loaded yet; do nothing.
    if current_offset == 0 {
        return;
    }

    // FIXME: Pick this dynamically.
    let dest_width = 768;
    let dest_cells_per_row = dest_width as usize / 3;
    let dest_height = div_ceil(current_offset as _, dest_cells_per_row as _) * 2;

    let mut dest_grid_data =
        vec![0; dest_width as usize * dest_height as usize * IRRADIANCE_GRID_BYTES_PER_SAMPLE];
    let dest_stride = dest_width as usize * IRRADIANCE_GRID_BYTES_PER_SAMPLE;

    for (irradiance_volume_handle, irradiance_volume_gpu_data) in
        irradiance_volume_handles.iter().zip(new_gpu_data.iter())
    {
        let src_volume = irradiance_volume_assets
            .get(irradiance_volume_handle)
            .unwrap();

        for src_cell_index in 0..src_volume.meta.sample_count() {
            let dest_cell_index = irradiance_volume_gpu_data.offset as usize + src_cell_index;
            let dest_origin = ivec2(
                (dest_cell_index % dest_cells_per_row * 3) as i32,
                (dest_cell_index / dest_cells_per_row * 2) as i32,
            );

            for y in 0..2 {
                for x in 0..3 {
                    let dest_offset = ivec2(x, y);
                    let src_sample_index = dest_offset.y as usize * 3
                        + dest_offset.x as usize
                        + src_cell_index * IRRADIANCE_GRID_SAMPLES_PER_CELL;
                    let src_byte_offset = src_sample_index * IRRADIANCE_GRID_BYTES_PER_SAMPLE;

                    let texel = &src_volume.data
                        [src_byte_offset..(src_byte_offset + IRRADIANCE_GRID_BYTES_PER_SAMPLE)];
                    put_texel(
                        &mut dest_grid_data,
                        texel.try_into().unwrap(),
                        dest_origin + dest_offset,
                        dest_stride,
                    );
                }
            }
        }
    }

    let grid_image = Image::from_dynamic(
        DynamicImage::ImageRgba8(
            ImageBuffer::from_vec(dest_width, dest_height, dest_grid_data).unwrap(),
        ),
        /*is_srgb=*/ false,
    );

    let grid_image_handle = image_assets.add(grid_image);

    *irradiance_grid = IrradianceGrid {
        texture: Some(grid_image_handle),
        gpu_data: new_gpu_data,
    }
}

fn write_irradiance_grid_to_materials(
    irradiance_grid: Res<IrradianceGrid>,
    mut materials_assets: ResMut<Assets<StandardGiMaterial>>,
) {
    let Some(ref texture) = irradiance_grid.texture else { return };
    // FIXME: Check distance, fill in appropriately.
    let Some(gpu_data) = irradiance_grid.gpu_data.get(0) else { return };

    for (_, material) in materials_assets.iter_mut() {
        material.irradiance_grid = (*texture).clone();
        material.grid_data = gpu_data.clone();
    }
}

fn div_ceil(a: u32, b: u32) -> u32 {
    (a + b - 1) / b
}

impl IrradianceVolumeMetadata {
    pub fn sample_count(&self) -> usize {
        self.resolution.x as usize * self.resolution.y as usize * self.resolution.z as usize
    }
}

/// `stride` is in bytes.
fn put_texel(buffer: &mut [u8], texel: [u8; 4], p: IVec2, stride: usize) {
    let offset = p.y as usize * stride + p.x as usize * IRRADIANCE_GRID_BYTES_PER_SAMPLE;
    buffer[offset..offset + 4].copy_from_slice(&texel)
}
