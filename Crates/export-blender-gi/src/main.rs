// bevy-baked-gi/Crates/export-blender-gi/src/main.rs

//! Exports global illumination from a Blender file into a Bevy scene.

use anyhow::Result as AnyhowResult;
use bevy::app::AppExit;
use bevy::prelude::{
    info, AddAsset, App, AppTypeRegistry, AssetPlugin, AssetServer, ComputedVisibility,
    EventWriter, GlobalTransform, Handle, ImagePlugin, Mesh, PostStartup, Res, ResMut, Resource,
    Shader, SpatialBundle, Transform, World,
};
use bevy::reflect::{ReflectSerialize, TypePath, TypeUuid};
use bevy::render::view::ViewPlugin;
use bevy::scene::DynamicScene;
use bevy::MinimalPlugins;
use bevy_baked_gi::irradiance_volumes::{
    IrradianceVolume, IrradianceVolumeMetadata, IRRADIANCE_GRID_BYTES_PER_CELL,
};
use bevy_baked_gi::reflection_probes::ReflectionProbe;
use blend::{Blend, Instance};
use byteorder::{LittleEndian, WriteBytesExt};
use clap::{Parser, ValueEnum};
use glam::{ivec2, vec4, IVec2, IVec3, Mat4, Vec3, Vec3Swizzles, Vec4};
use ibllib_bindings::{
    IBLLib_OutputFormat_B9G9R9E5_UFLOAT, IBLLib_OutputFormat_R16G16B16A16_SFLOAT,
    IBLLib_OutputFormat_R32G32B32A32_SFLOAT, IBLLib_OutputFormat_R8G8B8A8_UNORM,
};
use reflection_probes::ReflectionProbeExtractor;
use ruzstd::decoding::block_decoder::BlockHeaderReadError;
use ruzstd::frame::ReadFrameHeaderError;
use ruzstd::frame_decoder::FrameDecoderError;
use ruzstd::{BlockDecodingStrategy, FrameDecoder};
use serde::{Deserialize, Serialize};
use std::ffi::OsStr;
use std::fmt::Debug;
use std::fs::{self, File};
use std::io::{ErrorKind, Read, Seek, SeekFrom, Write};
use std::mem;
use std::path::{Path, PathBuf};
use std::process;
use std::{array, env};
use vk2dfd::VkFormat;

#[allow(non_upper_case_globals, non_camel_case_types, unused)]
mod ibllib_bindings;
mod reflection_probes;

const IRRADIANCE_GRID_BYTES_PER_SAMPLE: usize = 4;
const IRRADIANCE_GRID_SAMPLES_PER_CELL: usize = 6;

const SAMPLE_COUNT: u32 = 1024;
const LOD_BIAS: f32 = 0.0;

static BEVY_AXES_TO_BLENDER_AXES: Mat4 = Mat4::from_cols(
    vec4(1.0, 0.0, 0.0, 0.0),
    vec4(0.0, 0.0, -1.0, 0.0),
    vec4(0.0, 1.0, 0.0, 0.0),
    vec4(0.0, 0.0, 0.0, 1.0),
);

static ZSTD_MAGIC_NUMBER: [u8; 4] = [0x28, 0xb5, 0x2f, 0xfd];

/// Exports global illumination from a Blender file into a Bevy scene.
#[derive(Parser, Resource, Clone)]
#[command(author, version, about)]
struct Args {
    /// The input `.blend` file to unpack.
    #[arg()]
    input: PathBuf,

    /// The default path to the assets directory.
    ///
    /// This is the path to the assets directory for the Bevy project that the
    /// assets needed for global illumination will be added to, if not otherwise
    /// specified by a more specific directory for that asset (e.g. by the
    /// `reflection-probes-dir` option).
    ///
    /// This is needed in order for `export-blender-gi` to refer to each asset
    /// with the correct [HandleId], since handle IDs are hashed versions of
    /// asset paths, and asset paths are relative to the assets directory.
    #[arg(short, long)]
    assets_dir: Option<PathBuf>,

    /// The path to the directory where Bevy scenes will be stored.
    #[arg(long)]
    scenes_dir: Option<PathBuf>,

    /// The path to the directory where irradiance volume textures will be stored.
    #[arg(long)]
    irradiance_volumes_dir: Option<PathBuf>,

    /// The path to the directory where reflection probe cubemaps will be stored.
    #[arg(long)]
    reflection_probes_dir: Option<PathBuf>,

    /// The format to output reflection probes in.
    #[arg(short = 'f', long, value_enum, default_value_t = TextureFormat::B9G9R9E5Ufloat)]
    reflection_probe_format: TextureFormat,

    /// The number of mipmap levels diffuse reflection probes should have.
    ///
    /// If not specified, a default value is chosen.
    #[arg(long)]
    diffuse_reflection_probe_mipmap_count: Option<u32>,

    /// The pixel size on each side of the diffuse reflection probes.
    ///
    /// It may be desirable to store them at low res because they're blurry.
    #[arg(long)]
    diffuse_reflection_probe_size: Option<u32>,

    /// Uses proposed upstream Bevy types where possible.
    #[arg(short, long)]
    upstream: bool,
}

struct CubemapPaths {
    diffuse: PathBuf,
    specular: PathBuf,
}

#[derive(Clone, Copy, PartialEq, Debug, ValueEnum)]
#[repr(i32)]
enum TextureFormat {
    R32G32B32A32Sfloat = IBLLib_OutputFormat_R32G32B32A32_SFLOAT,
    R16G16B16A16Sfloat = IBLLib_OutputFormat_R16G16B16A16_SFLOAT,
    B9G9R9E5Ufloat = IBLLib_OutputFormat_B9G9R9E5_UFLOAT,
    R8G8B8A8Unorm = IBLLib_OutputFormat_R8G8B8A8_UNORM,
}

/// The component that defines an irradiance volume.
#[derive(Clone, Default, Debug, Serialize, Deserialize)]
struct UpstreamBevyIrradianceVolume {
    /// Transforms a canonical voxel cube with corners at (0, 0, 0) and (1, 1,
    /// 1) to world space.  In other words, this matrix specifies a
    /// transformation from a cube whose side length is 1 and centered at (0.5,
    /// 0.5, 0.5), representing a *single* voxel (not the entire voxel grid), to
    /// the scene's world space.
    pub transform: Transform,

    /// The size of the voxel grid, in voxels.
    pub resolution: IVec3,

    /// The voxel grid data, stored as 32-bit floating point RGBA.
    /// TODO(pcwalton): Switch to RGB9e5.
    pub data: Vec<Vec4>,
}

fn main() {
    let args = Args::parse();

    let mut bevy_app = App::new();
    bevy_app
        .add_plugins(MinimalPlugins)
        .add_plugins(AssetPlugin {
            asset_folder: args.assets_dir().to_string_lossy().into_owned(),
            watch_for_changes: None,
        })
        .add_asset::<Shader>()
        .add_asset::<Mesh>()
        .add_plugins(ViewPlugin)
        .add_plugins(ImagePlugin::default())
        .add_asset::<IrradianceVolume>()
        .insert_resource(args)
        .register_type::<Transform>()
        .register_type::<GlobalTransform>()
        .register_type::<ReflectionProbe>()
        .register_type::<IrradianceVolume>()
        .register_type_data::<Transform, ReflectSerialize>()
        .add_systems(PostStartup, go);

    bevy_app.run();
}

fn go(
    args: Res<Args>,
    type_registry: Res<AppTypeRegistry>,
    mut asset_server: ResMut<AssetServer>,
    mut exit_writer: EventWriter<AppExit>,
) {
    let blend_file = File::open(&args.input)
        .unwrap_or_else(|err| die(format!("Failed to open the Blender file: {:?}", err)));

    let assets_dir = args.assets_dir();
    let filename = args.input.file_stem().expect("No file stem found");
    let blend_data = decompress_blender_file(blend_file);

    // TODO: Decompress with `zlib` if necessary.
    let blend = Blend::new(&blend_data[..])
        .unwrap_or_else(|err| die(format!("Failed to parse the Blender file: {:?}", err)));

    let blender_scene = blend
        .instances_with_code(*b"SC")
        .next()
        .unwrap_or_else(|| die("The Blender file didn't find a scene."));
    let eevee = blender_scene.get("eevee");
    let light_cache_data = eevee.get("light_cache_data");

    let mut world = World::new();
    world.insert_resource((*type_registry).clone());

    extract_irradiance_volumes(
        &mut world,
        &mut asset_server,
        &light_cache_data,
        &args.irradiance_volumes_dir(),
        &assets_dir,
        filename,
        args.upstream,
    );

    ReflectionProbeExtractor {
        light_cache_data: &light_cache_data,
        reflection_probes_dir: &args.reflection_probes_dir(),
        assets_dir: &assets_dir,
        filename,
        reflection_probe_format: args.reflection_probe_format,
        diffuse_reflection_probe_mipmap_count: args.diffuse_reflection_probe_mipmap_count,
        diffuse_reflection_probe_size: args.diffuse_reflection_probe_size,
    }
    .extract_reflection_probes(&mut world, &mut asset_server);

    // Write scene.
    let bevy_scene = DynamicScene::from_world(&world);
    let scene_output_path = args
        .scenes_dir()
        .join(format!("{}.scn.ron", filename.to_string_lossy()));
    let serialized_scene = bevy_scene
        .serialize_ron(world.resource::<AppTypeRegistry>())
        .unwrap_or_else(|err| die(format!("Failed to serialize the Bevy scene: {:?}", err)));
    fs::write(scene_output_path, serialized_scene.as_bytes()).unwrap_or_else(|err| {
        die(format!(
            "Failed to write the serialized Bevy scene: {:?}",
            err
        ))
    });

    exit_writer.send(AppExit);
}

fn die(message: impl AsRef<str>) -> ! {
    eprintln!("Error: {}", message.as_ref());
    process::exit(1)
}

fn die_with_zstd_frame_decoder_error(err: impl Debug) -> ! {
    die(format!(
        "Failed to decode the Zstandard-compressed Blender file: {:?}",
        err
    ));
}

impl Args {
    fn assets_dir(&self) -> PathBuf {
        if let Some(ref assets_dir) = self.assets_dir {
            return assets_dir.to_owned();
        }
        if let Ok(current_dir) = env::current_dir() {
            return current_dir;
        }
        if let Some(parent_dir) = self.input.parent() {
            return parent_dir.to_owned();
        }
        die("Couldn't find a suitable output directory");
    }

    fn scenes_dir(&self) -> PathBuf {
        self.scenes_dir.clone().unwrap_or_else(|| self.assets_dir())
    }

    fn irradiance_volumes_dir(&self) -> PathBuf {
        self.irradiance_volumes_dir
            .clone()
            .unwrap_or_else(|| self.assets_dir())
    }

    fn reflection_probes_dir(&self) -> PathBuf {
        self.reflection_probes_dir
            .clone()
            .unwrap_or_else(|| self.assets_dir())
    }
}

fn extract_irradiance_volumes(
    world: &mut World,
    asset_server: &mut AssetServer,
    light_cache_data: &Instance,
    irradiance_volumes_dir: &Path,
    assets_dir: &Path,
    filename: &OsStr,
    using_upstream_types: bool,
) {
    let grid_texture = light_cache_data.get("grid_tx");
    let grid_texture_data = grid_texture.get_u8_vec("data");

    let grid_dimensions = IVec3::from_slice(&grid_texture.get_i32_vec("tex_size"));
    let cells_per_row = grid_dimensions.x / 3;
    let grid_stride = grid_dimensions.x as usize * IRRADIANCE_GRID_BYTES_PER_SAMPLE;

    for (irradiance_volume_index, grid_data) in light_cache_data.get_iter("grid_data").enumerate() {
        let resolution = IVec3::from_slice(&grid_data.get_i32_vec("resolution"));

        // There seems to always be a dummy irradiance volume present with a resolution of zero.
        // Ignore it.
        if resolution.x == 0 {
            continue;
        }

        let transform = BEVY_AXES_TO_BLENDER_AXES
            * Mat4::from_cols(
                Vec3::from_slice(&grid_data.get_f32_vec("increment_x")).extend(0.0),
                Vec3::from_slice(&grid_data.get_f32_vec("increment_y")).extend(0.0),
                Vec3::from_slice(&grid_data.get_f32_vec("increment_z")).extend(0.0),
                Vec3::from_slice(&grid_data.get_f32_vec("corner")).extend(1.0),
            );

        let meta = IrradianceVolumeMetadata {
            transform,
            inverse_transform: transform.inverse(),
            resolution,
        };

        let offset = grid_data.get_i32("offset");

        // Extract the relevant portion of the first layer of the texture. (The rest of the texture
        // contains shadow maps used for visibility, which we don't care about presently.)

        let sample_count: usize = meta.sample_count();
        let mut grid_sample_data =
            Vec::with_capacity(sample_count * IRRADIANCE_GRID_BYTES_PER_CELL);
        for sample_index in 0..(sample_count as i32) {
            let src_sample_index = sample_index + offset;
            let origin = ivec2(
                src_sample_index % cells_per_row * 3,
                src_sample_index / cells_per_row * 2,
            );

            for blender_offset in [
                ivec2(0, 0), // Bevy +X, Blender +X,
                ivec2(2, 0), // Bevy +Y, Blender +Z
                ivec2(1, 1), // Bevy +Z, Blender -Y
                ivec2(0, 1), // Bevy -X, Blender -X
                ivec2(2, 1), // Bevy -Y, Blender -Z
                ivec2(1, 0), // Bevy -Z, Blender +Y
            ] {
                grid_sample_data.extend_from_slice(&get_texel(
                    &grid_texture_data,
                    origin + blender_offset,
                    grid_stride,
                ));
            }
        }

        // This path is assets-directory-relative.
        let irradiance_volume_path = if !using_upstream_types {
            update_irradiance_grid(
                &grid_sample_data,
                irradiance_volumes_dir,
                assets_dir,
                filename,
                irradiance_volume_index,
            )
            .unwrap_or_else(|err| die(format!("{:?}", err)))
        } else {
            update_irradiance_grid_upstream(
                &grid_sample_data,
                irradiance_volumes_dir,
                assets_dir,
                filename,
                irradiance_volume_index,
                &Transform::from_matrix(transform),
                resolution,
            )
            .unwrap_or_else(|err| die(format!("{:?}", err)))
        };

        println!(
            "Saving irradiance volume to {}",
            irradiance_volume_path.display()
        );

        let irradiance_volume_image = load_asset(&irradiance_volume_path, asset_server);

        let irradiance_volume = IrradianceVolume {
            meta,
            image: irradiance_volume_image,
        };

        let matrix = Mat4::from_cols_slice(&grid_data.get_f32_vec("mat"));
        let transform = to_transform_matrix(&matrix);

        world
            .spawn(irradiance_volume)
            .insert(SpatialBundle {
                transform,
                ..SpatialBundle::default()
            })
            .remove::<ComputedVisibility>();
    }
}

/// `stride` is in bytes.
fn get_texel(buffer: &[u8], p: IVec2, stride: usize) -> [u8; 4] {
    let offset = p.y as usize * stride + p.x as usize * IRRADIANCE_GRID_BYTES_PER_SAMPLE;
    buffer[offset..offset + 4].try_into().unwrap()
}

pub(crate) fn assets_dir_relative(path: &Path, assets_dir: &Path) -> PathBuf {
    if let Some(diffed) = pathdiff::diff_paths(
        fs::canonicalize(path).unwrap_or_else(|_| path.to_owned()),
        fs::canonicalize(assets_dir).unwrap_or_else(|_| assets_dir.to_owned()),
    ) {
        return diffed;
    }

    eprintln!(
        "warning: asset `{}` isn't relative to the assets directory `{}`; using an absolute \
path instead",
        path.display(),
        assets_dir.display()
    );

    fs::canonicalize(path).unwrap()
}

pub(crate) fn load_asset<T>(
    asset_dir_relative_path: &Path,
    asset_server: &mut AssetServer,
) -> Handle<T>
where
    T: TypePath + TypeUuid + Send + Sync,
{
    asset_server.load::<T, _>(asset_dir_relative_path)
}

fn to_transform_matrix(matrix: &Mat4) -> Transform {
    let mut transform = Transform::from_matrix(matrix.inverse());
    transform.scale = transform.scale.xzy();
    transform.translation = transform.translation.xzy();
    transform
}

pub fn update_irradiance_grid(
    grid_sample_data: &[u8],
    irradiance_volumes_dir: &Path,
    assets_dir: &Path,
    filename: &OsStr,
    irradiance_volume_index: usize,
) -> AnyhowResult<PathBuf> {
    let sample_count = grid_sample_data.len() / IRRADIANCE_GRID_BYTES_PER_CELL;

    // FIXME: Pick this dynamically.
    let dest_width = 768;
    let dest_cells_per_row = dest_width as usize / 3;
    let dest_height = div_ceil(sample_count as _, dest_cells_per_row as _) * 2;

    let mut dest_grid_data =
        vec![0; dest_width as usize * dest_height as usize * IRRADIANCE_GRID_BYTES_PER_SAMPLE];
    let bevy_stride = dest_width as usize * IRRADIANCE_GRID_BYTES_PER_SAMPLE;

    for src_cell_index in 0..sample_count {
        let dest_cell_index = src_cell_index;
        let dest_origin = ivec2(
            (dest_cell_index % dest_cells_per_row * 3) as i32,
            (dest_cell_index / dest_cells_per_row * 2) as i32,
        );

        for negative in 0..2 {
            for axis in 0..3 {
                let dest_offset = ivec2(axis, negative);

                let src_sample_index = negative as usize * 3
                    + axis as usize
                    + src_cell_index * IRRADIANCE_GRID_SAMPLES_PER_CELL;
                let src_byte_offset = src_sample_index * IRRADIANCE_GRID_BYTES_PER_SAMPLE;

                let texel = &grid_sample_data
                    [src_byte_offset..(src_byte_offset + IRRADIANCE_GRID_BYTES_PER_SAMPLE)];

                put_texel(
                    &mut dest_grid_data,
                    texel.try_into().unwrap(),
                    dest_origin + dest_offset,
                    bevy_stride,
                );
            }
        }
    }

    let output_path = irradiance_volumes_dir.join(format!(
        "{}.{:0>3}.voxelgi.ktx2",
        filename.to_string_lossy(),
        irradiance_volume_index
    ));
    let mut output_file = File::create(&output_path)?;

    write_ktx2(
        &mut output_file,
        &dest_grid_data,
        TextureFormat::R8G8B8A8Unorm,
        ivec2(dest_width, dest_height as i32),
        1,
    )?;

    let relative = assets_dir_relative(&output_path, assets_dir);
    println!(
        "{} + {} -> {}",
        output_path.display(),
        assets_dir.display(),
        relative.display()
    );
    Ok(relative)
}

pub fn update_irradiance_grid_upstream(
    grid_sample_data: &[u8],
    irradiance_volumes_dir: &Path,
    assets_dir: &Path,
    filename: &OsStr,
    irradiance_volume_index: usize,
    transform: &Transform,
    resolution: IVec3,
) -> AnyhowResult<PathBuf> {
    let sample_count = grid_sample_data.len() / IRRADIANCE_GRID_BYTES_PER_CELL;

    let mut dest_grid_data = vec![Vec4::ZERO; sample_count * IRRADIANCE_GRID_SAMPLES_PER_CELL];

    for src_cell_index in 0..sample_count {
        let dest_start = src_cell_index * IRRADIANCE_GRID_SAMPLES_PER_CELL;

        for negative in 0..2 {
            for axis in 0..3 {
                let dest_offset = axis * 2 + negative;

                let src_sample_index = negative as usize * 3
                    + axis as usize
                    + src_cell_index * IRRADIANCE_GRID_SAMPLES_PER_CELL;
                let src_byte_offset = src_sample_index * IRRADIANCE_GRID_BYTES_PER_SAMPLE;

                let texel = &grid_sample_data
                    [src_byte_offset..(src_byte_offset + IRRADIANCE_GRID_BYTES_PER_SAMPLE)];

                put_upstream_sample(
                    &mut dest_grid_data,
                    texel.try_into().unwrap(),
                    dest_start + dest_offset,
                );
            }
        }
    }

    let output_path = irradiance_volumes_dir.join(format!(
        "{}.{:0>3}.voxelgi.bincode",
        filename.to_string_lossy(),
        irradiance_volume_index
    ));
    let mut output_file = File::create(&output_path)?;

    bincode::serialize_into(
        &mut output_file,
        &UpstreamBevyIrradianceVolume {
            transform: *transform,
            resolution,
            data: dest_grid_data,
        },
    )?;

    let relative = assets_dir_relative(&output_path, assets_dir);
    println!(
        "{} + {} -> {}",
        output_path.display(),
        assets_dir.display(),
        relative.display()
    );
    Ok(relative)
}

/// `stride` is in bytes.
fn put_texel(buffer: &mut [u8], texel: [u8; 4], p: IVec2, stride: usize) {
    let offset = p.y as usize * stride + p.x as usize * IRRADIANCE_GRID_BYTES_PER_SAMPLE;
    buffer[offset..offset + 4].copy_from_slice(&texel)
}

fn put_upstream_sample(buffer: &mut [Vec4], texel: [u8; 4], index: usize) {
    let exponent = f32::exp2(texel[3] as f32 - 128.0);
    let rgb = Vec3::from_array(array::from_fn(|i| texel[i] as f32 / 255.0 * exponent));
    buffer[index] = rgb.extend(1.0);
}

fn div_ceil(a: u32, b: u32) -> u32 {
    (a + b - 1) / b
}

pub(crate) fn write_ktx2<F>(
    output: &mut F,
    texture_data: &[u8],
    texture_format: TextureFormat,
    dimensions: IVec2,
    face_count: u32,
) -> AnyhowResult<()>
where
    F: Seek + Write,
{
    output.write_all(&[
        0xAB, 0x4B, 0x54, 0x58, 0x20, 0x32, 0x30, 0xBB, 0x0D, 0x0A, 0x1A, 0x0A,
    ])?;

    for value in [
        texture_format as u32,      // vkFormat
        texture_format.type_size(), // typeSize
        dimensions.x as u32,        // pixelWidth
        dimensions.y as u32,        // pixelHeight
        0,                          // pixelDepth
        0,                          // layerCount
        face_count,                 // faceCount
        1,                          // levelCount
        0,                          // supercompressionScheme
    ]
    .into_iter()
    {
        output.write_u32::<LittleEndian>(value)?;
    }

    let dfd_byte_offset_pos = output.stream_position()?;
    let dfd = vk2dfd::vk2dfd(texture_format as VkFormat)?;

    for value in [
        0,                            // dfdByteOffset
        mem::size_of_val(dfd) as u32, // dfdByteLength
        0,                            // kvdByteOffset
        0,                            // kvdByteLength
    ]
    .into_iter()
    {
        output.write_u32::<LittleEndian>(value)?;
    }

    for value in [
        0,                         // sgdByteOffset
        0,                         // sgdByteLength
        0,                         // byteOffset
        texture_data.len() as u64, // byteLength
        texture_data.len() as u64, // uncompressedByteLength
    ]
    .into_iter()
    {
        output.write_u64::<LittleEndian>(value)?;
    }

    let dfd_byte_offset = output.stream_position()?;

    for &word in dfd {
        output.write_u32::<LittleEndian>(word)?;
    }

    // Align up.
    let mut level_byte_offset = output.stream_position()?;
    while level_byte_offset % texture_format.bytes_per_sample() as u64 != 0 {
        output.write_all(&[0])?;
        level_byte_offset += 1;
    }

    output.write_all(texture_data)?;

    // Now backpatch the various file offsets.
    output.seek(SeekFrom::Start(dfd_byte_offset_pos))?;
    output.write_u32::<LittleEndian>(dfd_byte_offset as u32)?; // dfdByteOffset
    output.seek(SeekFrom::Current(28))?;
    output.write_u64::<LittleEndian>(level_byte_offset)?; // byteOffset

    Ok(())
}

impl TextureFormat {
    fn bytes_per_sample(self) -> u8 {
        match self {
            TextureFormat::R32G32B32A32Sfloat => 16,
            TextureFormat::R16G16B16A16Sfloat => 8,
            TextureFormat::B9G9R9E5Ufloat | TextureFormat::R8G8B8A8Unorm => 4,
        }
    }

    fn type_size(self) -> u32 {
        match self {
            TextureFormat::R32G32B32A32Sfloat => 4,
            TextureFormat::R16G16B16A16Sfloat => todo!(),
            TextureFormat::B9G9R9E5Ufloat => todo!(),
            TextureFormat::R8G8B8A8Unorm => 1,
        }
    }
}

fn decompress_blender_file(mut blend_file: File) -> Vec<u8> {
    let mut magic_number = [0; 4];
    if let Err(err) = blend_file.read_exact(&mut magic_number) {
        die(format!("File wasn't a Blender file: {:?}", err));
    }
    if let Err(err) = blend_file.seek(SeekFrom::Start(0)) {
        die(format!("Failed to read the Blender file: {:?}", err));
    }

    let mut blend_data = vec![];
    if magic_number != ZSTD_MAGIC_NUMBER {
        blend_file
            .read_to_end(&mut blend_data)
            .unwrap_or_else(|err| die(format!("Failed to read the Blender file: {:?}", err)));
        return blend_data;
    }

    let mut zstd_decoder = FrameDecoder::new();

    loop {
        match zstd_decoder.reset(&mut blend_file) {
            Ok(()) => {}

            Err(FrameDecoderError::ReadFrameHeaderError(ReadFrameHeaderError::BadMagicNumber(
                _,
            )))
            | Err(FrameDecoderError::ReadFrameHeaderError(
                ReadFrameHeaderError::MagicNumberReadError(_),
            )) => break,

            Err(FrameDecoderError::ReadFrameHeaderError(ReadFrameHeaderError::SkipFrame(
                _,
                len,
            ))) => {
                if let Err(err) = blend_file.seek(SeekFrom::Current(len as _)) {
                    die_with_zstd_frame_decoder_error(err)
                }
            }
            Err(err) => die_with_zstd_frame_decoder_error(err),
        }

        match zstd_decoder.decode_blocks(&mut blend_file, BlockDecodingStrategy::All) {
            Ok(_) => {}
            Err(FrameDecoderError::FailedToReadBlockHeader(BlockHeaderReadError::ReadError(
                io_error,
            ))) if io_error.kind() == ErrorKind::UnexpectedEof => break,

            Err(err) => die_with_zstd_frame_decoder_error(err),
        }

        if let Err(err) = zstd_decoder.read_to_end(&mut blend_data) {
            die_with_zstd_frame_decoder_error(err);
        }
    }

    info!(
        "Decompressed Zstandard-compressed Blender file to {} bytes",
        blend_data.len()
    );
    blend_data
}
