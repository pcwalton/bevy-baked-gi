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
use clap::Parser;
use glam::{ivec2, vec4, IVec2, IVec3, Mat4, Vec3, Vec3Swizzles};
use ibllib_bindings::{
    IBLLib_OutputFormat_R32G32B32A32_SFLOAT, IBLLib_OutputFormat_R8G8B8A8_UNORM,
};
use ruzstd::decoding::block_decoder::BlockHeaderReadError;
use ruzstd::frame::ReadFrameHeaderError;
use ruzstd::frame_decoder::FrameDecoderError;
use ruzstd::{BlockDecodingStrategy, FrameDecoder};
use std::env;
use std::ffi::OsStr;
use std::fmt::Debug;
use std::fs::{self, File};
use std::io::{ErrorKind, Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use std::process;

#[allow(non_upper_case_globals, non_camel_case_types, unused)]
mod ibllib_bindings;
mod reflection_probes;

const IRRADIANCE_GRID_BYTES_PER_SAMPLE: usize = 4;
const IRRADIANCE_GRID_SAMPLES_PER_CELL: usize = 6;

const KHR_DF_VERSIONNUMBER_1_3: u16 = 2;
const DFD_DESCRIPTOR_SIZE: u16 = 24 + 16 * 4;
const DFD_TOTAL_SIZE: u32 = DFD_DESCRIPTOR_SIZE as u32 + 4;
const KHR_DF_MODEL_RGBSDA: u8 = 1;
const KHR_DF_PRIMARIES_UNSPECIFIED: u8 = 0;
const KHR_DF_TRANSFER_LINEAR: u8 = 1;
const KHR_DF_FLAG_ALPHA_STRAIGHT: u8 = 0;
const KHR_DF_SAMPLE_DATATYPE_SIGNED: u8 = 1 << 6;
const KHR_DF_SAMPLE_DATATYPE_FLOAT: u8 = 1 << 7;
const KHR_DF_CHANNEL_RGBSDA_R: u32 = 0;
const KHR_DF_CHANNEL_RGBSDA_G: u32 = 1;
const KHR_DF_CHANNEL_RGBSDA_B: u32 = 2;
const KHR_DF_CHANNEL_RGBSDA_A: u32 = 15;

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
}

struct CubemapPaths {
    diffuse: PathBuf,
    specular: PathBuf,
}

#[derive(Clone, Copy, PartialEq, Debug)]
#[repr(i32)]
enum TextureFormat {
    R32G32B32A32Sfloat = IBLLib_OutputFormat_R32G32B32A32_SFLOAT,
    R8G8B8A8Unorm = IBLLib_OutputFormat_R8G8B8A8_UNORM,
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
        .unwrap_or_else(|| die(format!("The Blender file didn't find a scene.")));
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
    );

    reflection_probes::extract_reflection_probes(
        &mut world,
        &mut asset_server,
        &light_cache_data,
        &args.reflection_probes_dir(),
        &assets_dir,
        filename,
    );

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
        let irradiance_volume_path = update_irradiance_grid(
            &grid_sample_data,
            irradiance_volumes_dir,
            assets_dir,
            filename,
            irradiance_volume_index,
        )
        .unwrap_or_else(|err| die(format!("{:?}", err)));

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

/// `stride` is in bytes.
fn put_texel(buffer: &mut [u8], texel: [u8; 4], p: IVec2, stride: usize) {
    let offset = p.y as usize * stride + p.x as usize * IRRADIANCE_GRID_BYTES_PER_SAMPLE;
    buffer[offset..offset + 4].copy_from_slice(&texel)
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

    for value in [
        0,              // dfdByteOffset
        DFD_TOTAL_SIZE, // dfdByteLength
        0,              // kvdByteOffset
        0,              // kvdByteLength
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

    output.write_u32::<LittleEndian>(DFD_TOTAL_SIZE)?; // dfdTotalSize
    for value in [
        0,                        // vendorId
        0,                        // descriptorType
        KHR_DF_VERSIONNUMBER_1_3, // versionNumber
        DFD_DESCRIPTOR_SIZE,      // descriptorBlockSize
    ]
    .into_iter()
    {
        output.write_u16::<LittleEndian>(value)?;
    }

    output.write_all(&[
        KHR_DF_MODEL_RGBSDA,               // colorModel
        KHR_DF_PRIMARIES_UNSPECIFIED,      // colorPrimaries
        KHR_DF_TRANSFER_LINEAR,            // transferFunction
        KHR_DF_FLAG_ALPHA_STRAIGHT,        // flags
        0,                                 // texelBlockDimension0
        0,                                 // texelBlockDimension1
        0,                                 // texelBlockDimension2
        0,                                 // texelBlockDimension3
        texture_format.bytes_per_sample(), // bytesPlane0
        0,                                 // bytesPlane1
        0,                                 // bytesPlane2
        0,                                 // bytesPlane3
        0,                                 // bytesPlane4
        0,                                 // bytesPlane5
        0,                                 // bytesPlane6
        0,                                 // bytesPlane7
    ])?;

    let mut current_bit = 0;
    for &channel_type in [
        KHR_DF_CHANNEL_RGBSDA_R,
        KHR_DF_CHANNEL_RGBSDA_G,
        KHR_DF_CHANNEL_RGBSDA_B,
        KHR_DF_CHANNEL_RGBSDA_A,
    ]
    .iter()
    {
        let bit_length = texture_format.bits_per_channel();
        output.write_u16::<LittleEndian>(current_bit)?; // bitOffset
        output.write_all(&[
            bit_length as u8 - 1, // bitLength
            // channelType
            (channel_type as u8) | texture_format.special_sample_flags(),
            0, // samplePosition0
            0, // samplePosition1
            0, // samplePosition2
            0, // samplePosition3
        ])?;
        output.write_u32::<LittleEndian>(0)?; // sampleLower
        output.write_u32::<LittleEndian>(1065353216)?; // sampleUpper
        current_bit += bit_length;
    }

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
            TextureFormat::R8G8B8A8Unorm => 4,
        }
    }

    fn bits_per_channel(self) -> u16 {
        match self {
            TextureFormat::R32G32B32A32Sfloat => 32,
            TextureFormat::R8G8B8A8Unorm => 8,
        }
    }

    fn special_sample_flags(self) -> u8 {
        match self {
            TextureFormat::R32G32B32A32Sfloat => {
                KHR_DF_SAMPLE_DATATYPE_SIGNED | KHR_DF_SAMPLE_DATATYPE_FLOAT
            }
            TextureFormat::R8G8B8A8Unorm => 0,
        }
    }

    fn type_size(self) -> u32 {
        match self {
            TextureFormat::R32G32B32A32Sfloat => 4,
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
