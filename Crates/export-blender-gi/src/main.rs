// bevy-baked-gi/Crates/export-blender-gi/src/main.rs

use anyhow::Result as AnyhowResult;
use bevy_baked_gi::irradiance_volumes::{
    IrradianceVolume, IrradianceVolumeMetadata, IRRADIANCE_GRID_BYTES_PER_CELL,
    IRRADIANCE_GRID_BYTES_PER_SAMPLE,
};
use blend::{Blend, Instance};
use byteorder::{LittleEndian, WriteBytesExt};
use clap::Parser;
use glam::{ivec2, IVec2, IVec3, Mat4, UVec3, Vec3};
use std::env;
use std::ffi::OsStr;
use std::fs::File;
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use std::process;

const VK_FORMAT_R8G8B8A8_UNORM: u32 = 37;
const VK_FORMAT_B10G11R11_UFLOAT_PACK32: u32 = 122;

const KHR_DF_VERSIONNUMBER_1_3: u16 = 2;
const DFD_DESCRIPTOR_SIZE: u16 = 24 + 16 * 3;
const DFD_TOTAL_SIZE: u32 = 76;
const KHR_DF_MODEL_RGBSDA: u8 = 1;
const KHR_DF_PRIMARIES_UNSPECIFIED: u8 = 0;
const KHR_DF_TRANSFER_UNSPECIFIED: u8 = 0;
const KHR_DF_TRANSFER_SRGB: u8 = 2;
const KHR_DF_FLAG_ALPHA_STRAIGHT: u8 = 0;
const KHR_DF_SAMPLE_DATATYPE_FLOAT: u8 = 1 << 7;

#[derive(Parser)]
#[command(author, version, about)]
struct Args {
    #[arg()]
    input: PathBuf,

    #[arg(short, long)]
    out_dir: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();

    let mut blend_file = File::open(&args.input)
        .unwrap_or_else(|err| die(format!("Failed to open the Blender file: {:?}", err)));

    let output_dir = args
        .out_dir
        .or_else(|| env::current_dir().ok())
        .or_else(|| args.input.parent().map(|path| (*path).to_owned()))
        .unwrap_or_else(|| die("Couldn't find a suitable output directory"));
    let filename = args.input.file_stem().expect("No file stem found");

    let mut blend_data = vec![];
    blend_file
        .read_to_end(&mut blend_data)
        .unwrap_or_else(|err| die(format!("Failed to read the Blender file: {:?}", err)));

    // TODO: Decompress with `zlib` or Zstd if necessary.
    let blend = Blend::new(&blend_data[..])
        .unwrap_or_else(|err| die(format!("Failed to parse the Blender file: {:?}", err)));

    let scene = blend
        .instances_with_code(*b"SC")
        .next()
        .unwrap_or_else(|| die(format!("The Blender file didn't find a scene.")));
    let eevee = scene.get("eevee");
    println!("eevee={:#?}", eevee);
    let light_cache_data = eevee.get("light_cache_data");
    println!("light_cache_data={:#?}", light_cache_data);

    extract_irradiance_volumes(&light_cache_data, &output_dir, filename);
    extract_reflection_probes(&light_cache_data, &output_dir, filename);
}

fn die(message: impl AsRef<str>) -> ! {
    eprintln!("Error: {}", message.as_ref());
    process::exit(1)
}

fn extract_irradiance_volumes(light_cache_data: &Instance, output_dir: &Path, filename: &OsStr) {
    let grid_texture = light_cache_data.get("grid_tx");
    let grid_texture_data = grid_texture.get_u8_vec("data");

    let grid_dimensions = IVec3::from_slice(&grid_texture.get_i32_vec("tex_size"));
    let cells_per_row = grid_dimensions.x / 3;
    let grid_stride = grid_dimensions.x as usize * IRRADIANCE_GRID_BYTES_PER_SAMPLE;

    let grid_data = light_cache_data
        .get_iter("grid_data")
        .find(|grid_data| grid_data.get_i32_vec("resolution")[0] > 0)
        .unwrap_or_else(|| {
            die("The Blender file didn't contain any baked irradiance volume data.")
        });

    let meta = IrradianceVolumeMetadata {
        resolution: IVec3::from_slice(&grid_data.get_i32_vec("resolution")),
        corner: Vec3::from_slice(&grid_data.get_f32_vec("corner")),
        increment_x: Vec3::from_slice(&grid_data.get_f32_vec("increment_x")),
        increment_y: Vec3::from_slice(&grid_data.get_f32_vec("increment_y")),
        increment_z: Vec3::from_slice(&grid_data.get_f32_vec("increment_z")),
        level_bias: grid_data.get_f32("level_bias"),
    };

    let offset = grid_data.get_i32("offset");

    // Extract the relevant portion of the first layer of the texture. (The rest of the texture
    // contains shadow maps used for visibility, which we don't care about presently.)

    let sample_count: usize = meta.sample_count();
    let mut grid_sample_data = Vec::with_capacity(sample_count * IRRADIANCE_GRID_BYTES_PER_CELL);
    for sample_index in 0..(sample_count as i32) {
        let src_sample_index = sample_index + offset;
        let origin = ivec2(
            src_sample_index % cells_per_row * 3,
            src_sample_index / cells_per_row * 2,
        );
        for y in 0..2 {
            for x in 0..3 {
                grid_sample_data.extend_from_slice(&get_texel(
                    &grid_texture_data,
                    origin + ivec2(x, y),
                    grid_stride,
                ));
            }
        }
    }

    println!("meta={:#?}", meta);

    let irradiance_volume = IrradianceVolume {
        meta,
        data: grid_sample_data,
    };

    println!(
        "matrix={:#?}",
        Mat4::from_cols_slice(&grid_data.get_f32_vec("mat"))
    );
    println!("offset={}", grid_data.get_i32("offset"));

    let mut output_path = output_dir.to_owned();
    output_path.push(filename);
    output_path.set_extension("voxelgi.bincode");

    let mut output = File::create(&output_path)
        .unwrap_or_else(|err| die(format!("Failed to create the output file: {:?}", err)));
    bincode::serialize_into(&mut output, &irradiance_volume).unwrap_or_else(|err| {
        die(format!(
            "Failed to write the irradiance volume to disk: {:?}",
            err
        ))
    });
}

/// `stride` is in bytes.
fn get_texel(buffer: &[u8], p: IVec2, stride: usize) -> [u8; 4] {
    let offset = p.y as usize * stride + p.x as usize * IRRADIANCE_GRID_BYTES_PER_SAMPLE;
    buffer[offset..offset + 4].try_into().unwrap()
}

fn extract_reflection_probes(light_cache_data: &Instance, output_dir: &Path, filename: &OsStr) {
    let cube_texture = light_cache_data.get("cube_tx");
    println!("cube_tx={:#?}", cube_texture);

    let cube_dimensions = IVec3::from_slice(&cube_texture.get_i32_vec("tex_size"));
    let cube_texture_data = cube_texture.get_u8_vec("data");

    let cubemap_byte_size =
        cube_dimensions.x as usize * cube_dimensions.y as usize * 6 * 4;
    let cubemap_count = cube_texture_data.len() / cubemap_byte_size;
    debug_assert_eq!(cube_texture_data.len() % cubemap_byte_size, 0);

    for cubemap_index in 0..cubemap_count {
        let mut output_path = output_dir.to_owned();
        output_path.push(&format!(
            "{}.{:0>3}.ktx2",
            filename.to_string_lossy(),
            cubemap_index
        ));
        let mut output = File::create(&output_path)
            .unwrap_or_else(|err| die(format!("Failed to create an output cubemap: {:?}", err)));

        write_ktx2(
            &mut output,
            &cube_texture_data
                [(cubemap_index * cubemap_byte_size)..((cubemap_index + 1) * cubemap_byte_size)],
            cube_dimensions,
        )
        .unwrap();
    }
}

fn write_ktx2(output: &mut File, texture_data: &[u8], dimensions: IVec3) -> AnyhowResult<()> {
    output.write_all(&[
        0xAB, 0x4B, 0x54, 0x58, 0x20, 0x32, 0x30, 0xBB, 0x0D, 0x0A, 0x1A, 0x0A,
    ])?;

    println!("dimensions={:?} size={:?}", dimensions, texture_data.len());

    for value in [
        VK_FORMAT_B10G11R11_UFLOAT_PACK32, // vkFormat
        4,                                 // typeSize
        dimensions.x as _,                 // pixelWidth
        dimensions.y as _,                 // pixelHeight
        0,                                 // pixelDepth
        0,                                 // layerCount
        6,                                 // faceCount
        1,                                 // levelCount
        0,                                 // supercompressionScheme
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
        KHR_DF_MODEL_RGBSDA,          // colorModel
        KHR_DF_PRIMARIES_UNSPECIFIED, // colorPrimaries
        KHR_DF_TRANSFER_SRGB,         // transferFunction
        KHR_DF_FLAG_ALPHA_STRAIGHT,   // flags
        0,                            // texelBlockDimension0
        0,                            // texelBlockDimension1
        0,                            // texelBlockDimension2
        0,                            // texelBlockDimension3
        4,                            // bytesPlane0
        0,                            // bytesPlane1
        0,                            // bytesPlane2
        0,                            // bytesPlane3
        0,                            // bytesPlane4
        0,                            // bytesPlane5
        0,                            // bytesPlane6
        0,                            // bytesPlane7
    ])?;

    let mut current_bit = 0;
    for (channel, bit_length) in [(0, 11), (1, 11), (2, 10)] {
        output.write_u16::<LittleEndian>(current_bit)?; // bitOffset
        output.write_all(&[
            bit_length as u8 - 1,                           // bitLength
            (channel as u8) | KHR_DF_SAMPLE_DATATYPE_FLOAT, // channelType
            0,                                              // samplePosition0
            0,                                              // samplePosition1
            0,                                              // samplePosition2
            0,                                              // samplePosition3
        ])?;
        output.write_u32::<LittleEndian>(0)?; // sampleLower
        output.write_u32::<LittleEndian>(1065353216)?; // sampleUpper
        current_bit += bit_length;
    }

    let level_byte_offset = output.stream_position()?;

    output.write_all(texture_data)?;

    // Now backpatch the various file offsets.
    output.seek(SeekFrom::Start(dfd_byte_offset_pos))?;
    output.write_u32::<LittleEndian>(dfd_byte_offset as u32)?; // dfdByteOffset
    output.seek(SeekFrom::Current(28))?;
    output.write_u64::<LittleEndian>(level_byte_offset)?; // byteOffset

    Ok(())
}
