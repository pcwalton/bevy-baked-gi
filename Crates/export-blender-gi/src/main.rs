// bevy-irradiance-volumes/Crates/export-blender-gi/src/main.rs

use anyhow::Result as AnyhowResult;
use blend::Blend;
use byteorder::{LittleEndian, WriteBytesExt};
use clap::Parser;
use glam::{IVec3, UVec3};
use itertools::Itertools;
use std::env;
use std::fs::File;
use std::io::{Read, Seek, SeekFrom, Write};
use std::path::PathBuf;
use std::process;

const VK_FORMAT_R8G8B8A8_UNORM: u32 = 37;

const DFD_DESCRIPTOR_SIZE: u16 = 24 + 16 * 4;
const DFD_TOTAL_SIZE: u32 = DFD_DESCRIPTOR_SIZE as u32 + 4;
const KHR_DF_MODEL_RGBSDA: u8 = 0;
const KHR_DF_PRIMARIES_UNSPECIFIED: u8 = 0;
const KHR_DF_TRANSFER_UNSPECIFIED: u8 = 0;
const KHR_DF_FLAG_ALPHA_STRAIGHT: u8 = 0;

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
    let light_cache_data = scene.get("eevee").get("light_cache_data");
    let grid_texture = light_cache_data.get("grid_tx");
    let grid_texture_data = grid_texture.get_u8_vec("data");
    let grid_dimensions = IVec3::from_slice(&grid_texture.get_i32_vec("tex_size")).as_uvec3();

    for grid_data in light_cache_data.get_iter("grid_data") {
        println!("{}", grid_data);
    }

    let mut output_path = args
        .out_dir
        .or_else(|| env::current_dir().ok())
        .or_else(|| args.input.parent().map(|path| (*path).to_owned()))
        .unwrap_or_else(|| die("Couldn't find a suitable output directory"));
    let filename = args.input.file_stem().expect("Failed to find a file stem");
    output_path.push(filename);
    output_path.set_extension("ktx2");

    // TODO: Write to `ktx2`.
    let mut output = File::create(&output_path)
        .unwrap_or_else(|err| die(format!("Failed to create the output file: {:?}", err)));

    write_ktx2(&mut output, &grid_texture_data, grid_dimensions)
        .unwrap_or_else(|err| die(format!("Failed to write the KTX2 file: {}", err)))
}

fn write_ktx2(output: &mut File, texture_data: &[u8], dimensions: UVec3) -> AnyhowResult<()> {
    output.write_all(&[
        0xAB, 0x4B, 0x54, 0x58, 0x20, 0x32, 0x30, 0xBB, 0x0D, 0x0A, 0x1A, 0x0A,
    ])?;

    println!("dimensions={:?} size={:?}", dimensions, texture_data.len());

    for value in [
        VK_FORMAT_R8G8B8A8_UNORM, // vkFormat
        1,                        // typeSize
        dimensions.x,             // pixelWidth
        dimensions.y,             // pixelHeight
        dimensions.z,             // pixelDepth
        0,                        // layerCount
        1,                        // faceCount
        1,                        // levelCount
        0,                        // supercompressionScheme
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
        0,                   // vendorId
        0,                   // descriptorType
        0,                   // versionNumber
        DFD_DESCRIPTOR_SIZE, // descriptorBlockSize
    ]
    .into_iter()
    {
        output.write_u16::<LittleEndian>(value)?;
    }

    output.write_all(&[
        KHR_DF_MODEL_RGBSDA,          // colorModel
        KHR_DF_PRIMARIES_UNSPECIFIED, // colorPrimaries
        KHR_DF_TRANSFER_UNSPECIFIED,  // transferFunction
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

    for channel in 0..4 {
        output.write_u16::<LittleEndian>(channel as u16 * 8)?;
        output.write_all(&[
            8,       // bitLength
            channel, // channelType
            0,       // samplePosition0
            0,       // samplePosition1
            0,       // samplePosition2
            0,       // samplePosition3
        ])?;
        output.write_u16::<LittleEndian>(0)?; // sampleLower
        output.write_u16::<LittleEndian>(255)?; // sampleUpper
    }

    let level_byte_offset = output.stream_position()?;

    output.write_all(texture_data)?;

    // Now backpatch the various file offsets.
    output.seek(SeekFrom::Start(dfd_byte_offset_pos))?;
    output.write_u32::<LittleEndian>(dfd_byte_offset as u32)?; // dfdByteOffset
    output.seek(SeekFrom::Current(4))?;
    output.write_u32::<LittleEndian>(level_byte_offset as u32)?; // kvdByteOffset
    output.seek(SeekFrom::Current(4))?;
    output.write_u64::<LittleEndian>(level_byte_offset)?; // sgdByteOffset
    output.seek(SeekFrom::Current(8))?;
    output.write_u64::<LittleEndian>(level_byte_offset)?; // byteOffset

    Ok(())
}

fn die(message: impl AsRef<str>) -> ! {
    eprintln!("Error: {}", message.as_ref());
    process::exit(1)
}
