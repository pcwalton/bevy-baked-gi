// bevy-baked-gi/Crates/export-blender-gi/src/main.rs

use bevy_baked_gi::irradiance_volumes::{
    IrradianceVolume, IrradianceVolumeMetadata, IRRADIANCE_GRID_BYTES_PER_CELL,
    IRRADIANCE_GRID_BYTES_PER_SAMPLE,
};
use blend::Blend;
use clap::Parser;
use glam::{ivec2, IVec2, IVec3, Mat4, Vec3};
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;
use std::process;

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

    let mut output_path = args
        .out_dir
        .or_else(|| env::current_dir().ok())
        .or_else(|| args.input.parent().map(|path| (*path).to_owned()))
        .unwrap_or_else(|| die("Couldn't find a suitable output directory"));
    let filename = args.input.file_stem().expect("Failed to find a file stem");
    output_path.push(filename);
    output_path.set_extension("voxelgi.bincode");

    println!(
        "matrix={:#?}",
        Mat4::from_cols_slice(&grid_data.get_f32_vec("mat"))
    );
    println!("offset={}", grid_data.get_i32("offset"));

    let mut output = File::create(&output_path)
        .unwrap_or_else(|err| die(format!("Failed to create the output file: {:?}", err)));
    bincode::serialize_into(&mut output, &irradiance_volume).unwrap_or_else(|err| {
        die(format!(
            "Failed to write the irradiance volume to disk: {:?}",
            err
        ))
    });
}

fn die(message: impl AsRef<str>) -> ! {
    eprintln!("Error: {}", message.as_ref());
    process::exit(1)
}

/// `stride` is in bytes.
fn get_texel(buffer: &[u8], p: IVec2, stride: usize) -> [u8; 4] {
    let offset = p.y as usize * stride + p.x as usize * IRRADIANCE_GRID_BYTES_PER_SAMPLE;
    buffer[offset..offset + 4].try_into().unwrap()
}
