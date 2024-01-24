// bevy-baked-gi/Crates/export-blender-gi/src/cubemap.rs

use bevy::prelude::{AssetServer, ComputedVisibility, SpatialBundle, World};
use bevy_baked_gi::reflection_probes::ReflectionProbe;
use blend::Instance;
use byteorder::{ByteOrder, LittleEndian};
use glam::{uvec2, vec3, IVec2, IVec3, Mat4, UVec2, Vec3, Vec3Swizzles};
use std::ffi::{CString, OsStr};
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::NamedTempFile;

use crate::ibllib_bindings::{
    self, IBLLib_Distribution_GGX, IBLLib_Distribution_Lambertian, IBLLib_Result_Success,
};
use crate::{CubemapPaths, TextureFormat, LOD_BIAS, SAMPLE_COUNT};

static CUBEMAP_FACES: [(usize, CubeFaceRotation); 6] = [
    (0, CubeFaceRotation::Rotate90Ccw),
    (1, CubeFaceRotation::Rotate90Cw),
    (4, CubeFaceRotation::Rotate180),
    (5, CubeFaceRotation::None),
    (2, CubeFaceRotation::Rotate180),
    (3, CubeFaceRotation::None),
];

#[derive(Clone, Copy, PartialEq, Debug)]
enum CubeFaceRotation {
    None,
    Rotate90Ccw,
    Rotate180,
    Rotate90Cw,
}

pub(crate) struct ReflectionProbeExtractor<'a> {
    pub(crate) light_cache_data: &'a Instance<'a>,
    pub(crate) reflection_probes_dir: &'a Path,
    pub(crate) assets_dir: &'a Path,
    pub(crate) filename: &'a OsStr,
    pub(crate) reflection_probe_format: TextureFormat,
    pub(crate) diffuse_reflection_probe_mipmap_count: Option<u32>,
    pub(crate) diffuse_reflection_probe_size: Option<u32>,
}

struct CubeData<'a> {
    cube_data: &'a Instance<'a>,
    cubemap_index: usize,
    cube_dimensions: IVec2,
    cube_texture_data: &'a [u8],
}

impl<'a> ReflectionProbeExtractor<'a> {
    pub(crate) fn extract_reflection_probes(
        &self,
        world: &mut World,
        asset_server: &mut AssetServer,
    ) {
        let cube_texture = self.light_cache_data.get("cube_tx");
        let cube_dimensions = IVec3::from_slice(&cube_texture.get_i32_vec("tex_size")).xy();
        let cube_texture_data = cube_texture.get_u8_vec("data");

        let cube_data = self
            .light_cache_data
            .get_iter("cube_data")
            .collect::<Vec<_>>();

        let cubemap_face_byte_size_r11g11b10 =
            cube_dimensions.x as usize * cube_dimensions.y as usize * 4;
        let cubemap_byte_size_r11g11b10 = cubemap_face_byte_size_r11g11b10 * 6;
        let cubemap_count = cube_texture_data.len() / cubemap_byte_size_r11g11b10;
        debug_assert_eq!(cube_texture_data.len() % cubemap_byte_size_r11g11b10, 0);
        debug_assert_eq!(cubemap_count, cube_data.len());

        for (cubemap_index, cube_data) in cube_data.iter().enumerate() {
            self.extract_single_reflection_probe(
                world,
                asset_server,
                &CubeData {
                    cube_data,
                    cubemap_index,
                    cube_dimensions,
                    cube_texture_data: &cube_texture_data,
                },
            );
        }
    }

    fn extract_single_reflection_probe(
        &self,
        world: &mut World,
        asset_server: &mut AssetServer,
        cube_data: &CubeData,
    ) {
        // There seems to always be a reflection probe present with an all-zero matrix. Ignore it.
        let attenuation_matrix =
            Mat4::from_cols_slice(&cube_data.cube_data.get_f32_vec("attenuationmat"));
        if attenuation_matrix == Mat4::ZERO {
            return;
        }

        let transform = crate::to_transform_matrix(&attenuation_matrix);

        let output_cubemap_face_byte_size_rgba_f32 = cube_data.cube_dimensions.x as usize
            * cube_data.cube_dimensions.y as usize
            * TextureFormat::R32G32B32A32Sfloat.bytes_per_sample() as usize;
        let output_cubemap_byte_size_rgba_f32 = output_cubemap_face_byte_size_rgba_f32 * 6;

        let mut output_data = vec![0; output_cubemap_byte_size_rgba_f32];

        let (width, height) = (
            cube_data.cube_dimensions.x as u32,
            cube_data.cube_dimensions.y as u32,
        );

        // Cubemap faces are stored in the order +X, -X, +Y, -Y, +Z, -Z.
        //   +X ← mirror(ccw(+X))
        //   -X ← mirror(cw(-X))
        //   +Y ← mirror(rot180(+Z))
        //   -Y ← mirror(-Z)
        //   +Z ← mirror(+Y)
        //   -Z ← mirror(-Y)
        for (dest_face_index, &(src_face_index, rotation)) in CUBEMAP_FACES.iter().enumerate() {
            for y in 0..height {
                for x in 0..width {
                    let src_pos = uvec2(x, y);
                    let texel = get_cubemap_texel(
                        cube_data.cube_texture_data,
                        cube_data.cube_dimensions.as_uvec2(),
                        cube_data.cubemap_index,
                        src_face_index,
                        src_pos,
                    );

                    let mut dest_pos = match rotation {
                        CubeFaceRotation::None => src_pos,
                        CubeFaceRotation::Rotate90Ccw => uvec2(height - y - 1, x),
                        CubeFaceRotation::Rotate180 => uvec2(width - x - 1, height - y - 1),
                        CubeFaceRotation::Rotate90Cw => uvec2(y, width - x - 1),
                    };

                    dest_pos.x = width - dest_pos.x - 1;

                    put_cubemap_texel(
                        &mut output_data,
                        texel,
                        cube_data.cube_dimensions.as_uvec2(),
                        dest_face_index,
                        dest_pos,
                    );
                }
            }
        }

        let mut raw_cubemap =
            NamedTempFile::new().expect("Failed to create temporary file for the raw cubemap");
        crate::write_ktx2(
            &mut raw_cubemap,
            &output_data,
            TextureFormat::R32G32B32A32Sfloat,
            cube_data.cube_dimensions.extend(0),
            6,
        )
        .unwrap();
        let (_, raw_cubemap_path) = raw_cubemap.keep().unwrap();

        let lut = NamedTempFile::new().expect("Failed to create temporary file for the LUT");
        let (_, lut_path) = lut.keep().unwrap();

        let cubemap_paths =
            self.sample_cubemap(cube_data.cubemap_index, height, raw_cubemap_path, lut_path);

        let diffuse_map = crate::load_asset(&cubemap_paths.diffuse, asset_server);
        let specular_map = crate::load_asset(&cubemap_paths.specular, asset_server);

        world
            .spawn(ReflectionProbe {
                diffuse_map,
                specular_map,
            })
            .insert(SpatialBundle {
                transform,
                ..SpatialBundle::default()
            })
            .remove::<ComputedVisibility>();
    }

    fn sample_cubemap(
        &self,
        cubemap_index: usize,
        height: u32,
        raw_cubemap_path: PathBuf,
        lut_path: PathBuf,
    ) -> CubemapPaths {
        let output_path = self.reflection_probes_dir.to_owned();
        let diffuse_output_path = output_path.join(format!(
            "{}.diffuse.{:0>3}.ktx2",
            self.filename.to_string_lossy(),
            cubemap_index
        ));
        let specular_output_path = output_path.join(format!(
            "{}.specular.{:0>3}.ktx2",
            self.filename.to_string_lossy(),
            cubemap_index
        ));

        unsafe {
            let raw_cubemap_path = CString::new(raw_cubemap_path.to_str().unwrap()).unwrap();
            let diffuse_output_path = CString::new(diffuse_output_path.to_str().unwrap()).unwrap();
            let specular_output_path =
                CString::new(specular_output_path.to_str().unwrap()).unwrap();
            let lut_path = CString::new(lut_path.to_str().unwrap()).unwrap();

            let result = ibllib_bindings::IBLSample(
                raw_cubemap_path.as_ptr(),
                diffuse_output_path.as_ptr(),
                lut_path.as_ptr(),
                IBLLib_Distribution_Lambertian,
                self.diffuse_reflection_probe_size.unwrap_or(height),
                self.diffuse_reflection_probe_mipmap_count.unwrap_or(0),
                SAMPLE_COUNT,
                self.reflection_probe_format as _,
                LOD_BIAS,
                false,
            );
            assert_eq!(result, IBLLib_Result_Success);

            let result = ibllib_bindings::IBLSample(
                raw_cubemap_path.as_ptr(),
                specular_output_path.as_ptr(),
                lut_path.as_ptr(),
                IBLLib_Distribution_GGX,
                height,
                0,
                SAMPLE_COUNT,
                self.reflection_probe_format as _,
                LOD_BIAS,
                false,
            );
            assert_eq!(result, IBLLib_Result_Success);
        }

        let _ = fs::remove_file(lut_path);
        let _ = fs::remove_file(raw_cubemap_path);

        CubemapPaths {
            diffuse: crate::assets_dir_relative(&diffuse_output_path, self.assets_dir),
            specular: crate::assets_dir_relative(&specular_output_path, self.assets_dir),
        }
    }
}

fn cubemap_texel_byte_offset(
    cube_dimensions: UVec2,
    cubemap_index: usize,
    cubemap_face_index: usize,
    bytes_per_pixel: usize,
    pos: UVec2,
) -> usize {
    let bytes_per_face = cube_dimensions.x as usize * cube_dimensions.y as usize * bytes_per_pixel;
    let bytes_per_cubemap = bytes_per_face * 6;
    let stride = cube_dimensions.x as usize * bytes_per_pixel;
    cubemap_index * bytes_per_cubemap
        + cubemap_face_index * bytes_per_face
        + pos.y as usize * stride
        + pos.x as usize * bytes_per_pixel
}

fn cubemap_texel_byte_offset_r11g11b10(
    cube_dimensions: UVec2,
    cubemap_index: usize,
    cubemap_face_index: usize,
    pos: UVec2,
) -> usize {
    cubemap_texel_byte_offset(cube_dimensions, cubemap_index, cubemap_face_index, 4, pos)
}

fn cubemap_texel_byte_offset_rgba_f32(
    cube_dimensions: UVec2,
    cubemap_index: usize,
    cubemap_face_index: usize,
    pos: UVec2,
) -> usize {
    cubemap_texel_byte_offset(cube_dimensions, cubemap_index, cubemap_face_index, 16, pos)
}

fn get_cubemap_texel(
    cubemap_data: &[u8],
    cube_dimensions: UVec2,
    cubemap_index: usize,
    cubemap_face_index: usize,
    pos: UVec2,
) -> Vec3 {
    let start = cubemap_texel_byte_offset_r11g11b10(
        cube_dimensions,
        cubemap_index,
        cubemap_face_index,
        pos,
    );
    let packed = LittleEndian::read_u32(&cubemap_data[start..(start + 4)]);
    let r = unpack_f11(packed & 0x7ff);
    let g = unpack_f11((packed >> 11) & 0x7ff);
    let b = unpack_f10(packed >> 22);
    vec3(r, g, b)
}

fn put_cubemap_texel(
    cubemap_data: &mut [u8],
    texel: Vec3,
    cube_dimensions: UVec2,
    cubemap_face_index: usize,
    pos: UVec2,
) {
    let start = cubemap_texel_byte_offset_rgba_f32(cube_dimensions, 0, cubemap_face_index, pos);
    let texel = texel.extend(1.0);
    for i in 0..4 {
        LittleEndian::write_f32(
            &mut cubemap_data[(start + i * 4)..(start + (i + 1) * 4)],
            texel[i],
        );
    }
}

// https://registry.khronos.org/DataFormat/specs/1.3/dataformat.1.3.html#11bitfp
fn unpack_f11(n: u32) -> f32 {
    let x: f64 = match (n / 64, n % 64) {
        (0, 0) => 0.0,
        (0, m) => 2.0f64.powi(-14) * (m as f64 / 64.0),
        (31, 0) => f64::INFINITY,
        (31, _) => f64::NAN,
        (e, m) => 2.0f64.powi(e as i32 - 15) * (1.0 + m as f64 / 64.0),
    };
    x as f32
}

// https://registry.khronos.org/DataFormat/specs/1.3/dataformat.1.3.html#10bitfp
fn unpack_f10(n: u32) -> f32 {
    let x: f64 = match (n / 32, n % 32) {
        (0, 0) => 0.0,
        (0, m) => 2.0f64.powi(-14) * (m as f64 / 32.0),
        (31, 0) => f64::INFINITY,
        (31, _) => f64::NAN,
        (e, m) => 2.0f64.powi(e as i32 - 15) * (1.0 + m as f64 / 32.0),
    };
    x as f32
}
