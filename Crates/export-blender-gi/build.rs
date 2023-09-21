// bevy-baked-gi/Crates/export-blender-gi/build.rs

extern crate cc;

use cc::Build;
use std::path::Path;

fn main() {
    println!("cargo:rerun-if-changed=ibllib-wrapper.h");

    // Build the glTF IBL Sampler.
    let gltf_ibl_sampler_dir = Path::new("../../Contrib/glTF-IBL-Sampler/").to_owned();
    Build::new()
        .files(
            [
                "FileHelper.cpp",
                "format.cpp",
                "ktxImage.cpp",
                "lib.cpp",
                "STBImage.cpp",
                "vkHelper.cpp",
            ]
            .into_iter()
            .map(|filename| gltf_ibl_sampler_dir.join("lib/source").join(filename)),
        )
        .file(gltf_ibl_sampler_dir.join("thirdparty/volk/volk.c"))
        .include(gltf_ibl_sampler_dir.join("lib/include"))
        .include(gltf_ibl_sampler_dir.join("thirdparty/stb"))
        .include(gltf_ibl_sampler_dir.join("thirdparty/volk"))
        .include(gltf_ibl_sampler_dir.join("thirdparty/Vulkan-Headers/include"))
        .compile("IBLLib");
}
