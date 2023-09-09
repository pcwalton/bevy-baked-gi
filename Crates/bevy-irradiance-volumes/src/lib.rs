// bevy-irradiance-volumes/Crates/bevy-irradiance-volumes/src/lib.rs

use bevy::prelude::{App, Handle, Image, Material, MaterialPlugin, Plugin, Mat4, IVec3, Vec3};
use bevy::reflect::{Reflect, TypeUuid};
use bevy::render::render_resource::{AsBindGroup, ShaderRef, ShaderType};

pub struct IrradianceVolumesPlugin;

#[derive(Clone, Default, Reflect, AsBindGroup, TypeUuid, Debug)]
#[uuid = "d18d9aa6-5053-4cb4-8b59-a1b2d1e6b6db"]
pub struct IrradianceVolumeMaterial {
    #[uniform(0)]
    pub grid_data: GridData,
    #[texture(1, dimension = "3d")]
    pub irradiance_grid: Handle<Image>,
}

#[derive(Clone, Default, Reflect, Debug, ShaderType)]
pub struct GridData {
    pub matrix: Mat4,
    pub resolution: IVec3,
    pub offset: i32,
    pub corner: Vec3,
    pub increment_x: Vec3,
    pub increment_y: Vec3,
    pub level_bias: f32,
    pub increment_z: Vec3,
}

impl Plugin for IrradianceVolumesPlugin {
    fn build(&self, app: &mut App) {
        app.register_type::<IrradianceVolumeMaterial>()
            .add_plugins(MaterialPlugin::<IrradianceVolumeMaterial>::default());
    }
}

impl Material for IrradianceVolumeMaterial {
    fn fragment_shader() -> ShaderRef {
        // TODO: Use `include_bytes!` instead.
        "IrradianceVolume.wgsl".into()
    }
}
