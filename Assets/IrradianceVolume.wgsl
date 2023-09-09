#import bevy_pbr::mesh_vertex_output MeshVertexOutput

@group(1) @binding(0)
var grid_texture: texture_3d<f32>;
@group(1) @binding(1)
var grid_sampler: sampler;

@fragment
fn fragment(mesh: MeshVertexOutput) -> @location(0) vec4<f32> {
    return vec4(0.0, 0.0, 1.0, 1.0);
}
