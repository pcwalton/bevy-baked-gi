#import bevy_pbr::mesh_vertex_output MeshVertexOutput

struct IrradianceData {
    cubesides: array<vec3<f32>, 3>,
}

struct GridData {
    matrix: mat4x4<f32>,
    resolution: vec3<i32>,
    offset: i32,
    corner: vec3<f32>,
    increment_x: vec3<f32>,
    increment_y: vec3<f32>,
    level_bias: f32,
    increment_z: vec3<f32>,
}

@group(1) @binding(0)
var<uniform> grid_data: GridData;
@group(1) @binding(1)
var irradiance_grid: texture_3d<f32>;

fn texel_fetch(st: vec2<i32>) -> vec4<f32> {
    let flipped = vec2(st.x, i32(textureDimensions(irradiance_grid, 0).y) - st.y - 1);
    return textureLoad(irradiance_grid, vec3(st, 0), 0);
}

fn to_blender_coords(p: vec3<f32>) -> vec3<f32> {
    return vec3(p.x, -p.z, p.y);
}

fn eevee_irradiance_decode(data: vec4<f32>) -> vec3<f32> {
    let fexp = data.a * 255.0 - 128.0;
    return data.rgb * exp2(fexp);
}

fn eevee_load_irradiance_cell(cell: i32, N: vec3<f32>) -> IrradianceData {
    var cell_co = vec2<i32>(3, 2);
    let cell_per_row = i32(textureDimensions(irradiance_grid, 0).x) / cell_co.x;
    cell_co.x *= cell % cell_per_row;
    cell_co.y *= cell / cell_per_row;

    let is_negative = vec3<i32>(step(vec3<f32>(0.0), -N));

    var ir: IrradianceData;
    ir.cubesides[0] = eevee_irradiance_decode(texel_fetch(cell_co + vec2(0, is_negative.x)));
    ir.cubesides[1] = eevee_irradiance_decode(texel_fetch(cell_co + vec2(1, is_negative.y)));
    ir.cubesides[2] = eevee_irradiance_decode(texel_fetch(cell_co + vec2(2, is_negative.z)));

    return ir;
}

fn eevee_compute_irradiance(N: vec3<f32>, cubesides: array<vec3<f32>, 3>) -> vec3<f32> {
    var irradiance = vec3(0.0);

    let n_squared = N * N;

    irradiance += n_squared.x * cubesides[0];
    irradiance += n_squared.y * cubesides[1];
    irradiance += n_squared.z * cubesides[2];

    return irradiance;
}

fn eevee_irradiance_from_cell_get(cell: i32, ir_dir: vec3<f32>) -> vec3<f32> {
    let ir_data = eevee_load_irradiance_cell(cell, ir_dir);
    return eevee_compute_irradiance(ir_dir, ir_data.cubesides);
}

@fragment
fn fragment(mesh: MeshVertexOutput) -> @location(0) vec4<f32> {
    let N = normalize(to_blender_coords(mesh.world_normal.xyz));
    //return vec4((normal + 1.0) * 0.5, 1.0);

    let corner = (grid_data.matrix * vec4(grid_data.corner, 1.0)).xyz;
    let increment = vec3(
        (grid_data.matrix * vec4(grid_data.increment_x, 1.0)).x,
        (grid_data.matrix * vec4(grid_data.increment_y, 1.0)).y,
        (grid_data.matrix * vec4(grid_data.increment_z, 1.0)).z,
    );

    let P = to_blender_coords(mesh.world_position.xyz);
    let localpos = (P - corner) / increment;

    let localpos_floored = floor(localpos);
    let trilinear_weight = fract(localpos);

    //return vec4(localpos_floored / vec3<f32>(grid_data.resolution), 1.0);

    var weight_accum = 0.0;
    var irradiance_accum = vec3(0.0);

    for (var i = 0; i < 8; i++) {
        let offset = vec3(i, i / 2, i / 4) & vec3(1);
        let cell_cos = clamp(
            localpos_floored + vec3<f32>(offset),
            vec3(0.0),
            vec3<f32>(grid_data.resolution) - vec3(1.0));

        let icell_cos = vec3<i32>(cell_cos);
        let cell = grid_data.offset + icell_cos.z +
            grid_data.resolution.z * (icell_cos.y + grid_data.resolution.y * icell_cos.x);

        let color = eevee_irradiance_from_cell_get(cell, N);

        let ws_cell_location = grid_data.corner +
            (grid_data.increment_x * cell_cos.x +
             grid_data.increment_y * cell_cos.y +
             grid_data.increment_z * cell_cos.z);

        let ws_point_to_cell = ws_cell_location - P;
        let ws_dist_point_to_cell = length(ws_point_to_cell);
        let ws_light = ws_point_to_cell / ws_dist_point_to_cell;

        var weight = clamp(dot(ws_light, N), 0.0, 1.0);

        weight += 1.0;

        let trilinear = mix(1.0 - trilinear_weight, trilinear_weight, vec3<f32>(offset));
        weight *= trilinear.x * trilinear.y * trilinear.z;

        weight = max(0.00001, weight);

        weight_accum += weight;
        irradiance_accum += color * weight;
    }

    let rgb = irradiance_accum / weight_accum;
    return vec4(rgb, 1.0);

    //return vec4(grid_pos / vec3<f32>(grid_data.resolution), 1.0);
    //let rgb = eevee_irradiance_from_cell_get(20, vec3(1.0, 0.0, 0.0));
}
