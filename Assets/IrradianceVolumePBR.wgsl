// bevy-baked-gi/Assets/IrradianceVolumePBR.wgsl

#import bevy_pbr::pbr_functions as pbr_functions
#import bevy_pbr::pbr_bindings as pbr_bindings
#import bevy_pbr::pbr_types as pbr_types
#import bevy_pbr::prepass_utils
#import bevy_pbr::lighting as lighting

#import bevy_pbr::mesh_bindings            mesh
#import bevy_pbr::mesh_functions           mesh_position_local_to_clip, mesh_position_local_to_world, mesh_normal_local_to_world
#import bevy_pbr::mesh_view_bindings       view, fog, screen_space_ambient_occlusion_texture
#import bevy_pbr::mesh_view_types          FOG_MODE_OFF
#import bevy_core_pipeline::tonemapping    screen_space_dither, powsafe, tone_mapping
#import bevy_pbr::parallax_mapping         parallaxed_uv

#import bevy_pbr::prepass_utils

#import bevy_pbr::environment_map          EnvironmentMapLight

#ifdef SCREEN_SPACE_AMBIENT_OCCLUSION
#import bevy_pbr::gtao_utils gtao_multibounce
#endif

struct Vertex {
    @location(0) position: vec3<f32>,
    @location(1) normal: vec3<f32>,
    @location(2) uv: vec2<f32>,
#ifdef VERTEX_LIGHTMAP_UVS
    @location(5) lightmap_uv: vec2<f32>,
#endif
};

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) world_position: vec4<f32>,
    @location(1) world_normal: vec3<f32>,
    @location(2) uv: vec2<f32>,
#ifdef VERTEX_TANGENTS
    @location(3) world_tangent: vec4<f32>,
#endif
#ifdef VERTEX_COLORS
    @location(4) color: vec4<f32>,
#endif
#ifdef VERTEX_LIGHTMAP_UVS
    @location(5) lightmap_uv: vec2<f32>,
#endif
};

struct SplitSum {
    irradiance: vec3<f32>,
    radiance: vec3<f32>,
}

#ifdef FRAGMENT_IRRADIANCE_VOLUME

struct IrradianceData {
    cubesides: array<vec3<f32>, 3>,
}

struct GridMetadata {
    resolution: vec3<i32>,
    corner: vec3<f32>,
    increment_x: vec3<f32>,
    increment_y: vec3<f32>,
    increment_z: vec3<f32>,
    level_bias: f32,
}

struct GridData {
    metadata: GridMetadata,
    transform: mat4x4<f32>,
    offset: i32,
}

#endif  // FRAGMENT_IRRADIANCE_VOLUME

@group(1) @binding(0)
var<uniform> base_color: vec4<f32>;
@group(1) @binding(1)
var base_color_texture: texture_2d<f32>;
@group(1) @binding(2)
var base_color_sampler: sampler;

#ifdef FRAGMENT_IRRADIANCE_VOLUME
@group(3) @binding(0)
var<uniform> grid_data: GridData;
@group(3) @binding(1)
var irradiance_grid: texture_2d<f32>;
#endif

#ifdef VERTEX_LIGHTMAP_UVS
@group(3) @binding(0)
var lightmap_texture: texture_2d<f32>;
@group(3) @binding(1)
var lightmap_sampler: sampler;
@group(3) @binding(2)
var<uniform> lightmap_uv_rect: vec4<f32>;
#endif

#ifdef FRAGMENT_REFLECTION_PROBE
@group(4) @binding(0)
var reflection_probe_diffuse: texture_cube<f32>;
@group(4) @binding(1)
var reflection_probe_specular: texture_cube<f32>;
@group(4) @binding(2)
var reflection_probe_sampler: sampler;
#endif

fn compute_ibl(
    irradiance: vec3<f32>,
    radiance: vec3<f32>,
    roughness: f32,
    diffuse_color: vec3<f32>,
    NdotV: f32,
    f_ab: vec2<f32>,
    F0: vec3<f32>,
) -> EnvironmentMapLight {
    // Multiscattering approximation: https://www.jcgt.org/published/0008/01/03/paper.pdf
    // Useful reference: https://bruop.github.io/ibl
    let Fr = max(vec3(1.0 - roughness), F0) - F0;
    let kS = F0 + Fr * pow(1.0 - NdotV, 5.0);
    let FssEss = kS * f_ab.x + f_ab.y;
    let Ess = f_ab.x + f_ab.y;
    let Ems = 1.0 - Ess;
    let Favg = F0 + (1.0 - F0) / 21.0;
    let Fms = FssEss * Favg / (1.0 - Ems * Favg);
    let FmsEms = Fms * Ems;
    let Edss = 1.0 - (FssEss + FmsEms);
    let kD = diffuse_color * Edss;

    var out: EnvironmentMapLight;
    out.diffuse = (FmsEms + kD) * irradiance;
    out.specular = FssEss * radiance;
    return out;

}

#ifdef FRAGMENT_IRRADIANCE_VOLUME

fn texel_fetch(st: vec2<i32>) -> vec4<f32> {
    return textureLoad(irradiance_grid, st, 0);
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

fn eevee_sample_irradiance_volume(p: vec3<f32>, n: vec3<f32>, r: vec3<f32>) -> SplitSum {
    let P = to_blender_coords(p);
    let N = normalize(to_blender_coords(n));
    let R = normalize(to_blender_coords(r));

    let corner = vec4(grid_data.metadata.corner, 1.0).xyz;
    let increment = vec3(
        vec4(grid_data.metadata.increment_x, 1.0).x,
        vec4(grid_data.metadata.increment_y, 1.0).y,
        vec4(grid_data.metadata.increment_z, 1.0).z,
    );

    var localpos = (P - corner) / increment;

    let localpos_floored = floor(localpos);
    let trilinear_weight = fract(localpos);

    //return localpos_floored / vec3<f32>(grid_data.metadata.resolution);

    var weight_accum = 0.0;
    var irradiance_accum = vec3(0.0);
    var radiance_accum = vec3(0.0);

    for (var i = 0; i < 8; i++) {
        let offset = vec3(i, i / 2, i / 4) & vec3(1);
        let cell_cos = clamp(
            localpos_floored + vec3<f32>(offset),
            vec3(0.0),
            vec3<f32>(grid_data.metadata.resolution) - 1.0);

        let icell_cos = vec3<i32>(cell_cos);
        let cell = grid_data.offset + icell_cos.z +
            grid_data.metadata.resolution.z *
            (icell_cos.y + grid_data.metadata.resolution.y * icell_cos.x);

        let irradiance = eevee_irradiance_from_cell_get(cell, N);
        let radiance = eevee_irradiance_from_cell_get(cell, R);

        let ws_cell_location = grid_data.metadata.corner +
            (grid_data.metadata.increment_x * cell_cos.x +
             grid_data.metadata.increment_y * cell_cos.y +
             grid_data.metadata.increment_z * cell_cos.z);

        let ws_point_to_cell = ws_cell_location - P;
        let ws_dist_point_to_cell = length(ws_point_to_cell);
        let ws_light = ws_point_to_cell / ws_dist_point_to_cell;

        var weight = clamp(dot(ws_light, N), 0.0, 1.0);

        weight += 1.0;

        let trilinear = mix(1.0 - trilinear_weight, trilinear_weight, vec3<f32>(offset));
        weight *= trilinear.x * trilinear.y * trilinear.z;

        weight = max(0.00001, weight);

        weight_accum += weight;
        irradiance_accum += irradiance * weight;
        radiance_accum += radiance * weight;
    }

    var out: SplitSum;
    out.irradiance = irradiance_accum / weight_accum;
    out.radiance = radiance_accum / weight_accum;
    return out;
}

fn irradiance_volume_light(
    perceptual_roughness: f32,
    roughness: f32,
    diffuse_color: vec3<f32>,
    NdotV: f32,
    f_ab: vec2<f32>,
    P: vec3<f32>,
    N: vec3<f32>,
    R: vec3<f32>,
    F0: vec3<f32>,
) -> EnvironmentMapLight {
    let split_sum = eevee_sample_irradiance_volume(P, N, R);
    return compute_ibl(
        split_sum.irradiance, split_sum.radiance, roughness, diffuse_color, NdotV, f_ab, F0);
}

#endif  // FRAGMENT_IRRADIANCE_VOLUME

#ifdef FRAGMENT_REFLECTION_PROBE

fn reflection_probe_light(
    perceptual_roughness: f32,
    roughness: f32,
    diffuse_color: vec3<f32>,
    NdotV: f32,
    f_ab: vec2<f32>,
    N: vec3<f32>,
    R: vec3<f32>,
    F0: vec3<f32>,
) -> EnvironmentMapLight {
    // Split-sum approximation for image based lighting: https://cdn2.unrealengine.com/Resources/files/2013SiggraphPresentationsNotes-26915738.pdf
    // Technically we could use textureNumLevels(environment_map_specular) - 1 here, but we use a uniform
    // because textureNumLevels() does not work on WebGL2
    let radiance_level = perceptual_roughness * 2.0;
    let irradiance = textureSample(reflection_probe_diffuse, reflection_probe_sampler, vec3(N.xy, -N.z)).rgb;
    let radiance = textureSampleLevel(reflection_probe_specular, reflection_probe_sampler, vec3(R.xy, -R.z), radiance_level).rgb;

    return compute_ibl(irradiance, radiance, roughness, diffuse_color, NdotV, f_ab, F0);
}

#endif  // FRAGMENT_REFLECTION_PROBE

@vertex
fn vertex(vertex: Vertex) -> VertexOutput {
    var model = mesh.model;

    var out: VertexOutput;
    out.position = mesh_position_local_to_clip(mesh.model, vec4<f32>(vertex.position, 1.0));
    out.world_position = mesh_position_local_to_world(model, vec4<f32>(vertex.position, 1.0));
    out.world_normal = mesh_normal_local_to_world(vertex.normal);
    out.uv = vertex.uv;
#ifdef VERTEX_LIGHTMAP_UVS
    out.lightmap_uv = vertex.lightmap_uv;
#endif
    return out;
}

@fragment
fn fragment(mesh: VertexOutput) -> @location(0) vec4<f32> {
    var color = base_color;
    color *= textureSample(base_color_texture, base_color_sampler, mesh.uv);
#ifdef VERTEX_LIGHTMAP_UVS
    let lightmap_uv = mix(lightmap_uv_rect.xy, lightmap_uv_rect.zw, mesh.lightmap_uv);
    color *= textureSample(lightmap_texture, lightmap_sampler, lightmap_uv) * 0.00075;
#endif

    let perceptual_roughness = 0.0;
    let roughness = lighting::perceptualRoughnessToRoughness(perceptual_roughness);
    let P = mesh.world_position.xyz;
    let V = pbr_functions::calculate_view(mesh.world_position, false);
    let N = mesh.world_normal.xyz;
    let NdotV = max(dot(N, V), 0.0001);
    let R = reflect(-V, N);
    let f_ab = lighting::F_AB(perceptual_roughness, NdotV);
    let F0 = vec3<f32>(1.0);

#ifdef FRAGMENT_IRRADIANCE_VOLUME
    let environment_light = irradiance_volume_light(perceptual_roughness, roughness, vec3<f32>(1.0), NdotV, f_ab, P, N, R, F0);
    color = vec4(environment_light.diffuse + environment_light.specular, 1.0);
#endif

#ifdef FRAGMENT_REFLECTION_PROBE
    // FIXME: This is wrong.
#ifndef VERTEX_LIGHTMAP_UVS
    let environment_light = reflection_probe_light(perceptual_roughness, roughness, vec3<f32>(1.0), NdotV, f_ab, N, R, F0);
    color = vec4(environment_light.diffuse + environment_light.specular, 1.0);
#endif
#endif

    return color;
}

/*
@fragment
fn fragment(
    in: MeshVertexOutput,
    @builtin(front_facing) is_front: bool,
) -> @location(0) vec4<f32> {
    var output_color: vec4<f32> = pbr_bindings::material.base_color;

    let is_orthographic = view.projection[3].w == 1.0;
    let V = pbr_functions::calculate_view(in.world_position, is_orthographic);
#ifdef VERTEX_UVS
    var uv = in.uv;
#ifdef VERTEX_TANGENTS
    if ((pbr_bindings::material.flags & pbr_types::STANDARD_MATERIAL_FLAGS_DEPTH_MAP_BIT) != 0u) {
        let N = in.world_normal;
        let T = in.world_tangent.xyz;
        let B = in.world_tangent.w * cross(N, T);
        // Transform V from fragment to camera in world space to tangent space.
        let Vt = vec3(dot(V, T), dot(V, B), dot(V, N));
        uv = parallaxed_uv(
            pbr_bindings::material.parallax_depth_scale,
            pbr_bindings::material.max_parallax_layer_count,
            pbr_bindings::material.max_relief_mapping_search_steps,
            uv,
            // Flip the direction of Vt to go toward the surface to make the
            // parallax mapping algorithm easier to understand and reason
            // about.
            -Vt,
        );
    }
#endif
#endif

#ifdef VERTEX_COLORS
    output_color = output_color * in.color;
#endif
#ifdef VERTEX_UVS
    if ((pbr_bindings::material.flags & pbr_types::STANDARD_MATERIAL_FLAGS_BASE_COLOR_TEXTURE_BIT) != 0u) {
        output_color = output_color * textureSampleBias(pbr_bindings::base_color_texture, pbr_bindings::base_color_sampler, uv, view.mip_bias);
    }
#endif

    // NOTE: Unlit bit not set means == 0 is true, so the true case is if lit
    if ((pbr_bindings::material.flags & pbr_types::STANDARD_MATERIAL_FLAGS_UNLIT_BIT) == 0u) {
        // Prepare a 'processed' StandardMaterial by sampling all textures to resolve
        // the material members
        var pbr_input: pbr_functions::PbrInput;

        pbr_input.material.base_color = output_color;
        pbr_input.material.reflectance = pbr_bindings::material.reflectance;
        pbr_input.material.flags = pbr_bindings::material.flags;
        pbr_input.material.alpha_cutoff = pbr_bindings::material.alpha_cutoff;

        // TODO use .a for exposure compensation in HDR
        var emissive: vec4<f32> = pbr_bindings::material.emissive;
#ifdef VERTEX_UVS
        if ((pbr_bindings::material.flags & pbr_types::STANDARD_MATERIAL_FLAGS_EMISSIVE_TEXTURE_BIT) != 0u) {
            emissive = vec4<f32>(emissive.rgb * textureSampleBias(pbr_bindings::emissive_texture, pbr_bindings::emissive_sampler, uv, view.mip_bias).rgb, 1.0);
        }
#endif
        pbr_input.material.emissive = emissive;

        var metallic: f32 = pbr_bindings::material.metallic;
        var perceptual_roughness: f32 = pbr_bindings::material.perceptual_roughness;
#ifdef VERTEX_UVS
        if ((pbr_bindings::material.flags & pbr_types::STANDARD_MATERIAL_FLAGS_METALLIC_ROUGHNESS_TEXTURE_BIT) != 0u) {
            let metallic_roughness = textureSampleBias(pbr_bindings::metallic_roughness_texture, pbr_bindings::metallic_roughness_sampler, uv, view.mip_bias);
            // Sampling from GLTF standard channels for now
            metallic = metallic * metallic_roughness.b;
            perceptual_roughness = perceptual_roughness * metallic_roughness.g;
        }
#endif
        pbr_input.material.metallic = metallic;
        pbr_input.material.perceptual_roughness = perceptual_roughness;

        // TODO: Split into diffuse/specular occlusion?
        var occlusion: vec3<f32> = vec3(1.0);
#ifdef VERTEX_UVS
        if ((pbr_bindings::material.flags & pbr_types::STANDARD_MATERIAL_FLAGS_OCCLUSION_TEXTURE_BIT) != 0u) {
            occlusion = vec3(textureSampleBias(pbr_bindings::occlusion_texture, pbr_bindings::occlusion_sampler, uv, view.mip_bias).r);
        }
#endif
#ifdef SCREEN_SPACE_AMBIENT_OCCLUSION
        let ssao = textureLoad(screen_space_ambient_occlusion_texture, vec2<i32>(in.position.xy), 0i).r;
        let ssao_multibounce = gtao_multibounce(ssao, pbr_input.material.base_color.rgb);
        occlusion = min(occlusion, ssao_multibounce);
#endif
        pbr_input.occlusion = occlusion;

        pbr_input.frag_coord = in.position;
        pbr_input.world_position = in.world_position;

        pbr_input.world_normal = pbr_functions::prepare_world_normal(
            in.world_normal,
            (pbr_bindings::material.flags & pbr_types::STANDARD_MATERIAL_FLAGS_DOUBLE_SIDED_BIT) != 0u,
            is_front,
        );

        pbr_input.is_orthographic = is_orthographic;

#ifdef LOAD_PREPASS_NORMALS
        pbr_input.N = bevy_pbr::prepass_utils::prepass_normal(in.position, 0u);
#else
        pbr_input.N = pbr_functions::apply_normal_mapping(
            pbr_bindings::material.flags,
            pbr_input.world_normal,
#ifdef VERTEX_TANGENTS
#ifdef STANDARDMATERIAL_NORMAL_MAP
            in.world_tangent,
#endif
#endif
#ifdef VERTEX_UVS
            uv,
#endif
            view.mip_bias,
        );
#endif

        pbr_input.V = V;
        pbr_input.occlusion = occlusion;

        pbr_input.flags = mesh.flags;

        output_color = pbr_functions::pbr(pbr_input);
    } else {
        output_color = pbr_functions::alpha_discard(pbr_bindings::material, output_color);
    }

    // fog
    if (fog.mode != FOG_MODE_OFF && (pbr_bindings::material.flags & pbr_types::STANDARD_MATERIAL_FLAGS_FOG_ENABLED_BIT) != 0u) {
        output_color = pbr_functions::apply_fog(fog, output_color, in.world_position.xyz, view.world_position.xyz);
    }

#ifdef TONEMAP_IN_SHADER
    output_color = tone_mapping(output_color, view.color_grading);
#ifdef DEBAND_DITHER
    var output_rgb = output_color.rgb;
    output_rgb = powsafe(output_rgb, 1.0 / 2.2);
    output_rgb = output_rgb + screen_space_dither(in.position.xy);
    // This conversion back to linear space is required because our output texture format is
    // SRGB; the GPU will assume our output is linear and will apply an SRGB conversion.
    output_rgb = powsafe(output_rgb, 2.2);
    output_color = vec4(output_rgb, output_color.a);
#endif
#endif
#ifdef PREMULTIPLY_ALPHA
    output_color = pbr_functions::premultiply_alpha(pbr_bindings::material.flags, output_color);
#endif
    return output_color;
}
*/