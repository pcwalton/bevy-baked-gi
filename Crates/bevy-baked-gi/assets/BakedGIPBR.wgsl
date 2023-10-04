// bevy-baked-gi/Crates/bevy-baked-gi/assets/IrradianceVolumePBR.wgsl

#import bevy_pbr::pbr_functions as pbr_functions
#import bevy_pbr::pbr_bindings as pbr_bindings
#import bevy_pbr::pbr_types as pbr_types
#import bevy_pbr::prepass_utils
#import bevy_pbr::lighting as lighting
#import bevy_pbr::mesh_view_bindings as view_bindings
#import bevy_pbr::clustered_forward as clustering
#import bevy_pbr::mesh_view_types as mesh_view_types
#import bevy_pbr::mesh_functions as mesh_functions
#import bevy_pbr::shadows as shadows
#import bevy_pbr::ambient as ambient

#import bevy_pbr::mesh_bindings            mesh
#import bevy_pbr::mesh_functions           mesh_position_local_to_clip, mesh_position_local_to_world, mesh_normal_local_to_world
#import bevy_pbr::mesh_view_bindings       view, fog, screen_space_ambient_occlusion_texture
#import bevy_pbr::mesh_view_types          FOG_MODE_OFF
#import bevy_core_pipeline::tonemapping    screen_space_dither, powsafe, tone_mapping
#import bevy_pbr::parallax_mapping         parallaxed_uv
#import bevy_pbr::pbr_functions            PbrInput, alpha_discard
#import bevy_pbr::mesh_types               MESH_FLAGS_SHADOW_RECEIVER_BIT

#import bevy_pbr::prepass_utils

#import bevy_pbr::environment_map          EnvironmentMapLight

#ifdef SCREEN_SPACE_AMBIENT_OCCLUSION
#import bevy_pbr::gtao_utils gtao_multibounce
#endif

struct Vertex {
    @location(0) position: vec3<f32>,
    @location(1) normal: vec3<f32>,
#ifdef VERTEX_UVS
    @location(2) uv: vec2<f32>,
#endif
#ifdef VERTEX_TANGENTS
    @location(3) tangent: vec4<f32>,
#endif
#ifdef VERTEX_COLORS
    @location(4) color: vec4<f32>,
#endif
#ifdef SKINNED
    @location(5) joint_indices: vec4<u32>,
    @location(6) joint_weights: vec4<f32>,
#endif
#ifdef VERTEX_LIGHTMAP_UVS
    @location(7) lightmap_uv: vec2<f32>,
#endif
#ifdef MORPH_TARGETS
    @builtin(vertex_index) index: u32,
#endif
};

struct MeshGiVertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) world_position: vec4<f32>,
    @location(1) world_normal: vec3<f32>,
#ifdef VERTEX_UVS
    @location(2) uv: vec2<f32>,
#endif
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

#ifdef VERTEX_LIGHTMAP_UVS

struct LightmapSettings {
    uv_rect: vec4<f32>,
    exposure: f32,
}

#endif

#ifdef FRAGMENT_IRRADIANCE_VOLUME

struct IrradianceData {
    cubesides: array<vec3<f32>, 3>,
}

struct IrradianceVolumeMetadata {
    transform: mat4x4<f32>,
    inverse_transform: mat4x4<f32>,
    resolution: vec3<i32>,
}

struct IrradianceVolumeDescriptor {
    metadata: IrradianceVolumeMetadata,
    transform: mat4x4<f32>,
}

#endif  // FRAGMENT_IRRADIANCE_VOLUME

#ifdef FRAGMENT_IRRADIANCE_VOLUME
@group(3) @binding(0)
var<uniform> irradiance_volume_descriptor: IrradianceVolumeDescriptor;
@group(3) @binding(1)
var irradiance_volume_texture: texture_2d<f32>;
#endif

#ifdef VERTEX_LIGHTMAP_UVS
@group(3) @binding(0)
var lightmap_texture: texture_2d<f32>;
@group(3) @binding(1)
var lightmap_sampler: sampler;
@group(3) @binding(2)
var<uniform> lightmap_settings: LightmapSettings;
#endif

#ifdef FRAGMENT_REFLECTION_PROBE
@group(3) @binding(0)
var reflection_probe_diffuse: texture_cube<f32>;
@group(3) @binding(1)
var reflection_probe_specular: texture_cube<f32>;
@group(3) @binding(2)
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

// This is based on Blender Eevee's shader for sampling irradiance volumes, heavily modified for
// our use case.

fn voxelgi_fetch_and_decode(st: vec2<i32>) -> vec3<f32> {
    let data = textureLoad(irradiance_volume_texture, st, 0);
    return data.rgb * exp2(data.a * 255.0 - 128.0);
}

fn voxelgi_load_irradiance_cell(cell_index: i32, N: vec3<f32>) -> mat3x3<f32> {
    let cells_per_row = i32(textureDimensions(irradiance_volume_texture, 0).x) / 3;
    let cell_origin = vec2(cell_index % cells_per_row * 3, cell_index / cells_per_row * 2);

    let is_negative = vec3<i32>(N < vec3(0.0));

    return mat3x3<f32>(
        voxelgi_fetch_and_decode(cell_origin + vec2(0, is_negative.x)),
        voxelgi_fetch_and_decode(cell_origin + vec2(1, is_negative.y)),
        voxelgi_fetch_and_decode(cell_origin + vec2(2, is_negative.z)));
}

fn voxelgi_sample_irradiance_volume(P: vec3<f32>, n: vec3<f32>) -> vec3<f32> {
    let N = normalize(n);

    let resolution = irradiance_volume_descriptor.metadata.resolution;
    let transform = irradiance_volume_descriptor.metadata.transform;
    let inverse_transform = irradiance_volume_descriptor.metadata.inverse_transform;

    let max_cell = vec3<f32>(resolution) - 1.0;

    let local_space_pos = (inverse_transform * vec4(P, 1.0)).xyz;
    let local_space_pos_floored = floor(local_space_pos);
    let trilinear_weight = fract(local_space_pos);

    var weight_accum = 0.0;
    var irradiance_accum = vec3(0.0);
    var radiance_accum = vec3(0.0);

    for (var i = 0; i < 8; i++) {
        let offset = vec3(i, i / 2, i / 4) & vec3(1);
        let cell_center = clamp(local_space_pos_floored + vec3<f32>(offset), vec3(0.0), max_cell);
        let cell_index = i32(cell_center.z) +
            resolution.z * (i32(cell_center.y) + resolution.y * i32(cell_center.x));

        let irradiance = voxelgi_load_irradiance_cell(cell_index, N) * (N * N);

        var weight = clamp(
            dot(normalize((transform * vec4(cell_center, 1.0)).xyz - P), N), 0.0, 1.0) + 1.0;
        let trilinear = mix(1.0 - trilinear_weight, trilinear_weight, vec3<f32>(offset));
        weight = max(0.00001, weight * trilinear.x * trilinear.y * trilinear.z);

        weight_accum += weight;
        irradiance_accum += irradiance * weight;
    }

    return irradiance_accum / weight_accum;
}

fn irradiance_volume_light(diffuse_color: vec3<f32>, P: vec3<f32>, N: vec3<f32>) -> vec3<f32> {
    return voxelgi_sample_irradiance_volume(P, N) * diffuse_color;
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

// From `pbr_functions.wgsl`, modified to support reflection probes and irradiance volumes.
#ifndef PREPASS_FRAGMENT
fn pbr(
    in: PbrInput,
    lightmap_uv: vec2<f32>,
) -> vec4<f32> {
    var output_color: vec4<f32> = in.material.base_color;

    // TODO use .a for exposure compensation in HDR
    let emissive = in.material.emissive;

    // calculate non-linear roughness from linear perceptualRoughness
    let metallic = in.material.metallic;
    let perceptual_roughness = in.material.perceptual_roughness;
    let roughness = lighting::perceptualRoughnessToRoughness(perceptual_roughness);

    let occlusion = in.occlusion;

    output_color = alpha_discard(in.material, output_color);

    // Neubelt and Pettineo 2013, "Crafting a Next-gen Material Pipeline for The Order: 1886"
    let NdotV = max(dot(in.N, in.V), 0.0001);

    // Remapping [0,1] reflectance to F0
    // See https://google.github.io/filament/Filament.html#materialsystem/parameterization/remapping
    let reflectance = in.material.reflectance;
    let F0 = 0.16 * reflectance * reflectance * (1.0 - metallic) + output_color.rgb * metallic;

    // Diffuse strength inversely related to metallicity
    let diffuse_color = output_color.rgb * (1.0 - metallic);

    let R = reflect(-in.V, in.N);

    let f_ab = lighting::F_AB(perceptual_roughness, NdotV);

    var direct_light: vec3<f32> = vec3<f32>(0.0);

    let view_z = dot(vec4<f32>(
        view.inverse_view[0].z,
        view.inverse_view[1].z,
        view.inverse_view[2].z,
        view.inverse_view[3].z
    ), in.world_position);
    let cluster_index = clustering::fragment_cluster_index(in.frag_coord.xy, view_z, in.is_orthographic);
    let offset_and_counts = clustering::unpack_offset_and_counts(cluster_index);

    // Point lights (direct)
    for (var i: u32 = offset_and_counts[0]; i < offset_and_counts[0] + offset_and_counts[1]; i = i + 1u) {
        let light_id = clustering::get_light_id(i);
        var shadow: f32 = 1.0;
        if ((mesh.flags & MESH_FLAGS_SHADOW_RECEIVER_BIT) != 0u
                && (view_bindings::point_lights.data[light_id].flags & mesh_view_types::POINT_LIGHT_FLAGS_SHADOWS_ENABLED_BIT) != 0u) {
            shadow = shadows::fetch_point_shadow(light_id, in.world_position, in.world_normal);
        }
        let light_contrib = lighting::point_light(in.world_position.xyz, light_id, roughness, NdotV, in.N, in.V, R, F0, f_ab, diffuse_color);
        direct_light += light_contrib * shadow;
    }

    // Spot lights (direct)
    for (var i: u32 = offset_and_counts[0] + offset_and_counts[1]; i < offset_and_counts[0] + offset_and_counts[1] + offset_and_counts[2]; i = i + 1u) {
        let light_id = clustering::get_light_id(i);

        var shadow: f32 = 1.0;
        if ((mesh.flags & MESH_FLAGS_SHADOW_RECEIVER_BIT) != 0u
                && (view_bindings::point_lights.data[light_id].flags & mesh_view_types::POINT_LIGHT_FLAGS_SHADOWS_ENABLED_BIT) != 0u) {
            shadow = shadows::fetch_spot_shadow(light_id, in.world_position, in.world_normal);
        }
        let light_contrib = lighting::spot_light(in.world_position.xyz, light_id, roughness, NdotV, in.N, in.V, R, F0, f_ab, diffuse_color);
        direct_light += light_contrib * shadow;
    }

    // directional lights (direct)
    let n_directional_lights = view_bindings::lights.n_directional_lights;
    for (var i: u32 = 0u; i < n_directional_lights; i = i + 1u) {
        var shadow: f32 = 1.0;
        if ((mesh.flags & MESH_FLAGS_SHADOW_RECEIVER_BIT) != 0u
                && (view_bindings::lights.directional_lights[i].flags & mesh_view_types::DIRECTIONAL_LIGHT_FLAGS_SHADOWS_ENABLED_BIT) != 0u) {
            shadow = shadows::fetch_directional_shadow(i, in.world_position, in.world_normal, view_z);
        }
        var light_contrib = lighting::directional_light(i, roughness, NdotV, in.N, in.V, R, F0, f_ab, diffuse_color);
#ifdef DIRECTIONAL_LIGHT_SHADOW_MAP_DEBUG_CASCADES
        light_contrib = shadows::cascade_debug_visualization(light_contrib, i, view_z);
#endif
        direct_light += light_contrib * shadow;
    }

    // Ambient light (indirect)
    var indirect_light = ambient::ambient_light(in.world_position, in.N, in.V, NdotV, diffuse_color, F0, perceptual_roughness, occlusion);

#ifdef VERTEX_LIGHTMAP_UVS
    indirect_light += textureSample(lightmap_texture, lightmap_sampler, lightmap_uv).rgb * lightmap_settings.exposure * diffuse_color;
#else
#ifdef FRAGMENT_REFLECTION_PROBE
    let environment_light = reflection_probe_light(perceptual_roughness, roughness, diffuse_color, NdotV, f_ab, in.N, R, F0);
    indirect_light += (environment_light.diffuse * occlusion) + environment_light.specular;
#else
#ifdef FRAGMENT_IRRADIANCE_VOLUME
    let environment_light = irradiance_volume_light(diffuse_color, in.world_position.xyz, in.N);
    indirect_light += environment_light;
#else
#ifdef ENVIRONMENT_MAP
    // Environment map light (indirect)
    let environment_light = bevy_pbr::environment_map::environment_map_light(perceptual_roughness, roughness, diffuse_color, NdotV, f_ab, in.N, R, F0);
    indirect_light += (environment_light.diffuse * occlusion) + environment_light.specular;
#endif  // ENVIRONMENT_MAP
#endif  // FRAGMENT_IRRADIANCE_VOLUME
#endif  // FRAGMENT_REFLECTION_PROBE
#endif  // VERTEX_LIGHTMAP_UVS

    let emissive_light = emissive.rgb * output_color.a;

    // Total light
    output_color = vec4<f32>(
        direct_light + indirect_light + emissive_light,
        output_color.a
    );

    output_color = clustering::cluster_debug_visualization(
        output_color,
        view_z,
        in.is_orthographic,
        offset_and_counts,
        cluster_index,
    );

    return output_color;
}
#endif // PREPASS_FRAGMENT


// From `mesh.wgsl`.
@vertex
fn vertex(vertex_no_morph: Vertex) -> MeshGiVertexOutput {
    var out: MeshGiVertexOutput;

#ifdef MORPH_TARGETS
    var vertex = morph_vertex(vertex_no_morph);
#else
    var vertex = vertex_no_morph;
#endif

#ifdef SKINNED
    var model = bevy_pbr::skinning::skin_model(vertex.joint_indices, vertex.joint_weights);
#else
    var model = mesh.model;
#endif

#ifdef VERTEX_NORMALS
#ifdef SKINNED
    out.world_normal = bevy_pbr::skinning::skin_normals(model, vertex.normal);
#else
    out.world_normal = mesh_functions::mesh_normal_local_to_world(vertex.normal);
#endif
#endif

#ifdef VERTEX_POSITIONS
    out.world_position = mesh_functions::mesh_position_local_to_world(model, vec4<f32>(vertex.position, 1.0));
    out.position = mesh_functions::mesh_position_world_to_clip(out.world_position);
#endif

#ifdef VERTEX_UVS
    out.uv = vertex.uv;
#endif

#ifdef VERTEX_TANGENTS
    out.world_tangent = mesh_functions::mesh_tangent_local_to_world(model, vertex.tangent);
#endif

#ifdef VERTEX_COLORS
    out.color = vertex.color;
#endif

#ifdef VERTEX_LIGHTMAP_UVS
    out.lightmap_uv = vertex.lightmap_uv;
#endif

    return out;
}

@fragment
fn fragment(
    in: MeshGiVertexOutput,
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

#ifdef VERTEX_LIGHTMAP_UVS
    let lightmap_uv = mix(lightmap_settings.uv_rect.xy, lightmap_settings.uv_rect.zw, in.lightmap_uv);
#else
    let lightmap_uv = vec2(0.0);
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

        output_color = pbr(pbr_input, lightmap_uv);
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
