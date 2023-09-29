# `bevy-baked-gi` glTF Extras Reference

This document details the extensions to the [glTF] format that `bevy-baked-gi` uses for global illumination. It may be useful for those writing tools that interoperate with `bevy-baked-gi`, such as lightmappers.

## Introduction

The glTF format is extensible with [extras], described in the specification as "application-specific data". `bevy-baked-gi` takes advantage of this flexibility in order to encode some global-illumination-related information that the glTF format doesn't provide any standard mechanism for.

In [Blender], glTF extras are exposed as [Custom Properties] in the editor. When exporting glTF from that application, make sure "Custom Properties" is checked under the [export settings].

Each glTF extra is expected to be a JSON object. The object keys that `bevy-baked-gi` supports are detailed below.

## Enabling global illumination

### The `DisableGI` property

`DisableGI` is a Boolean property applied to objects (not meshes). If present and set to `true`, then no baked global illumination will be applied to the object, and its materials will not be upgraded into `GiPbrMaterial`s.

## Lightmapping

### The `Lightmap` property

`Lightmap` is a string property applied to objects (not meshes). If present, it specifies the path to a `.hdr` file that specifies the lightmap texture for the object in [Radiance HDR] format. The path is relative to the exported glTF file. EXR and other formats are currently unsupported. By default, the entire texture rectangle is mapped to UV range (0.0, 0.0) to (1.0, 1.0), but this can be changed with the `LightmapMinU`, `LightmapMinV`, `LightmapMaxU`, and `LightmapMaxV` properties below.

### The `LightmapMinU`, `LightmapMinV`, `LightmapMaxU`, and `LightmapMaxV` properties

`LightmapMinU`, `LightmapMinV`, `LightmapMaxU`, and `LightmapMaxV` are numeric properties that specify the rectangle within the lightmap texture from which the lightmap is to be sampled. When these four properties are set, the UV coordinate (0.0, 0.0) will be mapped to `(LightmapMinU, LightmapMinV)`, and the UV coordinate (1.0, 1.0) will be mapped to `(LightmapMaxU, LightmapMaxV)`. These properties allow lightmaps for different meshes (and different copies of the same mesh) to be packed into texture atlases.

By default, `LightmapMinU` and `LightmapMinV` are set to 0.0, and `LightmapMaxU` and `LightmapMaxV` are set to 1.0. These properties have no effect if `Lightmap` isn't specified. If any of these properties are specified, all four must be specified; an error is reported otherwise.

[glTF]: https://www.khronos.org/gltf/

[Blender]: https://www.blender.org/

[Custom Properties]: https://docs.blender.org/manual/en/3.6/files/custom_properties.html

[export settings]: https://docs.blender.org/manual/en/2.80/addons/io_scene_gltf2.html#general-tab

[extras]: https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-extras
