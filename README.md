# `bevy-baked-gi`

An implementation of precomputed global illumination for the Bevy game
engine.

## Introduction

This crate, `bevy-baked-gi`, provides a workflow for *baked* global illumination
in Bevy, using three well-established techniques: *lightmapping*, *irradiance
volumes*, and *reflection probes*. [Global illumination] enhances the realism
of rendered scenes by modeling the effects of multiple light bounces; as a
simple example, GI allows objects next to a red wall to appear red.  Generally,
GI is expensive to simulate, so a popular technique is to precompute the
lighting for static elements of a scene to allow for fast approximation of
global illumination at runtime. This crate provides a straightforward Bevy
implementation of these three common methods.

It's important to note that *neither this crate nor Bevy itself provides any
built-in lightmapper*. You'll need to precompute the lightmaps for your scenes
using a tool external to Bevy. Fortunately, `bevy-baked-gi` provides a
convenient tool, `export-blender-gi`, that allows for the use of [Blender]
(optionally with [The Lightmapper] addon) to bake global illumination.

## Tooling

The following tools may be useful for baking lightmaps.

### Blender lightmapping

`bevy-baked-gi` provides a built-in asset loader for [glTF] scenes with a
`.gi.gltf` or `.gi.glb` extension that correctly handles global illumination
baked with The Lightmapper. Lightmaps built with that addon and exported to glTF
with a `.gl.glb` extension are automatically detected and applied to the scene.

You may also use [Cycles' built-in baking], as long as you place the lightmap
UVs into the second UV channel (UV1, `TEXCOORD1`) and add the relative path to
the lightmap as a glTF extra named "Lightmap" on each mesh object. glTF extras
are accessible in Blender as "Custom Properties", as long as you check the
"Custom Properties" box during export. Be sure to put the custom property on the
*object* containing the mesh, not the mesh itself.

### `export-blender-gi`

`bevy-baked-gi` comes with a command-line tool called `export-blender-gi` that
you can apply to a Blender `.blend` file to convert baked irradiance volumes and
reflection probes from Blender's native format into a native Bevy `.scn.ron`
asset. For cubemap reflection probes, the tool uses a [fork] of the [glTF IBL
Sampler] to preprocess the cubemap images into diffuse and specular parts, so
they can be used just as environment maps are used in Bevy.

### glTF Unity Lightmapper

Within the UnityAssets directory there is an experimental [Unity] plugin to
allow for using Unity's [Progressive Lightmapper] (or another tool, like
[Bakery]) to bake the lighting for scenes. Please note that this plugin is
highly experimental.

### Other tools

In case you want to bake the lightmaps using a tool other than Blender and
Unity, a detailed description of the [lightmap] and [irradiance volume format]
is available in [the documentation].

## Roadmap

An eventual goal of this work is for the rendering code to go upstream. If that happens, then the focus of this crate will narrow from rendering to tooling: reading and applying the glTF extras and providing the baking tools.

## License

Dual-licensed under the MIT and Apache 2.0 license. See the `LICENSE-MIT` and `LICENSE-APACHE2` files.

## Code of conduct

`bevy-baked-gi` follows the same code of conduct as Rust itself. Reports can be
made to the project authors.

[Global illumination]: http://en.wikipedia.org/wiki/Global_illumination

[Blender]: https://www.blender.org/

[The Lightmapper]: https://github.com/Naxela/The_Lightmapper

[glTF]: https://www.khronos.org/gltf/

[Cycles' built-in baking]: https://docs.blender.org/manual/en/latest/render/cycles/baking.html

[fork]: https://github.com/pcwalton/glTF-IBL-Sampler

[glTF IBL Sampler]: https://github.com/KhronosGroup/glTF-IBL-Sampler

[Unity]: http://unity.com/

[Progressive Lightmapper]: https://docs.unity3d.com/Manual/progressive-lightmapper.html

[Bakery]: https://assetstore.unity.com/packages/tools/level-design/bakery-gpu-lightmapper-122218

[lightmap]: Docs/GLTFExtras.md

[irradiance volume format]: Docs/IrradianceVolumes.md

[the documentation]: Docs/
