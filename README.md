# `bevy-baked-gi`

Tooling to support precomputed global illumination for the Bevy game engine.

## Introduction

This crate, `bevy-baked-gi`, provides a workflow for *baked* global illumination
in Bevy, using three well-established techniques: *lightmapping*, *irradiance
volumes*, and *reflection probes*. [Global illumination] enhances the realism of
rendered scenes by modeling the effects of multiple light bounces; as a simple
example, GI allows objects next to a red wall to appear red.  Generally, GI is
expensive to simulate, so a popular technique is to precompute the lighting for
static elements of a scene to allow for fast approximation of global
illumination at runtime. This crate provides straightforward Bevy tooling to
support these three common methods.

It's important to note that *neither this crate nor Bevy itself provides any
built-in lightmapper*. You'll need to precompute the lightmaps for your scenes
using a tool external to Bevy. Fortunately, `bevy-baked-gi` provides a
convenient tool, `export-blender-gi`, that allows for the use of [Blender]
(optionally with [The Lightmapper] addon) to bake global illumination.

## Tooling

The following tool may be useful for baking lightmaps.

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
Unity, a detailed description of the lightmap and irradiance volume format
is available in the Bevy documentation.

## License

Dual-licensed under the MIT and Apache 2.0 license. See the `LICENSE-MIT` and
`LICENSE-APACHE2` files.

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

[the documentation]: Docs/
