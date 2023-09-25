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

`bevy-baked-gi` provides a built-in asset loader for glTF scenes with a
`.gi.gltf` or `.gi.glb` extension that correctly handles global illumination
baked with The Lightmapper. Lightmaps built with that addon and exported to glTF
with a `.gl.glb` extension are automatically detected and applied to the scene.

You may also use Cycles' built-in baking, as long as you place the lightmap UVs
into the second UV channel (UV1, `TEXCOORD1`) and add the relative path to the
lightmap as a glTF extra named "Lightmap" on each mesh object. glTF extras are
accessible in Blender as "Custom Properties", as long as you check the "Custom
Properties" box during export. Be sure to put the custom property on the
*object* containing the mesh, not the mesh itself.

### `export-blender-gi`

`bevy-baked-gi` comes with a command-line tool called `export-blender-gi` that
you can apply to a `.blend` file to convert baked irradiance volumes and
reflection probes from Blender's native format into a native Bevy `.scn.ron`
asset. For cubemap reflection probes, the tool uses a fork of the [glTF IBL
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
is available in [the documentation].

[Global illumination]: http://en.wikipedia.org/wiki/Global_illumination

[Unity]: http://unity.com/
