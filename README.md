# `bevy-baked-gi`

An implementation of precomputed global illumination for the Bevy game
engine.

This crate, `bevy-baked-gi`, provides a workflow for *baked* global
illumination in Bevy, using two well-established techniques: *lightmapping*
and *irradiance volumes*.  [Global illumination] enhances the realism of
rendered scenes by modeling the effects of multiple light bounces; as a
simple example, GI allows objects next to a red wall to appear red.
Generally, GI is expensive to simulate, so a very popular technique is to
precompute the lighting for static elements of a scene to allow for fast
approximation of global illumination at runtime. This crate provides a
straightforward Bevy implementation of two of the most common methods:
lightmapping and irradiance volumes.

It's important to note that *this crate doesn't currently provide any
lightmapping or baking tool*. You'll need to precompute the lightmaps for
your scenes using a tool external to Bevy. Fortunately, `bevy-baked-gi`
provides convenient tools that allow the use of either [Blender] with [The
Lightmapper] add-on or [Unity] to compute the lighting. You can use either
these tools or another of your choice:

1. `bevy-baked-gi` provides a built-in asset loader for glTF scenes with a
`.gi.gltf` or `.gi.glb` extension that correctly handles global illumination
baked with The Lightmapper. Lightmaps built with The Lightmapper will be
automatically detected and applied to the scene. Simply use The Lightmapper
to bake your scenes and then export them as glTF with a `.gi.glb` extension,
and `bevy-baked-gi` will pick up and apply the lightmaps.

2. You can use the `export-blender-gi` tool on a `.blend` file to convert
baked irradiance volumes from Blender's native format into a native Bevy
`.scn.ron` asset.

3. For scenes in which The Lightmapper is insufficient, a Unity-based
lightmap workflow is also supported:

  a. Copy the contents of the Unity folder into a new Unity project.

  b. Set any settings you wish. The HDR pipeline is recommended, but any
  pipeline should work.

  c. Import your glTF file as an asset, and place it in the scene.

  d. Mark the resulting prefab, and all its descendants, as Static. Make any
  adjustments to the scene that you need to.

  e. Bake the lightmap. You can either use Unity's built-in Progressive
  Lightmapper, available under Window → Lighting, or [Bakery] from the Unity
  Asset Store.

  f. Open the Bevy Lightmapper window with Window → Bevy Lightmapper.

  g. Drag your prefab into "glTF Scene Root".

  h. Adjust the output file and lightmaps as you want.

  i. Click "Export Lightmap UV" and "Export Lightmaps".

  j. Load the resulting `.gi.glb` file into Bevy as usual.

In case you want to bake the lightmaps using a tool other than Blender and
Unity, a detailed description of the lightmap and irradiance volume format
is available in [the documentation].

[Global illumination]: http://en.wikipedia.org/wiki/Global_illumination

[Unity]: http://unity.com/
