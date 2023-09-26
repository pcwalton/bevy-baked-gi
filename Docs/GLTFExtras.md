# `bevy-baked-gi` glTF Extras Reference

The [glTF] format is extensible with [extras], described in the specification as "application-specific data". `bevy-baked-gi` takes advantage of this flexibility in order to encode some global-illumination-related information that the glTF format doesn't provide any standard mechanism for.

In [Blender], glTF extras are exposed as [Custom Properties] in the editor. When exporting glTF from that application, make sure "Custom Properties" is checked under the export settings.

## Enabling global illumination

### The `DisableGI` property

`DisableGI` is a Boolean property applied to objects (not meshes). If present and set to `true`, then no baked global illumination will be applied to the object, and its materials will not be upgraded into [GiPbrMaterial]s.

[extras]: https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#reference-extras
