# Irradiance Volumes Reference

This document describes the technical details of the format of `bevy-baked-gi`'s irradiance volumes. It may be useful for those implementing custom baking tools or writing custom shaders that take irradiance volume information into account.

## Algorithm

The overall approach is known as *ambient cubes*, which was popularized by *Half-Life 2* [^Valve2006]. The irradiance volume, a cuboid, is divided into voxels, which need not be perfect cubes. Each voxel contains a 1×1 pixel cubemap, each side of which contains a single high-dynamic-range RGB color.

[^Valve2006]: Mitchell, Jason. 2006. “Shading in Valve’s Source Engine.” Slide show. https://advances.realtimerendering.com/s2006/Mitchell-ShadingInValvesSourceEngine.pdf.
