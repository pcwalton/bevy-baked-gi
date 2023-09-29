# Irradiance Volumes Reference

This document describes the technical details of the format of `bevy-baked-gi`'s irradiance volumes. It may be useful for those implementing custom baking tools or writing custom shaders that take irradiance volume information into account.

## Algorithm

The overall approach is known as *ambient cubes*, which was popularized by
*Half-Life 2* [^Valve2006]. The irradiance volume, a cuboid, is divided into
voxels, which need not be perfect cubes. Each voxel contains a 1×1 pixel
cubemap, each side of which contains a single high-dynamic-range RGB color.
Because current GPUs don't support 4D (width × height × depth × cubemap face)
textures directly, the cubes are instead packed into a 2D texture and
interpolated in software.

The actual implementation is very similar to that of [Blender]'s [Eevee] renderer. Thus using Blender as a baking tool to construct the irradiance volumes is a straightforward process.

At runtime, the nearest eight voxels to the world-space position of the fragment are fetched and interpolated between to produce a single cubemap. This cubemap is then sampled with the normal vector to produce an approximate diffuse color and with the reflection vector to produce an approximate specular color. These are then provided to the Bevy PBR shader as the indirect light term, overriding any environment map.

Irradiance volumes only affect meshes whose axis-aligned bounding box center falls within the bounding cuboid. It's expected that irradiance volumes not overlap; if they do, the behavior is presently undefined.

## Texture format

We assume that $(u, v) = (0, 0)$ represents the upper left corner of the texture and that $(u, v) = (1, 1)$ represents the lower right corner of the texture. We currently require that the texture be 2D and in linear RGB (not sRGB) `R8G8B8A8_UNORM` format.

The texture is divided into *cells*, each of which is a 3×2 texel block. (Thus, the size of the texture must be a multiple of 3 in the U direction and 2 in the V direction.) Cells are laid out in order from left to right, then top to bottom. The exact width of the texture can be set freely as long as it's a multiple of 3.

Given the texture width $w$, the upper left corner $\mathrm{uv}$ of the cell at index $i$ can be determined with the following formula:

$$\mathrm{uv}_i = \left(3(i\ \mathrm{mod}\ w/3), 2\left\lfloor \frac{i}{w/3}\right\rfloor\right)$$

Regarding the voxel grid, the cells are laid out in $z$-major order, then $y$-major order, then $x$. Thus the index $i$ of a cell at voxel coordinate $(x, y, z)$ in a voxel grid of width $w$ and height $h$ can be determined as follows:

$$i_{(x, y, z)} = zwh + yw + x$$

Each texel of the cell encodes the color of a side of the voxel cube. The upper row of each cell represents the colors of the positive cube sides, while the lower row represents the colors of the negative cube sides. The columns of each cell represent the $x$, $y$, and $z$ axes in that order. Consequently, the cell layout looks like this:

|       |       |       |
|  :-:  |  :-:  |  :-:  |
|  +x   |  +y   |  +z   |
|  -x   |  -y   |  -z   |

Colors are encoded using an 8-bit shared-exponent format. The alpha value $A$, ranging from 0.0 to 1.0, represents a shared exponent which is applied to all the red, green, and blue channels $(R, G, B)$, which also range from 0.0 to 1.0. A color $C$ is encoded in this manner:

$$C = 2^{255A - 128}(R, G, B)$$

[^Valve2006]: Mitchell, Jason. 2006. “Shading in Valve’s Source Engine.” Slide show. https://advances.realtimerendering.com/s2006/Mitchell-ShadingInValvesSourceEngine.pdf.

[Blender]: http://blender.org/

[Eevee]: https://docs.blender.org/manual/en/latest/render/eevee/introduction.html
