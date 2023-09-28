# Unity glTF Lightmapper Manual

This document describes the Unity glTF Lightmapper, which is an experimental, optional tool that comes with `bevy-baked-gi` and allows you to use the [Unity] editor to perform lightmapping. Unity is just one of many tools that you can use for lightmapping; you don't have to use this tool if you don't want to.

## Introduction

*Global illumination* (GI) is a technique that enhances the realism of computer-generated scenes by simulating not only *direct* light coming from light sources but also *indirect* light emitted from surfaces when direct light bounces off them. Because global illumination is expensive to compute, one popular technique to speed it up is *lightmapping*, which performs the heavy calculations offline in a process known as *baking*. The baking process stores the computed indirect illumination in textures known as *lightmaps*, which are applied to the meshes to be rendered at runtime.

A piece of software that bakes lightmaps is known as a *lightmapper*. Standalone lightmappers include [Blender] (optionally augmented with [The Lightmapper] add-on). Most popular 3D game engines, such as Unity, [Unreal], and [Godot], also include built-in integrated lightmappers. However, some game engines, such as [Bevy], don't include any lightmapper, because lightmapping is a large and complex task. For these game engines, an external lightmapping tool is necessary to achieve lightmap-based GI.

## Setup

TODO

Once the Unity glTF Lightmapper is properly installed into your project, select the menu item *Window → glTF Lightmapper* to open the main glTF Lightmapper window.

## The main window

### Export Lightmap Images

The *Export Lightmap Images* button instructs the Unity glTF Lightmapper to reencode the baked lightmaps into [Radiance HDR] format and to save the resulting HDR files in the output lightmap directory. As it requires baked lightmap images to be present, the lightmaps should be baked first, either with Unity's built-in Progressive Lightmapper or with another lightmapper like Bakery.

### Export Modified glTF

The *Export Modified glTF* button causes the Unity glTF Lightmapper to save a copy of the input glTF file with any lightmap texture coordinates that Unity created in the UV1 (`TEXCOORD1`) channel and with paths to the baked lightmap images correctly encoded using `bevy-baked-gi`'s [glTF extras]. You should bake the lightmaps before clicking this button.

Note that the paths to the baked lightmap images are the paths to the images in HDR format that the *Export Lightmap Images* functionality generates. Therefore, you should generally select *Export Lightmap Images* before selecting *Export Modified glTF*.

[glTF extras]: GLTFExtras.md
