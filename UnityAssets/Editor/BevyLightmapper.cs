// bevy-baked-gi/UnityAssets/Editor/BevyLightmapper.cs

using C;
using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Unity.Collections;
using UnityEngine;
using UnityEditor;

namespace Bevy.Lightmapper
{
public class LightmapperUI : EditorWindow
{
    private string mInputFilePath;
    private string mOutputFilePath;
    private string mOutputLightmapDirPath;

    [MenuItem("Window/Bevy Lightmapper")]
    public static void ShowWindow() {
        EditorWindow.GetWindow(typeof(LightmapperUI));
    }

    private void SetInputFilePath(string newInputFilePath) {
        mInputFilePath = newInputFilePath;

        string autoOutputFilePath;
        try
        {
            autoOutputFilePath = Path.ChangeExtension(newInputFilePath,
                                                      ".gi" + Path.GetExtension(mInputFilePath));
        }
        catch
        {
            return;
        }
        SetOutputFilePath(autoOutputFilePath);
    }

    private void SetOutputFilePath(string newOutputFilePath) {
        mOutputFilePath = newOutputFilePath;

        try
        {
            mOutputLightmapDirPath = Path.Join(Path.GetDirectoryName(mOutputFilePath), "Lightmaps");
        }
        catch { }
    }

    private string GetSuitableDirectoryForOutputBrowser() {
        string directory = Path.GetDirectoryName(mOutputFilePath);

        if (directory == null) {
            directory = Path.GetDirectoryName(mInputFilePath);
        }
        if (directory == null) {
            directory = "";
        }
        return(directory);
    }

    void OnGUI() {
        // Input file

        GUILayout.BeginHorizontal();

        string newInputFilePath = EditorGUILayout.TextField("glTF Input File:", mInputFilePath);
        if (newInputFilePath != mInputFilePath) {
            SetInputFilePath(newInputFilePath);
        }

        if (GUILayout.Button("Browse…", GUILayout.ExpandWidth(false))) {
            string directory;
            try
            {
                directory = Path.GetDirectoryName(mInputFilePath);
                if (directory == null) {
                    throw new Exception();
                }
            }
            catch
            {
                directory = "";
            }

            newInputFilePath =
                EditorUtility.OpenFilePanel("Bevy Lightmapper", directory, "gltf,glb");
            if (newInputFilePath != null) {
                SetInputFilePath(newInputFilePath);
            }
        }

        GUILayout.EndHorizontal();

        // Output file

        GUILayout.BeginHorizontal();

        string newOutputFilePath =
            EditorGUILayout.TextField("glTF Output File:", mOutputFilePath);
        if (newOutputFilePath != mOutputFilePath) {
            SetOutputFilePath(newOutputFilePath);
        }

        if (GUILayout.Button("Browse…", GUILayout.ExpandWidth(false))) {
            string directory = GetSuitableDirectoryForOutputBrowser();

            string basename = Path.GetFileNameWithoutExtension(mOutputFilePath);
            if (basename == null) {
                basename = Path.GetFileNameWithoutExtension(mInputFilePath);
            }
            if (basename == null) {
                basename = "";
            }
            basename += ".glb";

            newOutputFilePath = EditorUtility.SaveFilePanel(
                "Bevy Lightmapper", directory, basename, "glb");
            if (newOutputFilePath != null) {
                SetOutputFilePath(newOutputFilePath);
            }
        }

        GUILayout.EndHorizontal();

        // Output lightmap directory

        GUILayout.BeginHorizontal();

        mOutputLightmapDirPath = EditorGUILayout.TextField("Lightmap Output Folder:",
                                                           mOutputLightmapDirPath);

        if (GUILayout.Button("Browse…", GUILayout.ExpandWidth(false))) {
            string directory = GetSuitableDirectoryForOutputBrowser();

            mOutputLightmapDirPath =
                EditorUtility.OpenFolderPanel("Bevy Lightmapper", directory, "");
        }

        GUILayout.EndHorizontal();

        // Export Lightmaps button

        if (GUILayout.Button("Export Lightmaps")) {
            ExportLightmaps();
        }
    }

    private void ExportLightmaps() {
        // Make the lightmap directory if necessary.
        Directory.CreateDirectory(mOutputLightmapDirPath);

        GameObject[] gameObjects = FindObjectsOfType <GameObject>();
        var usedLightmapIndices = new HashSet<int>();

        // Find all lightmaps used by static GameObjects.
        foreach (GameObject gameObject in gameObjects) {
            // Is the object static?
            if (!gameObject.isStatic) {
                continue;
            }

            // Does the object have a mesh renderer?
            MeshRenderer meshRenderer = gameObject.GetComponent <MeshRenderer>();
            if (meshRenderer == null) {
                continue;
            }

            // Does the object have a baked lightmap?
            if (meshRenderer.lightmapIndex >= LightmapSettings.lightmaps.Length) {
                continue;
            }

            usedLightmapIndices.Add(meshRenderer.lightmapIndex);
        }

        // Convert every lightmap.
        foreach (int lightmapIndex in usedLightmapIndices) {
            // Does the lightmap have a color texture?
            LightmapData lightmapData  = LightmapSettings.lightmaps[lightmapIndex];
            Texture2D    lightmapColor = lightmapData.lightmapColor;
            if (lightmapColor == null) {
                continue;
            }

            // Get the lumel data.
            Color[] lumelData = lightmapColor.GetPixels(0);

            Debug.Log("lumelData size=" + lumelData.Length + " texsize=" + lightmapColor.width * lightmapColor.height * 3);

            string outputPath = Path.Join(mOutputLightmapDirPath, lightmapColor.name + ".hdr");

            using (FileStream outputStream = File.Open(outputPath, FileMode.Create)) {
                using (var binaryWriter = new BinaryWriter(outputStream)) {
                    EncodeRadianceHDR(
                        binaryWriter,
                        new Vector2Int(lightmapColor.width, lightmapColor.height),
                        lumelData);
                    Debug.Log("Wrote " + outputPath);
                }
            }
        }

        //EncodeRadianceHDR();
    }

    // http://c0de517e.blogspot.com/2013/07/tiny-hdr-writer.html
    private void EncodeRadianceHDR(
        BinaryWriter writer, Vector2Int size, Color[] rgbData) {
        byte[][] scanlines = new byte[4][];

        for (int i = 0; i < 4; i++) {
            scanlines[i] = new byte[size.x];
        }

        writer.Write(Encoding.ASCII.GetBytes("#?RADIANCE\nFORMAT=32-bit_rle_rgbe\n\n"));
        writer.Write(Encoding.ASCII.GetBytes(String.Format("-Y {0} +X {1}\n", size.y, size.x)));

        byte[] encodedPixel = new byte[4];
        uint   inputOffset  = 0;

        for (uint y = 0; y < size.y; y++) {
            // Write a fake RLE header.
            writer.Write(new byte[] { 2, 2, (byte)((size.x >> 8) & 0xff), (byte)(size.x & 0xff) });

            for (uint x = 0; x < size.x; x++) {
                Color rgba = rgbData[inputOffset];
                float r = rgba.r, g = rgba.g, b = rgba.b;

                double maxV = Math.Max(Math.Max(r, g), b);
                if (maxV == 0.0) {
                    for (int i = 0; i < 4; i++) {
                        encodedPixel[i] = 0;
                    }
                } else {
                    int exponent = 0;
                    maxV            = math.frexp(maxV, ref exponent) * 256.0 / maxV;
                    encodedPixel[0] = (byte)(maxV * r);
                    encodedPixel[1] = (byte)(maxV * g);
                    encodedPixel[2] = (byte)(maxV * b);
                    encodedPixel[3] = (byte)(exponent + 128);
                }

                for (int i = 0; i < 4; i++) {
                    scanlines[i][x] = encodedPixel[i];
                }
                inputOffset++;
            }

            // Don't actually use RLE, but encode as though it were.
            for (uint line = 0; line < 4; line++) {
                uint scanlineStartOffset = 0, scanlineEndOffset = (uint)size.x;
                while (scanlineStartOffset < scanlineEndOffset) {
                    // We need to make sure we don't set the high bit here, because that would indicate
                    // an RLE run.
                    uint count = Math.Min(127, scanlineEndOffset - scanlineStartOffset);
                    writer.Write((byte)count);
                    writer.Write(scanlines[line], (int)scanlineStartOffset, (int)count);
                    scanlineStartOffset += count;
                }
            }
        }
    }
}
}
