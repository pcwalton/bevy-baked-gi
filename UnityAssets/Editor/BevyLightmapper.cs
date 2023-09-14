// bevy-baked-gi/UnityAssets/Editor/BevyLightmapper.cs

using C;
using SharpGLTF.Geometry.VertexTypes;
using SharpGLTF.IO;
using SharpGLTF.Schema2;
using Siccity.GLTFUtility;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Text;
using UnityEngine;
using UnityEngine.Rendering;
using UnityEditor;

namespace Bevy.Lightmapper
{
internal class GameObjectLightmapInfo {
    public UnityEngine.Mesh Mesh;
    public int LightmapIndex;
}

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

        // Import glTF button

        if (GUILayout.Button("Import glTF")) {
            ImportGLTF();
        }

        // Export Lightmap UVs button
        if (GUILayout.Button("Export Lightmap UVs")) {
            ExportLightmapUVs();
        }

        // Export Lightmaps button

        if (GUILayout.Button("Export Lightmaps")) {
            ExportLightmaps();
        }
    }

    private void ImportGLTF() {
        GameObject root = Importer.LoadFromFile(mInputFilePath);

        var foundMeshes = new Dictionary <int, UnityEngine.Mesh>();
        ProcessNewlyImportedGameObject(root, foundMeshes);

        // Create lightmap UVs.
        foreach (var pair in foundMeshes) {
            Unwrapping.GenerateSecondaryUVSet(pair.Value);
        }
    }

    private void ProcessNewlyImportedGameObject(
        GameObject gameObject,
        Dictionary <int, UnityEngine.Mesh> foundMeshes) {
        gameObject.isStatic = true;

        if (gameObject.GetComponent <MeshRenderer>() != null) {
            MeshFilter meshFilter = gameObject.GetComponent <MeshFilter>();
            if (meshFilter != null) {
                UnityEngine.Mesh mesh = meshFilter.sharedMesh;
                if (!foundMeshes.ContainsKey(mesh.GetInstanceID())) {
                    foundMeshes.Add(mesh.GetInstanceID(), mesh);
                }
            }
        }

        for (int i = 0; i < gameObject.transform.childCount; i++) {
            ProcessNewlyImportedGameObject(
                gameObject.transform.GetChild(i).gameObject,
                foundMeshes);
        }
    }

    private GameObjectLightmapInfo GetGameObjectLightmapInfo(GameObject gameObject) {
        // Make sure the GameObject is static.
        if (!gameObject.isStatic) {
            return(null);
        }

        // Make sure the GameObject has a mesh renderer.
        MeshRenderer meshRenderer = gameObject.GetComponent <MeshRenderer>();
        if (meshRenderer == null) {
            return(null);
        }

        // Make sure the GameObject's mesh renderer has a lightmap, and that that lightmap has
        // a color component.
        int lightmapIndex = meshRenderer.lightmapIndex;
        if (lightmapIndex < 0 || lightmapIndex >= LightmapSettings.lightmaps.Length ||
            LightmapSettings.lightmaps[lightmapIndex].lightmapColor == null) {
            return(null);
        }

        // Make sure the GameObject has an attached mesh.
        MeshFilter meshFilter = gameObject.GetComponent <MeshFilter>();
        if (meshFilter == null || meshFilter.sharedMesh == null) {
            return(null);
        }

        // OK, we're going to process the object. Return the lightmap info.
        var lightmapInfo = new GameObjectLightmapInfo();
        lightmapInfo.LightmapIndex = lightmapIndex;
        lightmapInfo.Mesh          = meshFilter.sharedMesh;
        return(lightmapInfo);
    }

    private string GetColorLightmapPath(Texture2D lightmapColor) {
        return(Path.Join(mOutputLightmapDirPath, lightmapColor.name + ".hdr"));
    }

    private void ExportLightmapUVs() {
        var gltf = ModelRoot.Load(mInputFilePath);

        // TODO: Other scenes?
        var scene = gltf.DefaultScene;

        foreach (var buffer in gltf.LogicalBuffers) {
            Debug.Log(buffer);
        }

        var          usedMeshes  = new Dictionary <string, UnityEngine.Mesh>();
        GameObject[] gameObjects =
            (GameObject[])GameObject.FindSceneObjectsOfType(typeof(GameObject));

        foreach (GameObject gameObject in gameObjects) {
            GameObjectLightmapInfo lightmapInfo = GetGameObjectLightmapInfo(gameObject);
            if (lightmapInfo == null) {
                continue;
            }

            usedMeshes.Add(lightmapInfo.Mesh.name, lightmapInfo.Mesh);
        }

        ExportLightmapUVsForGLTFNode(gltf, usedMeshes, scene);
        WriteLightmapPathsToGLTFNodes(gltf, usedMeshes, scene);

        gltf.SaveGLB(mOutputFilePath);
    }

    private void ExportLightmapUVsForGLTFNode(
        ModelRoot gltf,
        Dictionary <string, UnityEngine.Mesh> usedMeshes,
        IVisualNodeContainer nodeContainer) {
        // FIXME: This is wrong! We should be grabbing meshes, not traversing objects.
        if (nodeContainer is Node) {
            Node node = (Node)nodeContainer;
            if (node.Mesh != null) {
                var mesh = node.Mesh;
                if (usedMeshes.ContainsKey(mesh.Name)) {
                    var unityMesh = usedMeshes[mesh.Name];

                    int unitySubmeshCount = unityMesh.subMeshCount;
                    if (mesh.Primitives.Count != unitySubmeshCount) {
                        Debug.LogWarning("Couldn't export lightmap UVs for " + mesh.Name +
                                         " because the Unity mesh has " +
                                         unitySubmeshCount +
                                         " submesh(es) but the glTF mesh has " +
                                         mesh.Primitives.Count + " primitive(s)");
                    } else {
                        var uvs = new List <UnityEngine.Vector2>();
                        unityMesh.GetUVs(1, uvs);

                        var gltfUVs = new List <System.Numerics.Vector2>();
                        for (int i = 0; i < uvs.Count; i++) {
                            UnityEngine.Vector2 uv = uvs[i];
                            gltfUVs.Add(new System.Numerics.Vector2(uv.x, uv.y));
                        }

                        if (uvs.Count != 0) {
                            foreach (MeshPrimitive primitive in mesh.Primitives) {
                                primitive.WithVertexAccessor(
                                    "TEXCOORD_1",
                                    (IReadOnlyList <System.Numerics.Vector2>)gltfUVs);
                            }

                            Debug.Log("Added " + uvs.Count + " lightmap UVs for mesh " +
                                      mesh.Name + " with " + mesh.Primitives.Count +
                                      " primitives");
                        }
                    }
                }
            }
        }

        foreach (var kid in nodeContainer.VisualChildren) {
            ExportLightmapUVsForGLTFNode(gltf, usedMeshes, kid);
        }
    }

    private void WriteLightmapPathsToGLTFNodes(
        ModelRoot gltf,
        Dictionary <string, UnityEngine.Mesh> usedMeshes,
        IVisualNodeContainer nodeContainer) {
        if (nodeContainer is Node) {
            Node node = (Node)nodeContainer;
            if (node.Mesh != null && usedMeshes.ContainsKey(node.Mesh.Name)) {
                // FIXME: Scope this to the root node.
                GameObject gameObject = GameObject.Find(node.Name);
                if (gameObject != null) {
                    GameObjectLightmapInfo lightmapInfo = GetGameObjectLightmapInfo(gameObject);
                    if (lightmapInfo != null) {
                        int          lightmapIndex = lightmapInfo.LightmapIndex;
                        LightmapData lightmapData  = LightmapSettings.lightmaps[lightmapIndex];
                        Texture2D    lightmapColor = lightmapData.lightmapColor;
                        string       lightmapPath  = GetColorLightmapPath(lightmapColor);

                        Dictionary <string, object> extrasDictionary;
                        if (node.Extras.Content != null) {
                            extrasDictionary = ((IReadOnlyDictionary <string, object>)
                                                node.Extras.Content).ToDictionary(
                                pair => pair.Key, pair => pair.Value);
                        } else {
                            extrasDictionary = new Dictionary <string, object>();
                        }

                        extrasDictionary.Add("Lightmap", lightmapPath);
                        node.Extras = JsonContent.CreateFrom(extrasDictionary);

                        Debug.Log("Annotated " + node.Name + " with lightmap path " + lightmapPath);
                    }
                }
            }
        }

        foreach (var kid in nodeContainer.VisualChildren) {
            WriteLightmapPathsToGLTFNodes(gltf, usedMeshes, kid);
        }
    }

    private void ExportLightmaps() {
        // Make the lightmap directory if necessary.
        Directory.CreateDirectory(mOutputLightmapDirPath);

        GameObject[] gameObjects         = FindObjectsOfType <GameObject>();
        var          usedLightmapIndices = new HashSet <int>();

        // Find all lightmaps used by static GameObjects.
        foreach (GameObject gameObject in gameObjects) {
            GameObjectLightmapInfo lightmapInfo = GetGameObjectLightmapInfo(gameObject);
            if (lightmapInfo != null) {
                usedLightmapIndices.Add(lightmapInfo.LightmapIndex);
            }
        }

        // Convert every lightmap.
        foreach (int lightmapIndex in usedLightmapIndices) {
            LightmapData lightmapData  = LightmapSettings.lightmaps[lightmapIndex];
            Texture2D    lightmapColor = lightmapData.lightmapColor;

            // Make the texture readable if necessary.
            if (!lightmapColor.isReadable) {
                string sourcePath      = AssetDatabase.GetAssetPath(lightmapColor);
                var    textureImporter = (TextureImporter)AssetImporter.GetAtPath(sourcePath);
                textureImporter.isReadable = true;
                textureImporter.SaveAndReimport();
            }

            // Get the lumel data.
            Color[] lumelData = lightmapColor.GetPixels(0);
            Debug.Log("lumelData size=" + lumelData.Length + " texsize=" + lightmapColor.width * lightmapColor.height * 3);

            string outputPath = GetColorLightmapPath(lightmapColor);

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
