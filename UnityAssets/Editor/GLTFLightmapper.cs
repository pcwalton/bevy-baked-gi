// bevy-baked-gi/UnityAssets/Editor/GLTFLightmapper.cs

using C;
using GLTFast;
using SharpGLTF.Geometry.VertexTypes;
using SharpGLTF.IO;
using SharpGLTF.Schema2;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Numerics;
using System.Text;
using UnityEngine;
using UnityEngine.Rendering;
using UnityEditor;

namespace GLTFLightmapper
{
internal class GameObjectLightmapInfo {
	public UnityEngine.Mesh Mesh;
	public int LightmapIndex;

	// Upper-left coordinate of the rect in the lightmap.
	public UnityEngine.Vector2 LightmapMin;

	// Lower-right coordinate of the rect in the lightmap.
	public UnityEngine.Vector2 LightmapMax;
}

public class LightmapperUI : EditorWindow
{
	private GameObject mInputPrefab;
	private string mInputFilePath;
	private string mOutputFilePath;
	private string mOutputLightmapDirPath;

	[MenuItem("Window/glTF Lightmapper")]
	public static void ShowWindow() {
		LightmapperUI lightmapperUI = EditorWindow.GetWindow<LightmapperUI>();
		lightmapperUI.titleContent = new GUIContent("glTF Lightmapper");
	}

	private void SetInputFilePath(string newInputFilePath) {
		mInputFilePath = newInputFilePath;

		string autoOutputFilePath;
		try {
			autoOutputFilePath =
				Path.ChangeExtension(newInputFilePath, ".gi" + Path.GetExtension(mInputFilePath));
		} catch {
			return;
		}
		SetOutputFilePath(autoOutputFilePath);
	}

	private void SetOutputFilePath(string newOutputFilePath) {
		mOutputFilePath = newOutputFilePath;

		try {
			mOutputLightmapDirPath =
				Path.Join(Path.GetDirectoryName(mOutputFilePath), "Lightmaps");
		} catch {}
	}

	private string GetSuitableDirectoryForOutputBrowser() {
		string directory = Path.GetDirectoryName(mOutputFilePath);

		if (directory == null)
			directory = Path.GetDirectoryName(mInputFilePath);
		if (directory == null)
			directory = "";
		return directory;
	}

	void OnGUI() {
		// Input prefab

		GameObject newInputPrefab = (GameObject)EditorGUILayout.ObjectField("glTF Prefab:",
		                                                                    mInputPrefab,
		                                                                    typeof(GameObject),
		                                                                    true);
		if (newInputPrefab != mInputPrefab) {
			if (newInputPrefab == null) {
				mInputPrefab = null;
			}else {
				string newInputPath = PrefabUtility.GetPrefabAssetPathOfNearestInstanceRoot(
					newInputPrefab);
				if (newInputPath != null) {
					mInputPrefab = newInputPrefab;
					SetInputFilePath(newInputPath);
				}
			}
		}

		// Input file

		using (new EditorGUI.DisabledScope(true))
		{
			EditorGUILayout.TextField("glTF Input File:", mInputFilePath);
		}

		// Output file

		GUILayout.BeginHorizontal();

		string newOutputFilePath = EditorGUILayout.TextField("glTF Output File:", mOutputFilePath);
		if (newOutputFilePath != mOutputFilePath)
			SetOutputFilePath(newOutputFilePath);

		if (GUILayout.Button(EditorGUIUtility.FindTexture("FolderOpened Icon"),
		                     GUILayout.ExpandWidth(false),
		                     GUILayout.MaxHeight(18.0f))) {
			string directory = GetSuitableDirectoryForOutputBrowser();

			string basename =
				Path.GetFileNameWithoutExtension(mOutputFilePath);
			if (basename == null)
				basename = Path.GetFileNameWithoutExtension(mInputFilePath);
			if (basename == null)
				basename = "";
			basename += ".glb";

			newOutputFilePath = EditorUtility.SaveFilePanel("glTF Lightmapper", directory,
			                                                basename, "glb");
			if (newOutputFilePath != null)
				SetOutputFilePath(newOutputFilePath);
		}

		GUILayout.EndHorizontal();

		// Output lightmap directory

		GUILayout.BeginHorizontal();

		mOutputLightmapDirPath = EditorGUILayout.TextField("Lightmap Folder:",
		                                                   mOutputLightmapDirPath);

		if (GUILayout.Button(EditorGUIUtility.FindTexture("FolderOpened Icon"),
		                     GUILayout.ExpandWidth(false),
		                     GUILayout.MaxHeight(18.0f))) {
			string directory = GetSuitableDirectoryForOutputBrowser();

			mOutputLightmapDirPath = EditorUtility.OpenFolderPanel("glTF Lightmapper", directory,
			                                                       "");
		}

		GUILayout.EndHorizontal();

		// Export Lightmap Images button
		if (GUILayout.Button("Export Lightmap Images"))
			ExportLightmaps();

		// Export Modified glTF button
		if (GUILayout.Button("Export Modified glTF"))
			ExportModifiedGLTF();
	}

	private void ProcessNewlyImportedGameObject(GameObject gameObject,
	                                            Dictionary<int,
	                                                       UnityEngine.Mesh> foundMeshes)
	{
		gameObject.isStatic = true;

		if (gameObject.GetComponent<MeshRenderer>() != null) {
			MeshFilter meshFilter = gameObject.GetComponent<MeshFilter>();
			if (meshFilter != null) {
				UnityEngine.Mesh mesh = meshFilter.sharedMesh;
				if (!foundMeshes.ContainsKey(mesh.GetInstanceID()))
					foundMeshes.Add(mesh.GetInstanceID(), mesh);
			}
		}

		for (int i = 0; i < gameObject.transform.childCount; i++)
			ProcessNewlyImportedGameObject(gameObject.transform.GetChild(i).gameObject,
			                               foundMeshes);
	}

	private GameObjectLightmapInfo GetGameObjectLightmapInfo(GameObject gameObject) {
		// Make sure the GameObject is static.
		if (!gameObject.isStatic)
			return null;

		// Make sure the GameObject has a mesh renderer.
		MeshRenderer meshRenderer = gameObject.GetComponent<MeshRenderer>();
		if (meshRenderer == null)
			return null;

		// Make sure the GameObject's mesh renderer has a lightmap, and that that lightmap has
		// a color component.
		int lightmapIndex = meshRenderer.lightmapIndex;
		if (lightmapIndex < 0 || lightmapIndex >= LightmapSettings.lightmaps.Length ||
		    LightmapSettings.lightmaps[lightmapIndex].lightmapColor == null)
			return null;

		// Make sure the GameObject has an attached mesh.
		MeshFilter meshFilter = gameObject.GetComponent<MeshFilter>();
		if (meshFilter == null || meshFilter.sharedMesh == null)
			return null;

		// OK, we're going to process the object. Compute the UV rect.
		UnityEngine.Vector2 lightmapMin =
			new UnityEngine.Vector2(meshRenderer.lightmapScaleOffset.z,
			                        meshRenderer.lightmapScaleOffset.w);
		UnityEngine.Vector2 lightmapMax = lightmapMin + new UnityEngine.Vector2(
			meshRenderer.lightmapScaleOffset.x, meshRenderer.lightmapScaleOffset.y);

		// Return the lightmap info.
		var lightmapInfo = new GameObjectLightmapInfo();
		lightmapInfo.LightmapIndex = lightmapIndex;
		lightmapInfo.LightmapMin = lightmapMin;
		lightmapInfo.LightmapMax = lightmapMax;
		lightmapInfo.Mesh = meshFilter.sharedMesh;
		return lightmapInfo;
	}

	private string GetColorLightmapPath(
		Texture2D lightmapColor) {
		return Path.Join(mOutputLightmapDirPath,
		                 lightmapColor.name + ".hdr");
	}

	private void ExportModifiedGLTF() {
		var gltf = ModelRoot.Load(mInputFilePath, new ReadSettings {
				Validation = SharpGLTF.Validation.ValidationMode.Skip
			});

		// TODO: Other scenes?
		var scene = gltf.DefaultScene;

		foreach (var buffer in gltf.LogicalBuffers)
			Debug.Log(buffer);

		var usedMeshes  = new Dictionary<string, UnityEngine.Mesh>();
		GameObject[] gameObjects =
			(GameObject[])GameObject.FindSceneObjectsOfType(typeof(GameObject));

		foreach (GameObject gameObject in gameObjects) {
			GameObjectLightmapInfo lightmapInfo = GetGameObjectLightmapInfo(gameObject);
			if (lightmapInfo == null)
				continue;

			usedMeshes.Add(lightmapInfo.Mesh.name, lightmapInfo.Mesh);
		}

		ExportModifiedGLTFForGLTFNode(gltf, usedMeshes, scene);
		WriteLightmapPathsToGLTFNodes(gltf, usedMeshes, scene);

		gltf.SaveGLB(mOutputFilePath, new WriteSettings {
				Validation = SharpGLTF.Validation.ValidationMode.Skip
			});
	}

	private void ExportModifiedGLTFForGLTFNode(
		ModelRoot gltf,
		Dictionary<string, UnityEngine.Mesh> usedMeshes,
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
						Debug.LogWarning(
							"Couldn't export lightmap UVs for " + mesh.Name +
							" because the Unity mesh has " + unitySubmeshCount +
							" submesh(es) but the glTF mesh has " + mesh.Primitives.Count +
							" primitive(s)");
					} else {
						RewriteMeshAttributeBuffer<UnityEngine.Vector3,
						                           System.Numerics.Vector3>(
							unityMesh,
							mesh,
							"POSITION",
							unityUVs => unityMesh.GetVertices(unityUVs),
							unityUV => {
								return new System.Numerics.Vector3(unityUV.x, unityUV.y,
								                                   unityUV.z);
							}
							);
						RewriteMeshAttributeBuffer<UnityEngine.Vector3,
						                           System.Numerics.Vector3>(
							unityMesh,
							mesh,
							"NORMAL",
							unityUVs => unityMesh.GetNormals(unityUVs),
							unityUV => {
								return new System.Numerics.Vector3(unityUV.x, unityUV.y,
								                                   unityUV.z);
							}
							);
						RewriteMeshAttributeBuffer<UnityEngine.Vector4,
						                           System.Numerics.Vector4>(
							unityMesh,
							mesh,
							"TANGENT",
							unityUVs => unityMesh.GetTangents(unityUVs),
							unityUV => {
								return new System.Numerics.Vector4(unityUV.x, unityUV.y, unityUV.z,
								                                   unityUV.w);
							}
							);

						// Original textures are flipped.
						RewriteMeshAttributeBuffer<UnityEngine.Vector2,
						                           System.Numerics.Vector2>(
							unityMesh,
							mesh,
							"TEXCOORD_0",
							unityUVs => unityMesh.GetUVs(0, unityUVs),
							unityUV => new System.Numerics.Vector2(unityUV.x, 1.0f - unityUV.y));

						// Lightmaps are *not* flipped.
						RewriteMeshAttributeBuffer<UnityEngine.Vector2,
						                           System.Numerics.Vector2>(
							unityMesh,
							mesh,
							"TEXCOORD_1",
							unityUVs => unityMesh.GetUVs(1, unityUVs),
							unityUV => new System.Numerics.Vector2(unityUV.x, unityUV.y));

						RewriteMeshIndexBuffer(unityMesh, mesh);
					}
				}
			}
		}

		foreach (var kid in nodeContainer.VisualChildren)
			ExportModifiedGLTFForGLTFNode(gltf, usedMeshes, kid);
	}

	private void RewriteMeshAttributeBuffer<UnityType, GLTFType>(UnityEngine.Mesh unityMesh,
	                                                             SharpGLTF.Schema2.Mesh gltfMesh,
	                                                             string gltfAccessorName,
	                                                             Action<List<UnityType> > getter,
	                                                             Func<UnityType,
	                                                                  GLTFType> converter) {
		var unityBuffer = new List<UnityType>();

		getter(unityBuffer);

		var gltfBuffer = new List<GLTFType>();
		for (int i = 0; i < unityBuffer.Count; i++) {
			UnityType unityElement = unityBuffer[i];
			gltfBuffer.Add(converter(unityElement));
		}

		if (unityBuffer.Count != 0)
			foreach (MeshPrimitive primitive in gltfMesh.Primitives) {
				if (typeof(GLTFType) == typeof(System.Numerics.Vector2))
					primitive.WithVertexAccessor(gltfAccessorName,
					                             (IReadOnlyList<System.Numerics.Vector2>)
					                             gltfBuffer);
				else if (typeof(GLTFType) == typeof(System.Numerics.Vector3))
					primitive.WithVertexAccessor(gltfAccessorName,
					                             (IReadOnlyList<System.Numerics.Vector3>)
					                             gltfBuffer);
				else if (typeof(GLTFType) == typeof(System.Numerics.Vector4))
					primitive.WithVertexAccessor(gltfAccessorName,
					                             (IReadOnlyList<System.Numerics.Vector4>)
					                             gltfBuffer);
				else if (typeof(GLTFType) == typeof(int) && gltfAccessorName == null)
					primitive.WithIndicesAccessor(SharpGLTF.Schema2.PrimitiveType.TRIANGLES,
					                              (IReadOnlyList<int>)gltfBuffer);
				else
					throw new Exception("Unsupported GLTFType");
			}

		Debug.Log(
			"Added " + unityBuffer.Count + " " + gltfAccessorName + " for mesh " + unityMesh.name +
			" with " + gltfMesh.Primitives.Count + " primitives");
	}

	private void RewriteMeshIndexBuffer(UnityEngine.Mesh unityMesh,
	                                    SharpGLTF.Schema2.Mesh gltfMesh) {
		for (int primitiveIndex = 0;
		     primitiveIndex < unityMesh.subMeshCount;
		     primitiveIndex++) {
			int[] unityIndices = unityMesh.GetTriangles(
				primitiveIndex);

			var gltfBuffer = new List<int>();
			for (int i = 0; i < unityIndices.Length; i++)
				gltfBuffer.Add(unityIndices[i]);

			// FIXME: This is wrong.
			gltfMesh.Primitives[primitiveIndex].
			WithIndicesAccessor(
				SharpGLTF.Schema2.PrimitiveType.TRIANGLES,
				(IReadOnlyList<int>)gltfBuffer);
		}
	}

	private void WriteLightmapPathsToGLTFNodes(
		ModelRoot gltf,
		Dictionary<string, UnityEngine.Mesh> usedMeshes,
		IVisualNodeContainer nodeContainer) {
		if (nodeContainer is Node) {
			Node node = (Node)nodeContainer;
			if (node.Mesh != null && usedMeshes.ContainsKey(node.Mesh.Name)) {
				// FIXME: Scope this to the root node.
				GameObject gameObject = GameObject.Find(node.Name);
				if (gameObject != null) {
					GameObjectLightmapInfo lightmapInfo = GetGameObjectLightmapInfo(gameObject);
					if (lightmapInfo != null) {
						int lightmapIndex = lightmapInfo.LightmapIndex;
						LightmapData lightmapData = LightmapSettings.lightmaps[ lightmapIndex];
						Texture2D lightmapColor = lightmapData.lightmapColor;
						string lightmapPath = GetColorLightmapPath(lightmapColor);

						Dictionary<string, object> extrasDictionary;
						if (node.Extras.Content != null)
							extrasDictionary =
								((IReadOnlyDictionary<string,
								                      object>)node.Extras.Content).ToDictionary(
									pair => pair.Key,
									pair => pair.Value);
						else
							extrasDictionary = new Dictionary<string, object>();

						extrasDictionary.Add("Lightmap", lightmapPath);
						extrasDictionary.Add("LightmapMinU", lightmapInfo.LightmapMin.x);
						extrasDictionary.Add("LightmapMinV", lightmapInfo.LightmapMin.y);
						extrasDictionary.Add("LightmapMaxU", lightmapInfo.LightmapMax.x);
						extrasDictionary.Add("LightmapMaxV", lightmapInfo.LightmapMax.y);

						node.Extras = JsonContent.CreateFrom(extrasDictionary);

						Debug.Log("Annotated " + node.Name + " with lightmap path " +
						          lightmapPath);
					}
				}
			}
		}

		foreach (var kid in nodeContainer.VisualChildren)
			WriteLightmapPathsToGLTFNodes(gltf, usedMeshes, kid);
	}

	private void ExportLightmaps() {
		// Make the lightmap directory if necessary.
		Directory.CreateDirectory(mOutputLightmapDirPath);

		GameObject[] gameObjects = FindObjectsOfType<GameObject>();
		var usedLightmapIndices = new HashSet<int>();

		// Find all lightmaps used by static GameObjects.
		foreach (GameObject gameObject in gameObjects) {
			GameObjectLightmapInfo lightmapInfo = GetGameObjectLightmapInfo(gameObject);
			if (lightmapInfo != null)
				usedLightmapIndices.Add(lightmapInfo.LightmapIndex);
		}

		// Convert every lightmap.
		foreach (int lightmapIndex in usedLightmapIndices) {
			LightmapData lightmapData = LightmapSettings.lightmaps[lightmapIndex];
			Texture2D lightmapColor = lightmapData.lightmapColor;

			// Make the texture readable if necessary.
			if (!lightmapColor.isReadable) {
				string sourcePath = AssetDatabase.GetAssetPath(lightmapColor);
				var textureImporter = (TextureImporter)AssetImporter.GetAtPath(sourcePath);
				textureImporter.isReadable = true;
				textureImporter.SaveAndReimport();
			}

			// Get the lumel data.
			Color[] lumelData = lightmapColor.GetPixels(0);
			Debug.Log(
				"lumelData size=" + lumelData.Length + " texsize=" +
				lightmapColor.width * lightmapColor.height * 3);

			string outputPath = GetColorLightmapPath(lightmapColor);

			using (FileStream outputStream = File.Open(outputPath, FileMode.Create)) {
				using (var binaryWriter = new BinaryWriter(outputStream)) {
					EncodeRadianceHDR(binaryWriter,
					                  new Vector2Int(lightmapColor.width,
					                                 lightmapColor.height), lumelData);
					Debug.Log("Wrote " + outputPath);
				}
			}
		}
	}

	// http://c0de517e.blogspot.com/2013/07/tiny-hdr-writer.html
	private void EncodeRadianceHDR(BinaryWriter writer, Vector2Int size, Color[] rgbData) {
		byte[][] scanlines = new byte[4][];

		for (int i = 0; i < 4; i++)
			scanlines[i] = new byte[size.x];

		writer.Write(Encoding.ASCII.GetBytes("#?RADIANCE\nFORMAT=32-bit_rle_rgbe\n\n"));
		writer.Write(Encoding.ASCII.GetBytes(String.Format("-Y {0} +X {1}\n", size.y, size.x)));

		byte[] encodedPixel = new byte[4];
		uint inputOffset = 0;

		for (uint y = 0; y < size.y; y++) {
			// Write a fake RLE header.
			writer.Write(new byte[] {2, 2, (byte)((size.x >> 8) & 0xff), (byte)(size.x & 0xff)});

			for (uint x = 0; x < size.x; x++) {
				Color rgba = rgbData[inputOffset];
				float r = rgba.r, g = rgba.g, b = rgba.b;

				double maxV = Math.Max(Math.Max(r, g), b);
				if (maxV == 0.0) {
					for (int i = 0; i < 4; i++)
						encodedPixel[i] = 0;
				} else {
					int exponent = 0;
					maxV = math.frexp(maxV, ref exponent) * 256.0 / maxV;
					encodedPixel[0] = (byte)(maxV * r);
					encodedPixel[1] = (byte)(maxV * g);
					encodedPixel[2] = (byte)(maxV * b);
					encodedPixel[3] = (byte)(exponent + 128);
				}

				for (int i = 0; i < 4; i++)
					scanlines[i][x] = encodedPixel[i];
				inputOffset++;
			}

			// Don't actually use RLE, but encode as though it were.
			for (uint line = 0; line < 4; line++) {
				uint scanlineStartOffset = 0, scanlineEndOffset = (uint)size.x;
				while (scanlineStartOffset < scanlineEndOffset) {
					// We need to make sure we don't set the high bit here, because that would
					// indicate an RLE run.
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
