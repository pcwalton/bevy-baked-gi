# export-blender-gi/BlenderAddons/export-bevy-gi/export-bevy-gi.py

bl_info = {
    "name": "Bevy Global Illumination Export",
    "author": "Patrick Walton",
    "version": (0, 1, 0),
    "blender": (4, 0, 2),
    "location": "File > Import-Export",
    "description": "Exports global illumination to Bevy",
    "warning": "",
    "support": "COMMUNITY",
    "doc_url": "https://github.com/pcwalton/export-blender-gi",
    "tracker_url": "https://github.com/pcwalton/export-blender-gi",
    "category": "Import-Export",
}

import bpy
from bpy.props import StringProperty, BoolProperty
from bpy.types import AddonPreferences, Operator
from os import path
import subprocess


# Silly support for testing in the Scripting menu.
class MockAddonPrefs:
    export_blender_gi_exe_path = None

    def __init__(self):
        self.export_blender_gi_exe_path = path.expanduser(
            "~/Source/Rust/export-blender-gi/target/debug/export-blender-gi.exe"
        )


class ExportBevyGI(Operator):
    """Export global illumination to Bevy"""

    bl_idname = "export_gi.bevy"
    bl_label = "Export GI to Bevy"
    bl_description = "Export global illumination to Bevy"
    bl_options = {"REGISTER", "UNDO"}

    directory: StringProperty(name="Assets Folder", description="Output assets folder")

    scenes_directory: StringProperty(
        name="Scenes Folder",
        description="Where scenes will be stored, relative to the assets folder",
    )

    irradiance_volumes_directory: StringProperty(
        name="Irradiance Volumes Folder",
        description="Where irradiance volumes will be stored, relative to the assets folder",
    )

    reflection_probes_directory: StringProperty(
        name="Reflection Probes Folder",
        description="Where reflection probes will be stored, relative to the assets folder",
    )

    filter_folder: BoolProperty(default=True, options={"HIDDEN"})

    def execute(self, context):
        preferences = context.preferences
        if __name__ in preferences.addons:
            addon_prefs = preferences.addons[__name__].preferences
        else:
            addon_prefs = MockAddonPrefs()

        blend_file_path = bpy.data.filepath

        # Set up tool args.

        tool_args = [
            addon_prefs.export_blender_gi_exe_path,
            blend_file_path,
            "--assets-dir",
            self.directory,
        ]

        if self.scenes_directory is not None and self.scenes_directory != "":
            tool_args.extend(
                ["--scenes-dir", path.join(self.directory, self.scenes_directory)]
            )

        if (
            self.irradiance_volumes_directory is not None
            and self.irradiance_volumes_directory != ""
        ):
            tool_args.extend(
                [
                    "--irradiance-volumes-dir",
                    path.join(self.directory, self.irradiance_volumes_directory),
                ]
            )

        if (
            self.reflection_probes_directory is not None
            and self.reflection_probes_directory != ""
        ):
            tool_args.extend(
                [
                    "--reflection-probes-dir",
                    path.join(self.directory, self.reflection_probes_directory),
                ]
            )

        # Run the tool.
        #
        # TODO(pcwalton): Async.

        print("Running: " + " ".join(tool_args))
        context.window_manager.progress_begin(0, 10)

        result = subprocess.run(
            tool_args, cwd=path.dirname(blend_file_path), capture_output=True
        )
        print(result.stdout.decode("utf8"))

        context.window_manager.progress_end()

        if result.returncode != 0:
            self.report(
                {"ERROR"},
                "Failed to export the global illumination: \n"
                + result.stderr.decode("utf8"),
            )
            return {"CANCELLED"}

        self.report({"INFO"}, "Successfully exported baked global illumination")
        return {"FINISHED"}

    def invoke(self, context, event):
        preferences = context.preferences
        if __name__ in preferences.addons:
            addon_prefs = preferences.addons[__name__].preferences
        else:
            addon_prefs = MockAddonPrefs()

        if bpy.data.is_dirty:
            self.report({"ERROR"}, "The file must be saved before exporting to Bevy.")
            return {"CANCELLED"}

        if (
            addon_prefs.export_blender_gi_exe_path is None
            or addon_prefs.export_blender_gi_exe_path == ""
        ):
            self.report(
                {"ERROR"},
                "To export, first set the location of the `export-blender-gi` "
                + "executable in the add-on preferences.",
            )

        if not path.exists(addon_prefs.export_blender_gi_exe_path):
            self.report(
                {"ERROR"},
                "The `export-blender-gi` executable set in the add-on "
                + "preferences doesn't exist.",
            )

        context.window_manager.fileselect_add(self)
        return {"RUNNING_MODAL"}


class ExportBevyGIAddonPreferences(AddonPreferences):
    bl_idname = __name__

    export_blender_gi_exe_path: StringProperty(
        name="`export_blender_gi` Executable Location",
        subtype="FILE_PATH",
    )

    def draw(self, context):
        layout = self.layout
        layout.label(
            text="To use the exporter, locate the `export-blender-gi` executable."
        )
        layout.prop(self, "export_blender_gi_exe_path")


def menu_func_export(self, context):
    self.layout.operator(
        ExportBevyGI.bl_idname, text="Bevy Global Illumination (.scn.ron)"
    )


def register():
    bpy.utils.register_class(ExportBevyGI)
    bpy.utils.register_class(ExportBevyGIAddonPreferences)
    bpy.types.TOPBAR_MT_file_export.append(menu_func_export)


def unregister():
    bpy.types.TOPBAR_MT_file_export.remove(menu_func_export)
    bpy.utils.unregister_class(ExportBevyGIAddonPreferences)
    bpy.utils.unregister_class(ExportBevyGI)


if __name__ == "__main__":
    register()
