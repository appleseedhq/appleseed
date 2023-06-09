#
# This source file is part of appleseed.
# Visit https://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2017 Petra Gospodnetic, The appleseedhq Organization
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#



import colorsys
import math
import random
import signal
import sys
import time
import threading
import os
import numpy as np

import appleseed as asr

# Initial parameters for generating grid light scene
GridLightsCount = 5
Color = "white"
PlaneSize = 100
RectangleLightSize = 10
output_scene_name = "{0}x{0}_{1}_EMT_lights".format(GridLightsCount, Color)


def build_project():
    # Create an empty project.
    project = asr.Project("grid-point-lights-generator")

    paths = project.get_search_paths()
    paths.append("data")
    project.set_search_paths(paths)

    # Add default configurations to the project.
    project.add_default_configurations()

    # Set the number of samples. This is basically the quality parameter: the higher the number
    # of samples, the smoother the image but the longer the rendering time.
    # todo: fix.
    conf = project.configurations()["final"]
    conf.insert_path("uniform_pixel_renderer.samples", 1)

    # Create a scene.
    scene = asr.Scene()

    # Create an assembly.
    assembly = asr.Assembly("assembly")

    # Prepare the orientation of all the objects in the scene.
    orientation = asr.Matrix4d.make_rotation(asr.Vector3d(1.0, 0.0, 0.0), math.radians(-90.0))

    #------------------------------------------------------------------------
    # COLOR
    #------------------------------------------------------------------------

    assembly.colors().insert(asr.ColorEntity("light_color",
                                             {
                                                 "color_space": "linear_rgb",
                                                 "multiplier": 1.0,
                                                 "wavelength_range": "400.0 700.0"
                                             },
                                             [1.000000, 0.830634, 0.378440]))

    #------------------------------------------------------------------------
    # EDF
    #------------------------------------------------------------------------

    # Create light edfs.
    assembly.edfs().insert(asr.EDF(
        "diffuse_edf",
        "light_material_edf",
        {
            "cast_indirect_light": "true",
            "importance_multiplier": "1.0",
            "light_near_start": "0.0",
            "radiance": "light_color",
            "radiance_multiplier": "9"
        }))

    #------------------------------------------------------------------------
    # Materials
    #------------------------------------------------------------------------
    # Create a material called "01 - Default_mat" and insert it into the assembly.
    assembly.materials().insert(asr.Material(
        "disney_material",
        "01 - Default_mat",
        {
            "alpha_map": "1",
            "layer1": {
                "anisotropic": "0",
                "base_color": "[1, 1, 1]",
                "clearcoat": "0",
                "clearcoat_gloss": "0",
                "layer_name": "layer1",
                "layer_number": "0",
                "mask": "1.0",
                "metallic": "0",
                "roughness": "1",
                "sheen": "0",
                "sheen_tint": "0",
                "specular": "0",
                "specular_tint": "0",
                "subsurface": "0.0"
            }
        }))

    # Create light material.
    assembly.materials().insert(asr.Material(
        "generic_material",
        "light_material",
        {
            "bump_amplitude": "1.0",
            "displacement_method": "bump",
            "edf": "light_material_edf",
            "normal_map_up": "z",
            "shade_alpha_cutouts": "false"
        }))

    #------------------------------------------------------------------------
    # Geometry
    #------------------------------------------------------------------------

    # Load the scene geometry from disk.
    objects = asr.MeshObjectReader.read(project.get_search_paths(), "plane", {"filename": "Plane001.binarymesh"})

    # Insert all the objects into the assembly.
    for object in objects:
        # Create an instance of this object and insert it into the assembly.
        instance_name = object.get_name() + "_inst"
        material_name = {"material_slot_0": "01 - Default_mat"}
        mat = orientation * asr.Matrix4d.make_translation(asr.Vector3d(0.0, 0.0, 0.0))
        instance = asr.ObjectInstance(
            instance_name,
            {"visibility": {}},
            object.get_name(),
            asr.Transformd(mat),
            material_name,
            material_name)

        assembly.object_instances().insert(instance)

        # Insert this object into the scene.
        assembly.objects().insert(object)

    #------------------------------------------------------------------------
    # Lights
    #------------------------------------------------------------------------
    light_z_distance = 1.0

    lights = asr.MeshObjectReader.read(project.get_search_paths(), "rectangle", {"filename": "rectangle.obj"})

    for light in lights:

        if Color == "white":
            step = float(PlaneSize) / GridLightsCount
            light_count = 0
            grid_range = np.linspace((-PlaneSize + step) / 2, (PlaneSize - step) / 2, GridLightsCount)
            for j in grid_range:
                for i in grid_range:
                    # Create an instance of this light and insert it into the assembly.
                    instance_name = light.get_name() + "_inst_" + str(light_count)
                    light_count = light_count + 1
                    material_front = {"material_slot_0": "01 - Default_mat"}
                    material_back = {"material_slot_0": "light_material"}
                    light_position = asr.Vector3d(i, j, light_z_distance)
                    mat = orientation * asr.Matrix4d.make_translation(light_position)
                    instance = asr.ObjectInstance(
                        instance_name,
                        {"visibility":
                         {
                             "camera": "false",
                             "diffuse": "true",
                             "glossy": "true",
                             "light": "true",
                             "probe": "true",
                             "shadow": "true",
                             "specular": "true",
                             "subsurface": "true",
                             "transparency": "true"
                         }},
                        light.get_name(),
                        asr.Transformd(mat),
                        material_front,
                        material_back)

                    assembly.object_instances().insert(instance)
        else:
            print("Unknown Color: {0}".format(Color))
            return

        # Insert this light into the scene.
        assembly.objects().insert(light)

    #------------------------------------------------------------------------
    # Assembly instance
    #------------------------------------------------------------------------

    # Create an instance of the assembly and insert it into the scene.
    assembly_inst = asr.AssemblyInstance("assembly_inst", {}, assembly.get_name())
    assembly_inst.transform_sequence().set_transform(0.0, asr.Transformd(asr.Matrix4d.identity()))
    scene.assembly_instances().insert(assembly_inst)

    # Insert the assembly into the scene.
    scene.assemblies().insert(assembly)

    #------------------------------------------------------------------------
    # Environment
    #------------------------------------------------------------------------

    # Create an environment called "env" and bind it to the scene.
    scene.set_environment(asr.Environment("env", {}))

    #------------------------------------------------------------------------
    # Camera
    #------------------------------------------------------------------------

    # Create an orthographic camera.
    params = {
        "controller_target": "0 0 0",
        "film_dimensions": "128 128",
        "near_z": "-0.1",
        "shutter_close_time": "1.0",
        "shutter_open_time": "0.0"
    }

    camera = asr.Camera("orthographic_camera", "camera", params)

    # Place and orient the camera.
    mat = orientation * asr.Matrix4d.make_translation(asr.Vector3d(0.0, 0.0, 0.0))
    camera.transform_sequence().set_transform(0.0, asr.Transformd(mat))

    # Bind the camera to the scene.
    scene.cameras().insert(camera)

    #------------------------------------------------------------------------
    # Frame
    #------------------------------------------------------------------------

    # Create a frame and bind it to the project.
    params = {
        "camera": "camera",
        "clamping": "false",
        "color_space": "srgb",
        "filter": "box",
        "filter_size": "0.5",
        "gamma_correction": "1.0",
        "pixel_format": "float",
        "premultiplied_alpha": "true",
        "resolution": "512 512",
        "tile_size": "64 64"}
    project.set_frame(asr.Frame("beauty", params))

    # Bind the scene to the project.
    project.set_scene(scene)

    return project


def main():
    # Build the project.
    project = build_project()

    # Save the project to disk.
    asr.ProjectFileWriter().write(project, output_scene_name + ".appleseed")

if __name__ == "__main__":
    main()

