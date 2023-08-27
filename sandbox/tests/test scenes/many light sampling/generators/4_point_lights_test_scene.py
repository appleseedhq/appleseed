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

import math
import signal
import sys
import time
import threading
import os

import appleseed as asr

output_scene_name = "4-colored-point-lights"


def build_project():
    # Create an empty project.
    project = asr.Project("4-point-lights")

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
            {"visibility":
             {
                 "camera": "true",
                 "diffuse": "true",
                 "glossy": "true",
                 "light": "true",
                 "probe": "true",
                 "shadow": "true",
                 "specular": "true",
                 "subsurface": "true",
                 "transparency": "true"
             }},
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

    # Create a list of colors and for each of them create a light.
    light_colors = {
        "white": [1.0, 1.0, 1.0],
        "red": [1.0, 0.0, 0.0],
        "green": [0.0, 1.0, 0.0],
        "blue": [0.0, 0.0, 1.0]
    }

    light_positions = [
        asr.Vector3d(25.0, -25.0, 5.0),
        asr.Vector3d(-25.0, -25.0, 5.0),
        asr.Vector3d(25.0,  25.0, 5.0),
        asr.Vector3d(-25.0,  25.0, 5.0)
    ]

    for key in light_colors:
        color_name = "color_" + key
        # Add colors to the project.
        assembly.colors().insert(asr.ColorEntity(color_name, {"color_space": "linear_rgb", "multiplier": 1.0}, light_colors[key]))
        idx = list(light_colors.keys()).index(key)
        light_name = "light_" + key
        # Create the light.
        light = asr.Light("max_omni_light", light_name, {
            "decay_exponent": "0",
            "decay_start": "40",
            "intensity": color_name,
            "intensity_multiplier": "3.14159"

        })
        mat = orientation * asr.Matrix4d.make_translation(light_positions[idx])
        light.set_transform(asr.Transformd(mat))
        assembly.lights().insert(light)

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

    # Create an orthographic camera with film dimensions 128 x 128 in.
    params = {
        "controller_target": "0 0 0",
        "film_dimensions": "128 128",
        "near_z": "-0.1",
        "shutter_close_time": "1.0",
        "shutter_open_time": "0.0"
    }

    camera = asr.Camera("orthographic_camera", "camera", params)

    # Place and orient the camera. By default cameras are located in (0.0, 0.0, 0.0)
    # and are looking toward Z- (0.0, 0.0, -1.0).
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

