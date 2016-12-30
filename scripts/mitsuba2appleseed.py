#!/usr/bin/python

#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2016 Francois Beaune, The appleseedhq Organization
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

from __future__ import print_function
from xml.etree.ElementTree import ElementTree
import appleseed as asr
import argparse
import math
import sys
import traceback


#--------------------------------------------------------------------------------------------------
# Utility functions.
#--------------------------------------------------------------------------------------------------

def info(message):
    print(message)

def progress(message):
    print(message + "...")

def warning(message):
    print("Warning: {0}.".format(message))

def fatal(message):
    print("Fatal: {0}. Aborting.".format(message))
    # if sys.exc_info()[0]:
    #     print(traceback.format_exc())
    sys.exit(1)


#--------------------------------------------------------------------------------------------------
# Conversion code.
#--------------------------------------------------------------------------------------------------

def get_vector(element):
    return [
        float(element.attrib["x"]),
        float(element.attrib["y"]),
        float(element.attrib["z"])
    ]

def get_matrix(element):
    values = [ float(x) for x in element.attrib["value"].split() ]
    if len(values) != 16:
        fatal("Matrix was expected to contain 16 coefficients but contained {0} instead".format(len(values)))
    matrix = asr.Matrix4d()
    for i in range(16):
        matrix[i / 4, i % 4] = values[i]
    return matrix

def get_rgb(element):
    values = [ float(x) for x in element.attrib["value"].split(",") ]
    if len(values) != 3:
        fatal("RGB color was expected to contain 3 coefficients but contained {0} instead".format(len(values)))
    max_value = max(values)
    if max_value > 1.0:
        return [ x / max_value for x in values ], max_value
    else:
        return values, 1.0

def convert_integrator(project, element):
    for child in element:
        if child.tag == "integer":
            if child.attrib["name"] == "maxDepth":
                max_depth = int(child.attrib["value"])
                project.configurations().get_by_name("final").insert_path("pt.max_path_length", max_depth)
                project.configurations().get_by_name("interactive").insert_path("pt.max_path_length", max_depth)

def convert_film(camera_params, frame_params, element):
    width = int(element.find("integer[@name='width']").attrib["value"])
    height = int(element.find("integer[@name='height']").attrib["value"])
    dimensions = "{0} {1}".format(width, height)
    camera_params["film_dimensions"] = dimensions
    frame_params["resolution"] = dimensions

def convert_sampler(project, element):
    for child in element:
        if child.tag == "integer":
            if child.attrib["name"] == "sampleCount":
                sample_count = int(child.attrib["value"])
                project.configurations().get_by_name("final").insert_path("uniform_pixel_renderer.samples", sample_count)

def convert_sensor(project, scene, element):
    camera_params = {}
    camera_matrix = None
    frame_params = {
        "camera" : "camera",
        "color_space" : "srgb"
    }

    for child in element:
        if child.tag == "float":
            if child.attrib["name"] == "fov":
                camera_params["horizontal_fov"] = child.attrib["value"]
        elif child.tag == "transform":
            camera_matrix = get_matrix(child.find("matrix"))
        elif child.tag == "sampler":
            convert_sampler(project, child)
        elif child.tag == "film":
            convert_film(camera_params, frame_params, child)

    camera = asr.Camera("pinhole_camera", "camera", camera_params)
    if camera_matrix is not None:
        roty = asr.Matrix4d.make_rotation(asr.Vector3d(0.0, 1.0, 0.0), math.radians(180.0))
        camera_matrix = camera_matrix * roty
        camera.transform_sequence().set_transform(0.0, asr.Transformd(camera_matrix))
    scene.cameras().insert(camera)

    project.set_frame(asr.Frame("beauty", frame_params))

def create_texture(base_group, texture_name, filepath):
    if filepath.endswith(".jpg"):
        # Hack because appleseed doesn't currently support JPEG images in built-in materials.
        filepath += ".png"

    if filepath.endswith(".hdr"):
        # Hack because appleseed doesn't currently support HDR images in built-in materials.
        filepath += ".exr"

    color_space = "linear_rgb" if filepath.endswith(".exr") or \
                                  filepath.endswith(".hdr") else "srgb"
    base_group.textures().insert(asr.Texture("disk_texture_2d", texture_name, {
        "filename": filepath,
        "color_space": color_space
    }, []))

    texture_instance_name = "{0}_inst".format(texture_name)
    texture_instance = asr.TextureInstance(texture_instance_name, {}, texture_name, asr.Transformf.identity())
    base_group.texture_instances().insert(texture_instance)
    return texture_instance_name

def convert_texture(assembly, texture_name, element):
    type = element.attrib["type"]
    if type == "bitmap":
        filepath = element.find("string[@name='filename']").attrib["value"]
        return create_texture(assembly, texture_name, filepath)
    else:
        warning("Don't know how to convert texture of type {0}".format(type))
        color_params = {
            "color_space" : "srgb",
            "multiplier" : 1.0
        }
        assembly.colors().insert(asr.ColorEntity(texture_name, color_params, [0.7, 0.7, 0.7]))
        return texture_name

def convert_colormap(assembly, parent_name, element):
    reflectance_name = element.attrib["name"]
    if element.tag == "texture":
        texture_name = "{0}_{1}".format(parent_name, reflectance_name)
        return convert_texture(assembly, texture_name, element)
    elif element.tag == "rgb":
        color_name = "{0}_{1}".format(parent_name, reflectance_name)
        rgb, multiplier = get_rgb(element)
        color_params = {
            "color_space" : "linear_rgb",
            "multiplier" : multiplier
        }
        assembly.colors().insert(asr.ColorEntity(color_name, color_params, rgb))
        return color_name
    else:
        warning("Don't know how to convert color map of type {0}".format(element.tag))

def convert_diffuse_bsdf(assembly, bsdf_name, element):
    bsdf_params = {}

    reflectance = element.find("*[@name='reflectance']")
    bsdf_params["reflectance"] = convert_colormap(assembly, bsdf_name, reflectance)

    assembly.bsdfs().insert(asr.BSDF("lambertian_brdf", bsdf_name, bsdf_params))

def convert_plastic_bsdf(assembly, bsdf_name, element):
    bsdf_params = {}

    reflectance = element.find("*[@name='diffuseReflectance']")
    bsdf_params["base_color"] = convert_colormap(assembly, bsdf_name, reflectance)

    bsdf_params["specular"] = 1.0
    bsdf_params["roughness"] = 0.0

    assembly.bsdfs().insert(asr.BSDF("disney_brdf", bsdf_name, bsdf_params))

def convert_roughplastic_bsdf(assembly, bsdf_name, element):
    bsdf_params = {}

    reflectance = element.find("*[@name='diffuseReflectance']")
    bsdf_params["base_color"] = convert_colormap(assembly, bsdf_name, reflectance)

    bsdf_params["specular"] = 0.5
    bsdf_params["roughness"] = min(1.0, float(element.find("float[@name='alpha']").attrib["value"]) * 3.0)

    assembly.bsdfs().insert(asr.BSDF("disney_brdf", bsdf_name, bsdf_params))

def convert_conductor_bsdf(assembly, bsdf_name, element):
    bsdf_params = {}

    reflectance = element.find("*[@name='specularReflectance']")
    if reflectance is not None:
        bsdf_params["normal_reflectance"] = convert_colormap(assembly, bsdf_name, reflectance)

    material = element.find("string[@name='material']")
    if material is not None:
        material_name = material.attrib["value"]
        if material_name == "none":
            bsdf_params["normal_reflectance"] = 1.0

    # todo: fix.
    bsdf_params["edge_tint"] = 1
    bsdf_params["mdf"] = "ggx"

    assembly.bsdfs().insert(asr.BSDF("metal_brdf", bsdf_name, bsdf_params))

def convert_roughconductor_bsdf(assembly, bsdf_name, element):
    bsdf_params = {}

    reflectance = element.find("*[@name='specularReflectance']")
    bsdf_params["normal_reflectance"] = convert_colormap(assembly, bsdf_name, reflectance)

    # todo: fix.
    bsdf_params["edge_tint"] = 1
    bsdf_params["mdf"] = "ggx"

    assembly.bsdfs().insert(asr.BSDF("metal_brdf", bsdf_name, bsdf_params))

def convert_dielectric_bsdf(assembly, bsdf_name, element):
    bsdf_params = {}

    # todo: support textured IOR.
    bsdf_params["ior"] = float(element.find("float[@name='intIOR']").attrib["value"])

    # todo: fix.
    bsdf_params["surface_transmittance"] = 1.0
    bsdf_params["mdf"] = "ggx"
    bsdf_params["roughness"] = 0.0

    assembly.bsdfs().insert(asr.BSDF("glass_bsdf", bsdf_name, bsdf_params))

def convert_bsdf(assembly, bsdf_name, element):
    type = element.attrib["type"]
    if type == "diffuse":
        convert_diffuse_bsdf(assembly, bsdf_name, element)
    elif type == "plastic":
        convert_plastic_bsdf(assembly, bsdf_name, element)
    elif type == "roughplastic":
        convert_roughplastic_bsdf(assembly, bsdf_name, element)
    elif type == "conductor":
        convert_conductor_bsdf(assembly, bsdf_name, element)
    elif type == "roughconductor":
        convert_roughconductor_bsdf(assembly, bsdf_name, element)
    elif type == "dielectric":
        convert_dielectric_bsdf(assembly, bsdf_name, element)
    else:
        warning("Don't know how to convert BSDF of type {0}".format(type))

def convert_area_emitter(assembly, emitter_name, element):
    if emitter_name is None:
        fatal("Area emitters must have a name")

    edf_params = {}

    radiance = element.find("*[@name='radiance']")
    edf_params["radiance"] = convert_colormap(assembly, emitter_name, radiance)

    assembly.edfs().insert(asr.EDF("diffuse_edf", emitter_name, edf_params))

def convert_envmap_emitter(scene, assembly, emitter_name, element):
    filepath = element.find("string[@name='filename']").attrib["value"]

    texture_instance_name = create_texture(scene, "environment_map", filepath)

    env_edf = asr.EnvironmentEDF("latlong_map_environment_edf", "environment_edf", {
        "radiance" : texture_instance_name
    })

    matrix = element.find("transform/matrix")
    if matrix is not None:
        roty = asr.Matrix4d.make_rotation(asr.Vector3d(0.0, 1.0, 0.0), math.radians(-90.0))
        bob = get_matrix(matrix)
        bob = bob * roty
        env_edf.transform_sequence().set_transform(0.0, asr.Transformd(bob))

    scene.environment_edfs().insert(env_edf)

    scene.environment_shaders().insert(asr.EnvironmentShader("edf_environment_shader", "environment_shader", {
        "environment_edf" : 'environment_edf'
    }))

    scene.set_environment(asr.Environment("sky", {
        "environment_edf" : "environment_edf",
        "environment_shader" : "environment_shader"
    }))

def convert_sun_emitter(scene, assembly, emitter_name, element):
    sun_params = {}

    turbidity = element.find("float[@name='turbidity']")
    sun_params["turbidity"] = float(turbidity) - 2.0 if turbidity is not None else 1.0

    scale = element.find("float[@name='scale']")
    if scale is not None:
        sun_params["radiance_multiplier"] = float(scale.attrib["value"])

    sun = asr.Light("sun_light", "sun_light", sun_params)

    sun_direction = element.find("vector[@name='sunDirection']")
    if sun_direction is not None:
        from_direction = asr.Vector3d(0.0, 0.0, 1.0)
        to_direction = asr.Vector3d(get_vector(sun_direction))
        sun.set_transform( \
            asr.Transformd( \
                asr.Matrix4d.make_rotation( \
                    asr.Quaterniond.make_rotation(from_direction, to_direction))))

    assembly.lights().insert(sun)

def convert_emitter(scene, assembly, emitter_name, element):
    type = element.attrib["type"]
    if type == "area":
        convert_area_emitter(assembly, emitter_name, element)
    elif type == "envmap":
        convert_envmap_emitter(scene, assembly, emitter_name, element)
    elif type == "sun":
        convert_sun_emitter(scene, assembly, emitter_name, element)
    else:
        warning("Don't know how to convert emitter of type {0}".format(type))

def convert_twosided_material(assembly, material_name, edf_name, element):
    # todo: somehow mark the material as two-sided.
    if material_name is None and "id" in element.attrib:
        material_name = element.attrib["id"]
    convert_material(assembly, material_name, edf_name, element.find("bsdf"))

def convert_bumpmap_material(assembly, material_name, edf_name, element):
    # todo: add bump mapping support.
    if material_name is None and "id" in element.attrib:
        material_name = element.attrib["id"]
    convert_material(assembly, material_name, edf_name, element.find("bsdf"))

def convert_material(assembly, material_name, edf_name, element):
    type = element.attrib["type"]
    if material_name is None and "id" in element.attrib:
        material_name = element.attrib["id"]
    if type == "twosided":
        convert_twosided_material(assembly, material_name, edf_name, element)
    elif type == "bumpmap":
        convert_bumpmap_material(assembly, material_name, edf_name, element)
    else:
        bsdf_name = "{0}_bsdf".format(material_name)
        convert_bsdf(assembly, bsdf_name, element)
        material_params = {
            "surface_shader" : "physical_surface_shader",
            "bsdf" : bsdf_name
        }
        if edf_name is not None:
            material_params["edf"] = edf_name
        assembly.materials().insert(asr.Material("generic_material", material_name, material_params))

def convert_obj_shape(project, assembly, element):
    object_count = len(assembly.objects())
    object_name = "object_{0}".format(object_count)
    instance_name = "{0}_inst".format(object_name)

    filepath = element.find("string[@name='filename']").attrib["value"]

    matrix = get_matrix(element.find("transform/matrix"))
    transform = asr.Transformd(matrix)

    front_material_mappings = {}
    back_material_mappings = {}

    # Material reference.
    ref = element.find("ref")
    if ref is not None:
        front_material_mappings = { "default": element.find("ref").attrib["id"] }
        back_material_mappings = front_material_mappings

    objects = asr.MeshObjectReader.read(project.get_search_paths(), object_name, { "filename" : filepath })

    for object in objects:
        assembly.object_instances().insert(asr.ObjectInstance(instance_name, {}, object.get_name(), transform,
                                                              front_material_mappings, back_material_mappings))
        assembly.objects().insert(object)

def convert_rectangle_shape(scene, assembly, element):
    object_count = len(assembly.objects())
    object_name = "object_{0}".format(object_count)
    instance_name = "{0}_inst".format(object_name)

    matrix = get_matrix(element.find("transform/matrix"))
    rotx = asr.Matrix4d.make_rotation(asr.Vector3d(1.0, 0.0, 0.0), math.radians(90.0))
    matrix = matrix * rotx
    transform = asr.Transformd(matrix)

    front_material_mappings = {}
    back_material_mappings = {}

    # Material reference.
    ref = element.find("ref")
    if ref is not None:
        front_material_mappings = { "default": element.find("ref").attrib["id"] }
        back_material_mappings = front_material_mappings

    # Embedded emitter.
    edf_name = None
    emitter = element.find("emitter")
    if emitter is not None:
        edf_name = "{0}_edf".format(instance_name)
        convert_emitter(scene, assembly, edf_name, emitter)

    # Embedded BSDF.
    bsdf = element.find("bsdf")
    if bsdf is not None:
        material_name = "{0}_material".format(instance_name)
        convert_material(assembly, material_name, edf_name, bsdf)
        front_material_mappings = { "default": material_name }
        back_material_mappings = front_material_mappings

    object = asr.create_primitive_mesh(object_name, {
        "primitive": "grid",
        "resolution_u": 1,
        "resolution_v": 1,
        "width": 2.0,
        "height": 2.0
    })

    assembly.object_instances().insert(asr.ObjectInstance(instance_name, {}, object.get_name(), transform,
                                                          front_material_mappings, back_material_mappings))
    assembly.objects().insert(object)

def convert_shape(project, scene, assembly, element):
    type = element.attrib["type"]
    if type == "obj":
        convert_obj_shape(project, assembly, element)
    elif type == "rectangle":
        convert_rectangle_shape(scene, assembly, element)
    else:
        warning("Don't know how to convert shape of type {0}".format(type))

def convert_scene(project, scene, assembly, element):
    for child in element:
        if child.tag == "integrator":
            convert_integrator(project, child)
        elif child.tag == "sensor":
            convert_sensor(project, scene, child)
        elif child.tag == "bsdf":
            convert_material(assembly, None, None, child)
        elif child.tag == "shape":
            convert_shape(project, scene, assembly, child)
        elif child.tag == "emitter":
            convert_emitter(scene, assembly, None, child)

def convert(tree):
    project = asr.Project("project")

    # Search paths.
    paths = project.get_search_paths()
    paths.append("models")
    paths.append("textures")
    project.set_search_paths(paths)

    # Add default configurations to the project.
    project.add_default_configurations()

    # Enable caustics.
    project.configurations().get_by_name("final").insert_path("pt.enable_caustics", True)
    project.configurations().get_by_name("interactive").insert_path("pt.enable_caustics", True)

    # Create a scene.
    scene = asr.Scene()

    # Create an assembly.
    assembly = asr.Assembly("assembly")
    assembly.surface_shaders().insert(asr.SurfaceShader("physical_surface_shader", "physical_surface_shader"))

    # Convert the Mitsuba scene.
    convert_scene(project, scene, assembly, tree.getroot())

    # Create an instance of the assembly.
    assembly_inst = asr.AssemblyInstance("assembly_inst", {}, assembly.get_name())
    assembly_inst.transform_sequence().set_transform(0.0, asr.Transformd(asr.Matrix4d.identity()))
    scene.assembly_instances().insert(assembly_inst)

    # Insert the assembly into the scene.
    scene.assemblies().insert(assembly)

    # Bind the scene to the project.
    project.set_scene(scene)

    return project


#--------------------------------------------------------------------------------------------------
# Entry point.
#--------------------------------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="convert Mitsuba scenes to appleseed format.")
    parser.add_argument("input_file", metavar="input-file", help="Mitsuba scene (*.xml)")
    parser.add_argument("output_file", metavar="output-file", help="appleseed scene (*.appleseed)")
    args = parser.parse_args()

    # Create a log target that outputs to stderr, and binds it to the renderer's global logger.
    # Eventually you will want to redirect log messages to your own target.
    # For this you will need to subclass appleseed.ILogTarget.
    log_target = asr.ConsoleLogTarget(sys.stderr)

    # It is important to keep log_target alive, as the global logger does not
    # take ownership of it. In this example, we do that by removing the log target
    # when no longer needed, at the end of this function.
    asr.global_logger().add_target(log_target)

    tree = ElementTree()
    try:
        progress("Loading {0}".format(args.input_file))
        tree.parse(args.input_file)
    except IOError:
        fatal("Failed to load {0}".format(args.input_file))

    project = convert(tree)

    progress("Writing {0}".format(args.output_file))
    asr.ProjectFileWriter().write(project, args.output_file,
                                  asr.ProjectFileWriterOptions.OmitHandlingAssetFiles)

if __name__ == '__main__':
    main()
