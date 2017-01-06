#!/usr/bin/python

#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2016-2017 Francois Beaune, The appleseedhq Organization
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
    values = [float(x) for x in element.attrib["value"].split()]
    if len(values) != 16:
        fatal("Matrix was expected to contain 16 coefficients but contained {0} instead".format(len(values)))
    matrix = asr.Matrix4d()
    for i in range(16):
        matrix[i / 4, i % 4] = values[i]
    return matrix


def get_rgb(element):
    values = [float(x) for x in element.attrib["value"].split(",")]
    if len(values) != 3:
        fatal("RGB color was expected to contain 3 coefficients but contained {0} instead".format(len(values)))
    max_value = max(values)
    if max_value > 1.0:
        return [x / max_value for x in values], max_value
    else:
        return values, 1.0


def set_private_param(params, key, value):
    params.setdefault("mitsuba2appleseed", {})[key] = value


def get_private_param(params, key, default):
    return params.setdefault("mitsuba2appleseed", {}).get(key, default)


def clear_private_params(params):
    params.pop("mitsuba2appleseed", None)


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
        "camera": "camera",
        "color_space": "srgb"
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
    base_group.textures().insert(asr.Texture("disk_texture_2d", texture_name, {
        "filename": filepath,
        "color_space": "linear_rgb" if filepath.endswith(".exr") or filepath.endswith(".hdr") else "srgb"
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
            "color_space": "srgb",
            "multiplier": 1.0
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
            "color_space": "linear_rgb",
            "multiplier": multiplier
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
    bsdf_params["roughness"] = math.sqrt(float(element.find("float[@name='alpha']").attrib["value"]))

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
        "radiance": texture_instance_name
    })

    matrix_element = element.find("transform/matrix")
    if matrix_element is not None:
        matrix = get_matrix(matrix_element)
        roty = asr.Matrix4d.make_rotation(asr.Vector3d(0.0, 1.0, 0.0), math.radians(-90.0))
        matrix = matrix * roty
        env_edf.transform_sequence().set_transform(0.0, asr.Transformd(matrix))

    scene.environment_edfs().insert(env_edf)

    scene.environment_shaders().insert(asr.EnvironmentShader("edf_environment_shader", "environment_shader", {
        "environment_edf": 'environment_edf'
    }))

    scene.set_environment(asr.Environment("environment", {
        "environment_edf": "environment_edf",
        "environment_shader": "environment_shader"
    }))


def convert_sun_emitter(scene, assembly, emitter_name, element):
    sun_params = {}

    turbidity = element.find("float[@name='turbidity']")
    if turbidity is not None:
        sun_params["turbidity"] = float(turbidity.attrib["value"]) - 2.0
    else:
        sun_params["turbidity"] = 1.0

    scale = element.find("float[@name='scale']")
    if scale is not None:
        sun_params["radiance_multiplier"] = float(scale.attrib["value"])

    sun = asr.Light("sun_light", "sun_light", sun_params)

    sun_direction = element.find("vector[@name='sunDirection']")
    if sun_direction is not None:
        from_direction = asr.Vector3d(0.0, 0.0, 1.0)
        to_direction = asr.Vector3d(get_vector(sun_direction))
        sun.set_transform(
            asr.Transformd(
                asr.Matrix4d.make_rotation(
                    asr.Quaterniond.make_rotation(from_direction, to_direction))))

    assembly.lights().insert(sun)


def convert_sunsky_emitter(scene, assembly, emitter_name, element):
    turbidity_element = element.find("float[@name='turbidity']")
    if turbidity_element is not None:
        turbidity = float(turbidity_element.attrib["value"]) - 2.0
    else:
        turbidity = 1.0

    # Sky.
    sun_direction = element.find("vector[@name='sunDirection']")
    if sun_direction is not None:
        d = get_vector(sun_direction)
        sun_theta = math.acos(d[1])
        sun_phi = math.atan2(d[2], d[0])
    else:
        sun_theta = 0.0
        sun_phi = 0.0
    env_edf = asr.EnvironmentEDF("hosek_environment_edf", "environment_edf", {
        "sun_theta": math.degrees(sun_theta),
        "sun_phi": math.degrees(sun_phi),
        "turbidity": turbidity
    })
    scene.environment_edfs().insert(env_edf)
    scene.environment_shaders().insert(asr.EnvironmentShader("edf_environment_shader", "environment_shader", {
        "environment_edf": 'environment_edf'
    }))
    scene.set_environment(asr.Environment("environment", {
        "environment_edf": "environment_edf",
        "environment_shader": "environment_shader"
    }))

    # Sun.
    sun_params = {"environment_edf": "environment_edf", "turbidity": turbidity}
    sun_scale = element.find("float[@name='sunScale']")
    if sun_scale is not None:
        sun_params["radiance_multiplier"] = float(sun_scale.attrib["value"])
    sun = asr.Light("sun_light", "sun_light", sun_params)
    assembly.lights().insert(sun)


def convert_emitter(scene, assembly, emitter_name, element):
    type = element.attrib["type"]
    if type == "area":
        convert_area_emitter(assembly, emitter_name, element)
    elif type == "envmap":
        convert_envmap_emitter(scene, assembly, emitter_name, element)
    elif type == "sun":
        convert_sun_emitter(scene, assembly, emitter_name, element)
    elif type == "sunsky":
        convert_sunsky_emitter(scene, assembly, emitter_name, element)
    else:
        warning("Don't know how to convert emitter of type {0}".format(type))


def convert_material(assembly, material_name, material_params, element):
    if material_name is None and "id" in element.attrib:
        material_name = element.attrib["id"]

    type = element.attrib["type"]

    if type == "twosided":
        set_private_param(material_params, "two_sided", True)
        return convert_material(assembly, material_name, material_params, element.find("bsdf"))

    if type == "bumpmap":
        # todo: add bump mapping support.
        return convert_material(assembly, material_name, material_params, element.find("bsdf"))

    if type == "mask":
        # todo: add masking support.
        return convert_material(assembly, material_name, material_params, element.find("bsdf"))

    # BSDF.
    bsdf_name = "{0}_bsdf".format(material_name)
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
        set_private_param(material_params, "two_sided", True)
        convert_dielectric_bsdf(assembly, bsdf_name, element)
    elif type == "thindielectric":
        set_private_param(material_params, "two_sided", True)
        set_private_param(material_params, "thin_dielectric", True)
        convert_dielectric_bsdf(assembly, bsdf_name, element)
    else:
        warning("Don't know how to convert BSDF of type {0}".format(type))
        return

    # Hack: force light-emitting materials to be single-sided.
    if "edf" in material_params:
        set_private_param(material_params, "two_sided", False)

    # Material.
    material_params["bsdf"] = bsdf_name
    material_params["surface_shader"] = "physical_surface_shader"
    assembly.materials().insert(asr.Material("generic_material", material_name, material_params))


def process_shape_material(scene, assembly, instance_name, element):
    material = None

    # Material reference.
    ref_element = element.find("ref")
    if ref_element is not None:
        material_name = ref_element.attrib["id"]
        material = assembly.materials().get_by_name(material_name)

    # Embedded material (has priority over the referenced material).
    bsdf_element = element.find("bsdf")
    if bsdf_element is not None:
        material_name = "{0}_material".format(instance_name)
        convert_material(assembly, material_name, {}, bsdf_element)
        material = assembly.materials().get_by_name(material_name)

    # Embedded emitter (we suppose it's an area emitter).
    emitter_element = element.find("emitter")
    if emitter_element is not None:
        edf_name = "{0}_edf".format(instance_name)
        convert_emitter(scene, assembly, edf_name, emitter_element)
        params = material.get_parameters()
        params["edf"] = edf_name
        material.set_parameters(params)

    return material.get_name() if material is not None else None


def make_object_instance(assembly, object, instance_name, material_name, transform):
    front_mappings = {}
    back_mappings = {}
    instance_params = {}

    if material_name is not None:
        material = assembly.materials().get_by_name(material_name)
        two_sided = get_private_param(material.get_parameters(), "two_sided", False) if material is not None else False
        thin_dielectric = get_private_param(material.get_parameters(), "thin_dielectric", False) if material is not None else False

        slots = object.material_slots()
        if len(slots) == 0:
            slots = ["default"]

        front_mappings = dict([(slot, material_name) for slot in slots])
        back_mappings = front_mappings if two_sided else {}

        if thin_dielectric:
            instance_params = {"visibility": {"shadow": False}}

    return asr.ObjectInstance(instance_name, instance_params, object.get_name(), transform, front_mappings, back_mappings)


def convert_obj_shape(project, scene, assembly, element):
    object_count = len(assembly.objects())
    object_name = "object_{0}".format(object_count)
    instance_name = "{0}_inst".format(object_name)

    # Objects.
    filepath = element.find("string[@name='filename']").attrib["value"]
    objects = asr.MeshObjectReader.read(project.get_search_paths(), object_name, {"filename": filepath})

    # Instance transform.
    matrix = get_matrix(element.find("transform/matrix"))
    transform = asr.Transformd(matrix)

    # Instance material.
    material_name = process_shape_material(scene, assembly, instance_name, element)

    for object in objects:
        instance = make_object_instance(assembly, object, instance_name, material_name, transform)
        assembly.object_instances().insert(instance)
        assembly.objects().insert(object)


def convert_rectangle_shape(scene, assembly, element):
    object_count = len(assembly.objects())
    object_name = "object_{0}".format(object_count)
    instance_name = "{0}_inst".format(object_name)

    # Object.
    object = asr.create_primitive_mesh(object_name, {
        "primitive": "grid",
        "resolution_u": 1,
        "resolution_v": 1,
        "width": 2.0,
        "height": 2.0
    })

    # Instance transform.
    matrix = get_matrix(element.find("transform/matrix"))
    rotx = asr.Matrix4d.make_rotation(asr.Vector3d(1.0, 0.0, 0.0), math.radians(90.0))
    matrix = matrix * rotx
    transform = asr.Transformd(matrix)

    # Instance material.
    material_name = process_shape_material(scene, assembly, instance_name, element)

    instance = make_object_instance(assembly, object, instance_name, material_name, transform)
    assembly.object_instances().insert(instance)
    assembly.objects().insert(object)


def convert_sphere_shape(scene, assembly, element):
    object_count = len(assembly.objects())
    object_name = "object_{0}".format(object_count)
    instance_name = "{0}_inst".format(object_name)

    # Radius.
    radius_element = element.find("float[@name='radius']")
    radius = float(radius_element.attrib["value"]) if radius_element is not None else 1.0

    # Center.
    center_element = element.find("point[@name='center']")
    center = asr.Vector3d(get_vector(center_element)) if center_element is not None else asr.Vector3d(0.0)

    # Object.
    object = asr.create_primitive_mesh(object_name, {
        "primitive": "sphere",
        "resolution_u": 32,
        "resolution_v": 16,
        "radius": radius
    })

    # Instance transform.
    matrix = asr.Matrix4d.make_translation(center)
    matrix_element = element.find("transform/matrix")
    if matrix_element is not None:
        # todo: no idea what is the right multiplication order, untested.
        matrix = matrix * get_matrix(matrix_element)
    transform = asr.Transformd(matrix)

    # Instance material.
    material_name = process_shape_material(scene, assembly, instance_name, element)

    instance = make_object_instance(assembly, object, instance_name, material_name, transform)
    assembly.object_instances().insert(instance)
    assembly.objects().insert(object)


def convert_shape(project, scene, assembly, element):
    type = element.attrib["type"]
    if type == "obj":
        convert_obj_shape(project, scene, assembly, element)
    elif type == "rectangle":
        convert_rectangle_shape(scene, assembly, element)
    elif type == "sphere":
        convert_sphere_shape(scene, assembly, element)
    else:
        warning("Don't know how to convert shape of type {0}".format(type))


def convert_scene(project, scene, assembly, element):
    for child in element:
        if child.tag == "integrator":
            convert_integrator(project, child)
        elif child.tag == "sensor":
            convert_sensor(project, scene, child)
        elif child.tag == "bsdf":
            convert_material(assembly, None, {}, child)
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
    asr.global_logger().set_verbosity_level(asr.LogMessageCategory.Warning)

    tree = ElementTree()
    try:
        tree.parse(args.input_file)
    except IOError:
        fatal("Failed to load {0}".format(args.input_file))

    project = convert(tree)

    asr.ProjectFileWriter().write(project, args.output_file,
                                  asr.ProjectFileWriterOptions.OmitHandlingAssetFiles)

if __name__ == '__main__':
    main()
