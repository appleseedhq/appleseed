import appleseed as asr

def build_project():
    # Create an empty project.
    project = asr.Project('test project')

    # Add default configurations to the project.
    project.add_default_configurations()

    # Set the number of samples. This is basically the quality parameter: the higher the number
    # of samples, the smoother the image but the longer the rendering time.
    # todo: fix.
    conf = project.configurations()['final']
    conf.insert_path('uniform_pixel_renderer.samples', 16)

    # Create a scene.
    scene = asr.Scene()

    # Create an assembly.
    assembly = asr.Assembly("assembly")

    #------------------------------------------------------------------------
    # Materials
    #------------------------------------------------------------------------

    for i in range(0, 10):
        assembly.bsdfs().insert(
            asr.BSDF("glossy_brdf", "glossy" + str(i), {
                "mdf" : "ggx",
                "reflectance" : 1.0,
                "roughness" : i / 9.0,
                "energy_compensation" : 0.0
                }
            )
        )

        assembly.bsdfs().insert(
            asr.BSDF("glossy_brdf", "glossy_ec" + str(i), {
                "mdf" : "ggx",
                "reflectance" : 1.0,
                "roughness" : i / 9.0,
                "energy_compensation" : 1.0
                }
            )
        )

    for i in range(0, 10):
        assembly.materials().insert(
            asr.Material("generic_material", "mat" + str(i), {"bsdf": "glossy" + str(i)}))

        assembly.materials().insert(
            asr.Material("generic_material", "mat_ec" + str(i), {"bsdf": "glossy_ec" + str(i)}))

    #------------------------------------------------------------------------
    # Geometry
    #------------------------------------------------------------------------

    object_name = "sphere"
    object = asr.MeshObject(
        object_name, {
            "primitive": "sphere",
            "radius": 0.4
        }
    )
    assembly.objects().insert(object)

    obj_instance_params = {
        'visibility' : {
            "glossy" : False,
            "shadow" : False
        }
    }

    for i in range(0, 10):
        instance_name = object_name + "_inst" + str(i)
        material_names = {"default": "mat" + str(i)}

        mat = asr.Matrix4d.make_translation(asr.Vector3d(-5.0 + i, -0.5, 0.0))

        instance = asr.ObjectInstance(instance_name, obj_instance_params, object_name, asr.Transformd(mat), material_names)
        assembly.object_instances().insert(instance)

    for i in range(0, 10):
        instance_name = object_name + "_ec_inst" + str(i)
        material_names = {"default": "mat_ec" + str(i)}

        mat = asr.Matrix4d.make_translation(asr.Vector3d(-5.0 + i, 0.5, 0.0))

        instance = asr.ObjectInstance(instance_name, obj_instance_params, object_name, asr.Transformd(mat), material_names)
        assembly.object_instances().insert(instance)

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

    # Create a color called "gray" and insert it into the scene.
    Gray = [0.5, 0.5, 0.5]
    scene.colors().insert(asr.ColorEntity("gray", {'color_space': 'linear_rgb', 'multiplier': 1.0}, Gray))

    # Create an environment EDF called "gray_edf" and insert it into the scene.
    scene.environment_edfs().insert(asr.EnvironmentEDF("constant_environment_edf", "gray_edf", {'radiance': 'gray'}))

    # Create an environment shader called "gray_shader" and insert it into the scene.
    scene.environment_shaders().insert(asr.EnvironmentShader("edf_environment_shader", "gray_shader", {'environment_edf': 'gray_edf'}))

    # Create an environment called "sky" and bind it to the scene.
    scene.set_environment(asr.Environment("sky", {"environment_edf": "gray_edf", "environment_shader": "gray_shader"}))

    #------------------------------------------------------------------------
    # Camera
    #------------------------------------------------------------------------

    params = {'film_dimensions': asr.Vector2f(0.0640, 0.0200), 'focal_length': 0.035}
    camera = asr.Camera("pinhole_camera", "camera", params)

    mat = asr.Matrix4d.make_translation(asr.Vector3d(-0.444315058060864, -0.071277492791890, 5.674764299781837))
    camera.transform_sequence().set_transform(0.0, asr.Transformd(mat))

    # Bind the camera to the scene.
    scene.cameras().insert(camera)

    #------------------------------------------------------------------------
    # Frame
    #------------------------------------------------------------------------

    # Create a frame and bind it to the project.
    params = {'camera': 'camera',
              'resolution': asr.Vector2i(640, 200)}
    project.set_frame(asr.Frame("beauty", params))

    # Bind the scene to the project.
    project.set_scene(scene)

    return project

def main():
    # Build the project.
    project = build_project()

    # Save the project to disk.
    asr.ProjectFileWriter().write(project, "test.appleseed")

if __name__ == "__main__":
    main()
