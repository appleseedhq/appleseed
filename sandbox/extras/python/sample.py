
#
# This source file is part of appleseed.
# Visit http:#appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
#

import appleseed as asr

def build_project():
    project = asr.Project( 'test project')

    paths = project.get_search_paths()
    paths.append( 'data')
    project.set_search_paths( paths)

    project.add_default_configurations()
    conf = project.configurations()['final']
    params = { 'generic_tile_renderer' : { 'min_samples' : 25, 'max_samples' : 25} }
    conf.set_parameters( params)

    scene = asr.Scene()

    assembly = asr.Assembly( "assembly", {})

"""
    #------------------------------------------------------------------------
    # Materials
    #------------------------------------------------------------------------

    # Create a color called "gray" and insert it into the assembly.
    static const float GrayReflectance[] = { 0.5f, 0.5f, 0.5f };
    assembly->colors().insert(
        asr::ColorEntityFactory::create(
            "gray",
            asr::ParamArray()
                .insert("color_space", "srgb"),
            asr::ColorValueArray(3, GrayReflectance)));

    # Create a BRDF called "diffuse_gray_brdf" and insert it into the assembly.
    assembly->bsdfs().insert(
        asr::LambertianBRDFFactory().create(
            "diffuse_gray_brdf",
            asr::ParamArray()
                .insert("reflectance", "gray")));

    # Create a physical surface shader and insert it into the assembly.
    assembly->surface_shaders().insert(
        asr::PhysicalSurfaceShaderFactory().create(
            "physical_surface_shader",
            asr::ParamArray()));

    # Create a material called "gray_material" and insert it into the assembly.
    assembly->materials().insert(
        asr::MaterialFactory::create(
            "gray_material",
            asr::ParamArray()
                .insert("surface_shader", "physical_surface_shader")
                .insert("bsdf", "diffuse_gray_brdf")));

    #------------------------------------------------------------------------
    # Geometry
    #------------------------------------------------------------------------

    # Load the scene geometry from disk.
    asr::MeshObjectArray objects;
    asr::MeshObjectReader::read(
        project->get_search_paths(),
        "cube",
        asr::ParamArray()
            .insert("filename", "scene.obj"),
        objects);

    # Insert all the objects into the assembly.
    for (size_t i = 0; i < objects.size(); ++i)
    {
        # Insert this object into the scene.
        asr::MeshObject* object = objects[i];
        assembly->objects().insert(asf::auto_release_ptr<asr::Object>(object));

        # Create the array of material names.
        asf::StringArray material_names;
        material_names.push_back("gray_material");

        # Create an instance of this object and insert it into the assembly.
        const std::string instance_name = std::string(object->get_name()) + "_inst";
        assembly->object_instances().insert(
            asr::ObjectInstanceFactory::create(
                instance_name.c_str(),
                asr::ParamArray(),
                *object,
                asf::Transformd(asf::Matrix4d::identity()),
                material_names));
    }

    #------------------------------------------------------------------------
    # Light
    #------------------------------------------------------------------------

    # Create a color called "light_exitance" and insert it into the assembly.
    static const float LightExitance[] = { 1.0f, 1.0f, 1.0f };
    assembly->colors().insert(
        asr::ColorEntityFactory::create(
            "light_exitance",
            asr::ParamArray()
                .insert("color_space", "srgb")
                .insert("multiplier", "30.0"),
            asr::ColorValueArray(3, LightExitance)));

    # Create a point light called "light" and insert it into the assembly.
    asf::auto_release_ptr<asr::Light> light(
        asr::PointLightFactory().create(
            "light",
            asr::ParamArray()
                .insert("exitance", "light_exitance")));
    light->set_transform(asf::Transformd(
        asf::Matrix4d::translation(asf::Vector3d(0.6, 2.0, 1.0))));
    assembly->lights().insert(light);

    # Create an instance of the assembly and insert it into the scene.
    scene->assembly_instances().insert(
        asr::AssemblyInstanceFactory::create(
            "assembly_inst",
            asr::ParamArray(),
            *assembly,
            asf::Transformd(asf::Matrix4d::identity())));

    # Insert the assembly into the scene.
    scene->assemblies().insert(assembly);

    #------------------------------------------------------------------------
    # Environment
    #------------------------------------------------------------------------

    # Create a color called "sky_exitance" and insert it into the scene.
    static const float SkyExitance[] = { 0.75f, 0.80f, 1.0f };
    scene->colors().insert(
        asr::ColorEntityFactory::create(
            "sky_exitance",
            asr::ParamArray()
                .insert("color_space", "srgb")
                .insert("multiplier", "0.5"),
            asr::ColorValueArray(3, SkyExitance)));

    # Create an environment EDF called "sky_edf" and insert it into the scene.
    scene->environment_edfs().insert(
        asr::ConstantEnvironmentEDFFactory().create(
            "sky_edf",
            asr::ParamArray()
                .insert("exitance", "sky_exitance")));

    # Create an environment shader called "sky_shader" and insert it into the scene.
    scene->environment_shaders().insert(
        asr::EDFEnvironmentShaderFactory().create(
            "sky_shader",
            asr::ParamArray()
                .insert("environment_edf", "sky_edf")));

    # Create an environment called "sky" and bind it to the scene.
    scene->set_environment(
        asr::EnvironmentFactory::create(
            "sky",
            asr::ParamArray()
                .insert("environment_edf", "sky_edf")
                .insert("environment_shader", "sky_shader")));

"""

    #------------------------------------------------------------------------
    # Camera
    #------------------------------------------------------------------------

    # Create a pinhole camera with film dimensions 0.980 x 0.735 in (24.892 x 18.669 mm).
    params = { 'film_dimensions' : asr.Vector2f( 0.024892, 0.018669), 'focal_length' : 0.035}
    camera = asr.Camera.create( "pinhole_camera", "camera", params)

"""
    # Place and orient the camera. By default cameras are located in (0.0, 0.0, 0.0)
    # and are looking toward Z- (0.0, 0.0, -1.0).
    camera->transform_sequence().set_transform(
        0.0,
        asf::Transformd(
            asf::Matrix4d::rotation(asf::Vector3d(1.0, 0.0, 0.0), asf::deg_to_rad(-20.0)) *
            asf::Matrix4d::translation(asf::Vector3d(0.0, 0.8, 11.0))));
"""
    # Bind the camera to the scene.
    scene.set_camera( camera);

    #------------------------------------------------------------------------
    # Frame
    #------------------------------------------------------------------------

    params = { 'camera' : scene.get_camera().get_name(),
                'resolution' : asr.Vector2i( 640, 480),
                'color_space' : 'srgb'}
    frame = asr.Frame( "beauty", params)


    # Bind the scene to the project.
    project.set_scene( scene);
    return project;

class PyRendererController( asr.IRendererController):
    def __init__( self):
        super( PyRendererController, self).__init__()

def main()
    """
    # Create a log target that outputs to stderr, and binds it to the renderer's global logger.
    # Eventually you will probably want to redirect log messages to your own target. For this
    # you will need to subclass foundation::LogTargetBase (foundation/utility/log/logtargetbase.h).
    std::auto_ptr<asf::LogTargetBase> log_target(asf::create_console_log_target(stderr));
    asr::global_logger().add_target(log_target.get());
    """

    # Build the project.
    proj = build_project()

    renderer_controller = asr.DefaultRendererController()
    renderer = asr.MasterRenderer( project, project.configurations()['final'].get_inherited_parameters(), renderer_controller)
    renderer.render()

    project.get_frame().write( "output.test/png")
    asr.ProjectFileWriter().write( project, "output/test.appleseed")
