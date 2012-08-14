
#
# This source file is part of appleseed.
# Visit http:#appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
#

import appleseed as asr
import math

def build_project():
    project = asr.Project( 'test project')

    paths = project.get_search_paths()
    paths.append( '/home/est/Devel/appleseed/sandbox/extras/devkit/sample/data')
    project.set_search_paths( paths)

    project.add_default_configurations()
    conf = project.configurations()['final']
    params = { 'generic_tile_renderer' : { 'min_samples' : 25, 'max_samples' : 25} }
    conf.set_parameters( params)

    scene = asr.Scene()

    assembly = asr.Assembly( "assembly")

    #------------------------------------------------------------------------
    # Materials
    #------------------------------------------------------------------------

    # Create a color called "gray" and insert it into the assembly.
    GrayReflectance = [0.5, 0.5, 0.5]
    assembly.colors().insert( asr.ColorEntity( "gray", { 'color_space' : 'srgb'}, GrayReflectance))

    # Create a BRDF called "diffuse_gray_brdf" and insert it into the assembly.
    assembly.bsdfs().insert( asr.BSDF( "lambertian_brdf", "diffuse_gray_brdf", { 'reflectance' : 'gray'}))

    # Create a physical surface shader and insert it into the assembly.
    assembly.surface_shaders().insert( asr.SurfaceShader( "physical_surface_shader", "physical_surface_shader"))

    # Create a material called "gray_material" and insert it into the assembly.
    assembly.materials().insert( asr.Material( "gray_material", { "surface_shader" : "physical_surface_shader",
                                                                    "bsdf" : "diffuse_gray_brdf"}))
    #------------------------------------------------------------------------
    # Geometry
    #------------------------------------------------------------------------

    # Load the scene geometry from disk.
    objects = asr.MeshObjectReader.read( project.get_search_paths(), "cube", { 'filename' : 'scene.obj'})

    # Insert all the objects into the assembly.
    for o in objects:
        obj_name = o.get_name()

        # insert takes ownership of the object
        # it's necessary to save the index of the
        # object an get a new reference to o
        index = assembly.objects().insert( o)
        o = assembly.objects()[index]

        instance_name = obj_name + "_inst"
        material_names = ["gray_material"]

        # Create an instance of this object and insert it into the assembly.
        obj_inst = asr.ObjectInstance( instance_name, {}, o, asr.Transformd( asr.Matrix4d.identity()), material_names)
        assembly.object_instances().insert( obj_inst)

    #------------------------------------------------------------------------
    # Light
    #------------------------------------------------------------------------

    # Create a color called "light_exitance" and insert it into the assembly.
    LightExitance = [ 1.0, 1.0, 1.0 ]
    assembly.colors().insert( asr.ColorEntity( "light_exitance", { 'color_space' : 'srgb', 'multiplier' : 30.0 }, LightExitance))

    light = asr.Light( "point_light", "light", { 'exitance' : 'light_exitance'})
    light.set_transform( asr.Transformd( asr.Matrix4d.translation( asr.Vector3d(0.6, 2.0, 1.0))))
    assembly.lights().insert( light)

    # Create an instance of the assembly and insert it into the scene.
    assembly_inst = asr.AssemblyInstance( "assembly_inst", {}, assembly)
    assembly_inst.transform_sequence().set_transform( 0.0, asr.Transformd( asr.Matrix4d.identity()))
    scene.assembly_instances().insert( assembly_inst)

    # Insert the assembly into the scene.
    scene.assemblies().insert( assembly)

    #------------------------------------------------------------------------
    # Environment
    #------------------------------------------------------------------------

    # Create a color called "sky_exitance" and insert it into the scene.
    SkyExitance = [ 0.75, 0.80, 1.0 ];
    scene.colors().insert( asr.ColorEntity( "sky_exitance", { 'color_space' : 'srgb', 'multiplier' : 0.5 }, SkyExitance))

    scene.environment_edfs().insert( asr.EnvironmentEDF( "constant_environment_edf", "sky_edf", { 'exitance' : 'sky_exitance'}))
    scene.environment_shaders().insert( asr.EnvironmentShader( "edf_environment_shader", "sky_shader", { 'environment_edf' : 'sky_edf'}))

    scene.set_environment( asr.Environment( "sky", { "environment_edf" : "sky_edf", "environment_shader" : "sky_shader"}))

    #------------------------------------------------------------------------
    # Camera
    #------------------------------------------------------------------------

    # Create a pinhole camera with film dimensions 0.980 x 0.735 in (24.892 x 18.669 mm).
    params = { 'film_dimensions' : asr.Vector2f( 0.024892, 0.018669), 'focal_length' : 0.035}
    camera = asr.Camera( "pinhole_camera", "camera", params)

    # Place and orient the camera. By default cameras are located in (0.0, 0.0, 0.0)
    # and are looking toward Z- (0.0, 0.0, -1.0).
    mat = asr.Matrix4d.rotation( asr.Vector3d(1.0, 0.0, 0.0), math.radians(-20.0))
    mat = mat * asr.Matrix4d.translation( asr.Vector3d(0.0, 0.8, 11.0))
    camera.transform_sequence().set_transform( 0.0, asr.Transformd( mat))

    # Bind the camera to the scene.
    scene.set_camera( camera)

    #------------------------------------------------------------------------
    # Frame
    #------------------------------------------------------------------------

    params = { 'camera' : scene.get_camera().get_name(),
                'resolution' : asr.Vector2i( 640, 480),
                'color_space' : 'srgb'}
    project.set_frame( asr.Frame( "beauty", params))

    # Bind the scene to the project.
    project.set_scene( scene)
    return project;

class PyRendererController( asr.IRendererController):
    def __init__( self):
        super( PyRendererController, self).__init__()

    # This method is called before rendering begins.
    def on_rendering_begin( self):
        print( "rendering begin")

    # This method is called after rendering has succeeded.
    def on_rendering_success( self):
        print( "rendering success")

    # This method is called after rendering was aborted.
    def on_rendering_abort( self):
        print( "rendering abort")

    # This method is called before rendering a single frame.
    def on_frame_begin( self):
        print( "frame begin")

    # This method is called after rendering a single frame.
    def on_frame_end( self):
        print( "frame end")

    def on_progress( self):
        return asr.IRenderControllerStatus.ContinueRendering

def main():
    """
    # Create a log target that outputs to stderr, and binds it to the renderer's global logger.
    # Eventually you will probably want to redirect log messages to your own target. For this
    # you will need to subclass foundation::LogTargetBase (foundation/utility/log/logtargetbase.h).
    std::auto_ptr<asf::LogTargetBase> log_target(asf::create_console_log_target(stderr))
    asr::global_logger().add_target(log_target.get())
    """

    # Build the project.
    project = build_project()

    #renderer_controller = asr.DefaultRendererController()
    #renderer_controller = PyRendererController()
    #renderer = asr.MasterRenderer( project, project.configurations()['final'].get_inherited_parameters(), renderer_controller)
    #renderer.render()

    #project.get_frame().write( "output.test/png")
    asr.ProjectFileWriter().write( project, "/tmp/test.appleseed")

if __name__ == "__main__":
    main()
