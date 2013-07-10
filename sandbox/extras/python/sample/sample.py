
#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2012-2013 Esteban Tovagliari, Jupiter Jazz Limited
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

import sys
import signal
import math
import threading

import appleseed as asr

def build_project():
    # Create an empty project.
    project = asr.Project('test project')
    paths = project.get_search_paths()
    paths.append('data')
    project.set_search_paths(paths)

    # Add default configurations to the project.
    project.add_default_configurations()

    # Set the number of samples. This is basically the quality parameter: the higher the number
    # of samples, the smoother the image but the longer the rendering time.
    # todo: fix.
    conf = project.configurations()['final']
    params = { 'uniform_pixel_renderer' : { 'samples' : 25 } }

    # Create a scene.
    scene = asr.Scene()

    # Create an assembly.
    assembly = asr.Assembly("assembly")

    #------------------------------------------------------------------------
    # Materials
    #------------------------------------------------------------------------

    # Create a color called "gray" and insert it into the assembly.
    GrayReflectance = [ 0.5, 0.5, 0.5 ]
    assembly.colors().insert(asr.ColorEntity("gray", { 'color_space' : 'srgb' }, GrayReflectance))

    # Create a BRDF called "diffuse_gray_brdf" and insert it into the assembly.
    assembly.bsdfs().insert(asr.BSDF("lambertian_brdf", "diffuse_gray_brdf", { 'reflectance' : 'gray' }))

    # Create a physical surface shader and insert it into the assembly.
    assembly.surface_shaders().insert(asr.SurfaceShader("physical_surface_shader", "physical_surface_shader"))

    # Create a material called "gray_material" and insert it into the assembly.
    assembly.materials().insert(asr.Material("gray_material", { "surface_shader" : "physical_surface_shader",
                                                                "bsdf" : "diffuse_gray_brdf" }))

    #------------------------------------------------------------------------
    # Geometry
    #------------------------------------------------------------------------

    # Load the scene geometry from disk.
    objects = asr.MeshObjectReader.read(project.get_search_paths(), "cube", { 'filename' : 'scene.obj' })

    # Insert all the objects into the assembly.
    for obj in objects:
        # Create an instance of this object and insert it into the assembly.
        instance_name = obj.get_name() + "_inst"
        material_names = { "default" : "gray_material", "default2" : "gray_material" }
        instance = asr.ObjectInstance(instance_name, {}, obj.get_name(), asr.Transformd(asr.Matrix4d.identity()), material_names)
        assembly.object_instances().insert(instance)

        # Insert this object into the scene.
        assembly.objects().insert(obj)

    #------------------------------------------------------------------------
    # Light
    #------------------------------------------------------------------------

    # Create a color called "light_radiance" and insert it into the assembly.
    LightRadiance = [ 1.0, 1.0, 1.0 ]
    assembly.colors().insert(asr.ColorEntity("light_radiance", { 'color_space' : 'srgb', 'multiplier' : 30.0 }, LightRadiance))

    # Create a point light called "light" and insert it into the assembly.
    light = asr.Light("point_light", "light", { 'radiance' : 'light_radiance' })
    light.set_transform(asr.Transformd(asr.Matrix4d.translation(asr.Vector3d(0.6, 2.0, 1.0))))
    assembly.lights().insert(light)

    # Create an instance of the assembly and insert it into the scene.
    assembly_inst = asr.AssemblyInstance("assembly_inst", {}, assembly.get_name())
    assembly_inst.transform_sequence().set_transform(0.0, asr.Transformd(asr.Matrix4d.identity()))
    scene.assembly_instances().insert(assembly_inst)

    # Insert the assembly into the scene.
    scene.assemblies().insert(assembly)

    #------------------------------------------------------------------------
    # Environment
    #------------------------------------------------------------------------

    # Create a color called "sky_radiance" and insert it into the scene.
    SkyRadiance = [ 0.75, 0.80, 1.0 ]
    scene.colors().insert(asr.ColorEntity("sky_radiance", { 'color_space' : 'srgb', 'multiplier' : 0.5 }, SkyRadiance))

    scene.environment_edfs().insert(asr.EnvironmentEDF("constant_environment_edf", "sky_edf", { 'radiance' : 'sky_radiance' }))
    scene.environment_shaders().insert(asr.EnvironmentShader("edf_environment_shader", "sky_shader", { 'environment_edf' : 'sky_edf' }))

    scene.set_environment(asr.Environment("sky", { "environment_edf" : "sky_edf", "environment_shader" : "sky_shader" }))

    #------------------------------------------------------------------------
    # Camera
    #------------------------------------------------------------------------

    # Create a pinhole camera with film dimensions 0.980 x 0.735 in (24.892 x 18.669 mm).
    params = { 'film_dimensions' : asr.Vector2f(0.024892, 0.018669), 'focal_length' : 0.035 }
    camera = asr.Camera("pinhole_camera", "camera", params)

    # Place and orient the camera. By default cameras are located in (0.0, 0.0, 0.0)
    # and are looking toward Z- (0.0, 0.0, -1.0).
    mat = asr.Matrix4d.rotation(asr.Vector3d(1.0, 0.0, 0.0), math.radians(-20.0))
    mat = mat * asr.Matrix4d.translation(asr.Vector3d(0.0, 0.8, 11.0))
    camera.transform_sequence().set_transform(0.0, asr.Transformd(mat))

    # Bind the camera to the scene.
    scene.set_camera(camera)

    #------------------------------------------------------------------------
    # Frame
    #------------------------------------------------------------------------

    params = { 'camera' : scene.get_camera().get_name(),
               'resolution' : asr.Vector2i(640, 480),
               'color_space' : 'srgb' }
    project.set_frame(asr.Frame("beauty", params))

    # Bind the scene to the project.
    project.set_scene(scene)

    return project

class RendererController(asr.IRendererController):
    def __init__(self):
        super(RendererController, self).__init__()

        # catch Control-C
        signal.signal(signal.SIGINT, lambda signal, frame: self.__signal_handler(signal, frame))
        self.__abort = False
        self.__count = 0

    def __signal_handler(self, signal, frame):
        print "Ctrl+C, aborting."
        self.__abort = True

    # This method is called before rendering begins.
    def on_rendering_begin(self):
        pass

    # This method is called after rendering has succeeded.
    def on_rendering_success(self):
        pass

    # This method is called after rendering was aborted.
    def on_rendering_abort(self):
        pass

    # This method is called before rendering a single frame.
    def on_frame_begin(self):
        pass

    # This method is called after rendering a single frame.
    def on_frame_end(self):
        pass

    def on_progress(self):
        self.__count += 1

        if self.__count == 1000:
            sys.stdout.write('.')
            self.__count = 0

        if self.__abort:
            return asr.IRenderControllerStatus.AbortRendering

        return asr.IRenderControllerStatus.ContinueRendering

class TileCallback(asr.ITileCallback):
    def __init__(self):
        super(TileCallback, self).__init__()

    def pre_render(self, x, y, width, height):
        pass

    def post_render_tile(self, frame, tile_x, tile_y):
        pass

    def post_render(self, frame):
        pass

def main():
    log_target = asr.ConsoleLogTarget(sys.stderr)
    asr.global_logger().add_target(log_target)

    # Build the project.
    project = build_project()

    # Create the master renderer.
    renderer_controller = RendererController()
    tile_callback = TileCallback()
    renderer = asr.MasterRenderer(project,
                                  project.configurations()['final'].get_inherited_parameters(),
                                  renderer_controller,
                                  tile_callback)

    # Render the frame.
    render_thread = asr.RenderThread(renderer)
    render_thread.start()
    render_thread.join()

    # Save the frame to disk.
    project.get_frame().write_main_image("output/test.png")

    # Save the project to disk.
    asr.ProjectFileWriter().write(project, "output/test.appleseed")

if __name__ == "__main__":
    main()
