
#
# This source file is part of appleseed.
# Visit https://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2012-2013 Esteban Tovagliari, Jupiter Jazz Limited
# Copyright (c) 2014-2018 Esteban Tovagliari, The appleseedhq Organization
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
    conf.insert_path('uniform_pixel_renderer.samples', 25)

    # Create a scene.
    scene = asr.Scene()

    # Create an assembly.
    assembly = asr.Assembly("assembly")

    #------------------------------------------------------------------------
    # Materials
    #------------------------------------------------------------------------

    # Create a color called "gray" and insert it into the assembly.
    GrayReflectance = [0.5, 0.5, 0.5]
    assembly.colors().insert(asr.ColorEntity("gray", {'color_space': 'srgb'}, GrayReflectance))

    # Create a BRDF called "diffuse_gray_brdf" and insert it into the assembly.
    assembly.bsdfs().insert(asr.BSDF("lambertian_brdf", "diffuse_gray_brdf", {'reflectance': 'gray'}))

    # Create a physical surface shader and insert it into the assembly.
    assembly.surface_shaders().insert(asr.SurfaceShader("physical_surface_shader", "physical_surface_shader"))

    # Create a material called "gray_material" and insert it into the assembly.
    assembly.materials().insert(asr.Material("generic_material", "gray_material", {"surface_shader": "physical_surface_shader",
                                                                                   "bsdf": "diffuse_gray_brdf"}))

    #------------------------------------------------------------------------
    # Geometry
    #------------------------------------------------------------------------

    # Load the scene geometry from disk.
    objects = asr.MeshObjectReader.read(project.get_search_paths(), "cube", {'filename': 'scene.obj'})

    # Insert all the objects into the assembly.
    for object in objects:
        # Create an instance of this object and insert it into the assembly.
        instance_name = object.get_name() + "_inst"
        material_names = {"default": "gray_material", "default2": "gray_material"}
        instance = asr.ObjectInstance(instance_name, {}, object.get_name(), asr.Transformd(asr.Matrix4d.identity()), material_names)
        assembly.object_instances().insert(instance)

        # Insert this object into the scene.
        assembly.objects().insert(object)

    #------------------------------------------------------------------------
    # Light
    #------------------------------------------------------------------------

    # Create a color called "light_intensity" and insert it into the assembly.
    LightRadiance = [1.0, 1.0, 1.0]
    assembly.colors().insert(asr.ColorEntity("light_intensity", {'color_space': 'srgb', 'multiplier': 30.0}, LightRadiance))

    # Create a point light called "light" and insert it into the assembly.
    light = asr.Light("point_light", "light", {'intensity': 'light_intensity'})
    light.set_transform(asr.Transformd(asr.Matrix4d.make_translation(asr.Vector3d(0.6, 2.0, 1.0))))
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

    # Create a color called "sky_radiance" and insert it into the scene.
    SkyRadiance = [0.75, 0.80, 1.0]
    scene.colors().insert(asr.ColorEntity("sky_radiance", {'color_space': 'srgb', 'multiplier': 0.5}, SkyRadiance))

    # Create an environment EDF called "sky_edf" and insert it into the scene.
    scene.environment_edfs().insert(asr.EnvironmentEDF("constant_environment_edf", "sky_edf", {'radiance': 'sky_radiance'}))

    # Create an environment shader called "sky_shader" and insert it into the scene.
    scene.environment_shaders().insert(asr.EnvironmentShader("edf_environment_shader", "sky_shader", {'environment_edf': 'sky_edf'}))

    # Create an environment called "sky" and bind it to the scene.
    scene.set_environment(asr.Environment("sky", {"environment_edf": "sky_edf", "environment_shader": "sky_shader"}))

    #------------------------------------------------------------------------
    # Camera
    #------------------------------------------------------------------------

    # Create a pinhole camera with film dimensions 0.980 x 0.735 in (24.892 x 18.669 mm).
    params = {'film_dimensions': asr.Vector2f(0.024892, 0.018669), 'focal_length': 0.035}
    camera = asr.Camera("pinhole_camera", "camera", params)

    # Place and orient the camera. By default cameras are located in (0.0, 0.0, 0.0)
    # and are looking toward Z- (0.0, 0.0, -1.0).
    mat = asr.Matrix4d.make_rotation(asr.Vector3d(1.0, 0.0, 0.0), math.radians(-20.0))
    mat = mat * asr.Matrix4d.make_translation(asr.Vector3d(0.0, 0.8, 11.0))
    camera.transform_sequence().set_transform(0.0, asr.Transformd(mat))

    # Bind the camera to the scene.
    scene.cameras().insert(camera)

    #------------------------------------------------------------------------
    # Frame
    #------------------------------------------------------------------------

    # Create a frame and bind it to the project.
    params = {'camera': 'camera',
              'resolution': asr.Vector2i(640, 480),
              'color_space': 'srgb'}
    project.set_frame(asr.Frame("beauty", params))

    # Bind the scene to the project.
    project.set_scene(scene)

    return project


class RendererController(asr.IRendererController):

    def __init__(self):
        super(RendererController, self).__init__()
        self.__abort = False

    def abort_rendering(self):
        sys.stdout.write("aborting rendering...     \n")
        sys.stdout.flush()
        self.__abort = True

    # This method is called before rendering begins.
    def on_rendering_begin(self):
        self.__abort = False

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

    # This method is called continuously during rendering.
    def on_progress(self):
        pass

    #  Return the current rendering status.
    def get_status(self):
        if self.__abort:
            return asr.IRenderControllerStatus.AbortRendering
        else:
            return asr.IRenderControllerStatus.ContinueRendering


class TileCallback(asr.ITileCallback):

    def __init__(self):
        super(TileCallback, self).__init__()
        self.rendered_pixels = 0

    # This method is called before a frame is rendered.
    def on_tiled_frame_begin(self, frame):
        pass

    # This method is called after a frame is rendered.
    def on_tiled_frame_end(self, frame):
        pass

    # This method is called before a tile is rendered.
    def on_tile_begin(self, frame, tile_x, tile_y):
        pass

    # This method is called after a tile is rendered.
    def on_tile_end(self, frame, tile_x, tile_y):
        # Keep track of the total number of rendered pixels.
        tile = frame.image().tile(tile_x, tile_y)
        self.rendered_pixels += tile.get_pixel_count()

        # Retrieve the total number of pixels in the frame.
        total_pixels = frame.image().properties().m_pixel_count

        # Print a progress message.
        percent = (100.0 * self.rendered_pixels) / total_pixels
        sys.stdout.write("rendering, {0:.2f}% done   \r".format(percent))

    # This method is called after the frame has been updated.
    def on_progressive_frame_update(self, frame):
        pass


class RenderThread(threading.Thread):

    def __init__(self, master_renderer, renderer_controller):
        super(RenderThread, self).__init__()
        self.__master_renderer = master_renderer
        self.__renderer_controller = renderer_controller

    def run(self):
        self.__master_renderer.render(self.__renderer_controller)

RENDER_ON_THREAD = True


def main():
    # Create a log target that outputs to stderr, and binds it to the renderer's global logger.
    # Eventually you will want to redirect log messages to your own target.
    # For this you will need to subclass appleseed.ILogTarget.
    log_target = asr.ConsoleLogTarget(sys.stderr)

    # It is important to keep log_target alive, as the global logger does not
    # take ownership of it. In this example, we do that by removing the log target
    # when no longer needed, at the end of this function.
    asr.global_logger().add_target(log_target)

    # Build the project.
    project = build_project()

    # Create the master renderer.
    renderer_controller = RendererController()

    # Catch Control-C.
    signal.signal(signal.SIGINT, lambda signal, frame: renderer_controller.abort_rendering())

    resource_search_paths = []
    tile_callback = TileCallback()
    renderer = asr.MasterRenderer(project,
                                  project.configurations()['final'].get_inherited_parameters(),
                                  resource_search_paths,
                                  tile_callback)

    # Render the frame.
    if RENDER_ON_THREAD:
        render_thread = RenderThread(renderer, renderer_controller)
        render_thread.start()
        while render_thread.isAlive():
            render_thread.join(0.5)  # seconds
    else:
        renderer.render(renderer_controller)

    # Save the frame to disk.
    project.get_frame().write_main_image("output/test.png")

    # Save the project to disk.
    asr.ProjectFileWriter().write(project, "output/test.appleseed")

    # Remove the log target we added previosly.
    asr.global_logger().remove_target(log_target)

if __name__ == "__main__":
    main()
