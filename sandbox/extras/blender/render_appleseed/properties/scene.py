
#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2013 Franz Beaune, Joel Daniels, Esteban Tovagliari.
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

import bpy

class AppleseedSceneSettings( bpy.types.PropertyGroup):
    @classmethod
    def register( cls):
        bpy.types.Scene.appleseed = bpy.props.PointerProperty( name = "Appleseed Scene Settings",
                                                                description = "appleseed scene settings",
                                                                type = cls
                                                                )

        cls.display_mode = bpy.props.EnumProperty(  name = "Display Mode",
                                                    description = "Select where rendered images will be displayed",
                                                    items=(( 'KEEP_UI', "Keep UI", ""),
                                                           ( 'NEW_WINDOW', "New Window", ""),
                                                           ( 'IMAGE_EDITOR', "Image Editor", ""),
                                                           ( 'FULL_SCREEN', "Full Screen", ""),
                                                           ( 'AS_STUDIO', "Appleseed Studio", "")),
                                                    default = 'IMAGE_EDITOR')

        # sampling
        cls.decorrelate_pixels = bpy.props.BoolProperty( name = "Decorrelate Pixels", description = '', default = False)
        
        cls.pixel_filter = bpy.props.EnumProperty(  name = "Filter",
                                                    description = "Pixel filter to use",
                                                    items = [ ( "box", "Box", "Box" ),
                                                              ( "gaussian", "Gaussian", "Gaussian"),
                                                              ( "mitchell", "Mitchell", "Mitchell")],
                                                    default = "mitchell")

        cls.filter_size = bpy.props.IntProperty( name = "Filter Size",
                                                 description = "Filter size",
                                                 min = 1,
                                                 max = 64,
                                                 default = 2,
                                                 subtype = 'UNSIGNED')

        cls.pixel_sampler = bpy.props.EnumProperty( name = "Sampler",
                                                    description = "Sampler",
                                                    items = [( "uniform", "Uniform", "Uniform" ),
                                                             ( "adaptive", "Adaptive", "Adaptive")],
                                                    default = "adaptive")

        cls.sampler_min_samples = bpy.props.IntProperty( name = "Min Samples",
                                                         description = "Min Samples",
                                                         min = 1,
                                                         max = 1000000,
                                                         default = 2,
                                                         subtype = 'UNSIGNED')

        cls.sampler_max_samples = bpy.props.IntProperty( name = "Max Samples",
                                                         description = "Max Samples",
                                                         min = 1,
                                                         max = 1000000,
                                                         default = 64,
                                                         subtype = 'UNSIGNED')

        cls.sampler_max_contrast = bpy.props.FloatProperty(  name = "Max Contrast",
                                                             description = "Max contrast",
                                                             min = 0,
                                                             max = 1000,
                                                             default = 1)

        cls.sampler_max_variation = bpy.props.FloatProperty( name = "Max Variation",
                                                             description = "Max variation",
                                                             min = 0,
                                                             max = 1000,
                                                             default = 1)

        # lighting
        cls.lighting_engine = bpy.props.EnumProperty(   name = "Lighting Engine",
                                                        description = "Select the lighting engine to use",
                                                        items = [ ( 'pt', "Path Tracing", "Full Global Illumination"),
                                                                  ( 'drt', "Distributed Ray Tracing", "Direct Lighting Only")],
                                                        default = 'pt')

        cls.caustics_enable = bpy.props.BoolProperty( name = "Caustics",
                                                      description = "Caustics",
                                                      default = True)

    @classmethod
    def unregister( cls):
        del bpy.types.Scene.appleseed

def register():
    pass

def unregister():
    pass
