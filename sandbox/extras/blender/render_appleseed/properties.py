
#
# This source file is part of appleseed.
# Visit http://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2012 Esteban Tovagliari.
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

from bpy.props import PointerProperty, StringProperty, BoolProperty, EnumProperty
from bpy.props import IntProperty, FloatProperty, FloatVectorProperty, CollectionProperty

class AppleseedRenderSettings( bpy.types.PropertyGroup):
    @classmethod
    def register(cls):
        bpy.types.Scene.appleseed = PointerProperty(
                name = "Appleseed Render Settings",
                description = "Appleseed render settings",
                type = cls
                )

        # sampling
        cls.pixel_filter = bpy.props.EnumProperty( name = "Filter",
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
                                                    items = [ ( "uniform", "Uniform", "Uniform" ),
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

        cls.sampler_max_contrast = bpy.props.FloatProperty( name = "Max Contrast",
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
        cls.lighting_engine = bpy.props.EnumProperty( name = "Lighting Engine",
                    								 description = "Select the lighting engine to use",
                            						 items = [ ( 'pt', "Path Tracing", "Full Global Illumination"),
                                    						( 'drt', "Distributed Ray Tracing", "Direct Lighting Only")],
                                            		 default = 'pt')

        # drt
        cls.drt_ibl_enable = BoolProperty( name = "Image Based Lighting",
                                            description = "Image based lighting",
                                            default = False)

    @classmethod
    def unregister(cls):
        del bpy.types.Scene.appleseed

def register():
	bpy.utils.register_class( AppleseedRenderSettings)

def unregister():
	bpy.utils.unregister_class( AppleseedRenderSettings)
