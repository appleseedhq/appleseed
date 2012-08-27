
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

def get_appleseed_path():
    return '/usr/local/appleseed'

class AppleseedRenderSettings(bpy.types.PropertyGroup):
    @classmethod
    def register(cls):
        bpy.types.Scene.appleseed = PointerProperty(
                name="Appleseed Render Settings",
                description="Appleseed render settings",
                type=cls,
                )

        cls.lighting_engine = bpy.props.EnumProperty(name="Lighting Engine",
                    								 description="Select the lighting engine to use",
                            						 items=[('pt', "Path Tracing", "Full Global Illumination"),
                                    						('drt', "Distributed Ray Tracing", "Direct Lighting Only")],
                                            		 default='pt')

        cls.sample_count = bpy.props.IntProperty(name="Sample Count",
                                                 description="Number of samples per pixels in final frame mode",
                                                 min=1,
                                                 max=1000000,
                                                 default=25,
                                                 subtype='UNSIGNED')

        cls.appleseed_path = bpy.props.StringProperty( name="Appleseed Path",
                                                       description="Path to Appleseed installation folder",
                                                       subtype='DIR_PATH',
                                                       default=get_appleseed_path())

    @classmethod
    def unregister(cls):
        del bpy.types.Scene.cycles


def register():
	bpy.utils.register_class( AppleseedRenderSettings)

def unregister():
	bpy.utils.unregister_class( AppleseedRenderSettings)
