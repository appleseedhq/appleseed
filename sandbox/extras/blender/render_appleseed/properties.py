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

class AppleseedSceneSettings(bpy.types.PropertyGroup):
	lighting_engine = bpy.props.EnumProperty(name="Lighting Engine",
											 description="Select the lighting engine to use",
											 items=[('pt', "Path Tracing", "Full Global Illumination"),
													('drt', "Distributed Ray Tracing", "Direct Lighting Only")],
											 default='pt')

	sample_count = bpy.props.IntProperty(name="Sample Count",
										 description="Number of samples per pixels in final frame mode",
										 min=1,
										 max=4096,
										 default=32,
										 subtype='UNSIGNED')

class AppleseedWorldSettings(bpy.types.PropertyGroup):
	pass

class AppleseedObjectSettings(bpy.types.PropertyGroup):
	pass

class AppleseedLampSettings(bpy.types.PropertyGroup):
	pass

class AppleseedMaterialSettings(bpy.types.PropertyGroup):
	pass

class AppleseedTextureSettings(bpy.types.PropertyGroup):
	pass

def register():
	bpy.utils.register_class( AppleseedSceneSettings)
	bpy.types.Scene.appleseed = PointerProperty( type=AppleseedSceneSettings, name="Appleseed Scene Settings")

	bpy.utils.register_class( AppleseedWorldSettings)
	bpy.types.World.appleseed = PointerProperty( type=AppleseedWorldSettings, name="Appleseed World Settings")

	bpy.utils.register_class( AppleseedObjectSettings)
	bpy.types.Object.appleseed = PointerProperty( type=AppleseedObjectSettings, name="Appleseed Object Settings")

	bpy.utils.register_class( AppleseedLampSettings)
	bpy.types.Lamp.appleseed = PointerProperty( type=AppleseedLampSettings, name="Appleseed Lamp Settings")

	bpy.utils.register_class( AppleseedMaterialSettings)
	bpy.types.Material.appleseed = PointerProperty( type=AppleseedMaterialSettings, name="Appleseed Material Settings")

	bpy.utils.register_class( AppleseedTextureSettings)
	bpy.types.Texture.appleseed = PointerProperty( type=AppleseedTextureSettings, name="Appleseed Texture Settings")

def unregister():
	bpy.utils.unregister_module( __name__)
