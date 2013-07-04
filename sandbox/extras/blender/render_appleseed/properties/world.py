
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

def sun_enumerator( self, context):
    sun = []
    for object in context.scene.objects:
        if object.type == 'LAMP':
            if object.data.type == 'SUN':
                sun.append(( object.name, object.name, ""))
    return sun

class AppleseedWorldSettings( bpy.types.PropertyGroup):
    @classmethod
    def register( cls):
        bpy.types.World.appleseed = bpy.props.PointerProperty( name = "Appleseed World Settings",
                                                               description = "appleseed world settings",
                                                               type = cls)

        cls.env_type = bpy.props.EnumProperty( name = "Environment", 
                                                description = "Environment", 
                                                items = [( 'none', "None", ''),
                                                         ( 'image', "Image", ''),
                                                         ( 'sky', "Sky", '')], 
                                                default = 'none')
        # ibl

        # sky
        cls.sun_theta = bpy.props.FloatProperty( name = "Sun theta angle", description = '', default = 0.0, min = -180, max = 180)
        cls.sun_phi = bpy.props.FloatProperty( name = "Sun phi angle", description = '', default = 0.0, min = -180, max = 180)        
        cls.sun_lamp = bpy.props.EnumProperty( items = sun_enumerator, name = "Sun Lamp", description = "Sun lamp to export")                                    
        
        cls.sun_model = bpy.props.EnumProperty( name = "Physical sky model", 
                                                description = "Physical sky model for environment EDF", 
                                                items = [( 'hosek_environment_edf', "Hosek-Wilkie", ''),
                                                         ( 'preetham_environment_edf', "Preetham", '')], 
                                                default = 'hosek_environment_edf')
        
        cls.horiz_shift = bpy.props.FloatProperty( name = "Horizon shift", description = '', default = 0.0, min = -2.0, max = 2.0)
        
        cls.luminance_multiplier = bpy.props.FloatProperty( name = "Sky luminance multiplier", description ='', default = 1.0, min = 0.0, max = 20.0)
        
        cls.radiance_multiplier = bpy.props.FloatProperty( name = "Sun radiance multiplier", description = '', default = 0.05, min = 0.0, max = 1.0)
        
        cls.saturation_multiplier = bpy.props.FloatProperty( name= "Saturation multiplier", description = '', default = 1.0, min = 0.0, max = 10.0)
        
        cls.turbidity = bpy.props.FloatProperty( name = "Turbidity", description = '', default = 4.0, min = 0.0, max = 10.0)
        cls.turbidity_max = bpy.props.FloatProperty( name = "Turbidity max", description = '', default = 6.0, min = 0, max = 10.0)
        cls.turbidity_min = bpy.props.FloatProperty( name = "Turbidity min", description = '', default = 2.0, min = 0, max = 10.0)
        cls.ground_albedo = bpy.props.FloatProperty( name = "Ground albedo", description = '', default = 0.3, min = 0.0, max = 1.0)

    @classmethod
    def unregister(cls):
        del bpy.types.World.appleseed

def register():
    pass

def unregister():
    pass
