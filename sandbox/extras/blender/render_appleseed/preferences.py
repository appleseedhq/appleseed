# ##### BEGIN MIT LICENSE BLOCK #####
#
# Copyright (c) 2013 Matt Ebb
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
#
# ##### END MIT LICENSE BLOCK #####

import bpy

class AppleseedPreferences( bpy.types.PropertyGroup):
    name = bpy.props.StringProperty( name = "", subtype = 'DIR_PATH')

class AppleseedPreferencesPanel( bpy.types.AddonPreferences):
    bl_idname = __package__
    
    appleseed_path = bpy.props.StringProperty(  name = "Appleseed installation dir",
                                                description = "Path to Appleseed installation dir",
                                                subtype = 'DIR_PATH',
                                                default = "")

    def draw(self, context):
        self.layout.prop( self, "appleseed_path")

def register():
    bpy.utils.register_class( AppleseedPreferences)
    bpy.utils.register_class( AppleseedPreferencesPanel)

def unregister():
    bpy.utils.unregister_class( AppleseedPreferences)
    bpy.utils.unregister_class( AppleseedPreferencesPanel)
