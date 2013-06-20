
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

class AppleseedRenderButtons( bpy.types.Panel):
    bl_context = "render"
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_label = "Render"
    COMPAT_ENGINES = {'APPLESEED'}

    @classmethod
    def poll( cls, context):
        renderer = context.scene.render
        return renderer.engine == 'APPLESEED_RENDER'
    
    def draw( self, context):
        scene = context.scene
        layout = self.layout
        layout.prop( scene.appleseed, "appleseed_dir", text = "Appleseed")

        row = layout.row( align=True)
        row.operator( "appleseed.render_frame", text="Render", icon='RENDER_STILL')
        row.operator( "appleseed.render_anim", text="Animation", icon='RENDER_ANIMATION')

        layout.prop( scene.appleseed, "display_mode", text = "Display")

def register():
    bpy.types.RENDER_PT_dimensions.COMPAT_ENGINES.add( 'APPLESEED_RENDER')
    bpy.types.RENDER_PT_output.COMPAT_ENGINES.add( 'APPLESEED_RENDER')

def unregister():
    bpy.types.RENDER_PT_dimensions.COMPAT_ENGINES.remove( 'APPLESEED_RENDER')
    bpy.types.RENDER_PT_shading.COMPAT_ENGINES.remove( 'APPLESEED_RENDER')
