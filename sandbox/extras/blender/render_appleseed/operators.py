
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
import os

class AppleseedRenderFrame( bpy.types.Operator):
    """Render active scene"""
    bl_idname = "appleseed.render_frame"
    bl_label = "Appleseed Render Frame"

    @classmethod
    def poll( cls, context):
        renderer = context.scene.render
        return renderer.engine == 'APPLESEED_RENDER'

    def execute( self, context):
        scene = context.scene

        if scene.appleseed.display_mode != 'AS_STUDIO':
            scene.render.display_mode = scene.appleseed.display_mode
            bpy.ops.render.render()
        else:
            # launch appleseed studio here
            pass

        # get the scene from the context, 
        # copy appleseed.display_mode to render.display_mode
        # launch render
        return {'FINISHED'}


def register():
    bpy.utils.register_class( AppleseedRenderFrame)

def unregister():
    bpy.utils.unregister_class( AppleseedRenderFrame)
