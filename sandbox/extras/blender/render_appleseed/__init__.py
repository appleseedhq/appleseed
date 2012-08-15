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

bl_info = {
    "name": "Appleseed",
    "author": "Est.",
    "version": (0, 1, 0),
    "blender": (2, 6, 3),
    "location": "Info Header (engine dropdown)",
    "description": "Appleseed integration",
    "warning": "",
    "wiki_url": "",
    "tracker_url": "",
    "category": "Render"}

import appleseed

if "bpy" in locals():
    import imp
    imp.reload( properties)
    imp.reload( ui)
    imp.reload( exporter)
else:
    import bpy
    from . import properties
    from . import ui
    from . import exporter

class RenderAppleseed( bpy.types.RenderEngine):
    bl_idname = 'APPLESEED_RENDER'
    bl_label = "Appleseed"
    bl_use_preview = True

    def __init__( self):
        exporter.init( self)

    def __del__( self):
        exporter.free( self)

    # final
    def update( self, data, scene):
        print( "RenderEngine.update called.")
        exporter.update( self, data, scene)

    def render( self, scene):
        print( "RenderEngine.render called.")
        exporter.render( self, scene)

    # preview
    def preview_update( self, context, id):
        print( "RenderEngine.preview_update called.")
        pass

    def preview_render( self):
        print( "RenderEngine.preview_render called.")
        pass

    # viewport
    def view_update( self, context):
        print( "RenderEngine.view_update called.")

    def view_draw( self, context):
        print( "RenderEngine.view_draw called.")

def register():
    properties.register()
    exporter.register()
    bpy.utils.register_module(__name__)

def unregister():
    properties.unregister()
    ui.unregister()
    exporter.unregister()
    bpy.utils.unregister_module(__name__)

if __name__ == "__main__":
    register()
