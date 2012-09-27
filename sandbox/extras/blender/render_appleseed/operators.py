
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

import os

import bpy
from bpy.types import Operator
from bpy.props import StringProperty, BoolProperty, EnumProperty
from bpy_extras.io_utils import ExportHelper

from .project_writer import ProjectWriter

class ExportAppleseedProject( Operator, ExportHelper):
    bl_idname = "appleseed.export"
    bl_label = "Export Appleseed Project"

    filename_ext = ".appleseed"
    filter_glob = StringProperty( default="*.appleseed", options={'HIDDEN'} )

    use_anim = BoolProperty( name="Animation", description = "Animation", default = False)

    def execute( self, context):
        proj_path = os.path.dirname( self.filepath)
        filename = os.path.basename( self.filepath)

        if filename.endswith( ".appleseed"):
            filename = filename[0 : -len( ".appleseed")]

        if self.use_anim:
            for frame in range( context.scene.frame_start, context.scene.frame_end + 1):
                writer = ProjectWriter( context.scene, proj_path, filename + "." + str( frame), True)
                writer.write()
        else:
            writer = ProjectWriter( context.scene, proj_path, filename)
            writer.write()

        return { 'FINISHED'}

def menu_func_export_appleseed( self, context):
    self.layout.operator( ExportAppleseedProject.bl_idname, text = "Appleseed project (.appleseed)")

def register():
    bpy.utils.register_class( ExportAppleseedProject)
    bpy.types.INFO_MT_file_export.append( menu_func_export_appleseed)

def unregister():
    bpy.utils.unregister_class( ExportAppleseedProject)
    bpy.types.INFO_MT_file_export.remove( menu_func_export_appleseed)
