
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

bl_info = {
    "name": "Appleseed",
    "author": "Franz Beaune, Joel Daniels, Esteban Tovagliari",
    "version": (0, 0, 1),
    "blender": (2, 6, 7),
    "location": "Render > Engine > Appleseed",
    "description": "Appleseed integration for blender",
    "warning": "",
    "wiki_url": "",
    "tracker_url": "",
    "category": "Render"}

import bpy
from . import preferences
from . import properties
from . import nodes
from . import operators
from . import export
from . import ui
from . import render

def register():
    preferences.register()
    properties.register()
    nodes.register()
    operators.register()
    export.register()
    ui.register()
    bpy.utils.register_module( __name__)

def unregister():
    preferences.unregister()
    properties.unregister()
    nodes.unregister()
    operators.register()
    export.unregister()
    ui.unregister()
    bpy.utils.unregister_module( __name__)

if __name__ == "__main__":
    register()
