#
# This source file is part of appleseed.
# Visit https://appleseedhq.net/ for additional information and resources.
#
# This software is released under the MIT license.
#
# Copyright (c) 2017-2018 Francois Beaune, The appleseedhq Organization
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

from appleseed.studio import *


def register():
    # In the future, this plugin will be able to add menus or panels to appleseed.studio.
    pass


def list_objects():
    """Print names of all objects in the scene."""

    scene = current_project().get_scene()
    assemblies = scene.assemblies()

    for ass_key in assemblies:
        list_objects_in_assembly(assemblies[ass_key])


def list_objects_in_assembly(ass):
    """Print names of objects in a given assembly and all its child assemblies."""

    # Print names of objects inside this assembly.
    for obj in ass.objects():
        print((obj.get_name()))

    # Recurse into child assemblies.
    child_assemblies = ass.assemblies()
    for sub_ass_key in child_assemblies:
        list_objects_in_assembly(child_assemblies[sub_ass_key])

