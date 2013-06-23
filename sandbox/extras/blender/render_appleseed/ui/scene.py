
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

def register():
    bpy.types.SCENE_PT_scene.COMPAT_ENGINES.add( 'APPLESEED_RENDER')
    bpy.types.SCENE_PT_color_management.COMPAT_ENGINES.add( 'APPLESEED_RENDER')
    bpy.types.SCENE_PT_custom_props.COMPAT_ENGINES.add( 'APPLESEED_RENDER')
    bpy.types.SCENE_PT_audio.COMPAT_ENGINES.add( 'APPLESEED_RENDER')
    bpy.types.SCENE_PT_unit.COMPAT_ENGINES.add( 'APPLESEED_RENDER')
    bpy.types.SCENE_PT_keying_sets.COMPAT_ENGINES.add( 'APPLESEED_RENDER')
    bpy.types.SCENE_PT_keying_set_paths.COMPAT_ENGINES.add( 'APPLESEED_RENDER')
    bpy.types.SCENE_PT_physics.COMPAT_ENGINES.add( 'APPLESEED_RENDER')
    bpy.types.SCENE_PT_rigid_body_world.COMPAT_ENGINES.add( 'APPLESEED_RENDER')
    bpy.types.SCENE_PT_rigid_body_cache.COMPAT_ENGINES.add( 'APPLESEED_RENDER')
    bpy.types.SCENE_PT_rigid_body_field_weights.COMPAT_ENGINES.add( 'APPLESEED_RENDER')
    bpy.types.SCENE_PT_custom_props.COMPAT_ENGINES.add( 'APPLESEED_RENDER')
    
def unregister():
    bpy.types.SCENE_PT_scene.COMPAT_ENGINES.remove( 'APPLESEED_RENDER')
    bpy.types.SCENE_PT_color_management.COMPAT_ENGINES.remove( 'APPLESEED_RENDER')
    bpy.types.SCENE_PT_custom_props.COMPAT_ENGINES.remove( 'APPLESEED_RENDER')
    bpy.types.SCENE_PT_audio.COMPAT_ENGINES.remove( 'APPLESEED_RENDER')
    bpy.types.SCENE_PT_unit.COMPAT_ENGINES.remove( 'APPLESEED_RENDER')
    bpy.types.SCENE_PT_keying_sets.COMPAT_ENGINES.remove( 'APPLESEED_RENDER')
    bpy.types.SCENE_PT_keying_set_paths.COMPAT_ENGINES.remove( 'APPLESEED_RENDER')
    bpy.types.SCENE_PT_physics.COMPAT_ENGINES.remove( 'APPLESEED_RENDER')
    bpy.types.SCENE_PT_rigid_body_world.COMPAT_ENGINES.remove( 'APPLESEED_RENDER')
    bpy.types.SCENE_PT_rigid_body_cache.COMPAT_ENGINES.remove( 'APPLESEED_RENDER')
    bpy.types.SCENE_PT_rigid_body_field_weights.COMPAT_ENGINES.remove( 'APPLESEED_RENDER')
    bpy.types.SCENE_PT_custom_props.COMPAT_ENGINES.remove( 'APPLESEED_RENDER')
