
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
from bpy.types import Panel, Menu

# Use some of the existing buttons.
from bl_ui import properties_render
properties_render.RENDER_PT_render.COMPAT_ENGINES.add( 'APPLESEED')
properties_render.RENDER_PT_dimensions.COMPAT_ENGINES.add( 'APPLESEED')
properties_render.RENDER_PT_output.COMPAT_ENGINES.add( 'APPLESEED')
properties_render.RENDER_PT_post_processing.COMPAT_ENGINES.add( 'APPLESEED')
properties_render.RENDER_PT_stamp.COMPAT_ENGINES.add( 'APPLESEED')
del properties_render

import bl_ui.properties_scene as properties_scene
properties_scene.SCENE_PT_scene.COMPAT_ENGINES.add( 'APPLESEED')
properties_scene.SCENE_PT_audio.COMPAT_ENGINES.add( 'APPLESEED')
properties_scene.SCENE_PT_physics.COMPAT_ENGINES.add( 'APPLESEED')
properties_scene.SCENE_PT_keying_sets.COMPAT_ENGINES.add( 'APPLESEED')
properties_scene.SCENE_PT_keying_set_paths.COMPAT_ENGINES.add( 'APPLESEED')
properties_scene.SCENE_PT_unit.COMPAT_ENGINES.add( 'APPLESEED')
properties_scene.SCENE_PT_custom_props.COMPAT_ENGINES.add( 'APPLESEED')
del properties_scene

import bl_ui.properties_world as properties_world
properties_world.WORLD_PT_context_world.COMPAT_ENGINES.add( 'APPLESEED')
del properties_world

import bl_ui.properties_material as properties_material
properties_material.MATERIAL_PT_context_material.COMPAT_ENGINES.add( 'APPLESEED')
properties_material.MATERIAL_PT_preview.COMPAT_ENGINES.add( 'APPLESEED')
properties_material.MATERIAL_PT_custom_props.COMPAT_ENGINES.add( 'APPLESEED')
del properties_material

import bl_ui.properties_texture as properties_texture
properties_texture.TEXTURE_PT_preview.COMPAT_ENGINES.add( 'APPLESEED')
del properties_texture

import bl_ui.properties_data_lamp as properties_data_lamp
properties_data_lamp.DATA_PT_context_lamp.COMPAT_ENGINES.add( 'APPLESEED')
properties_data_lamp.DATA_PT_spot.COMPAT_ENGINES.add( 'APPLESEED')
del properties_data_lamp

# enable all existing panels for these contexts
import bl_ui.properties_data_mesh as properties_data_mesh
for member in dir( properties_data_mesh):
    subclass = getattr( properties_data_mesh, member)
    try: subclass.COMPAT_ENGINES.add( 'APPLESEED')
    except: pass
del properties_data_mesh

import bl_ui.properties_object as properties_object
for member in dir( properties_object):
    subclass = getattr( properties_object, member)
    try: subclass.COMPAT_ENGINES.add( 'APPLESEED')
    except: pass
del properties_object

import bl_ui.properties_data_mesh as properties_data_mesh
for member in dir( properties_data_mesh):
    subclass = getattr( properties_data_mesh, member)
    try: subclass.COMPAT_ENGINES.add( 'APPLESEED')
    except: pass
del properties_data_mesh

import bl_ui.properties_data_camera as properties_data_camera
for member in dir( properties_data_camera):
    subclass = getattr( properties_data_camera, member)
    try: subclass.COMPAT_ENGINES.add( 'APPLESEED')
    except: pass
del properties_data_camera

import bl_ui.properties_particle as properties_particle
for member in dir( properties_particle):
    if member == 'PARTICLE_PT_render': continue

    subclass = getattr( properties_particle, member)
    try: subclass.COMPAT_ENGINES.add( 'APPLESEED')
    except:  pass
del properties_particle

######################################################################

class appleseed_render_panel_base( object):
    bl_context = "render"
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"

    @classmethod
    def poll( cls, context):
        renderer = context.scene.render
        return renderer.engine == 'APPLESEED'

class appleseed_lighting_panel( Panel, appleseed_render_panel_base):
    bl_label = "Lighting"
    COMPAT_ENGINES = {'APPLESEED'}
    bl_options = {'DEFAULT_CLOSED'}

    def draw( self, context):
        layout = self.layout
        scene = context.scene
        ascene = scene.appleseed

        col = layout.column()
        col.prop( ascene, "lighting_engine")
        sub = col.row( align=True)
        sub.prop( ascene, "sample_count")

class appleseed_search_paths_panel( Panel, appleseed_render_panel_base):
    bl_label = "Search Paths"
    bl_options = {'DEFAULT_CLOSED'}

    def draw( self, context):
        layout = self.layout
        scene = context.scene
        ascene = scene.appleseed
        layout.prop( ascene, "appleseed_path")

def register():
    pass

def unregister():
    pass
