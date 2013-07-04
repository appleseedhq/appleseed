
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

class AppleseedWorldPanel( bpy.types.Panel):
    bl_label = "Environment"
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    COMPAT_ENGINES = {'APPLESEED'}
    bl_context = "world"
    
    @classmethod
    def poll( cls, context):
        renderer = context.scene.render
        return renderer.engine == 'APPLESEED_RENDER'
        
    def draw( self, context):
        layout = self.layout
        asr_props = context.world.appleseed

        layout.prop( asr_props, "env_type")

        if asr_props.env_type == 'none':
            return
        elif asr_props.env_type == 'image':
            return
        else:
            split = layout.split()
            col = split.column()
            col.label( "Sun to export:")
            col.prop( asr_props, "sun_lamp", text = '')

            col = split.column()
            col.label( "Physical sky model:")
            col.prop(asr_props, "sun_model", text = '')
            
            layout.prop( asr_props, "luminance_multiplier")
            layout.prop( asr_props, "radiance_multiplier")
            layout.prop( asr_props, "saturation_multiplier")
            row = layout.row( align = True)
            row.prop( asr_props, "sun_theta")
            row.prop( asr_props, "sun_phi")
            layout.prop( asr_props, "turbidity")
            row = layout.row( align = True)
            row.prop( asr_props, "turbidity_min")
            row.prop( asr_props, "turbidity_max")
            layout.prop( asr_props, "horiz_shift")
            
            if asr_props.sun_model == "hosek_environment_edf":
                layout.prop( asr_props, "ground_albedo")        

def register():
    bpy.types.WORLD_PT_context_world.COMPAT_ENGINES.add( 'APPLESEED_RENDER')
    bpy.types.WORLD_PT_custom_props.COMPAT_ENGINES.add( 'APPLESEED_RENDER')

def unregister():
    bpy.types.WORLD_PT_context_world.COMPAT_ENGINES.remove( 'APPLESEED_RENDER')
    bpy.types.WORLD_PT_custom_props.COMPAT_ENGINES.remove( 'APPLESEED_RENDER')
