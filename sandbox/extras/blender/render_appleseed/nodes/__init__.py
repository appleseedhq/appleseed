
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

# Shortcut for node type menu
def add_nodetype( layout, type):
    layout.operator( "node.add_node", text = type.bl_label).type = type.bl_rna.identifier

class AppleseedMaterialTree( bpy.types.NodeTree):
    '''Material'''
    bl_idname = 'AppleseedMaterialTree'
    bl_label = 'Appleseed Material Tree'
    bl_icon = 'MATERIAL_DATA'

    def draw_add_menu( self, context, layout):
        add_nodetype( layout, bpy.types.AppleseedMaterialNodeType)


class AppleseedMaterialSocket( bpy.types.NodeSocket):
    '''Material Input Socket'''
    bl_idname = 'AppleseedMaterialSocketType'
    bl_label = 'Appleseed Material Input Socket'

    def draw( self, context, layout, node):
    	layout.label( self.name)

    # Socket color
    def draw_color( self, context, node):
        return( 1.0, 0.4, 0.216, 0.5)

class AppleseedMaterialNode( bpy.types.Node):
    '''Material'''
    bl_idname = 'AppleseedMaterialNodeType'
    bl_label = 'Material'
    bl_icon = 'MATERIAL'

    @classmethod
    def poll( cls, ntree):
        return ntree.bl_idname == 'AppleseedMaterialTree'

    def init(self, context):
        self.inputs.new( 'AppleseedMaterialSocketType', "Shader")
        self.inputs.new( 'AppleseedMaterialSocketType', "Emission")
        self.inputs.new( 'AppleseedMaterialSocketType', "Alpha map")
        self.inputs.new( 'AppleseedMaterialSocketType', "Normal map")

    def copy( self, node):
        pass

    def free( self):
        pass

    def draw_buttons( self, context, layout):
    	pass

    def draw_buttons_ext( self, context, layout):
    	pass

class AppleseedEmissionNode( bpy.types.Node):
    ''' Emission'''
    bl_idname = 'AppleseedEmissionNodeType'
    bl_label = 'Emission'
    bl_icon = 'LAMP_SUN'

    @classmethod
    def poll( cls, ntree):
        return ntree.bl_idname == 'AppleseedMaterialTree'

    def init(self, context):
        self.outputs.new( 'AppleseedMaterialSocketType', "Emission")

    def copy( self, node):
        pass

    def free( self):
        pass

    def draw_buttons( self, context, layout):
    	pass

    def draw_buttons_ext( self, context, layout):
    	pass

class AppleseedLambertNode( bpy.types.Node):
    '''Diffuse'''
    bl_idname = 'AppleseedLambertNodeType'
    bl_label = 'Diffuse'
    bl_icon = 'MATERIAL'

    @classmethod
    def poll( cls, ntree):
        return ntree.bl_idname == 'AppleseedMaterialTree'

    def init(self, context):
        self.outputs.new( 'AppleseedMaterialSocketType', "Shader")

    def copy( self, node):
        pass

    def free( self):
        pass

    def draw_buttons( self, context, layout):
    	pass

    def draw_buttons_ext( self, context, layout):
    	pass

class AppleseedSpecularNode( bpy.types.Node):
    '''Specular'''
    bl_idname = 'AppleseedSpecularNodeType'
    bl_label = 'Specular'
    bl_icon = 'MATERIAL'

    @classmethod
    def poll( cls, ntree):
        return ntree.bl_idname == 'AppleseedMaterialTree'

    def init(self, context):
        self.outputs.new( 'AppleseedMaterialSocketType', "Shader")

    def copy( self, node):
        pass

    def free( self):
        pass

    def draw_buttons( self, context, layout):
    	pass

    def draw_buttons_ext( self, context, layout):
    	pass

class AppleseedMixShadersNode( bpy.types.Node):
    '''Mix'''
    bl_idname = 'AppleseedMixNodeType'
    bl_label = 'Mix'
    bl_icon = 'MATERIAL'

    @classmethod
    def poll( cls, ntree):
        return ntree.bl_idname == 'AppleseedMaterialTree'

    def init(self, context):
        self.inputs.new( 'AppleseedMaterialSocketType', "Shader")
        self.inputs.new( 'AppleseedMaterialSocketType', "Shader")
        self.inputs.new( 'NodeSocketFloat', "Mix")
        self.outputs.new( 'AppleseedMaterialSocketType', "Shader")

    def copy( self, node):
        pass

    def free( self):
        pass

    def draw_buttons( self, context, layout):
    	pass

    def draw_buttons_ext( self, context, layout):
    	pass

def register():
    bpy.utils.register_class( AppleseedMaterialTree)
    bpy.utils.register_class( AppleseedMaterialSocket)
    bpy.utils.register_class( AppleseedMaterialNode)
    bpy.utils.register_class( AppleseedEmissionNode)


def unregister():
    bpy.utils.unregister_class( AppleseedMaterialTree)
    bpy.utils.unregister_class( AppleseedMaterialSocket)
    bpy.utils.unregister_class( AppleseedMaterialNode)
    bpy.utils.unregister_class( AppleseedEmissionNode)

