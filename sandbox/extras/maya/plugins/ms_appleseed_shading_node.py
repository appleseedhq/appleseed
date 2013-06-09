
#
# Copyright (c) 2012-2013 Jonathan Topf
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


import sys
import maya.OpenMaya as OpenMaya
import maya.OpenMayaMPx as OpenMayaMPx
import maya.cmds as cmds

# Plug-in information:
kPluginNodeName = 'ms_appleseed_shading_node'
kPluginNodeClassify = 'utility/color'
kPluginNodeId = OpenMaya.MTypeId(0x00335)

#--------------------------------------------------------------------------------------------------
# ms_appleseed_shading_node.
#--------------------------------------------------------------------------------------------------

class appleseed_shading_node(OpenMayaMPx.MPxNode):
    outColorAttribute = OpenMaya.MObject()
    inColorAttribute = OpenMaya.MObject()

    def __init__(self):
        OpenMayaMPx.MPxNode.__init__(self)
 

    def compute(self, pPlug, pDataBlock):
        if ( pPlug == appleseed_shading_node.outColorAttribute ):

            inColorDataHandle = pDataBlock.inputValue( appleseed_material.inColorAttribute )

            inColorValue = inColorDataHandle.asFloatVector()

            outColor = OpenMaya.MFloatVector(0, 0, 0)

            outColor.x = inColorValue.x
            outColor.y = inColorValue.y
            outColor.z = inColorValue.z

            outColorDataHandle = pDataBlock.outputValue( appleseed_shading_node.outColorAttribute )
            outColorDataHandle.setMFloatVector( outColor )
            outColorDataHandle.setClean()

        else:
            return OpenMaya.kUnknownParameter


def nodeCreator():
    return OpenMayaMPx.asMPxPtr( appleseed_shading_node() )

def nodeInitializer():
    # Create a numeric attribute function set, since our attributes will all be defined by numeric types.
    numericAttributeFn = OpenMaya.MFnNumericAttribute()

    # input attributes
    appleseed_shading_node.inColorAttribute = numericAttributeFn.createColor( 'incolor', 'ic' )
    numericAttributeFn.setStorable( True )
    numericAttributeFn.setHidden( True )
    numericAttributeFn.setDefault( 0.5,0.5,0.5 )
    appleseed_shading_node.addAttribute( appleseed_shading_node.inColorAttribute )

    # output attributes  
    appleseed_shading_node.outColorAttribute = numericAttributeFn.createColor( 'outColor', 'oc')
    numericAttributeFn.setStorable( False )
    numericAttributeFn.setWritable( False )
    numericAttributeFn.setReadable( True )
    numericAttributeFn.setHidden( False )
    appleseed_shading_node.addAttribute( appleseed_shading_node.outColorAttribute )


def initializePlugin( mobject ):
    mplugin = OpenMayaMPx.MFnPlugin( mobject )
    try:
        mplugin.registerNode( kPluginNodeName, kPluginNodeId, nodeCreator, nodeInitializer, OpenMayaMPx.MPxNode.kDependNode, kPluginNodeClassify )
    except:
        sys.stderr.write( "Failed to register node: " + kPluginNodeName )
        raise

def uninitializePlugin( mobject ):
    mplugin = OpenMayaMPx.MFnPlugin( mobject )
    try:
        mplugin.deregisterNode( kPluginNodeId )
    except:
        sys.stderr.write( "Failed to deregister node: " + kPluginNodeName )
        raise

