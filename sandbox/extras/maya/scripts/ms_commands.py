# Copyright (c) 2012 Jonathan Topf

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.



import maya.cmds as cmds
import maya.mel
import maya.utils as mu
import os
import sys
import inspect
import subprocess


#****************************************************************************************************************************************************************************************************
# constant vars *************************************************************************************************************************************************************************************
#****************************************************************************************************************************************************************************************************

MAYASEED_VERSION = '0.1.7'
MAYASEED_URL = 'https://github.com/jonathantopf/mayaseed'
APPLESEED_URL = 'http://appleseedhq.net/'
ROOT_DIRECTORY = os.path.split((os.path.dirname(inspect.getfile(inspect.currentframe()))))[0]

#****************************************************************************************************************************************************************************************************
# utilitiy functions & classes **********************************************************************************************************************************************************************
#****************************************************************************************************************************************************************************************************

#
# addMsShadingAttribs function ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#    

def addShadingAttribs():
    shaderName = False
    try:
        shape = cmds.listRelatives(cmds.ls(sl=True)[0], s=True)[0]
        shadingEngine = cmds.listConnections(shape, t='shadingEngine')[0]
        shaderName = cmds.connectionInfo((shadingEngine + ".surfaceShader"),sourceFromDestination=True).split('.')[0] 
    except:
        print '# No objects with shader connectoins selectd'
    if shaderName:
        if not cmds.objExists(shaderName + '.mayaseed_bsdf'):
            cmds.addAttr(shaderName, ln='mayaseed_bsdf',  at='enum', en='Lambertian:Ashikhmin-Shirley:Kelemen:Specular_BRDF:<None>')
            cmds.addAttr(shaderName, ln='mayaseed_edf',  at='enum', en='<None>:Diffuse')
            cmds.addAttr(shaderName, ln='mayaseed_surface_shader', at='enum', en='Physical:Constant:<None>')
        else:
            print '# {0} already has Mayaseed shader attributes'.format(shaderName)

#
# removeMsShadingAttribs function -------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  

def removeShadingAttribs():
    shaderName = ''
    try:
        shape = cmds.listRelatives(cmds.ls(sl=True)[0], s=True)[0]
        shadingEngine = cmds.listConnections(shape, t='shadingEngine')[0]
        shaderName = cmds.connectionInfo((shadingEngine + ".surfaceShader"),sourceFromDestination=True).split('.')[0] 
    except:
        print '# No objects with shader connectoins selectd'
    if shaderName:
        if cmds.objExists(shaderName + '.mayaseed_bsdf'):
            cmds.deleteAttr(shaderName, at='mayaseed_bsdf')
            cmds.deleteAttr(shaderName, at='mayaseed_edf')
            cmds.deleteAttr(shaderName, at='mayaseed_surface_shader')
        else:
            print '# {0} has no Mayaseed shader attributes to remove'.format(shaderName)

#
# show info dialogue -------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 

class msInfoDial():
    def __init__(self):
        if cmds.window('ms_info_window', query=True, exists=True):
            cmds.deleteUI('ms_info_window')
        window = cmds.window('ms_info_window', title='Mayaseed info', sizeable=False)
        cmds.columnLayout(rs=10, columnOffset=['both', 20], width=600)
        cmds.rowLayout(numberOfColumns=2)
        cmds.text('', width=30)
        cmds.image(image=os.path.join(ROOT_DIRECTORY, 'graphics', 'mayaseed_graphic.png'))
        cmds.setParent('..')
        cmds.text('version: ' + MAYASEED_VERSION)
        cmds.text(open(os.path.join(ROOT_DIRECTORY, 'scripts', 'about.txt'),'r').read(), width=500, wordWrap=True, al='left')
        cmds.rowLayout(numberOfColumns=4)
        cmds.button( label='Mayaseed website', command=('import webbrowser\nwebbrowser.open_new_tab("http://www.jonathantopf.com/mayaseed/")'))
        cmds.button( label='appleseed website', command=('import webbrowser\nwebbrowser.open_new_tab("http://appleseedhq.net/")'))
        cmds.text('', width=166)
        cmds.button( label='Close', command=('import maya.cmds as cmds\ncmds.deleteUI(\"' + window + '\", window=True)'), width=100)
        cmds.setParent('..')
        cmds.rowLayout(numberOfColumns=2)
        cmds.text('', width=478)
        cmds.text('jt')
        cmds.setParent('..')
        cmds.text('')
        cmds.showWindow(window)
        
#
# clamp RGB values ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
#

def normalizeRGB(color):
    R = color[0]
    G = color[1]
    B = color[2]
    M = 1

    if R > M:
        M = R
    if G > M:
        M = G
    if B > M:
        M = B

    R = R / M
    G = G / M
    B = B / M

    return (R,G,B,M)

#
# convert shader connection to image ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#

def convertConnectionToImage(shader, attribute, dest_file, resolution=1024):
    if not cmds.objExists(shader+'.'+attribute):
        print 'error converting texture, no object named {0} exists'.format(shader+'.'+attribute)
    else:
        connection = cmds.listConnections(shader+'.'+attribute)
        if not connection:
            print 'nothing connected to {0}'.format(plug_name)
        else:
            cmds.hyperShade(objects=shader)
            connected_object = cmds.ls(sl=True)[0]
            print connected_object
            cmds.convertSolidTx(connection[0] ,connected_object ,fileImageName=dest_file, antiAlias=True, bm=3, fts=True, sp=True, alpha=True, doubleSided=True, resolutionX=resolution, resolutionY=resolution)
            return dest_file
  

#
# convert texture to exr ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#

def convertTexToExr(file_path, dest_dir, overwrite=True):
    if os.path.exists(file_path):
        dest_file = os.path.join(dest_dir, os.path.splitext(os.path.split(file_path)[1])[0] + '.exr')
        if (overwrite == False) and (os.path.exists(dest_file)):
            print '# {0} exists, skipping conversion'.format(dest_file)
        else:
            imf_copy_path = os.path.join(os.path.split(sys.path[0])[0], 'bin', 'imf_copy')
            if not os.path.exists(dest_dir):
                os.mkdir(dest_dir)
            p = subprocess.Popen([imf_copy_path, file_path, dest_file])
            return dest_file
    else:
        print '# error: {0} does not exist'.format(file_path)

#
# check if an object is exportable ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
#

def shapeIsExportable(node_name):
    
    #check the node exists
    if not cmds.objExists(node_name):
        return False
    
    #check if the node has a visibility attribute meaning ita a dag node
    if not cmds.attributeQuery('visibility', node=node_name, exists=True):
        return False
    
    #check visibility flag
    if not cmds.getAttr(node_name+'.visibility'):
        return False

        
    #check to see if its an intermediate mesh

    if cmds.attributeQuery('intermediateObject', node=node_name, exists=True):
        if cmds.getAttr(node_name+'.intermediateObject'):
            return False
        
    #is it in a hidden display layer
    if (cmds.attributeQuery('overrideEnabled', node=node_name, exists=True) and cmds.getAttr(node_name+'.overrideEnabled')):
        if not cmds.getAttr(node_name+'.overrideVisibility'):
            return False
    
    #has it got a parent and is it visible
    if cmds.listRelatives(node_name, parent=True):
        if not shapeIsExportable(cmds.listRelatives(node_name, parent=True)[0]):
            return False

    return True
    
def hasShaderConnected(node_name):

    #check that the shape has a shader connected
    if not cmds.listConnections(node_name, t='shadingEngine'):
        return False

    else:
        shadingEngine = cmds.listConnections(node_name, t='shadingEngine')[0]
        if not cmds.connectionInfo((shadingEngine + '.surfaceShader'),sourceFromDestination=True).split('.')[0]:
            return False
    return True






