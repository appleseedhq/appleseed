
#
# Copyright (c) 2012 Jonathan Topf
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


import maya.cmds as cmds
import maya.mel as mel
import maya.utils as mu
import os
import sys
import inspect
import subprocess
from xml.dom.minidom import parseString
import ms_export_obj
import random


#--------------------------------------------------------------------------------------------------
# Constants.
#--------------------------------------------------------------------------------------------------

MAYASEED_VERSION = '0.1.8'
MAYASEED_URL = 'https://github.com/jonathantopf/mayaseed'
APPLESEED_URL = 'http://appleseedhq.net/'
ROOT_DIRECTORY = os.path.split((os.path.dirname(inspect.getfile(inspect.currentframe()))))[0]


#--------------------------------------------------------------------------------------------------
# Show About dialog.
#--------------------------------------------------------------------------------------------------

class msInfoDial():
    def __init__(self):
        if cmds.window('ms_info_window', query=True, exists=True):
            cmds.deleteUI('ms_info_window')
        window = cmds.window('ms_info_window', title='Mayaseed info', sizeable=False)
        cmds.columnLayout(rs=10, columnOffset=['both', 20], width=600)
        cmds.rowLayout(numberOfColumns=2)
        cmds.text('', width=30)
        cmds.image(image=os.path.join(ROOT_DIRECTORY, 'graphics', 'mayaseed.png'))
        cmds.setParent('..')
        cmds.text('Version: ' + MAYASEED_VERSION)
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


#--------------------------------------------------------------------------------------------------
# Normalize an RGB color.
#--------------------------------------------------------------------------------------------------

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

    return (R, G, B, M)


#--------------------------------------------------------------------------------------------------
# Convert shader connection to image.
#--------------------------------------------------------------------------------------------------

def convertConnectionToImage(shader, attribute, dest_file, resolution=1024, pass_through=False):
    
    dest_dir = os.path.split(dest_file)[0]
    if not os.path.exists(dest_dir):
        os.makedirs(dest_dir)

    if not cmds.objExists(shader+'.'+attribute):
        print 'error converting texture, no object named {0} exists'.format(shader+'.'+attribute)
    else:
        connection = cmds.listConnections(shader+'.'+attribute)
        if not connection:
            print 'nothing connected to {0}, skipping conversion'.format(plug_name)
        elif pass_through == True:
            print '{0}: skipping conversion'.format(plug_name)
        else:
            cmds.hyperShade(objects=shader)
            connected_object = cmds.ls(sl=True)[0]
            print connected_object
            cmds.convertSolidTx(connection[0] ,connected_object ,fileImageName=dest_file, antiAlias=True, bm=3, fts=True, sp=True, alpha=True, doubleSided=True, resolutionX=resolution, resolutionY=resolution)
        
        return dest_file


#--------------------------------------------------------------------------------------------------
# Convert textures to OpenEXR format.
#--------------------------------------------------------------------------------------------------

def findPathToImfCopy():

    #
    # Values of maya_base_path:
    #
    #                   Maya 2012                           Maya 2013
    #   --------------------------------------------------------------------------
    #   Mac OS X        maya2012/Maya.app/Contents          maya2013/Maya.app/Contents
    #   Windows         Maya2012                            Maya2013
    #   Linux           ?                                   ?
    #
    # Locations of imf_copy:
    #
    #                   Maya 2012                           Maya 2013
    #   --------------------------------------------------------------------------
    #   Mac OS X        maya2012/Maya.app/Contents/bin      maya2013/mentalray/bin
    #   Windows         Maya2012\bin                        Maya2013\mentalray\bin
    #   Linux           ?                                   ?
    #

    maya_base_path = os.path.split(sys.path[0])[0]
    imf_copy_path = None

    if mel.eval('getApplicationVersionAsFloat()') >= 2013.0:
        if sys.platform == 'darwin':
            imf_copy_path = os.path.join(maya_base_path, '..', '..', 'mentalray', 'bin')
        elif sys.platform == 'win32':
            imf_copy_path = os.path.join(maya_base_path, 'mentalray', 'bin')
    else:
        imf_copy_path = os.path.join(maya_base_path, 'bin')

    return None if imf_copy_path is None else os.path.join(imf_copy_path, 'imf_copy')

def convertTexToExr(file_path, dest_dir, overwrite=True, pass_through=False):
    dest_file = os.path.join(dest_dir, os.path.splitext(os.path.split(file_path)[1])[0] + '.exr')

    if not os.path.exists(file_path):
        print "# error: {0} does not exist".format(file_path)
        return dest_file

    if pass_through:
        print "# skipping conversion of {0}".format(file_path)
        return dest_file

    if os.path.exists(dest_file) and not overwrite:
        print "# {0} already exists, skipping conversion".format(dest_file)
        return dest_file

    imf_copy_path = findPathToImfCopy()

    if imf_copy_path is None:
        print "# error: cannot convert {0}, imf_copy utility not found".format(file_path)
        return dest_file

    if not os.path.exists(dest_dir):
        os.mkdir(dest_dir)

    # -r: make a tiled OpenEXR file
    # -t: set the tile dimensions
    args = [imf_copy_path, "-r", "-t 32", file_path, dest_file]


    if sys.platform == 'win32':
        # http://stackoverflow.com/questions/2935704/running-shell-commands-without-a-shell-window
        p = subprocess.Popen(args, creationflags=0x08000000)
        p.wait()
    elif sys.platform == 'darwin':
        p = subprocess.Popen(args)
        p.wait()

    return dest_file


#--------------------------------------------------------------------------------------------------
# Check if an object is exportable.
#--------------------------------------------------------------------------------------------------

def shapeIsExportable(node_name):
    # check if the node exists
    if not cmds.objExists(node_name):
        return False

    # check if the node has a visibility attribute meaning it's a DAG node
    if not cmds.attributeQuery('visibility', node=node_name, exists=True):
        return False

    # check visibility flag
    if not cmds.getAttr(node_name + '.visibility'):
        return False

    # check to see if it's an intermediate mesh
    if cmds.attributeQuery('intermediateObject', node=node_name, exists=True):
        if cmds.getAttr(node_name + '.intermediateObject'):
            return False

    # check if it is a hidden display layer
    if cmds.attributeQuery('overrideEnabled', node=node_name, exists=True) and cmds.getAttr(node_name + '.overrideEnabled'):
        if not cmds.getAttr(node_name + '.overrideVisibility'):
            return False

    # has it got a parent and is it visible?
    if cmds.listRelatives(node_name, parent=True):
        if not shapeIsExportable(cmds.listRelatives(node_name, parent=True)[0]):
            return False

    return True


#--------------------------------------------------------------------------------------------------
# Check if an object has a shader connected.
#--------------------------------------------------------------------------------------------------

def hasShaderConnected(node_name):
    # check that the shape has a shader connected
    if not cmds.listConnections(node_name, t='shadingEngine'):
        return False

    shadingEngine = cmds.listConnections(node_name, t='shadingEngine')[0]
    if not cmds.connectionInfo(shadingEngine + '.surfaceShader', sourceFromDestination=True).split('.')[0]:
        return False

    return True


#--------------------------------------------------------------------------------------------------
# read entity defs xml file and return dict 
#--------------------------------------------------------------------------------------------------


def getEntityDefs(xml_file_path, list=False):
    nodes = dict()

    class Node():
        def __init__(self, name, type):
            self.name = name
            self.type = type
            self.attributes = dict()
            
    class Attribute():
        def __init__(self, name):
            self.name = name
            self.label = name
            self.type = 'entity'
            self.default_value = ''
            self.entity_types = []

    file = open(xml_file_path,'r')
    data = file.read()
    file.close()

    dom = parseString(data)

    for entity in dom.getElementsByTagName('entity'):
        
        #create new dict entry to store the node info
        nodes[entity.getAttribute('model')] = Node(entity.getAttribute('model'), entity.getAttribute('type'))

        for child in entity.childNodes:
            if child.nodeName =='parameters':
                
                #add an attribute and give it a name
                nodes[entity.getAttribute('model')].attributes[child.getAttribute('name')] = Attribute(child.getAttribute('name'))
                
                #itterate over child nodes and check that they are nodes not text
                for param in child.childNodes:
                    if not param.nodeName == '#text': 
                        
                        #node is a parameter with single value
                        if param.nodeName == 'parameter':
                            
                            #get attribute type
                            if param.getAttribute('name') == 'widget':
                                nodes[entity.getAttribute('model')].attributes[child.getAttribute('name')].type = param.getAttribute('value')
                            
                            elif param.getAttribute('name') == 'default':
                                nodes[entity.getAttribute('model')].attributes[child.getAttribute('name')].default_value = param.getAttribute('value')
                            
                            elif param.getAttribute('name') == 'label':
                                nodes[entity.getAttribute('model')].attributes[child.getAttribute('name')].label = param.getAttribute('value')
                                
                        #node is a parameter with multiple values
                        elif param.nodeName == 'parameters':
                            
                            #if the node contains entity types we are interested
                            if param.getAttribute('name') == 'entity_types':
                                for node in param.childNodes:
                                    if not param.nodeName == '#text': 
                                        if node.nodeName == 'parameter':
                                            nodes[entity.getAttribute('model')].attributes[child.getAttribute('name')].entity_types.append(node.getAttribute('name'))

    if list:
        print 'Found the following appleseed nodes:\n'

        for node_key in nodes.iterkeys():
            print '  ' + nodes[node_key].name
            
            print '    type                      : ' + nodes[node_key].type
            print '    attributes                : '

            for attr_key in nodes[node_key].attributes.iterkeys():
                print '      name                    : ' + nodes[node_key].attributes[attr_key].name
                print '      type                    : ' + nodes[node_key].attributes[attr_key].type
                print '      label                   : ' + nodes[node_key].attributes[attr_key].label
                print '      default                 : ' + nodes[node_key].attributes[attr_key].default_value
                print '      allowed connections     : '
                for entity_type in nodes[node_key].attributes[attr_key].entity_types:
                    print '        ' + entity_type
                print''

            print'\n'

    return nodes


#--------------------------------------------------------------------------------------------------
# Add color attribute to node.
#--------------------------------------------------------------------------------------------------

def addColorAttr(node_name, attribute_name, default_value=(0,0,0)):
    cmds.addAttr(node_name, longName=attribute_name, usedAsColor=True, attributeType='float3')
    cmds.addAttr(node_name, longName=(attribute_name + '_R'), attributeType='float', parent=attribute_name)
    cmds.addAttr(node_name, longName=(attribute_name + '_G'), attributeType='float', parent=attribute_name)
    cmds.addAttr(node_name, longName=(attribute_name + '_B'), attributeType='float', parent=attribute_name)
    cmds.setAttr(node_name + '.' + attribute_name, default_value[0], default_value[1], default_value[2])


#--------------------------------------------------------------------------------------------------
# Create shading node.
#--------------------------------------------------------------------------------------------------

def createShadingNode(model, entity_defs_obj=False):
    if entity_defs_obj:
        entity_defs = entity_defs_obj
    else:
        entity_defs = getEntityDefs(os.path.join(ROOT_DIRECTORY, 'scripts', 'appleseedEntityDefs.xml'))

    shading_node_name = cmds.shadingNode('ms_appleseed_shading_node', asUtility=True, name=model)

    cmds.addAttr(shading_node_name, longName='node_model', dt="string")
    cmds.setAttr((shading_node_name + '.node_model'), model, type="string", lock=True)

    cmds.addAttr(shading_node_name, longName='node_type', dt="string")
    cmds.setAttr((shading_node_name + '.node_type'), entity_defs[model].type, type="string", lock=True)
    
    for entity_key in entity_defs.keys():
        if entity_key == model:
            for attr_key in entity_defs[entity_key].attributes.keys():
                if entity_defs[entity_key].attributes[attr_key].type == 'entity_picker':

                    # if there is a default value, use it
                    if entity_defs[entity_key].attributes[attr_key].default_value:
                        default_value = float(entity_defs[entity_key].attributes[attr_key].default_value)
                        print default_value
                        addColorAttr(shading_node_name, attr_key, (default_value,default_value,default_value))
                    else:
                        addColorAttr(shading_node_name, attr_key)

                elif entity_defs[entity_key].attributes[attr_key].type == 'text_box':
                     cmds.addAttr(shading_node_name, longName=attr_key, dt="string")
                     cmds.setAttr((shading_node_name + '.' + attr_key), entity_defs[entity_key].attributes[attr_key].default_value, type="string")

    return shading_node_name


#--------------------------------------------------------------------------------------------------
# Get file texture node file name with correct frame number.
#--------------------------------------------------------------------------------------------------

def getFileTextureName(file_node):
    maya_file_texture_name = cmds.getAttr(file_node + '.fileTextureName')

    if sys.platform == 'darwin':
        maya_file_texture_name = maya_file_texture_name.replace('\\', '/')
    elif sys.platform == 'win32':
        maya_file_texture_name = maya_file_texture_name.replace('/', '\\')

    if os.path.exists(maya_file_texture_name):
        file_texture_name = maya_file_texture_name
    else:
        project_directory = cmds.workspace(q=True, rd=True)
        file_name = os.path.split(maya_file_texture_name)[1]
        project_relative_path = os.path.join(project_directory, 'sourceimages', file_name)
        if os.path.exists(project_relative_path):
            file_texture_name = project_relative_path
            cmds.warning("file not found: {0}, using equivalent texture from sourceimages".format(maya_file_texture_name))
        else:
            error_msg = "file not found: {0}".format(maya_file_texture_name)
            cmds.error(error_msg)
            raise RuntimeError(error_msg)

    if cmds.getAttr(file_node + '.useFrameExtension'):
        split_file_texture_name = maya_file_texture_name.split('.')
        frame_ofset = cmds.getAttr(file_node + '.frameOffset')
        current_frame = cmds.currentTime(q=True)
        frame_padding = len(split_file_texture_name[1])
        frame_number = str(int(current_frame + frame_ofset)).zfill(frame_padding)
        file_texture_name = split_file_texture_name[0] + '.' + frame_number + '.' + split_file_texture_name[2]

    return file_texture_name 


#--------------------------------------------------------------------------------------------------
# Export .obj file.
# This function is a wrapper for the C++ obj exporter.
#--------------------------------------------------------------------------------------------------

def export_obj(object_name, file_path, overwrite=True):
    directory = os.path.split(file_path)[0]

    if not os.path.exists(directory):
        os.makedirs(directory)

    safe_file_path = file_path.replace('\\', '\\\\')
    mel.eval('ms_export_obj -mesh "{0}" -filePath "{1}"'.format(object_name, safe_file_path))


#--------------------------------------------------------------------------------------------------
# Legalize a name.
#--------------------------------------------------------------------------------------------------

def legalizeName(filename):
    filename = filename.replace('\\', '_')
    filename = filename.replace('/', '_')
    filename = filename.replace(':', '_')
    filename = filename.replace('*', '_')
    filename = filename.replace('?', '_')
    filename = filename.replace('"', '')
    filename = filename.replace('<', '_')
    filename = filename.replace('>', '_')
    filename = filename.replace('|', '_')
    return filename


#--------------------------------------------------------------------------------------------------
# List objects by shader.
#--------------------------------------------------------------------------------------------------

def listObjectsByShader(shader):
    shading_engine = cmds.listConnections(shader, type='shadingEngine')[0]
    return cmds.listConnections(shading_engine, type='mesh')


#--------------------------------------------------------------------------------------------------
# Get connected node.
#--------------------------------------------------------------------------------------------------

def getConnectedNode(connection):
    connections = cmds.listConnections(connection, destination=False, source=True)
    return None if connections is None else connections[0]


#--------------------------------------------------------------------------------------------------
# Material conversion.
#--------------------------------------------------------------------------------------------------

def convertAllMaterials():
    materials = cmds.ls(mat=True)

    if not materials:
        cmds.warning('no materials in the scene') 
        return

    for material in materials:
        convertMaterial(material)

def convertSelectedMaterials():
    materials = cmds.ls(sl=True, mat=True)

    if not materials:
        cmds.warning('no materials selected') 
        return

    for material in materials:
        convertMaterial(material)

def convertMaterial(material):
    material_type = cmds.nodeType(material)

    if material_type == 'phong' or material_type == 'blinn':
        convertPhongBlinnMaterial(material)
    elif material_type == 'ms_appleseed_material':
        pass
    elif material_type == 'surfaceShader':
        convert_surface_shader_material(material)
    elif material_type == 'lambert':
        convert_lambert_material(material)
    else:
        cmds.warning("don't know how to convert material of type '{0}'".format(material_type))

def convertPhongBlinnMaterial(material):
    print '// converting shader', material

    new_material_node = cmds.shadingNode('ms_appleseed_material', asShader=True, name=(material + '_translation')) 

    # set random hardware color
    cmds.setAttr(new_material_node + '.hardware_color_in', random.random(), random.random(), random.random(), type='float3')

    color_connection = getConnectedNode(material + '.color')
    specular_color_connection = getConnectedNode(material + '.specularColor')
    transparency_connection = getConnectedNode(material + '.transparency')
    # bump_connection = getConnectedNode(material + '.bumpMapping')

    bsdf = createShadingNode('ashikhmin_brdf')
    cmds.connectAttr(bsdf + '.outColor', new_material_node + '.BSDF_front_color')

    # edf = createShadingNode('diffuse_edf')
    # cmds.connectAttr(edf + '.outColor', new_material_node + '.EDF_color')

    surface_shader = createShadingNode('physical_surface_shader')
    cmds.connectAttr(surface_shader + '.outColor', new_material_node + '.surface_shader_front_color')

    # diffuse
    color_value = cmds.getAttr(material + '.color')[0]
    cmds.setAttr(bsdf + '.diffuse_reflectance', color_value[0], color_value[1], color_value[2], type='float3')
    if color_connection: 
        print("connecting {0}.outColor to {1}.diffuse_reflectance".format(color_connection, bsdf))
        cmds.connectAttr(color_connection + '.outColor', bsdf + '.diffuse_reflectance')

    # glossy
    color_value = cmds.getAttr(material + '.specularColor')[0]
    cmds.setAttr(bsdf + '.glossy_reflectance', color_value[0], color_value[1], color_value[2], type='float3')
    if specular_color_connection:
        print("connecting {0}.outColor to {1}.glossy_reflectance".format(specular_color_connection, bsdf))
        cmds.connectAttr(specular_color_connection + '.outColor', bsdf + '.glossy_reflectance')

    # transparency
    color_value = cmds.getAttr(material + '.transparency')[0]
    cmds.setAttr(new_material_node + '.alpha_map_color', color_value[0], color_value[1], color_value[2], type='float3')
    if transparency_connection:
        print("connecting {0}.outColor to {1}.alpha_map_color".format(transparency_connection, new_material_node))
        cmds.connectAttr(transparency_connection + '.outColor', new_material_node + '.alpha_map_color')

    # shininess
    material_type = cmds.nodeType(material)
    if material_type == 'phong':
        shininess = cmds.getAttr(material + '.cosinePower')
    elif material_type == 'blinn':
        # 0..1 -> 100..0
        shininess = 100.0 * (1.0 - cmds.getAttr(material + '.eccentricity'))
    else:
        shininess = 0.0
    shininess = max(shininess, 0.0)
    cmds.setAttr(bsdf + '.shininess_u', shininess, shininess, shininess, type='float3')
    cmds.setAttr(bsdf + '.shininess_v', shininess, shininess, shininess, type='float3')

    material_shading_group = cmds.listConnections(material, type='shadingEngine')
    if material_shading_group != None:
        cmds.connectAttr(new_material_node + '.outColor', material_shading_group[0] + '.surfaceShader', force=True)


def convert_surface_shader_material(material):
    print '// converting shader', material

    new_material_node = cmds.shadingNode('ms_appleseed_material', asShader=True, name=(material + '_translation')) 

    # set random hardware color
    cmds.setAttr(new_material_node + '.hardware_color_in', random.random(), random.random(), random.random(), type='float3')

    out_color_connection = getConnectedNode(material + '.outColor')
    out_transparency_connection = getConnectedNode(material + '.outTransparency')

    surface_shader = createShadingNode('constant_surface_shader')
    cmds.connectAttr(surface_shader + '.outColor', new_material_node + '.surface_shader_front_color')

    # color
    color_value = cmds.getAttr(material + '.outColor')[0]
    cmds.setAttr(surface_shader + '.color', color_value[0], color_value[1], color_value[2], type='float3')
    if out_color_connection: 
        print("connecting {0}.outColor to {1}.color".format(out_color_connection, surface_shader))
        cmds.connectAttr(out_color_connection + '.outColor', surface_shader + '.color')

    # transparency
    color_value = cmds.getAttr(material + '.outTransparency')[0]
    cmds.setAttr(new_material_node + '.alpha_map_color', color_value[0], color_value[1], color_value[2], type='float3')
    if out_transparency_connection:
        print("connecting {0}.outColor to {1}.alpha_map_color".format(out_transparency_connection, new_material_node))
        cmds.connectAttr(out_transparency_connection + '.outColor', new_material_node + '.alpha_map_color')

    material_shading_group = cmds.listConnections(material, type='shadingEngine')
    if material_shading_group != None:
        cmds.connectAttr(new_material_node + '.outColor', material_shading_group[0] + '.surfaceShader', force=True)


def convert_lambert_material(material):
    print '// converting shader', material

    new_material_node = cmds.shadingNode('ms_appleseed_material', asShader=True, name=(material + '_translation')) 

    # set random hardware color
    cmds.setAttr(new_material_node + '.hardware_color_in', random.random(), random.random(), random.random(), type='float3')

    color_connection = getConnectedNode(material + '.color')
    transparency_connection = getConnectedNode(material + '.transparency')

    brdf = createShadingNode('lambertian_brdf')
    surface_shader = createShadingNode('physical_surface_shader')

    cmds.connectAttr(brdf + '.outColor', new_material_node + '.BSDF_front_color')
    cmds.connectAttr(surface_shader + '.outColor', new_material_node + '.surface_shader_front_color')


    # color
    color_value = cmds.getAttr(material + '.color')[0]
    cmds.setAttr(brdf + '.reflectance', color_value[0], color_value[1], color_value[2], type='float3')
    if color_connection: 
        print("connecting {0}.outColor to {1}.reflectance".format(color_connection, surface_shader))
        cmds.connectAttr(color_connection + '.outColor', brdf + '.reflectance')

    # transparency
    color_value = cmds.getAttr(material + '.transparency')[0]
    cmds.setAttr(new_material_node + '.alpha_map_color', color_value[0], color_value[1], color_value[2], type='float3')
    if transparency_connection:
        print("connecting {0}.outColor to {1}.alpha_map_color".format(transparency_connection, new_material_node))
        cmds.connectAttr(transparency_connection + '.outColor', new_material_node + '.alpha_map_color')

    material_shading_group = cmds.listConnections(material, type='shadingEngine')
    if material_shading_group != None:
        cmds.connectAttr(new_material_node + '.outColor', material_shading_group[0] + '.surfaceShader', force=True)



#--------------------------------------------------------------------------------------------------
# returns list of materials connecetd to mesh.
#--------------------------------------------------------------------------------------------------

def get_attached_materials(mesh_name):
    shading_engine = cmds.listConnections(mesh_name, t='shadingEngine')
    if shading_engine:
        return cmds.listConnections(shading_engine[0] + ".surfaceShader")
    else:
        return None
