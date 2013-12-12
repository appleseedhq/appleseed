
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
import math


#--------------------------------------------------------------------------------------------------
# Constants.
#--------------------------------------------------------------------------------------------------

MAYASEED_VERSION = '0.6.2'
MAYASEED_URL = 'https://github.com/jupiter-jazz/mayaseed'
APPLESEED_URL = 'http://appleseedhq.net/'
ROOT_DIRECTORY = os.path.split((os.path.dirname(inspect.getfile(inspect.currentframe()))))[0]


#--------------------------------------------------------------------------------------------------
# Export modifiers.
#--------------------------------------------------------------------------------------------------

# attr name, data type, attribute type, default_value

LIGHT_EXPORT_MODIFIERS = [
['ms_area_light_visibility', None, 'bool', False],
['ms_cast_indirect_light', None, 'bool', False],
['ms_importance_multiplier', None, 'float', 1],
]

MATERIAL_EXPORT_MODIFIERS = [
['ms_cast_indirect_light', None, 'bool', False],
['ms_importance_multiplier', None, 'float', 1],
['ms_front_lighting_samples', None, 'float', 1],
['ms_back_lighting_samples', None, 'float', 1],
['ms_double_sided_material', None, 'bool', True],
['ms_transparency_is_material_alpha', None, 'bool', False],
['ms_secondary_surface_shader', None, 'message', True],
['ms_emit_light', None, 'bool', True],
]

#--------------------------------------------------------------------------------------------------
# Show About dialog.
#--------------------------------------------------------------------------------------------------

class ms_info_dial():
    def __init__(self):
        if cmds.window('ms_info_window', query=True, exists=True):
            cmds.deleteUI('ms_info_window')

        window = cmds.window('ms_info_window', title='About Mayaseed', sizeable=False)

        cmds.columnLayout(rs=10, columnOffset=['both', 20], width=600)

        cmds.rowLayout(numberOfColumns=2)
        cmds.text('', width=30)
        cmds.image(image=os.path.join(ROOT_DIRECTORY, 'graphics', 'mayaseed.png'))
        cmds.setParent('..')

        cmds.text('Version ' + MAYASEED_VERSION)
        cmds.text('Mayaseed is a Maya plugin for exporting scenes to the appleseed renderer.')
        cmds.text('Written by Jonathan Topf.')

        cmds.rowLayout(numberOfColumns=4)
        cmds.button(label='Mayaseed website', command=('import webbrowser\nwebbrowser.open_new_tab("' + MAYASEED_URL + '")'))
        cmds.button(label='appleseed website', command=('import webbrowser\nwebbrowser.open_new_tab("' + APPLESEED_URL + '")'))
        cmds.text('', width=166)
        cmds.button(label='Close', command=('import maya.cmds as cmds\ncmds.deleteUI(\"' + window + '\", window=True)'), width=100)
        cmds.setParent('..')

        cmds.text('')

        cmds.setParent('..')

        cmds.showWindow(window)


#--------------------------------------------------------------------------------------------------
# Creates a directory if it doesn't already exist.
#--------------------------------------------------------------------------------------------------

def create_dir(path):
    if not os.path.exists(path):
        os.makedirs(path)

    return path


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

def convert_connection_to_image(shader, attribute, dest_file, resolution=1024, pass_through=False):

    dest_dir = os.path.split(dest_file)[0]

    create_dir(dest_dir)

    plug_name = shader + '.' + attribute

    if not cmds.objExists(plug_name):
        warning('error converting texture, no object named {0} exists'.format(plug_name))
    else:
        connection = cmds.listConnections(plug_name)
        if not connection:
            warning('nothing connected to {0}, skipping conversion'.format(plug_name))
        elif pass_through == True:
            warning('{0}: skipping conversion'.format(plug_name))
        else:
            cmds.hyperShade(objects=shader)
            connected_object = cmds.ls(sl=True)[0]
            cmds.convertSolidTx(connection[0] ,connected_object ,fileImageName=dest_file, antiAlias=True, bm=3, fts=True, sp=True, alpha=True, doubleSided=True, resolutionX=resolution, resolutionY=resolution)

        return dest_file


#--------------------------------------------------------------------------------------------------
# Convert textures to OpenEXR format.
#--------------------------------------------------------------------------------------------------

def find_path_to_imf_copy():
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


def convert_texture_to_exr(file_path, export_root, texture_dir, overwrite=True, pass_through=False, relative=True):
    relative_path = os.path.join(texture_dir, os.path.splitext(os.path.split(file_path)[1])[0] + '.exr')
    dest_file = os.path.join(export_root, relative_path)
    dest_dir = os.path.join(export_root, texture_dir)
    result_file = relative_path if relative else dest_file

    if not os.path.exists(file_path):
        info("# error: {0} does not exist".format(file_path))
        return result_file

    if pass_through:
        info("# skipping conversion of {0}".format(file_path))
        return result_file

    if os.path.exists(dest_file) and not overwrite:
        info("# {0} already exists, skipping conversion".format(dest_file))
        return result_file

    imf_copy_path = find_path_to_imf_copy()

    if imf_copy_path is None:
        info("# error: cannot convert {0}, imf_copy utility not found".format(file_path))
        return result_file

    create_dir(dest_dir)

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

    return result_file


#--------------------------------------------------------------------------------------------------
# Check if an object is visible for the current frame.
#--------------------------------------------------------------------------------------------------

def transform_is_visible(node_name):

    """ Returns the visibility state of a transform for the current frame, this visibility state may change over time """

    # check if the node exists
    if not cmds.objExists(node_name):
        return False

    # check if the node has a visibility attribute meaning it's a DAG node
    if not cmds.attributeQuery('visibility', node=node_name, exists=True):
        return False

    # check if the visibility set to off
    if not cmds.getAttr(node_name + '.visibility'):
        return False

    return True


def mesh_is_renderable(mesh_name):

    """ Returns the renderability state of a mesh, this value will not change over time """

    # check to see if it's an intermediate mesh
    if cmds.attributeQuery('intermediateObject', node=mesh_name, exists=True):
        if cmds.getAttr(mesh_name + '.intermediateObject'):
            return False

    return True


#--------------------------------------------------------------------------------------------------
# Check if an object is visible for the current frame.
#--------------------------------------------------------------------------------------------------

def transform_is_renderable(node_name):

    """ Returns the renderability state of a transform, this value will not change over time """

    # check if it is a hidden display layer
    if cmds.attributeQuery('overrideEnabled', node=node_name, exists=True) and cmds.getAttr(node_name + '.overrideEnabled'):
        if not cmds.getAttr(node_name + '.overrideVisibility'):
            return False

    return True


#--------------------------------------------------------------------------------------------------
# check if a transform or any of its parents are set as visible.
#--------------------------------------------------------------------------------------------------

def visible_in_hierarchy(parent):
    parents = cmds.listRelatives(parent, ap=True, f=True)
    if parents is not None:
        if cmds.getAttr(parents[0] + '.visibility') == False:
            return False
        return visible_in_hierarchy(parents)
    
    return True


#--------------------------------------------------------------------------------------------------
# Check keys on a given attribute are constant.
#--------------------------------------------------------------------------------------------------

def keys_are_constant(attr, value=None):

    keys = cmds.keyframe(attr, q=True, valueChange=True, absolute=True)

    if value == None:
        value = keys[0]

    for key in keys:
        if key != value:
            return False
    
    return True


#--------------------------------------------------------------------------------------------------
# Check if an object has a shader connected.
#--------------------------------------------------------------------------------------------------

def has_shader_connected(node_name):
    # check that the shape has a shader connected
    if not cmds.listConnections(node_name, t='shadingEngine'):
        return False

    shadingEngine = cmds.listConnections(node_name, t='shadingEngine')[0]
    if not cmds.connectionInfo(shadingEngine + '.surfaceShader', sourceFromDestination=True).split('.')[0]:
        return False

    return cmds.connectionInfo(shadingEngine + '.surfaceShader', sourceFromDestination=True).split('.')[0]


#--------------------------------------------------------------------------------------------------
# Read entity definitions from disk.
#--------------------------------------------------------------------------------------------------

def get_entity_defs(xml_file_path, list=False):
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

    file = open(xml_file_path, 'r')
    data = file.read()
    file.close()

    dom = parseString(data)

    for entity in dom.getElementsByTagName('entity'):
        entity_model = entity.getAttribute('model')

        # create new dict entry to store the node info
        nodes[entity_model] = Node(entity_model, entity.getAttribute('type'))

        for child in entity.childNodes:
            if child.nodeName != 'parameters':
                continue
                
            # add an attribute and give it a name
            child_name = child.getAttribute('name')
            nodes[entity_model].attributes[child_name] = Attribute(child_name)

            for param in child.childNodes:
                if param.nodeName == '#text':
                    continue

                # node is a parameter with single value
                if param.nodeName == 'parameter':
                    name = param.getAttribute('name')
                    value = param.getAttribute('value')
                    if name == 'type':
                        nodes[entity_model].attributes[child_name].type = value
                    elif name == 'default':
                        nodes[entity_model].attributes[child_name].default_value = value
                    elif name == 'label':
                        nodes[entity_model].attributes[child_name].label = value

                # node is a parameter with multiple values
                elif param.nodeName == 'parameters':
                    # if the node contains entity types we are interested
                    if param.getAttribute('name') == 'entity_types':
                        for node in param.childNodes:
                            if node.nodeName == 'parameter':
                                nodes[entity_model].attributes[child_name].entity_types.append(node.getAttribute('name'))

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

def add_color_attr(node_name, attribute_name, default_value=(0,0,0)):
    cmds.addAttr(node_name, longName=attribute_name, usedAsColor=True, attributeType='float3')
    cmds.addAttr(node_name, longName=(attribute_name + '_R'), attributeType='float', parent=attribute_name)
    cmds.addAttr(node_name, longName=(attribute_name + '_G'), attributeType='float', parent=attribute_name)
    cmds.addAttr(node_name, longName=(attribute_name + '_B'), attributeType='float', parent=attribute_name)
    cmds.setAttr(node_name + '.' + attribute_name, default_value[0], default_value[1], default_value[2])


#--------------------------------------------------------------------------------------------------
# Create shading node.
#--------------------------------------------------------------------------------------------------

def create_shading_node(model, name=None, entity_defs_obj=False):
    if entity_defs_obj:
        entity_defs = entity_defs_obj
    else:
        entity_defs = get_entity_defs(os.path.join(ROOT_DIRECTORY, 'scripts', 'appleseedEntityDefs.xml'))

    shading_node_name = cmds.shadingNode('ms_appleseed_shading_node', asUtility=True, name=name if name is not None else model)

    cmds.addAttr(shading_node_name, longName='node_model', dt="string")
    cmds.setAttr(shading_node_name + '.node_model', model, type="string", lock=True)

    cmds.addAttr(shading_node_name, longName='node_type', dt="string")
    cmds.setAttr(shading_node_name + '.node_type', entity_defs[model].type, type="string", lock=True)

    cmds.addAttr(shading_node_name, longName='render_layer', dt="string")
    cmds.setAttr(shading_node_name + '.render_layer', '', type="string")

    for entity_key in entity_defs.keys():
        if entity_key == model:
            for attr_key in entity_defs[entity_key].attributes.keys():
                attr = entity_defs[entity_key].attributes[attr_key]
                if attr.type == 'text' or attr.type == 'enumeration' or attr.type == 'boolean' or attr.type == 'numeric':
                     cmds.addAttr(shading_node_name, longName=attr_key, dt="string")
                     cmds.setAttr(shading_node_name + '.' + attr_key, attr.default_value, type="string")

                elif (attr.type == 'colormap') or (attr.type == 'entity'):
                    # if there is a default value, use it
                    if attr.default_value:
                        default_value = float(attr.default_value)
                        add_color_attr(shading_node_name, attr_key, (default_value, default_value, default_value))
                    else:
                        add_color_attr(shading_node_name, attr_key)
            break

    return shading_node_name


#--------------------------------------------------------------------------------------------------
# Get file texture node file name with correct frame number.
#--------------------------------------------------------------------------------------------------

def get_file_texture_name(file_node, frame=None):
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
            warning("file not found: {0}, using equivalent texture from sourceimages".format(maya_file_texture_name))
        else:
            error_msg = "file not found: {0}".format(maya_file_texture_name)
            error(error_msg)
            raise RuntimeError(error_msg)

    if cmds.getAttr(file_node + '.useFrameExtension'):
        split_file_texture_name = maya_file_texture_name.split('.')
        frame_ofset = cmds.getAttr(file_node + '.frameOffset')
        frame_padding = len(split_file_texture_name[1])
        if frame is not None:
            frame_number = str(int(frame + frame_ofset)).zfill(frame_padding)
        file_texture_name = split_file_texture_name[0] + '.' + frame_number + '.' + split_file_texture_name[2]

    return file_texture_name 


#--------------------------------------------------------------------------------------------------
# Export .obj file.
# This function is a wrapper for the C++ obj exporter.
#--------------------------------------------------------------------------------------------------

def export_obj(object_name, file_path, overwrite=True):
    directory = os.path.split(file_path)[0]

    create_dir(directory)

    safe_file_path = file_path.replace('\\', '\\\\')
    mel.eval('ms_export_obj -mesh "{0}" -filePath "{1}"'.format(object_name, safe_file_path))

    return safe_file_path


#--------------------------------------------------------------------------------------------------
# Legalize a name.
#--------------------------------------------------------------------------------------------------

def legalize_name(filename):
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

def list_objects_by_shader(shader):
    shading_engine = cmds.listConnections(shader, type='shadingEngine')[0]
    return cmds.listConnections(shading_engine, type='mesh')


#--------------------------------------------------------------------------------------------------
# Get connected node.
#--------------------------------------------------------------------------------------------------

def get_connected_node(connection):
    connections = cmds.listConnections(connection, destination=False, source=True)
    return None if connections is None else connections[0]


#--------------------------------------------------------------------------------------------------
# Material conversion.
#--------------------------------------------------------------------------------------------------

def convert_all_materials():
    materials = cmds.ls(mat=True)

    if not materials:
        warning('no materials in the scene') 
        return

    for material in materials:
        convert_material(material)


def convert_selected_materials():
    materials = cmds.ls(sl=True, mat=True)

    if not materials:
        selection = cmds.ls(sl=True, tr=True)
        if selection is None:
            error('No valid selection')
            return
        materials = []
        shape_node_connections = cmds.listRelatives(selection[0], shapes=True)
        if shape_node_connections is not None:
            materials.append(has_shader_connected(shape_node_connections[0]))

    for material in materials:
        convert_material(material)


def convert_material(material):
    if material == 'lambert1':
        info('Cannot convert default material "lambert1"')
        return

    info('converting shader ' + material)

    material_type = cmds.nodeType(material)

    if material_type == 'lambert':
        converted_material = convert_lambert_material(material)

    elif material_type == 'phong' or material_type == 'blinn':
        converted_material = convert_phong_blinn_material(material)

    elif material_type == 'surfaceShader':
        converted_material = convert_surface_shader_material(material)

    elif material_type == 'ms_appleseed_material':
        return

    else:
        warning("don't know how to convert material of type '{0}'".format(material_type))
        return

    # assign the new material to the geometry
    material_shading_group = cmds.listConnections(material, type='shadingEngine')
    if material_shading_group is not None:
        cmds.connectAttr(converted_material + '.outColor', material_shading_group[0] + '.surfaceShader', force=True)


def set_random_hardware_color(material):
    cmds.setAttr(material + '.hardware_color_in', random.random(), random.random(), random.random(), type='float3')


def assign_connection_or_color(input_name, connection_name, color_name):
    connection = get_connected_node(connection_name)
    if connection: 
        cmds.connectAttr(connection + '.outColor', input_name)
    else:
        color = cmds.getAttr(color_name)[0]
        cmds.setAttr(input_name, color[0], color[1], color[2], type='float3')


#--------------------------------------------------------------------------------------------------
# Convert a Lambert material.
#--------------------------------------------------------------------------------------------------

def convert_lambert_material(material):
    converted_material = cmds.shadingNode('ms_appleseed_material', asShader=True, name=(material + '_translation')) 
    set_random_hardware_color(converted_material)

    # surface shader
    surface_shader = create_shading_node('physical_surface_shader')
    cmds.connectAttr(surface_shader + '.outColor', converted_material + '.surface_shader_front_color')
    assign_connection_or_color(converted_material + '.alpha_map_color', material + '.transparency', material + '.transparency')

    # BRDF
    brdf = create_shading_node('lambertian_brdf')
    cmds.connectAttr(brdf + '.outColor', converted_material + '.BSDF_front_color')
    assign_connection_or_color(brdf + '.reflectance', material + '.color', material + '.color')

    return converted_material


#--------------------------------------------------------------------------------------------------
# Convert a Blinn or Phong material.
#--------------------------------------------------------------------------------------------------

def convert_phong_blinn_material(material):
    converted_material = cmds.shadingNode('ms_appleseed_material', asShader=True, name=material + '_translation')
    set_random_hardware_color(converted_material)

    # surface shader
    surface_shader = create_shading_node('physical_surface_shader', name=(material + '_surface_shader'))
    cmds.connectAttr(surface_shader + '.outColor', converted_material + '.surface_shader_front_color')
    assign_connection_or_color(converted_material + '.alpha_map_color', material + '.transparency', material + '.transparency')

    # diffuse component
    diffuse_brdf = create_shading_node('lambertian_brdf', name=(material + 'lambert_brdf'))
    color_connection = get_connected_node(material + '.color')
    if color_connection: 
        cmds.connectAttr(color_connection + '.outColor', diffuse_brdf + '.reflectance')
    else:
        color = cmds.getAttr(material + '.color')[0]
        scale = cmds.getAttr(material + '.diffuse')
        cmds.setAttr(diffuse_brdf + '.reflectance', color[0] * scale, color[1] * scale, color[2] * scale, type='float3')

    # glossy component
    glossy_brdf = create_shading_node('microfacet_brdf', name=(material + '_blinn_brdf'))
    cmds.setAttr(glossy_brdf + '.mdf', 'blinn', type='string')
    if cmds.nodeType(material) == 'phong':
        glossy_param = ((cmds.getAttr(material + '.cosinePower') - 2) / 98) * -1 + 1
    else:
        glossy_param = 0.5
    cmds.setAttr(glossy_brdf + '.glossiness', glossy_param, glossy_param, glossy_param, type='float3')
    assign_connection_or_color(glossy_brdf + '.reflectance', material + '.specularColor', material + '.specularColor')

    # bump component
    bump_node = cmds.listConnections( material + '.normalCamera')
    if bump_node is not None and len(bump_node) > 0:
        bump_tex = cmds.listConnections(bump_node[0] + '.bumpValue')
        cmds.setAttr(converted_material + '.bump_amplitude', cmds.getAttr(bump_node[0] + '.bumpDepth'))
        if bump_tex is not None:
            cmds.connectAttr(bump_tex[0] + '.outColor', converted_material + '.displacement_map_front')
            cmds.connectAttr(bump_tex[0] + '.outColor', converted_material + '.displacement_map_back')

    # mix diffuse and glossy
    mix_brdf = create_shading_node('bsdf_mix', name=(material + '_bsdf_mix'))
    cmds.connectAttr(diffuse_brdf + '.outColor', mix_brdf + '.bsdf0')       # diffuse BRDF
    cmds.connectAttr(glossy_brdf + '.outColor', mix_brdf + '.bsdf1')        # glossy BRDF
    cmds.setAttr(mix_brdf + '.weight0', 1.0, 1.0, 1.0, type='float3')       # diffuse weight
    cmds.setAttr(mix_brdf + '.weight1', 0.2, 0.2, 0.2, type='float3')       # glossy weight
    cmds.connectAttr(mix_brdf + '.outColor', converted_material + '.BSDF_front_color')

    return converted_material


#--------------------------------------------------------------------------------------------------
# Convert a surface shader material.
#--------------------------------------------------------------------------------------------------

def convert_surface_shader_material(material):
    converted_material = cmds.shadingNode('ms_appleseed_material', asShader=True, name=(material + '_translation')) 
    set_random_hardware_color(converted_material)

    # surface shader
    surface_shader = create_shading_node('constant_surface_shader', name=(material + '_surface_shader'))
    cmds.connectAttr(surface_shader + '.outColor', converted_material + '.surface_shader_front_color')
    assign_connection_or_color(surface_shader + '.color', material + '.outColor', material + '.outColor')
    assign_connection_or_color(converted_material + '.alpha_map_color', material + '.outTransparency', material + '.outTransparency')

    return converted_material


#--------------------------------------------------------------------------------------------------
# Returns the materials connected to a mesh.
#--------------------------------------------------------------------------------------------------

def get_attached_materials(mesh_name):
    shading_engines = cmds.listConnections(mesh_name, t='shadingEngine')
    materials = []
    if shading_engines is not None:
        for shading_engine in shading_engines:
            shading_engine_materials = cmds.listConnections(shading_engine + ".surfaceShader")
            if shading_engine_materials is not None:
                materials.append(shading_engine_materials[0])
    return materials


#--------------------------------------------------------------------------------------------------
# Log functions.
#--------------------------------------------------------------------------------------------------

def info(message):
    print('// {0}'.format(message))

def warning(message):
    print('#  {0}'.format(message))

def error(message):
    cmds.error(message)


#--------------------------------------------------------------------------------------------------
# vector functions.
#--------------------------------------------------------------------------------------------------

def vector_get_length(v):
    return math.sqrt((v[0] * v[0]) + (v[1] * v[1]) + (v[2] * v[2]))


#--------------------------------------------------------------------------------------------------
# matrix functions.
#--------------------------------------------------------------------------------------------------

# a maya matrix is represented as [0,0,0,0 ,0,0,0,0 ,0,0,0,0 ,0,0,0,0]
# 0 4 8  12
# 1 5 9  13
# 2 6 10 14
# 3 7 11 15

def matrix_multiply(transform_matrix, m):
    result = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0]

    return [(m[0] * transform_matrix[0])  + (m[4] * transform_matrix[1])  + (m[ 8] * transform_matrix[ 2]) + (m[12] * transform_matrix[3]),
            (m[1] * transform_matrix[0])  + (m[5] * transform_matrix[1])  + (m[ 9] * transform_matrix[ 2]) + (m[13] * transform_matrix[3]),
            (m[2] * transform_matrix[0])  + (m[6] * transform_matrix[1])  + (m[10] * transform_matrix[ 2]) + (m[14] * transform_matrix[3]),
            (m[3] * transform_matrix[0])  + (m[7] * transform_matrix[1])  + (m[11] * transform_matrix[ 2]) + (m[15] * transform_matrix[3]),

            (m[0] * transform_matrix[4])  + (m[4] * transform_matrix[5])  + (m[ 8] * transform_matrix[ 6]) + (m[12] * transform_matrix[7]),
            (m[1] * transform_matrix[4])  + (m[5] * transform_matrix[5])  + (m[ 9] * transform_matrix[ 6]) + (m[13] * transform_matrix[7]),
            (m[2] * transform_matrix[4])  + (m[6] * transform_matrix[5])  + (m[10] * transform_matrix[ 6]) + (m[14] * transform_matrix[7]),
            (m[3] * transform_matrix[4])  + (m[7] * transform_matrix[5])  + (m[11] * transform_matrix[ 6]) + (m[15] * transform_matrix[7]),

            (m[0] * transform_matrix[8])  + (m[4] * transform_matrix[9])  + (m[8]  * transform_matrix[10]) + (m[12] * transform_matrix[11]),
            (m[1] * transform_matrix[8])  + (m[5] * transform_matrix[9])  + (m[9]  * transform_matrix[10]) + (m[13] * transform_matrix[11]),
            (m[2] * transform_matrix[8])  + (m[6] * transform_matrix[9])  + (m[10] * transform_matrix[10]) + (m[14] * transform_matrix[11]),
            (m[3] * transform_matrix[8])  + (m[7] * transform_matrix[9])  + (m[11] * transform_matrix[10]) + (m[15] * transform_matrix[11]),

            (m[0] * transform_matrix[12]) + (m[4] * transform_matrix[13]) + (m[ 8] * transform_matrix[14]) + (m[12] * transform_matrix[15]),
            (m[1] * transform_matrix[12]) + (m[5] * transform_matrix[13]) + (m[ 9] * transform_matrix[14]) + (m[13] * transform_matrix[15]),
            (m[2] * transform_matrix[12]) + (m[6] * transform_matrix[13]) + (m[10] * transform_matrix[14]) + (m[14] * transform_matrix[15]),
            (m[3] * transform_matrix[12]) + (m[7] * transform_matrix[13]) + (m[11] * transform_matrix[14]) + (m[15] * transform_matrix[15]),]

def matrix_get_scale(m):
    x_scale = vector_get_length([m[0], m[1], m[ 2]])
    y_scale = vector_get_length([m[4], m[5], m[ 6]])
    z_scale = vector_get_length([m[8], m[9], m[10]])

    return [x_scale, y_scale, z_scale]

def matrix_remove_scale(m):
    scale = matrix_get_scale(m)
    
    inverse_scale = [1.0 / scale[0], 0.0, 0.0, 0.0,
                     0.0, 1.0 / scale[1], 0.0, 0.0,
                     0.0, 0.0, 1.0 / scale[2], 0.0,
                     0.0, 0.0, 0.0, 1.0]

    return matrix_multiply(m, inverse_scale)


#--------------------------------------------------------------------------------------------------
# Normalize a path to the Posix format (using / as directory separator), regardless of the host.
#--------------------------------------------------------------------------------------------------

def normalize_path(path):
    return path.replace('\\', '/') if os.name == "nt" else path


#--------------------------------------------------------------------------------------------------
# Add/remove export modifier.
#--------------------------------------------------------------------------------------------------

def add_export_modifier(node, attr):
    attr_signature = None

    for item in LIGHT_EXPORT_MODIFIERS + MATERIAL_EXPORT_MODIFIERS:
        if item[0] == attr:
            attr_signature = item

    if attr_signature is None:
        warning('attribute: {0} not found'.format(attr))
        return 

    if cmds.attributeQuery(attr, n=node, ex=True):
        warning('attribute exists: {0}.{1}'.format(node, attr))
        return

    if attr_signature[1] is None:
        cmds.addAttr(node, longName=attr, at=attr_signature[2])
    else:
        cmds.addAttr(node, longName=attr, dt=attr_signature[1])


def remove_export_modifier(node, attr):
    cmds.deleteAttr(node, at=attr)


#--------------------------------------------------------------------------------------------------
# Add/remove custom attributes to selection.
#--------------------------------------------------------------------------------------------------


def is_light(node):
    node_type = cmds.nodeType(node, i=True)
    if len(node_type) > 5:
        if node_type[4] == 'light':
            return True
    return False

def is_transform(node):
    return cmds.nodeType(node) == 'transform'

def is_material(node):
    node_type = cmds.nodeType(node, i=True)
    return node_type[0] == 'shadingDependNode'

def shape_from_transform(transform):
    selection = cmds.ls(sl=True)
    for i in range(len(selection)):
            relatives = cmds.listRelatives(selection[i])
            if relatives is not None:
                return relatives[0]
    return None


def selection_add_light_export_modifier(attr):
    for item in cmds.ls(sl=True):
        if is_light(item):
            add_export_modifier(item, attr)
        elif is_transform(item):
            shape = shape_from_transform(item)
            if is_light(shape):
                add_export_modifier(shape, attr)

def selection_remove_light_export_modifier(attr):
    for item in cmds.ls(sl=True):
        if is_light(item):
            remove_export_modifier(item, attr)
        elif is_transform(item):
            shape = shape_from_transform(item)
            if is_light(shape):
                remove_export_modifier(shape, attr)


def selection_add_material_export_modifier(attr):
    for item in cmds.ls(sl=True):
        if is_material(item):
            add_export_modifier(item, attr)
        elif is_transform(item):
            shape = shape_from_transform(item)
            if shape:
                material = has_shader_connected(shape)
                if material:
                    add_export_modifier(material, attr)


def selection_remove_material_export_modifier(attr):
    for item in cmds.ls(sl=True):
        if is_material(item):
            remove_export_modifier(item, attr)
        elif is_transform(item):
            shape = shape_from_transform(item)
            if shape:
                material = has_shader_connected(shape)
                if material:
                    remove_export_modifier(material, attr)




