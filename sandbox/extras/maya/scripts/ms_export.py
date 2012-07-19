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
import time
import re
import subprocess
import sys
import ms_commands

inch_to_meter = 0.02539999983236




#****************************************************************************************************************************************************************************************************
# utilitiy functions & classes **********************************************************************************************************************************************************************
#****************************************************************************************************************************************************************************************************


#
# writeXml class --
#

class WriteXml(): #(file_path)
    spaces_per_indentation_level = 4    
    def __init__(self, f_path):
        self.file_path = f_path
        self.indentation_level = 0
        self.file_object = None
        try:
            self.file_object = open(self.file_path, 'w') #open file for editing

        except IOError:
            cmds.error('IO error: file not accesable')
            raise RuntimeError('IO error: file not accesable')
            return
        
    def startElement(self,str):
        self.file_object.write(((self.indentation_level * self.spaces_per_indentation_level) * ' ') + "<" + str + '>\n')
        self.indentation_level += 1
        
    def endElement(self, str):
        self.indentation_level -= 1
        self.file_object.write(((self.indentation_level * self.spaces_per_indentation_level) * ' ') + '</' + str + '>\n')
        
    def appendElement(self, str):
        self.file_object.write(((self.indentation_level * self.spaces_per_indentation_level) * ' ') + '<' + str + '/>\n')
        
    def appendLine(self, str):
        self.file_object.write(((self.indentation_level * self.spaces_per_indentation_level) * ' ') + str + '\n')
        
    def close(self):
        self.file_object.close() #close file


#
# writeTransform function --
#

def writeTransform(doc, scale = 1, transform = [[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]):
    doc.startElement('transform')
    doc.appendElement('scaling value="{0}"'.format(scale))
    doc.startElement('matrix')

    doc.appendLine('{0:.15f} {1:.15f} {2:.15f} {3:.15f}'.format(transform[0][0], transform[1][0], transform[2][0], transform[3][0]))
    doc.appendLine('{0:.15f} {1:.15f} {2:.15f} {3:.15f}'.format(transform[0][1], transform[1][1], transform[2][1], transform[3][1]))
    doc.appendLine('{0:.15f} {1:.15f} {2:.15f} {3:.15f}'.format(transform[0][2], transform[1][2], transform[2][2], transform[3][2]))
    doc.appendLine('{0:.15f} {1:.15f} {2:.15f} {3:.15f}'.format(transform[0][3], transform[1][3], transform[2][3], transform[3][3]))    

    doc.endElement('matrix')
    doc.endElement('transform')

#
# load params function
#

def getMayaParams(render_settings_node):
    print('getting params from ui')
    #comile regular expression to check for non numeric chracters
    is_numeric = re.compile('^[0-9]+$')
    
    params = {'error':False}
    
    #main settings
    params['outputDir'] = cmds.getAttr(render_settings_node + '.output_directory')
    params['fileName'] = cmds.getAttr(render_settings_node + '.output_file')
    params['convertShadingNodes'] = cmds.getAttr(render_settings_node + '.convert_shading_nodes_to_textures')
    params['convertTexturesToExr'] = cmds.getAttr(render_settings_node + '.convert_textures_to_exr')
    params['overwriteExistingExrs'] = cmds.getAttr(render_settings_node + '.overwrite_existing_exrs')
    params['fileName'] = cmds.getAttr(render_settings_node + '.output_file')
    params['exportMotionBlur'] = cmds.getAttr(render_settings_node + '.export_motion_blur')
    params['exportAnimation'] = cmds.getAttr(render_settings_node + '.export_animation')
    params['scene_scale'] = 1
    
    #Advanced options
    #scene
    if cmds.listConnections(render_settings_node + '.environment'):
        params['environment'] = cmds.listRelatives(cmds.listConnections(render_settings_node + '.environment')[0])[0]
    else:
        params['environment'] = False

    #cameras
    # params['sceneCameraExportAllCameras'] = cmds.checkBox('ms_sceneCameraExportAllCameras', query=True, value=True)
    params['sceneCameraDefaultThinLens'] = cmds.getAttr(render_settings_node + '.export_all_cameras_as_thinlens')
    
    #assemblies
    #materials
    params['matLambertBSDF'] = 'Lambertian'
    params['matLambertEDF'] = 'None'
    params['matLambertSurfaceShader'] = 'Physical'
    params['matBlinnBSDF'] = 'Ashikhmin-Shirley'
    params['matBlinnEDF'] = 'None'
    params['matBlinnSurfaceShader'] = 'Physical'
    params['matPhongBSDF'] = 'Ashikhmin-Shirley'
    params['matPhongEDF'] = 'None'
    params['matPhongSurfaceShader'] = 'Physical'
    params['matSurfaceShaderBSDF'] = 'Lambertian'
    params['matSurfaceShaderEDF'] = 'Diffuse'
    params['matSurfaceShaderSurfaceShader'] = 'Physical'
    params['matDefaultBSDF'] = 'Lambertian'
    params['matDefaultEDF'] = 'None'
    params['matDefaultSurfaceShader'] = 'Physical'
    params['matDoubleShade'] = cmds.getAttr(render_settings_node + '.double_sided_shading')

    # output 
    if cmds.listConnections(render_settings_node + '.camera'):
        params['outputCamera'] = cmds.listConnections(render_settings_node + '.camera')[0]
    else:
        cmds.warning('no camera connected to ' + render_settings_node)
    
    if cmds.getAttr(render_settings_node + '.color_space') == 1:
        params['outputColorSpace'] = 'linear_rgb'
    elif cmds.getAttr(render_settings_node + '.color_space') == 2:
        params['outputColorSpace'] = 'spectral'
    elif cmds.getAttr(render_settings_node + '.color_space') == 3:
        params['outputColorSpace'] = 'ciexyz'
    else:
        params['outputColorSpace'] = 'srgb'

    params['outputResWidth'] = cmds.getAttr(render_settings_node + '.width')
    params['outputResHeight'] = cmds.getAttr(render_settings_node + '.height')

    # configurations
    # custom intercative config
    params['customInteractiveConfigCheck'] = cmds.getAttr(render_settings_node + '.export_custom_interactive_config')
    params['customInteractiveConfigEngine'] = cmds.getAttr(render_settings_node + '.interactive_lighting_engine')
    params['customInteractiveConfigMinSamples'] = cmds.getAttr(render_settings_node + '.interactive_min_samples')
    params['customInteractiveConfigMaxSamples'] = cmds.getAttr(render_settings_node + '.interactive_max_samples')
    params['customInteractiveConfigMaxRayDepth'] = cmds.getAttr(render_settings_node + '.interactive_max_ray_depth')
    params['customInteractiveConfigLightSamples'] = cmds.getAttr(render_settings_node + '.interactive_light_samples')

    # custom Final config
    params['customFinalConfigCheck'] = cmds.getAttr(render_settings_node + '.export_custom_final_config')
    params['customFinalConfigEngine'] = cmds.getAttr(render_settings_node + '.final_lighting_engine')
    params['customFinalConfigMinSamples'] = cmds.getAttr(render_settings_node + '.final_min_samples')
    params['customFinalConfigMaxSamples'] = cmds.getAttr(render_settings_node + '.final_max_samples')
    params['customFinalConfigMaxRayDepth'] = cmds.getAttr(render_settings_node + '.final_max_ray_depth')
    params['customFinalConfigLightSamples'] = cmds.getAttr(render_settings_node + '.final_light_samples')
    return(params)


#****************************************************************************************************************************************************************************************************
# entity classes ************************************************************************************************************************************************************************************
#****************************************************************************************************************************************************************************************************

#
# color object ---
#

class Color():
    def __init__(self, name, color, multiplier):
        self.name = name
        self.color = color
        self.multiplier = multiplier
        self.color_space = 'srgb'
        self.wavelength_range = '400.0 700.0'
        self.alpha = 1.0

        print self.name
        print self.color
        print self.multiplier



    def writeXML(self, doc):
        print('writing color {0}'.format(self.name))
        doc.startElement('color name="{0}"'.format(self.name))
        doc.appendElement('parameter name="color" value="{0:.6f} {1:.6f} {2:.6f}"'.format(self.color[0], self.color[1], self.color[2]))
        doc.appendElement('parameter name="color_space" value="{0}"'.format(self.color_space))
        doc.appendElement('parameter name="multiplier" value="{0}"'.format(self.multiplier))
        doc.appendElement('parameter name="alpha" value="{0}"'.format(self.alpha))
        doc.startElement('values')
        doc.appendLine('{0:.6f} {1:.6f} {2:.6f}'.format(self.color[0], self.color[1], self.color[2]))
        doc.endElement('values')
        doc.startElement('alpha')
        doc.appendLine('{0:.6f}'.format(self.alpha))
        doc.endElement('alpha')
        doc.endElement('color')

#
# texture class --
#

class Texture():
    def __init__(self, name, file_name, color_space='srgb'):
        self.name = name
        self.file_name = file_name
        self.color_space = color_space
    def writeXMLObject(self, doc):
        print('writing texture object {0}'.format(self.name))
        doc.startElement('texture name="{0}" model="disk_texture_2d"'.format(self.name))
        doc.appendElement('parameter name="color_space" value="{0}"'.format(self.color_space))
        doc.appendElement('parameter name="filename" value="{0}"'.format(self.file_name))
        doc.endElement('texture')
    def writeXMLInstance(self, doc):
        print('writing texture instance {0}_inst'.format(self.name))
        doc.startElement('texture_instance name="{0}_inst" texture="{0}"'.format(self.name, self.name))
        doc.appendElement('parameter name="addressing_mode" value="clamp"')
        doc.appendElement('parameter name="filtering_mode" value="bilinear"')
        doc.endElement('texture_instance')

#
# light object --
#

class Light():
    def __init__(self, params, name):
        self.params = params
        self.name = name
        self.color_name = self.name + '_exitance'
        self.color = cmds.getAttr(self.name+'.color')[0]
        self.multiplier = cmds.getAttr(self.name+'.intensity')
        self.decay = cmds.getAttr(self.name+'.decayRate')
        m = cmds.getAttr(self.name+'.matrix')
        self.transform = [m[0],m[1],m[2],m[3]], [m[4],m[5],m[6],m[7]], [m[8],m[9],m[10],m[11]], [m[12],m[13],m[14],m[15]]
    def writeXML(self, doc):
        print('writing light: {0}'.format(self.name))
        doc.startElement('light name="{0}" model="point_light"'.format(self.name))
        doc.appendElement('parameter name="exitance" value="{0}"'.format(self.color_name))
        writeTransform(doc, 1, self.transform)
        doc.endElement('light')

#
# shader object --
#

class Material(): #object transform name
    def __init__(self, params, name, bsdf=None, edf=None, surface_shader=None): 
        self.params = params
        self.name = name
        self.shader_type = cmds.nodeType(self.name)
        self.bsdf = bsdf 
        self.edf = edf
        self.surface_shader = surface_shader
        self.bsdf_color = (0.5, 0.5, 0.5, 1)
        self.bsdf_texture = None
        self.edf_color = (0,0,0,1)
        self.edf_texture = None
        self.specular_color = (0,0,0,1)
        self.specular_texture = None

        #for shaders with color & incandescence attributes interpret them as bsdf and edf
        if (self.shader_type == 'lambert') or (self.shader_type == 'blinn') or (self.shader_type == 'phong') or (self.shader_type == 'phongE'):
            self.bsdf_color = ms_commands.normalizeRGB(cmds.getAttr(self.name+'.color')[0])
            self.edf_color = ms_commands.normalizeRGB(cmds.getAttr(self.name+'.incandescence')[0])
            color_connection = cmds.connectionInfo((self.name + '.color'), sourceFromDestination=True).split('.')[0]
            incandecence_connection = cmds.connectionInfo((self.name+'.incandescence'), sourceFromDestination=True).split('.')[0]
            if color_connection:
                if cmds.nodeType(color_connection) == 'file':
                    print('# texture connected to {0}'.format(self.name + '.color'))
                    if params['convertTexturesToExr']:
                        self.bsdf_texture = ('textures/' + os.path.split(ms_commands.convertTexToExr(cmds.getAttr(color_connection+ '.fileTextureName'), os.path.join(params['outputDir'], 'textures'), params['overwriteExistingExrs']))[1])
                    else:
                        self.bsdf_texture = cmds.getAttr(color_connection+ '.fileTextureName')
                elif params['convertShadingNodes']:
                    #convert connection to exr
                    temp_dir = os.path.join(self.params['outputDir'],'temp')
                    temp_file = os.path.join(temp_dir, (color_connection + '.iff'))
                    ms_commands.convertConnectionToImage(self.name, 'color', temp_file, 1024)
                    self.bsdf_texture =  ('textures/' + os.path.split(ms_commands.convertTexToExr(temp_file, os.path.join(params['outputDir'], 'textures'), params['overwriteExistingExrs']))[1])


            if incandecence_connection:
                if cmds.nodeType(incandecence_connection) == 'file':
                    print('texture connected to {0}'.format(self.name + '.incandescence'))
                    if params['convertTexturesToExr']:
                        self.edf_texture = ('textures/' + os.path.split(ms_commands.convertTexToExr(cmds.getAttr(incandecence_connection+ '.fileTextureName'), os.path.join(params['outputDir'], 'textures'), params['overwriteExistingExrs']))[1])
                    else:
                        self.edf_texture = cmds.getAttr(incandecence_connection+ '.fileTextureName')
                else:
                    #convert connection to exr
                    temp_file = os.path.join(params['outputDir'], 'temp_files', (incandecence_connection + '.iff'))
                    ms_commands.convertConnectionToImage(self.name, 'incandescence', temp_file, 1024)
                    self.edf_texture =  ('textures/' + os.path.split(ms_commands.convertTexToExr(temp_file, os.path.join(params['outputDir'], 'textures'), params['overwriteExistingExrs']))[1])



        #get specular conponent for shaders which have one
        elif (self.shader_type == 'blinn') or (self.shader_type == 'phong') or (self.shader_type == 'phongE'):
            self.specular_color = ms_commands.normalizeRGB(cmds.getAttr(self.name+'.specularColor')[0])
            specular_connection = cmds.connectionInfo((self.name + '.specularColor'), sourceFromDestination=True).split('.')[0]
            if specular_connection:
                if cmds.nodeType(specular_connection) == 'file':
                    print('texture connected to {0}'.format(self.name + '.specularColor'))
                    if params['convertTexturesToExr']:
                        self.specular_texture = ('textures/' + os.path.split(ms_commands.convertTexToExr(cmds.getAttr(specular_connection+ '.fileTextureName'), os.path.join(params['outputDir'], 'textures'), params['overwriteExistingExrs']))[1])
                    else:
                        self.specular_texture = cmds.getAttr(specular_connection+ '.fileTextureName')
                else:
                    #convert connection to exr
                    temp_file = os.path.join(params['outputDir'], 'temp_files', (specular_connection + '.iff'))
                    ms_commands.convertConnectionToImage(self.name, 'specularColor', temp_file, 1024)
                    self.specular_texture =  ('textures/' + os.path.split(ms_commands.convertTexToExr(temp_file, os.path.join(params['outputDir'], 'textures'), params['overwriteExistingExrs']))[1])

        #for surface shaders interpret outColor as bsdf and edf
        elif self.shader_type == 'surfaceShader':
            self.edf_color = ms_commands.normalizeRGB(cmds.getAttr(self.name+'.outColor')[0])
            self.bsdf_color = self.edf_color


            outColor_connection = cmds.connectionInfo((self.name+'.outColor'), sourceFromDestination=True).split('.')[0]
            if outColor_connection:
                if cmds.nodeType(outColor_connection) == 'file':
                    print('texture connected to {0}'.format(self.name + '.outColor'))
                    if params['convertTexturesToExr']:
                        self.bsdf_texture = ('textures/' + os.path.split(ms_commands.convertTexToExr(cmds.getAttr(outColor_connection+ '.fileTextureName'), os.path.join(params['outputDir'], 'textures'), params['overwriteExistingExrs']))[1])
                    else:
                        self.bsdf_texture = cmds.getAttr(outColor_connection+ '.fileTextureName')

                    self.edf_texture = self.bsdf_texture
                else:
                    #convert connection to exr
                    temp_file = os.path.join(params['outputDir'], 'temp_files', (outColor_connection + '.iff'))
                    ms_commands.convertConnectionToImage(self.name, 'outColor', temp_file, 1024)
                    self.edf_texture =  ('textures/' + os.path.split(ms_commands.convertTexToExr(temp_file, os.path.join(params['outputDir'], 'textures'), params['overwriteExistingExrs']))[1])

            else:
                self.bsdf_texture = None
                self.edf_texture = None

        #else use default shader
        else:
            cmds.error('no valid texture connected to {0} using default'.format(self.name))
            self.name = 'default_texture'

    def writeXML(self,doc):

        print('writing material {0}'.format(self.name))
        doc.startElement('material name="{0}" model="generic_material"'.format(self.name))
        if self.bsdf:
            doc.appendElement('parameter name="bsdf" value="{0}"'.format(self.bsdf))
        if self.edf:
            doc.appendElement('parameter name="edf" value="{0}"'.format(self.edf))
        if self.surface_shader:
            doc.appendElement('parameter name="surface_shader" value="{0}"'.format(self.surface_shader))
        doc.endElement('material')

#
# bsdf class --
#

class Bsdf():
    def __init__(self, name, model, bsdf_params):
        self.name = name
        self.model = model
        self.bsdf_params = bsdf_params
    def writeXML(self, doc):
        print('writing bsdf {0}'.format(self.name))
        doc.startElement('bsdf name="{0}" model="{1}"'.format(self.name, self.model))
        for param in self.bsdf_params:
            doc.appendElement('parameter name="{0}" value="{1}"'.format(param, self.bsdf_params[param]))
        doc.endElement('bsdf')

#
# edf class --
#

class Edf():
    def __init__(self, name, model, edf_params):
        self.name = name
        self.model = model
        self.edf_params = edf_params
    def writeXML(self, doc):
        print('writing bsdf {0}'.format(self.name))
        doc.startElement('edf name="{0}" model="{1}"'.format(self.name, self.model))
        for param in self.edf_params:
            doc.appendElement('parameter name="{0}" value="{1}"'.format(param, self.edf_params[param]))
        doc.endElement('edf')

#
# surface shader class --
#

class SurfaceShader():
    def __init__(self, name, model, surface_shader_params=None):
        self.name = name
        self.model = model
        self.surface_shader_params = surface_shader_params

    def writeXML(self, doc):
        doc.startElement('surface_shader name="{0}" model="{1}"'.format(self.name, self.model))
        if self.model == 'constant_surface_shader':
            for param in self.surface_shader_params:
                doc.appendElement('parameter name="{0}" value="{1}"'.format(param, self.surface_shader_params[param]))
        doc.endElement('surface_shader')


#
# camera class --
#

class Camera(): #(camera_name)
    def __init__(self, params, cam):
        self.params = params
        if self.params['sceneCameraDefaultThinLens'] or cmds.getAttr(cam+'.depthOfField'):
            self.model = 'thinlens_camera'
            self.f_stop = cmds.getAttr(cam+'.fStop')
            self.focal_distance = cmds.getAttr(cam+'.focusDistance')
            self.diaphram_blades = 0
            self.diaphram_tilt_angle = 0.0
        else:
            self.model = 'pinhole_camera'
        self.name = cam

        maya_resolution_aspect = float(params['outputResWidth'])/float(params['outputResHeight'])
        maya_film_aspect = cmds.getAttr(cam + '.horizontalFilmAperture') / cmds.getAttr(cam + '.verticalFilmAperture')

        if maya_resolution_aspect > maya_film_aspect:
            self.film_width = float(cmds.getAttr(self.name + '.horizontalFilmAperture')) * inch_to_meter
            self.film_height = self.film_width / maya_resolution_aspect  
        else:
            self.film_height = float(cmds.getAttr(self.name + '.verticalFilmAperture')) * inch_to_meter
            self.film_width = self.film_height * maya_resolution_aspect 



        self.focal_length = float(cmds.getAttr(self.name+'.focalLength')) / 1000
        # transpose camera matrix -> XXX0, YYY0, ZZZ0, XYZ1
        m = cmds.getAttr(cam+'.matrix')
        self.transform = [m[0],m[1],m[2],m[3]], [m[4],m[5],m[6],m[7]], [m[8],m[9],m[10],m[11]], [m[12],m[13],m[14],m[15]]
   
    def writeXML(self, doc):
        print('writing camera: {0}'.format(self.name))
        doc.startElement('camera name="{0}" model="{1}"'.format(self.name, self.model))
        doc.appendElement('parameter name="film_dimensions" value="{0} {1}"'.format(self.film_width, self.film_height))
        doc.appendElement('parameter name="focal_length" value="{0}"'.format(self.focal_length))
        if self.model == 'thinlens_camera':
            print('exporting ' + self.name + ' as thinlens camera')
            doc.appendElement('parameter name="focal_distance" value="{0}"'.format(self.focal_distance))
            doc.appendElement('parameter name="f_stop" value="{0}"'.format(self.f_stop))
            doc.appendElement('parameter name="diaphragm_blades" value="{0}"'.format(self.diaphram_blades))
            doc.appendElement('parameter name="diaphragm_tilt_angle" value="{0}"'.format(self.diaphram_tilt_angle))
        #output transform matrix
        writeTransform(doc, self.params['scene_scale'], self.transform)
        doc.endElement('camera')


#
# environment class --
#

class Environment():
    def __init__(self, params, name, shader, edf):
        self.params = params
        self.name = name
        self.environment_shader = shader
        self.environment_edf = edf
    def writeXML(self, doc):
        print('writing environment: ' + self.name)
        doc.startElement('environment name="{0}" model="generic_environment"'.format(self.name))
        doc.appendElement('parameter name="environment_edf" value="{0}"'.format(self.environment_edf))
        doc.appendElement('parameter name="environment_shader" value="{0}"'.format(self.environment_shader))
        doc.endElement('environment')

#
# environment shader class --
#

class EnvironmentShader():
    def __init__(self, name, edf):
        self.name = name
        self.edf = edf
    def writeXML(self, doc):
        print('writing environment shader: ' + self.name)
        doc.startElement('environment_shader name="{0}" model="edf_environment_shader"'.format(self.name))
        doc.appendElement('parameter name="environment_edf" value="{0}"'.format(self.edf))
        doc.endElement('environment_shader')

#
# environment edf class --
#

class EnvironmentEdf():
    def __init__(self, name, model, edf_params):
        self.name = name
        self.model = model
        self.edf_params = edf_params
    def writeXML(self, doc):
        print('writing environment edf: ' + self.name)
        doc.startElement('environment_edf name="{0}" model="{1}"'.format(self.name, self.model))
        for param in self.edf_params:
            doc.appendElement('parameter name ="{0}" value="{1}"'.format(param, self.edf_params[param]))
        doc.endElement('environment_edf')

#
# geometry class --
#

class Geometry():
    def __init__(self, params, name, output_file, assembly='main_assembly'):
        self.params = params
        self.name = name
        #get name in heirarchy
        self.heirarchy_name = name

        current_object = name
        while cmds.listRelatives(current_object, parent=True):
            current_object = cmds.listRelatives(current_object, parent=True)[0]
            self.heirarchy_name = current_object + ' ' + self.heirarchy_name
        self.output_file = output_file
        self.assembly = assembly
        # get material name
        shape = cmds.listRelatives(self.name, s=True)[0]
        shadingEngine = None
        if not (cmds.listConnections(shape, t='shadingEngine')):
            cmds.error('no shader connected to ' + shape)
        else:
            shadingEngine = cmds.listConnections(shape, t='shadingEngine')[0]
        self.material = cmds.connectionInfo((shadingEngine + ".surfaceShader"),sourceFromDestination=True).split('.')[0] #find the attribute the surface shader is plugged into athen split off the attribute name to leave the shader name
       
        # transpose camera matrix -> XXX0, YYY0, ZZZ0, XYZ1
        m = cmds.getAttr(name+'.matrix')
        self.transform = [m[0],m[1],m[2],m[3]], [m[4],m[5],m[6],m[7]], [m[8],m[9],m[10],m[11]], [m[12],m[13],m[14],m[15]]
        

    def writeXMLInstance(self, doc):
        print('writing objecct instance: '+self.name)
        doc.startElement('object_instance name="{0}.0_inst" object="{1}.0"'.format(self.name, self.name))
        writeTransform(doc)
        doc.appendElement('assign_material slot="0" side="front" material="{0}"'.format(self.material))
        if self.params['matDoubleShade']:
            doc.appendElement('assign_material slot="0" side="back" material="{0}"'.format(self.material))
        doc.endElement('object_instance')
#
# assembly object --
#

class Assembly():
    def __init__(self, params, name='main_assembly'):
        self.params = params
        self.name = name
        self.light_objects = []
        self.geo_objects = dict()
        self.material_objects = dict()
        self.color_objects = dict()
        self.texture_objects = dict()
        self.surface_shader_objects = dict()
        self.bsdf_objects = dict()
        self.edf_objects = dict()

        #if name is default populate list with all lights otherwise just lights from set with the same name as the object
        if (self.name == 'main_assembly'):
            for light_shape in cmds.ls(lights=True):
                self.light_objects.append(Light(self.params, cmds.listRelatives(light_shape, ad=True, ap=True)[0]))
        else:
            for light_shape in cmds.listRelatives(self.name, typ='light'):
                self.light_objects.append(Light(self.params, cmds.listRelatives(light_shape, ad=True, ap=True)[0]))
        #add light colors to list
        for light_object in self.light_objects:
            self.addColor(light_object.color_name, light_object.color, light_object.multiplier)
        
        if not len(self.light_objects):
            cmds.warning('no light present in: ' + self.name)
        
        #if name is default populate list with all geometry otherwise just geometry from set with the same name as the object
        if (self.name == 'main_assembly'):
            #create a list of all geometry objects and itterate over them
            for geo in cmds.ls(typ='mesh'):
                if (ms_commands.shapeIsExportable(geo) and ms_commands.hasShaderConnected(geo)):
                    geo_transform = cmds.listRelatives(geo, ad=True, ap=True)[0]
                    if not (geo_transform in self.geo_objects):
                        self.geo_objects[geo_transform] = Geometry(self.params, geo_transform, ('geo/'+self.name+'.obj'), self.name)
        else:
            for geo in cmds.listConnections(self.name, sh=True):
                if (ms_commands.shapeIsExportable(geo) and ms_commands.hasShaderConnected(geo)):
                    geo_transform = cmds.listRelatives(geo, ad=True, ap=True)[0]
                    if not geo_transform in self.geo_objects:
                        self.geo_objects[geo_transform] = Geometry(self.params, geo_transform, ('geo/'+self.name+'.obj'), self.name)
                
        #populate list with individual materials
        for geo in self.geo_objects:
            self.addMaterial(self.geo_objects[geo].material)
        #if there are no objects in the scene raise error
        if not len(self.geo_objects):
            cmds.error('no objects present in ' + self.name)
            raise RuntimeError('no objects present in ' + self.name)

    def addColor(self, name, value, multiplier=1):
        if not name in self.color_objects:
            self.color_objects[name] = Color(name, value, multiplier)

    def addTexture(self, name, file_name):
        if not name in self.texture_objects:
            self.texture_objects[name] = Texture(name, file_name)
    
    def addEDF(self, material, type):
        edf_params = dict()
        if type == 'Diffuse':
            if material.edf_texture:
                self.addTexture(material.name + '_exitance', material.edf_texture)
                edf_params['exitance'] = (material.name + '_exitance_inst')
            else:
                self.addColor((material.name + '_exitance'), material.edf_color[0:3], material.edf_color[3])
                edf_params['exitance'] = material.name + '_exitance'
            self.edf_objects[material.name + '_edf'] = Edf(material.name + '_edf', 'diffuse_edf', edf_params)

    def addBSDF(self, material, type):
        print('translating {0} to {1} BSDF'.format(material.name, type))
        bsdf_params = dict()
        if type == 'Lambertian':
            #add reflectence component
            if material.bsdf_texture:
                self.addTexture(material.name + '_reflectance', material.bsdf_texture)
                bsdf_params['reflectance'] = (material.name + '_reflectance_inst')
            else:
                self.addColor((material.name + '_reflectance'), material.bsdf_color[0:3], material.bsdf_color[3])
                bsdf_params['reflectance'] = material.name + '_reflectance'
            self.bsdf_objects[material.name + '_bsdf'] = Bsdf(material.name + '_bsdf', 'lambertian_brdf', bsdf_params)

        elif type =='Ashikhmin-Shirley':
            bsdf_params['shininess_u'] = 0.5
            bsdf_params['shininess_v'] = 0.5
            # add diffuse reflectence component
            if material.bsdf_texture:
                self.addTexture(material.name + '_diffuse_reflectance', material.bsdf_texture)
                bsdf_params['diffuse_reflectance'] = (material.name + '_diffuse_reflectance_inst')
            else:
                self.addColor(material.name + '_diffuse_reflectance', material.bsdf_color[0:3], material.bsdf_color[3])
                bsdf_params['diffuse_reflectance'] = material.name + '_diffuse_reflectance'
            #add glossy reflectence component
            if material.specular_texture:
                self.addTexture(material.name + '_glossy_reflectance', material.specular_texture)
                bsdf_params['glossy_reflectance'] = material.name + '_glossy_reflectance'
            else:
                self.addColor(material.name + '_glossy_reflectance', material.specular_color[0:3], material.bsdf_color[3])
                bsdf_params['glossy_reflectance'] = material.name + '_glossy_reflectance'
            self.bsdf_objects[material.name + '_bsdf'] = Bsdf(material.name + '_bsdf', 'ashikhmin_brdf', bsdf_params)

        elif type == 'Kelemen':
            bsdf_params['roughness'] = 0.5
            #add matte_reflectance component
            if material.bsdf_texture:
                self.addTexture(material.name + '_matte_reflectance', material.bsdf_texture)
                bsdf_params['matte_reflectance'] = (material.name + '_matte_reflectance_inst')
            else:
                self.addColor(material.name + '_matte_reflectance', material.bsdf_color[0:3], material.bsdf_color[3])
                bsdf_params['matte_reflectance'] = material.name + '_matte_reflectance'
            #add specular_reflectance component
            if material.specular_texture:
                self.addTexture(material.name + '_specular_reflectance', material.specular_texture)
                bsdf_params['specular_reflectance'] = material.name + '_specular_reflectance'
            else:
                self.addColor(material.name + '_specular_reflectance', material.bsdf_color[0:3], material.bsdf_color[3])
                bsdf_params['specular_reflectance'] = material.name + '_specular_reflectance'
            self.bsdf_objects[material.name + '_bsdf'] = Bsdf(material.name + '_bsdf', 'kelemen_brdf', bsdf_params)

        elif type == 'Specular_BRDF':
            #add reflectence component
            if material.bsdf_texture:
                self.addTexture(material.name + '_reflectance', material.bsdf_texture)
                bsdf_params['reflectance'] = (material.name + '_reflectance_inst')
            else:
                self.addColor((material.name + '_reflectance'), material.bsdf_color[0:3], material.bsdf_color[3])
                bsdf_params['reflectance'] = material.name + '_reflectance'
            self.bsdf_objects[material.name + '_bsdf'] = Bsdf(material.name + '_bsdf', 'specular_brdf', bsdf_params)
        

    def addSurfaceShader(self, material, name, type): 
        if not name in self.surface_shader_objects:
            surface_shader_params = dict()
            model=None
            #check surface shader type and add appropriate parameters to dict
            if type == 'Physical':
                model = 'physical_surface_shader'
            elif type == 'Constant':
                model = 'constant_surface_shader'
                #if surface_shader is set to constant, take color or texture from material bsdf
                if material.bsdf_texture:
                    self.addTexture(material.name + '_surface_shader_color', material.bsdf_texture)
                    surface_shader_params['color'] = (material.name + '_surface_shader_color_inst ')
                else:
                    self.addColor((material.name + '_surface_shader_color'), material.bsdf_color[0:3], material.bsdf_color[3])
                    surface_shader_params['color'] = material.name + '_surface_shader_color'
            elif type == 'Ambient Occlusion':
                cmds.error('atempting to set {0} surface shader to ambient occlusion'.format(name))
                cmds.error('ambient occlusion not implimented yet')
            print('adding {0} surface shader to {0}'.format(type, name))
            self.surface_shader_objects[name] = SurfaceShader(name, model, surface_shader_params)


    def addMaterial(self, name):
        if not name in self.material_objects:
            self.material_objects[name] = Material(self.params, name)

            self.material_objects[name].bsdf = None
            self.material_objects[name].edf = None
            self.material_objects[name].surface_shader = None

            #if custom shader translation attribs exists use them
            if cmds.objExists(name + '.mayaseed_bsdf'):
                if not (cmds.getAttr(name+'.mayaseed_bsdf', asString=True) == '<None>'):
                    self.material_objects[name].bsdf = (name + '_bsdf')
                    self.addBSDF(self.material_objects[name], cmds.getAttr(name+'.mayaseed_bsdf', asString=True)) 
                if not (cmds.getAttr(name+'.mayaseed_edf', asString=True) == '<None>'):
                    self.material_objects[name].edf = (name + '_edf')
                    self.addEDF(self.material_objects[name], cmds.getAttr(name+'.mayaseed_edf', asString=True))  
                if not (cmds.getAttr(name+'.mayaseed_surface_shader', asString=True) == '<None>'):
                    self.material_objects[name].surface_shader = (name + '_surface_shader')
                    self.addSurfaceShader(self.material_objects[name], name + '_surface_shader', cmds.getAttr(name+'.mayaseed_surface_shader', asString=True))  

            else: #create bsdf,edf & surface shader based on defaults
                #create bsdf
                if self.material_objects[name].shader_type == 'lambert':
                    if not self.params['matLambertBSDF'] == 'None':
                        self.material_objects[name].bsdf = (name + '_bsdf')
                        self.addBSDF(self.material_objects[name], self.params['matLambertBSDF'])  
                          
                elif self.material_objects[name].shader_type == 'blinn':
                    if not self.params['matBlinnBSDF'] == 'None':
                        self.material_objects[name].bsdf = (name + '_bsdf')
                        self.addBSDF(self.material_objects[name], self.params['matBlinnBSDF'])   
                          
                elif self.material_objects[name].shader_type == 'phong' or self.material_objects[name].shader_type == 'phongE':
                    if not self.params['matPhongBSDF'] == 'None':
                        self.material_objects[name].bsdf = (name + '_bsdf')
                        self.addBSDF(self.material_objects[name], self.params['matPhongBSDF'])  
                          
                elif self.material_objects[name].shader_type == 'surfaceShader':
                    if not self.params['matSurfaceShaderBSDF'] == 'None':
                        self.material_objects[name].bsdf = (name + '_bsdf')
                        self.addBSDF(self.material_objects[name], self.params['matSurfaceShaderBSDF'])  
                          
                else:
                    if not self.params['matDefaultBSDF'] == 'None':
                        self.material_objects[name].bsdf = (name + '_bsdf')
                        self.addBSDF(self.material_objects[name], self.params['matDefaultBSDF'])  

                #create edf 
                if self.material_objects[name].shader_type == 'lambert':
                    if not self.params['matLambertEDF'] == 'None':
                        self.material_objects[name].edf = (name + '_edf')
                        self.addEDF(self.material_objects[name], self.params['matLambertEDF'])

                elif self.material_objects[name].shader_type == 'blinn':
                    if not self.params['matBlinnEDF'] == 'None':
                        self.material_objects[name].edf = (name + '_edf')
                        self.addEDF(self.material_objects[name], self.params['matBlinnEDF'])

                elif self.material_objects[name].shader_type == 'phong' or self.material_objects[name].shader_type == 'phongE':
                    if not self.params['matPhongEDF'] == 'None':
                        self.material_objects[name].edf = (name + '_edf')
                        self.addEDF(self.material_objects[name], self.params['matPhongEDF'])

                elif self.material_objects[name].shader_type == 'surfaceShader':
                    if not self.params['matSurfaceShaderEDF'] == 'None':
                        self.material_objects[name].edf = (name + '_edf')
                        self.addEDF(self.material_objects[name], self.params['matSurfaceShaderEDF'])
                else:
                    if not self.params['matDefaultEDF'] == 'None':
                        self.material_objects[name].edf = (name + '_edf')
                        self.addEDF(self.material_objects[name], self.params['matDefaultEDF'])

                #create surface
                if self.material_objects[name].shader_type == 'lambert':
                    if not self.params['matLambertSurfaceShader'] == 'None':   
                        if self.params['matLambertSurfaceShader'] == 'Physical':
                            self.material_objects[name].surface_shader = ('Physical_surface_shader')
                            self.addSurfaceShader(self.material_objects[name], 'Physical_surface_shader', self.params['matLambertSurfaceShader'])
                        else:
                            self.material_objects[name].surface_shader = (name + '_surface_shader')
                            self.addSurfaceShader(self.material_objects[name], name + '_surface_shader', self.params['matLambertSurfaceShader'])

                elif self.material_objects[name].shader_type == 'blinn':
                    if not self.params['matBlinnSurfaceShader'] == 'None':   
                        if self.params['matBlinnSurfaceShader'] == 'Physical':
                            self.material_objects[name].surface_shader = ('Physical_surface_shader')
                            self.addSurfaceShader(self.material_objects[name], 'Physical_surface_shader', self.params['matBlinnSurfaceShader'])
                        else:
                            self.material_objects[name].surface_shader = (name + '_surface_shader')
                            self.addSurfaceShader(self.material_objects[name], name + '_surface_shader', self.params['matBlinnSurfaceShader'])

                elif self.material_objects[name].shader_type == 'phong' or self.material_objects[name].shader_type == 'phongE':
                    if not self.params['matPhongSurfaceShader'] == 'None':   
                        if self.params['matPhongSurfaceShader'] == 'Physical':
                            self.material_objects[name].surface_shader = ('Physical_surface_shader')
                            self.addSurfaceShader(self.material_objects[name], 'Physical_surface_shader', self.params['matPhongSurfaceShader'])
                        else:
                            self.material_objects[name].surface_shader = (name + '_surface_shader')
                            self.addSurfaceShader(self.material_objects[name], name + '_surface_shader', self.params['matPhongSurfaceShader'])

                elif self.material_objects[name].shader_type == 'surfaceShader':
                    if not self.params['matSurfaceShaderSurfaceShader'] == 'None':   
                        if self.params['matSurfaceShaderSurfaceShader'] == 'Physical':
                            self.material_objects[name].surface_shader = ('Physical_surface_shader')
                            self.addSurfaceShader(self.material_objects[name], 'Physical_surface_shader', self.params['matSurfaceShaderSurfaceShader'])
                        else:
                            self.material_objects[name].surface_shader = (name + '_surface_shader')
                            self.addSurfaceShader(self.material_objects[name], name + '_surface_shader', self.params['matSurfaceShaderSurfaceShader'])

                else:
                    if not self.params['matDefaultSurfaceShader'] == 'None':   
                        if self.params['matDefaultSurfaceShader'] == 'Physical':
                            self.material_objects[name].surface_shader = ('Physical_surface_shader')
                            self.addSurfaceShader(self.material_objects[name], 'Physical_surface_shader', self.params['matDefaultSurfaceShader'])
                        else:
                            self.material_objects[name].surface_shader = (name + '_surface_shader')
                            self.addSurfaceShader(self.material_objects[name], name + '_surface_shader', self.params['matDefaultSurfaceShader'])


        
    def writeXML(self, doc):
        print('writing assembly: {0}'.format(self.name))
        doc.startElement('assembly name="{0}"'.format(self.name))

        #write colors
        for col in self.color_objects:
             self.color_objects[col].writeXML(doc)

        #write texture objects
        for tex in self.texture_objects:
            self.texture_objects[tex].writeXMLObject(doc)
        #write texture instances
        for tex in self.texture_objects:
            self.texture_objects[tex].writeXMLInstance(doc)

        #write bsdfs
        for bsdf in self.bsdf_objects:
            self.bsdf_objects[bsdf].writeXML(doc)

        #write eddfs
        for edf in self.edf_objects:
            self.edf_objects[edf].writeXML(doc)

        #write surface shaders
        for surface_shader in self.surface_shader_objects:
            self.surface_shader_objects[surface_shader].writeXML(doc)

        #write materials
        for material in self.material_objects:
            self.material_objects[material].writeXML(doc)
        
        #export and write .obj object
        for geo in self.geo_objects:
            #export geo
            cmds.select(geo)
            output_file = os.path.join(self.params['outputDir'], 'geo', (geo + '.obj'))
            print geo
            print output_file
            cmds.file(output_file, force=True, options='groups=0;ptgroups=0;materials=0;smoothing=0;normals=1', typ='OBJexport',pr=True, es=True)
            cmds.select(cl=True)
            #write xml
            doc.startElement('object name="{0}" model="mesh_object"'.format(geo))
            doc.appendElement('parameter name="filename" value="geo/{0}.obj"'.format(geo))
            doc.endElement('object')
        
        #write lights
        for light_name in self.light_objects:
            light_name.writeXML(doc)
        
        #write geo object instances
        for geo in self.geo_objects:
            self.geo_objects[geo].writeXMLInstance(doc)
        
        doc.endElement('assembly')
        doc.startElement('assembly_instance name="{0}_inst" assembly="{1}"'.format(self.name, self.name))
        writeTransform(doc, self.params['scene_scale'])
        doc.endElement('assembly_instance')

#
# scene class --
#

class Scene():
    def __init__(self,params):
        self.params = params
        self.assembly_list = []
        self.color_objects = dict()
        self.texture_objects = dict()

        #setup environment 
        if self.params['environment']:
            self.environment = Environment(self.params, self.params['environment'], (self.params['environment'] + '_env_shader'), (self.params['environment'] + '_env_edf'))

            #retrieve model and param values from ui
            environment_edf_model_enum = cmds.getAttr(self.params['environment'] + '.model')
            env_edf_params = dict()
            if environment_edf_model_enum == 0:
                environment_edf_model = 'constant_environment_edf'
                environment_color = ms_commands.normalizeRGB(cmds.getAttr(self.params['environment']+'.constant_exitance')[0])
                self.addColor('constant_env_exitance', environment_color[0:3], environment_color[3])
                env_edf_params['exitance'] =  'constant_env_exitance'

            elif environment_edf_model_enum == 1:
                environment_edf_model = 'gradient_environment_edf'

                horizon_exitance = ms_commands.normalizeRGB(cmds.getAttr(self.params['environment']+'.gradient_horizon_exitance')[0])
                self.addColor('gradient_env_horizon_exitance', horizon_exitance[0:3], horizon_exitance[3])

                zenith_exitance = ms_commands.normalizeRGB(cmds.getAttr(self.params['environment']+'.gradient_zenith_exitance')[0])
                self.addColor('gradient_env_zenith_exitance', zenith_exitance[0:3], zenith_exitance[3])

                env_edf_params['horizon_exitance'] = 'gradient_env_horizon_exitance'
                env_edf_params['zenith_exitance'] = 'gradient_env_zenith_exitance'

            elif environment_edf_model_enum == 2:
                lat_long_connection = cmds.connectionInfo((self.params['environment'] + '.latitude_longitude_exitance'), sourceFromDestination=True).split('.')[0]
                environment_edf_model = 'latlong_map_environment_edf'
                if lat_long_connection:
                    if cmds.nodeType(lat_long_connection) == 'file':
                        self.addTexture(self.params['environment'] + '_latlong_edf_map', cmds.getAttr(lat_long_connection + '.fileTextureName'))
                        env_edf_params['exitance'] = self.params['environment'] + '_latlong_edf_map_inst'
                        env_edf_params['horizontal_shift'] = 0
                        env_edf_params['vertical_shift'] = 0
                else:
                    cmds.error('no texture connected to {0}.latitude_longitude_exitance'.format(self.params['environment']))

            elif environment_edf_model_enum == 3:
                mirrorball_edf_connection = cmds.connectionInfo((self.params['environment'] + '.mirror_ball_exitance'), sourceFromDestination=True).split('.')[0]
                environment_edf_model = 'mirrorball_map_environment_edf'
                if mirrorball_edf_connection:
                    if cmds.nodeType(mirrorball_edf_connection) == 'file':
                        self.addTexture(self.params['environment'] + '_mirrorball_map_environment_edf', cmds.getAttr(mirrorball_edf_connection + '.fileTextureName'))
                        env_edf_params['exitance'] = self.params['environment'] + '_mirrorball_map_environment_edf_inst'
                else:
                    cmds.error('no texture connected to {0}.mirrorball_exitance'.format(self.params['environment']))

            else:
                cmds.error('no environment model selected for ' + self.params['environment'])
            
            self.environment_edf = EnvironmentEdf((self.params['environment'] + '_env_edf'), environment_edf_model, env_edf_params)
            self.environment_shader = EnvironmentShader((self.params['environment'] + '_env_shader'), (self.params['environment'] + '_env_edf'))

        else:
            self.environment = None

    def addColor(self, name, value, multiplier=1):
        if not name in self.color_objects:
            self.color_objects[name] = Color(name, value, multiplier)

    def addTexture(self, name, file_name):
        if not name in self.texture_objects:
            self.texture_objects[name] = Texture(name, file_name)

    def writeXML(self, doc):
        print('writing scene element')
        doc.startElement('scene')

        #write current camera
        camera_instance = Camera(self.params, self.params['outputCamera'])
        camera_instance.writeXML(doc)   
             
        #write colors
        for col in self.color_objects:
             self.color_objects[col].writeXML(doc)
             
        #write texture objects
        for tex in self.texture_objects:
            self.texture_objects[tex].writeXMLObject(doc)

        #write texture instances
        for tex in self.texture_objects:
            self.texture_objects[tex].writeXMLInstance(doc)
        
        #if tehre is an environment write it
        if self.environment:
            self.environment_edf.writeXML(doc)
            self.environment_shader.writeXML(doc)
            self.environment.writeXML(doc)

        #export assemblies
        #get maya geometry
        shape_list = cmds.ls(g=True, v=True) 
        geo_transform_list = []
        for g in shape_list:
            # add first connected transform to the list
            geo_transform_list.append(cmds.listRelatives(g, ad=True, ap=True)[0]) 

        #populate a list of assemblies
        for g in geo_transform_list:
            set = cmds.listSets(object=g)
            if set:
                if not set[0] in self.assembly_list:
                    self.assembly_list.append(cmds.listSets(object=g)[0])
        
        #create assemblys if any assembly names are present otherwise create default assembly
        if self.assembly_list:
            #for each assemply in assembly_list create an object and output its XML
            for assembly in self.assembly_list:
                new_assembly = Assembly(self.params, assembly)
                new_assembly.writeXML(doc)
        else:
            print('no populated maya sets present, using default "main_assembly"')
            new_assembly = Assembly(self.params, 'main_assembly')
            new_assembly.writeXML(doc)
        doc.endElement('scene')

#
# output class --
#

class Output():
    def __init__(self, params):
        self.params = params
    def writeXML(self, doc):
        doc.startElement('output')
        doc.startElement('frame name="beauty"')
        doc.appendElement('parameter name="camera" value="{0}"'.format(self.params['outputCamera']))
        doc.appendElement('parameter name="color_space" value="{0}"'.format(self.params['outputColorSpace']))
        doc.appendElement('parameter name="resolution" value="{0} {1}"'.format(self.params['outputResWidth'], self.params['outputResHeight']))
        doc.endElement('frame')
        doc.endElement('output')

#
# configurations class --
#

class Configurations():
    def __init__(self, params):
        self.params = params
    def writeXML(self,doc):
        doc.startElement("configurations")
        print('writing configurations')
        #if 'customise interactive configuration' is set read customised values
        if self.params['customInteractiveConfigCheck']:
            print('writing custom interactive config')
            doc.startElement('configuration name="interactive" base="base_interactive"')
            engine = ''
            if self.params['customInteractiveConfigEngine'] == 'Path Tracing':
                engine = "pt"
            else:
                engine = "drt"
            doc.appendElement('parameter name="lighting_engine" value="{0}"'.format(engine))
            doc.startElement('parameters name="{0}"'.format(engine))
            doc.appendElement('parameter name="max_path_length" value="{0}"'.format(self.params['customInteractiveConfigMaxRayDepth']))
            doc.endElement('parameters')
            doc.appendElement('parameter name="min_samples" value="{0}"'.format(self.params['customInteractiveConfigMinSamples']))
            doc.appendElement('parameter name="max_samples" value="{0}"'.format(self.params['customInteractiveConfigMaxSamples']))
            doc.endElement('configuration')
        else:# otherwise add default configurations
            print('writing default interactive config')
            doc.appendElement('configuration name="interactive" base="base_interactive"')

        #if 'customise final configuration' is set read customised values
        if self.params['customFinalConfigCheck']:
            print('writing custom final config')
            doc.startElement('configuration name="final" base="base_final"')
            engine = ''
            if cmds.optionMenu('ms_customFinalConfigEngine', query=True, value=True) == "Path Tracing":
                engine = 'pt'
            else:
                engine = 'drt'
            doc.appendElement('parameter name="lighting_engine" value="{0}"'.format(engine))
            doc.startElement('parameters name="{0}"'.format(engine))
            doc.appendElement('parameter name="max_path_length" value="{0}"'.format(self.params['customFinalConfigMaxRayDepth']))
            doc.endElement('parameters')
            doc.appendElement('parameter name="min_samples" value="{0}"'.format(self.params['customFinalConfigMinSamples']))
            doc.appendElement('parameter name="max_samples" value="{0}"'.format(self.params['customFinalConfigMaxSamples']))
            doc.endElement("configuration")
        else:# otherwise add default configurations
            print('writing default final config')
            doc.appendElement('configuration name="final" base="base_final"')
        # begin adding custom configurations
        doc.endElement('configurations')
	

#****************************************************************************************************************************************************************************************************
# export function ***********************************************************************************************************************************************************************************
#****************************************************************************************************************************************************************************************************

def export(render_settings_node):
    params = getMayaParams(render_settings_node)
    if not params['error']:
        #make output directories if they dont exists
        if not os.path.exists(os.path.join(params['outputDir'],'temp')):
            os.makedirs(os.path.join(params['outputDir'],'temp'))
        if not os.path.exists(os.path.join(params['outputDir'],'geo')):
            os.makedirs(os.path.join(params['outputDir'],'geo'))
        if not os.path.exists(os.path.join(params['outputDir'],'textures')):
            os.makedirs(os.path.join(params['outputDir'],'textures'))

        #begin export
        print('beginning export')
        print('opening output file: ' + params['fileName'])
        doc = WriteXml('{0}/{1}'.format(params['outputDir'], params['fileName'].replace("#",'{0:05}'.format(int(cmds.currentTime(query=True))))))
        doc.appendLine('<?xml version="1.0" encoding="UTF-8"?>') # XML format string
        doc.appendLine('<!-- File generated by mayaseed version {0} -->'.format(ms_commands.MAYASEED_VERSION))
        print('writing project element')
        doc.startElement('project')
        scene_element = Scene(params)
        scene_element.writeXML(doc)
        output_element = Output(params)
        output_element.writeXML(doc)
        config_element = Configurations(params)
        config_element.writeXML(doc)
    
        doc.endElement('project')
        doc.close()
        print('export finished')


    else:
        cmds.error('error validating ui attributes ')
        raise RuntimeError('check script editor for details')






