
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
import maya.OpenMayaRender as OpenMayaRender
import maya.OpenMayaUI as OpenMayaUI
import maya.cmds as cmds
import inspect
import os
import os.path

ROOT_DIRECTORY = os.path.split((os.path.dirname(inspect.getfile(inspect.currentframe()))))[0]
sys.path.append(os.path.join(ROOT_DIRECTORY, 'scripts'))
sys.path.append(os.path.join(ROOT_DIRECTORY, 'graphics'))

import ms_menu

#--------------------------------------------------------------------------------------------------
# ms_renderSettings node.
#--------------------------------------------------------------------------------------------------

ms_renderSettings_nodeTypeName = "ms_renderSettings"
ms_renderSettings_nodeTypeId = OpenMaya.MTypeId(0x00333)

class ms_renderSettings(OpenMayaMPx.MPxNode):
    def __init__(self):
        OpenMayaMPx.MPxNode.__init__(self)

def ms_renderSettings_nodeCreator():
    return OpenMayaMPx.asMPxPtr(ms_renderSettings())

def ms_renderSettings_nodeInitializer():
    # define attributes
    #  output directory
    output_dir_string = OpenMaya.MFnStringData().create(os.path.join("<ProjectDir>Mayaseed", "<SceneName>"))
    output_dir_Attr = OpenMaya.MFnTypedAttribute()
    ms_renderSettings.output_dir = output_dir_Attr.create("output_directory", "out_dir", OpenMaya.MFnData.kString, output_dir_string)
    ms_renderSettings.addAttribute(ms_renderSettings.output_dir)

    #  output file
    output_file_string = OpenMaya.MFnStringData().create("<SceneName>.#.appleseed")
    output_file_Attr = OpenMaya.MFnTypedAttribute()
    ms_renderSettings.output_file = output_file_Attr.create("output_file", "out_file", OpenMaya.MFnData.kString, output_file_string)  
    ms_renderSettings.addAttribute(ms_renderSettings.output_file)

    # export maya lights
    export_maya_lights_nAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.export_maya_lights = export_maya_lights_nAttr.create("export_maya_lights", "export_maya_lights", OpenMaya.MFnNumericData.kBoolean, True)
    ms_renderSettings.addAttribute(ms_renderSettings.export_maya_lights)

    # convert textures to exr
    convert_textures_to_exr_nAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.convert_textures_to_exr = convert_textures_to_exr_nAttr.create("convert_textures_to_exr", "convert_tex_to_exr", OpenMaya.MFnNumericData.kBoolean, True)
    ms_renderSettings.addAttribute(ms_renderSettings.convert_textures_to_exr)

    # convert shading nodes to textures
    convert_shading_nodes_to_textures_nAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.convert_shading_nodes_to_textures = convert_shading_nodes_to_textures_nAttr.create("convert_shading_nodes_to_textures", "convert_shading_nodes", OpenMaya.MFnNumericData.kBoolean, True)
    ms_renderSettings.addAttribute(ms_renderSettings.convert_shading_nodes_to_textures)

    #  overwrite existing textures
    overwrite_existing_textures_nAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.overwrite_existing_textures = overwrite_existing_textures_nAttr.create("overwrite_existing_textures", "overwrite_exrs", OpenMaya.MFnNumericData.kBoolean, True)
    ms_renderSettings.addAttribute(ms_renderSettings.overwrite_existing_textures)

    #  overwrite existing geometry
    overwrite_existing_geometry_nAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.overwrite_existing_geometry = overwrite_existing_geometry_nAttr.create("overwrite_existing_geometry", "overwrite_geo", OpenMaya.MFnNumericData.kBoolean, True)
    ms_renderSettings.addAttribute(ms_renderSettings.overwrite_existing_geometry)

    # export camera blur
    export_camera_blur_nAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.export_camera_blur = export_camera_blur_nAttr.create("export_camera_blur", "camera_blur", OpenMaya.MFnNumericData.kBoolean, False)
    ms_renderSettings.addAttribute(ms_renderSettings.export_camera_blur)

    # export transformation blur
    export_transformation_blur_nAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.export_transformation_blur = export_transformation_blur_nAttr.create("export_transformation_blur", "transformation_blur", OpenMaya.MFnNumericData.kBoolean, False)
    ms_renderSettings.addAttribute(ms_renderSettings.export_transformation_blur)

    # export deformation blur
    export_deformation_blur_nAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.export_deformation_blur = export_deformation_blur_nAttr.create("export_deformation_blur", "deformation_blur", OpenMaya.MFnNumericData.kBoolean, False)
    ms_renderSettings.addAttribute(ms_renderSettings.export_deformation_blur)

    # motion samples
    motion_samples_AttrInt = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.motion_samples = motion_samples_AttrInt.create("motion_samples", "motion_samples", OpenMaya.MFnNumericData.kInt, 2)
    motion_samples_AttrInt.setHidden(False)
    motion_samples_AttrInt.setKeyable(True)
    ms_renderSettings.addAttribute(ms_renderSettings.motion_samples)

    # shutter open time
    shutter_open_AttrFloat = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.shutter_open_time = shutter_open_AttrFloat.create("shutter_open_time", "shutter_open_time", OpenMaya.MFnNumericData.kFloat, 0)
    shutter_open_AttrFloat.setHidden(False)
    shutter_open_AttrFloat.setKeyable(True)
    ms_renderSettings.addAttribute(ms_renderSettings.shutter_open_time)

    # shutter close time
    shutter_close_AttrFloat = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.shutter_close_time = shutter_close_AttrFloat.create("shutter_close_time", "shutter_close_time", OpenMaya.MFnNumericData.kFloat, 1)
    shutter_close_AttrFloat.setHidden(False)
    shutter_close_AttrFloat.setKeyable(True)
    ms_renderSettings.addAttribute(ms_renderSettings.shutter_close_time)

    # export animation
    export_animation_nAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.export_animation = export_animation_nAttr.create("export_animation", "export_animation", OpenMaya.MFnNumericData.kBoolean, False)
    ms_renderSettings.addAttribute(ms_renderSettings.export_animation)

    # start_frame
    start_frame_AttrInt = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.start_frame = start_frame_AttrInt.create("animation_start_frame", "start_frame", OpenMaya.MFnNumericData.kInt, 1)
    start_frame_AttrInt.setHidden(False)
    start_frame_AttrInt.setKeyable(False)
    ms_renderSettings.addAttribute(ms_renderSettings.start_frame)

    # end frame
    end_frame_AttrInt = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.end_frame = end_frame_AttrInt.create("animation_end_frame", "end_frame", OpenMaya.MFnNumericData.kInt, 100)
    end_frame_AttrInt.setHidden(False)
    end_frame_AttrInt.setKeyable(False)
    ms_renderSettings.addAttribute(ms_renderSettings.end_frame)

    # export animated textures
    export_animated_textures_nAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.export_animated_textures = export_animated_textures_nAttr .create("export_animated_textures", "animated_textures", OpenMaya.MFnNumericData.kBoolean, False)
    ms_renderSettings.addAttribute(ms_renderSettings.export_animated_textures)

    # environent -----------------------------------------------
    # environment message
    environment_msgAttr = OpenMaya.MFnMessageAttribute()
    ms_renderSettings.environment = environment_msgAttr.create("environment", "env")   
    ms_renderSettings.addAttribute(ms_renderSettings.environment)

    # cameras --------------------------------------------------
    # export all cameras as thin lens bool attribute
    export_all_cameras_as_thin_lens_nAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.export_all_cameras_as_thin_lens = export_all_cameras_as_thin_lens_nAttr.create("export_all_cameras_as_thin_lens", "export_thinlens", OpenMaya.MFnNumericData.kBoolean)
    ms_renderSettings.addAttribute(ms_renderSettings.export_all_cameras_as_thin_lens)

    # output ---------------------------------------------------
    # camera
    camera_msgAttr = OpenMaya.MFnMessageAttribute()
    ms_renderSettings.camera = camera_msgAttr.create("camera", "cam")
    ms_renderSettings.addAttribute(ms_renderSettings.camera)

    # color space
    color_space_enumAttr = OpenMaya.MFnEnumAttribute()
    ms_renderSettings.color_space = color_space_enumAttr.create("color_space", "col_space")
    color_space_enumAttr.addField("sRGB", 0)
    color_space_enumAttr.addField("Linear RGB", 1)
    color_space_enumAttr.addField("Spectral", 2)
    color_space_enumAttr.addField("ciexyz", 3)
    ms_renderSettings.addAttribute(ms_renderSettings.color_space)

    # resolution width
    width_AttrInt = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.width = width_AttrInt.create("frame_width", "width", OpenMaya.MFnNumericData.kInt, 1280)
    width_AttrInt.setHidden(False)
    width_AttrInt.setKeyable(True)
    ms_renderSettings.addAttribute(ms_renderSettings.width)

    # resolution height
    height_AttrInt = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.height = height_AttrInt.create("frame_height", "height", OpenMaya.MFnNumericData.kInt, 720)
    height_AttrInt.setHidden(False)
    height_AttrInt.setKeyable(True)
    ms_renderSettings.addAttribute(ms_renderSettings.height)

    # export straight
    export_straight_alpha_nAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.export_straight_alpha = export_straight_alpha_nAttr.create("export_straight_alpha", "export_straight", OpenMaya.MFnNumericData.kBoolean, True)
    ms_renderSettings.addAttribute(ms_renderSettings.export_straight_alpha)

    # sampler
    sampler_enumAttr = OpenMaya.MFnEnumAttribute()
    ms_renderSettings.sampler = sampler_enumAttr.create("sampler", "sampler")
    sampler_enumAttr.addField("Adaptive", 0)
    sampler_enumAttr.addField("Uniform", 1)
    ms_renderSettings.addAttribute(ms_renderSettings.sampler)

    # uniform sampler
    # uniform_samples
    uniform_samples_AttrInt = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.uniform_samples = uniform_samples_AttrInt.create("uniform_samples", "uniform_samples", OpenMaya.MFnNumericData.kInt, 64)
    uniform_samples_AttrInt.setHidden(False)
    uniform_samples_AttrInt.setKeyable(True)
    ms_renderSettings.addAttribute(ms_renderSettings.uniform_samples)

    # decorrelate_pixel
    uniform_decorrelate_pixelsnAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.uniform_decorrelate_pixels = uniform_decorrelate_pixelsnAttr.create("uniform_decorrelate_pixels", "uniform_decorrelate_pixels", OpenMaya.MFnNumericData.kBoolean, True)
    ms_renderSettings.addAttribute(ms_renderSettings.uniform_decorrelate_pixels)

    # adaptive_sampler
    # adaptive_min_samples
    adaptive_min_samples_AttrInt = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.adaptive_min_samples = adaptive_min_samples_AttrInt.create("adaptive_min_samples", "adaptive_min_samples", OpenMaya.MFnNumericData.kInt, 16)
    adaptive_min_samples_AttrInt.setHidden(False)
    adaptive_min_samples_AttrInt.setKeyable(True)
    ms_renderSettings.addAttribute(ms_renderSettings.adaptive_min_samples)

    # adaptive_max_samples
    adaptive_max_samples_AttrInt = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.adaptive_max_samples = adaptive_max_samples_AttrInt.create("adaptive_max_samples", "adaptive_max_samples", OpenMaya.MFnNumericData.kInt, 128)
    adaptive_max_samples_AttrInt.setHidden(False)
    adaptive_max_samples_AttrInt.setKeyable(True)
    ms_renderSettings.addAttribute(ms_renderSettings.adaptive_max_samples)

    # adaptive_quality
    adaptive_quality_AttrFloat = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.adaptive_quality = adaptive_quality_AttrFloat.create("adaptive_quality", "adaptive_quality", OpenMaya.MFnNumericData.kFloat, 3)
    adaptive_quality_AttrFloat.setHidden(False)
    adaptive_quality_AttrFloat.setKeyable(True)
    ms_renderSettings.addAttribute(ms_renderSettings.adaptive_quality)


    # enable_ibl
    pt_iblnAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.pt_ibl = pt_iblnAttr.create("pt_ibl", "pt_ibl", OpenMaya.MFnNumericData.kBoolean, True)
    ms_renderSettings.addAttribute(ms_renderSettings.pt_ibl)

    # enable_caustics
    pt_causticsnAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.pt_caustics = pt_causticsnAttr.create("pt_caustics", "pt_caustics", OpenMaya.MFnNumericData.kBoolean, False)
    ms_renderSettings.addAttribute(ms_renderSettings.pt_caustics)

    # enable_dl
    pt_enable_dlnAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.pt_direct_lighting = pt_enable_dlnAttr.create("pt_direct_lighting", "pt_direct_lighting", OpenMaya.MFnNumericData.kBoolean, True)
    ms_renderSettings.addAttribute(ms_renderSettings.pt_direct_lighting)

    # next_event_estimation
    pt_next_event_estimationnAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.pt_next_event_estimation = pt_next_event_estimationnAttr.create("pt_next_event_estimation", "pt_next_event_estimation", OpenMaya.MFnNumericData.kBoolean, True)
    ms_renderSettings.addAttribute(ms_renderSettings.pt_next_event_estimation)

    # pt_max_bounces
    pt_max_bounces_AttrFloat = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.pt_max_bounces = pt_max_bounces_AttrFloat.create("pt_max_bounces", "pt_max_bounces", OpenMaya.MFnNumericData.kInt, 4)
    pt_max_bounces_AttrFloat.setHidden(False)
    pt_max_bounces_AttrFloat.setKeyable(True)
    ms_renderSettings.addAttribute(ms_renderSettings.pt_max_bounces)

    # pt_light_samples
    pt_light_samples_AttrFloat = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.pt_light_samples = pt_light_samples_AttrFloat.create("pt_light_samples", "pt_light_samples", OpenMaya.MFnNumericData.kInt, 1)
    pt_light_samples_AttrFloat.setHidden(False)
    pt_light_samples_AttrFloat.setKeyable(True)
    ms_renderSettings.addAttribute(ms_renderSettings.pt_light_samples)

    # pt_environment_samples
    pt_environment_samples_AttrFloat = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.pt_environment_samples = pt_environment_samples_AttrFloat.create("pt_environment_samples", "pt_environment_samples", OpenMaya.MFnNumericData.kInt, 1)
    pt_environment_samples_AttrFloat.setHidden(False)
    pt_environment_samples_AttrFloat.setKeyable(True)
    ms_renderSettings.addAttribute(ms_renderSettings.pt_environment_samples)

    # profile export
    profile_export_nAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.profile_export = profile_export_nAttr.create("profile_export", "profile_export", OpenMaya.MFnNumericData.kBoolean, False)
    ms_renderSettings.addAttribute(ms_renderSettings.profile_export)

    # autodetect alpha
    autodetect_alpha_nAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.autodetect_alpha = autodetect_alpha_nAttr.create("autodetect_alpha", "autodetect_alpha", OpenMaya.MFnNumericData.kBoolean, False)
    ms_renderSettings.addAttribute(ms_renderSettings.autodetect_alpha)

    # force_linear_texture_interpretation
    force_linear_texture_interpretation_nAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.force_linear_texture_interpretation = force_linear_texture_interpretation_nAttr.create("force_linear_texture_interpretation", "force_linear_texture_interpretation", OpenMaya.MFnNumericData.kBoolean, False)
    ms_renderSettings.addAttribute(ms_renderSettings.force_linear_texture_interpretation)

    # force_linear_color_interpretation
    force_linear_color_interpretation_nAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.force_linear_color_interpretation = force_linear_color_interpretation_nAttr.create("force_linear_color_interpretation", "force_linear_color_interpretation", OpenMaya.MFnNumericData.kBoolean, False)
    ms_renderSettings.addAttribute(ms_renderSettings.force_linear_color_interpretation)


def initializePlugin(obj):
    plugin = OpenMayaMPx.MFnPlugin(obj)
    try:
        plugin.registerNode(ms_renderSettings_nodeTypeName, ms_renderSettings_nodeTypeId, ms_renderSettings_nodeCreator, ms_renderSettings_nodeInitializer)
    except:
        sys.stderr.write("Failed to register node: %s\n" % ms_renderSettings_nodeTypeName)

    # load objExport plugin if its not loaded yet
    try:
        if not cmds.pluginInfo('objExport', query=True, loaded=True):
            cmds.loadPlugin('objExport')
        if not cmds.pluginInfo('ms_appleseed_material.py', query=True, loaded=True):
            cmds.loadPlugin('ms_appleseed_material.py')
        if not cmds.pluginInfo('ms_appleseed_shading_node.py', query=True, loaded=True):
            cmds.loadPlugin('ms_appleseed_shading_node.py')
        if not cmds.pluginInfo('ms_environment_node.py', query=True, loaded=True):
            cmds.loadPlugin('ms_environment_node.py')
    except: 
        print 'objExport plugin could not be loaded, cannot load mayaseed'

    ms_menu.createMenu()
    ms_menu.buildMenu()


def uninitializePlugin(obj):
    plugin = OpenMayaMPx.MFnPlugin(obj)
    try:
        plugin.deregisterNode(ms_renderSettings_nodeTypeId)
    except:
        sys.stderr.write("Failed to deregister node: %s\n" % ms_renderSettings_nodeTypeName)
    
    ms_menu.deleteMenu()
