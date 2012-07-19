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

import sys 
import maya.OpenMaya as OpenMaya
import maya.OpenMayaMPx as OpenMayaMPx
import maya.OpenMayaRender as OpenMayaRender
import maya.OpenMayaUI as OpenMayaUI
import inspect
import os
import os.path

ROOT_DIRECTORY = os.path.split((os.path.dirname(inspect.getfile(inspect.currentframe()))))[0]
sys.path.append(os.path.join(ROOT_DIRECTORY, 'scripts'))
sys.path.append(os.path.join(ROOT_DIRECTORY, 'graphics'))

import ms_menu

#****************************************************************************************************************************************************************************************************
# ms_renderSettings node ****************************************************************************************************************************************************************************
#****************************************************************************************************************************************************************************************************

ms_renderSettings_nodeTypeName = "ms_renderSettings"
ms_renderSettings_nodeTypeId = OpenMaya.MTypeId(0x00333)

class ms_renderSettings(OpenMayaMPx.MPxNode):

    def __init__(self):
        OpenMayaMPx.MPxNode.__init__(self)

def ms_renderSettings_nodeCreator():
    return OpenMayaMPx.asMPxPtr( ms_renderSettings() )

def ms_renderSettings_nodeInitializer():

    #define attributes
    #export button --------------------------------------------
    export_button_string = OpenMaya.MFnStringData().create("/")
    export_button_string_Attr = OpenMaya.MFnTypedAttribute()
    ms_renderSettings.export_button =  export_button_string_Attr.create("export", "export", OpenMaya.MFnData.kString, export_button_string) 

    #export file settings -------------------------------------
    #output directory attribute
    output_dir_string = OpenMaya.MFnStringData().create("/")
    output_dir_Attr = OpenMaya.MFnTypedAttribute()
    ms_renderSettings.output_dir =  output_dir_Attr.create("output_directory", "out_dir", OpenMaya.MFnData.kString, output_dir_string)   
    #output file attribute
    output_file_string = OpenMaya.MFnStringData().create("file_name.#.appleseed")
    output_file_Attr = OpenMaya.MFnTypedAttribute()
    ms_renderSettings.output_file =  output_file_Attr.create("output_file", "out_file", OpenMaya.MFnData.kString, output_file_string)  

    #convert textures to exr
    convert_textures_to_exr_nAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.convert_textures_to_exr = convert_textures_to_exr_nAttr.create("convert_textures_to_exr", "convert_tex_to_exr", OpenMaya.MFnNumericData.kBoolean, True)

    #convert shading nodes to textures
    convert_shading_nodes_to_textures_nAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.convert_shading_nodes_to_textures = convert_shading_nodes_to_textures_nAttr.create("convert_shading_nodes_to_textures", "convert_shading_nodes", OpenMaya.MFnNumericData.kBoolean, True)

    #overwrite existing exrs
    overwrite_existing_exrs_nAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.overwrite_existing_exrs = overwrite_existing_exrs_nAttr.create("overwrite_existing_exrs", "overwrite_exrs", OpenMaya.MFnNumericData.kBoolean, True)

    #export motion blur
    export_motion_blur_nAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.export_motion_blur = export_motion_blur_nAttr.create("export_motion_blur", "motion_blur", OpenMaya.MFnNumericData.kBoolean, True)

    #shutter open time
    shutter_open_AttrFloat = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.shutter_open = shutter_open_AttrFloat.create("shutter_open_time", "shutter_open", OpenMaya.MFnNumericData.kFloat, 0)
    shutter_open_AttrFloat.setHidden(False)
    shutter_open_AttrFloat.setKeyable(True)

    #shutter close time
    shutter_close_AttrFloat = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.shutter_close = shutter_close_AttrFloat.create("shutter_close_time", "shutter_close", OpenMaya.MFnNumericData.kFloat, 1)
    shutter_close_AttrFloat.setHidden(False)
    shutter_close_AttrFloat.setKeyable(True)

    #export animation
    export_animation_nAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.export_animation = export_animation_nAttr.create("export_animation", "export_animation", OpenMaya.MFnNumericData.kBoolean, False)

    #start_frame
    start_frame_AttrInt = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.start_frame = start_frame_AttrInt.create("animation_start_frame", "start_frame", OpenMaya.MFnNumericData.kInt, 1)
    start_frame_AttrInt.setHidden(False)
    start_frame_AttrInt.setKeyable(False)
    
    #end frame
    end_frame_AttrInt = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.end_frame = end_frame_AttrInt.create("animation_end_frame", "end_frame", OpenMaya.MFnNumericData.kInt, 100)
    end_frame_AttrInt.setHidden(False)
    end_frame_AttrInt.setKeyable(False)

    #environent -----------------------------------------------
    #environment message
    environment_msgAttr = OpenMaya.MFnMessageAttribute()
    ms_renderSettings.environment = environment_msgAttr.create("environment", "env")   

    #cameras --------------------------------------------------
    #export all cameras bool attribute
    export_all_cameras_nAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.export_all_cameras = export_all_cameras_nAttr.create("export_all_cameras", "export_all_cams", OpenMaya.MFnNumericData.kBoolean)
    #export all cameras as thin lens bool attribute
    export_all_cameras_as_thin_lens_nAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.export_all_cameras_as_thin_lens = export_all_cameras_as_thin_lens_nAttr.create("export_all_cameras_as_thinlens", "export_thinlens", OpenMaya.MFnNumericData.kBoolean)
    #interpret sets as assemblies bool attribute
    interpret_sets_as_assemblies_nAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.interpret_sets_as_assemblies = interpret_sets_as_assemblies_nAttr.create("interpret_sets_as_assemblies", "sets_as_assemblies", OpenMaya.MFnNumericData.kBoolean)
    #double sided shading bool attribute
    double_sided_shading_nAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.double_sided_shading = double_sided_shading_nAttr.create("double_sided_shading", "double_shade", OpenMaya.MFnNumericData.kBoolean)
    
    #output ---------------------------------------------------
    #camera
    camera_msgAttr = OpenMaya.MFnMessageAttribute()
    ms_renderSettings.camera = camera_msgAttr.create("camera", "cam")
    #color space
    color_space_enumAttr = OpenMaya.MFnEnumAttribute()
    ms_renderSettings.color_space = color_space_enumAttr.create("color_space", "col_space")
    color_space_enumAttr.addField("sRGB", 0)
    color_space_enumAttr.addField("Linear RGB", 1)
    color_space_enumAttr.addField("Spectral", 2)
    color_space_enumAttr.addField("ciexyz", 3)

    #resolution width samples
    width_AttrInt = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.width = width_AttrInt.create("resolution_width", "width", OpenMaya.MFnNumericData.kInt, 1280)
    width_AttrInt.setHidden(False)
    width_AttrInt.setKeyable(True)
    #resolution height samples
    height_AttrInt = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.height = height_AttrInt.create("resolution_height", "height", OpenMaya.MFnNumericData.kInt, 720)
    height_AttrInt.setHidden(False)
    height_AttrInt.setKeyable(True)


    #configurations -------------------------------------------
    #custom interactive config
    export_custom_interactive_config_nAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.export_custom_interactive_config = export_custom_interactive_config_nAttr.create("export_custom_interactive_config", "export_interactive", OpenMaya.MFnNumericData.kBoolean)  
    #lighting engine
    custom_interactive_config_lghting_engine_enumAttr = OpenMaya.MFnEnumAttribute()
    ms_renderSettings.custom_interactive_config_lghting_engine = custom_interactive_config_lghting_engine_enumAttr.create("interactive_lighting_engine", "interactive_engine")
    custom_interactive_config_lghting_engine_enumAttr.addField("Path Tracing", 0)
    custom_interactive_config_lghting_engine_enumAttr.addField("Distributed Ray Tracing", 1)
    #min samples
    custom_interactive_config_min_samples_AttrInt = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.custom_interactive_config_min_samples = custom_interactive_config_min_samples_AttrInt.create("interactive_min_samples", "interactive_min_samples", OpenMaya.MFnNumericData.kInt, 1)
    custom_interactive_config_min_samples_AttrInt.setHidden(False)
    custom_interactive_config_min_samples_AttrInt.setKeyable(True)
    #max samples
    custom_interactive_config_max_samples_AttrInt = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.custom_interactive_config_max_samples = custom_interactive_config_max_samples_AttrInt.create("interactive_max_samples", "interactive_max_samples", OpenMaya.MFnNumericData.kInt, 4)
    custom_interactive_config_max_samples_AttrInt.setHidden(False)
    custom_interactive_config_max_samples_AttrInt.setKeyable(True)
    #max ray depth
    custom_interactive_config_max_ray_depth_AttrInt = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.custom_interactive_config_max_ray_depth = custom_interactive_config_max_ray_depth_AttrInt.create("interactive_max_ray_depth", "interactive_ray_depth", OpenMaya.MFnNumericData.kInt, 4)
    custom_interactive_config_max_ray_depth_AttrInt.setHidden(False)
    custom_interactive_config_max_ray_depth_AttrInt.setKeyable(True)
    #light samples
    custom_interactive_config_light_samples_AttrInt = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.custom_interactive_config_light_samples = custom_interactive_config_light_samples_AttrInt.create("interactive_light_samples", "interactive_light_samples", OpenMaya.MFnNumericData.kInt, 16)
    custom_interactive_config_light_samples_AttrInt.setHidden(False)
    custom_interactive_config_light_samples_AttrInt.setKeyable(True)

    #custom final config
    export_custom_final_config_nAttr = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.export_custom_final_config = export_custom_final_config_nAttr.create("export_custom_final_config", "export_final", OpenMaya.MFnNumericData.kBoolean)  
    #lighting engine
    custom_final_config_lghting_engine_enumAttr = OpenMaya.MFnEnumAttribute()
    ms_renderSettings.custom_final_config_lghting_engine = custom_final_config_lghting_engine_enumAttr.create("final_lighting_engine", "final_engine")
    custom_final_config_lghting_engine_enumAttr.addField("Path Tracing", 0)
    custom_final_config_lghting_engine_enumAttr.addField("Distributed Ray Tracing", 1)
    #min samples
    custom_final_config_min_samples_AttrInt = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.custom_final_config_min_samples = custom_final_config_min_samples_AttrInt.create("final_min_samples", "final_min_samples", OpenMaya.MFnNumericData.kInt, 1)
    custom_final_config_min_samples_AttrInt.setHidden(False)
    custom_final_config_min_samples_AttrInt.setKeyable(True)
    #max samples
    custom_final_config_max_samples_AttrInt = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.custom_final_config_max_samples = custom_final_config_max_samples_AttrInt.create("final_max_samples", "final_max_samples", OpenMaya.MFnNumericData.kInt, 4)
    custom_final_config_max_samples_AttrInt.setHidden(False)
    custom_final_config_max_samples_AttrInt.setKeyable(True)
    #max ray depth
    custom_final_config_max_ray_depth_AttrInt = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.custom_final_config_max_ray_depth = custom_final_config_max_ray_depth_AttrInt.create("final_max_ray_depth", "final_ray_depth", OpenMaya.MFnNumericData.kInt, 4)
    custom_final_config_max_ray_depth_AttrInt.setHidden(False)
    custom_final_config_max_ray_depth_AttrInt.setKeyable(True)
    #light samples
    custom_final_config_light_samples_AttrInt = OpenMaya.MFnNumericAttribute()
    ms_renderSettings.custom_final_config_light_samples = custom_final_config_light_samples_AttrInt.create("final_light_samples", "final_light_samples", OpenMaya.MFnNumericData.kInt, 16)
    custom_final_config_light_samples_AttrInt.setHidden(False)
    custom_final_config_light_samples_AttrInt.setKeyable(True)


    # add attributes
    try:
        ms_renderSettings.addAttribute(ms_renderSettings.export_button)

        ms_renderSettings.addAttribute(ms_renderSettings.output_dir)
        ms_renderSettings.addAttribute(ms_renderSettings.output_file)
        ms_renderSettings.addAttribute(ms_renderSettings.convert_shading_nodes_to_textures)
        ms_renderSettings.addAttribute(ms_renderSettings.convert_textures_to_exr)
        ms_renderSettings.addAttribute(ms_renderSettings.overwrite_existing_exrs)
        ms_renderSettings.addAttribute(ms_renderSettings.export_motion_blur)
        ms_renderSettings.addAttribute(ms_renderSettings.shutter_open)
        ms_renderSettings.addAttribute(ms_renderSettings.shutter_close)
        ms_renderSettings.addAttribute(ms_renderSettings.export_animation)
        ms_renderSettings.addAttribute(ms_renderSettings.start_frame)
        ms_renderSettings.addAttribute(ms_renderSettings.end_frame)

        ms_renderSettings.addAttribute(ms_renderSettings.environment)
        ms_renderSettings.addAttribute(ms_renderSettings.export_all_cameras)
        ms_renderSettings.addAttribute(ms_renderSettings.export_all_cameras_as_thin_lens)

        ms_renderSettings.addAttribute(ms_renderSettings.interpret_sets_as_assemblies)
        ms_renderSettings.addAttribute(ms_renderSettings.double_sided_shading)
        ms_renderSettings.addAttribute(ms_renderSettings.camera)
        ms_renderSettings.addAttribute(ms_renderSettings.color_space)
        ms_renderSettings.addAttribute(ms_renderSettings.width)
        ms_renderSettings.addAttribute(ms_renderSettings.height)

        ms_renderSettings.addAttribute(ms_renderSettings.export_custom_interactive_config)
        ms_renderSettings.addAttribute(ms_renderSettings.custom_interactive_config_lghting_engine)
        ms_renderSettings.addAttribute(ms_renderSettings.custom_interactive_config_min_samples)
        ms_renderSettings.addAttribute(ms_renderSettings.custom_interactive_config_max_samples)
        ms_renderSettings.addAttribute(ms_renderSettings.custom_interactive_config_max_ray_depth)
        ms_renderSettings.addAttribute(ms_renderSettings.custom_interactive_config_light_samples)

        ms_renderSettings.addAttribute(ms_renderSettings.export_custom_final_config)
        ms_renderSettings.addAttribute(ms_renderSettings.custom_final_config_lghting_engine)
        ms_renderSettings.addAttribute(ms_renderSettings.custom_final_config_min_samples)
        ms_renderSettings.addAttribute(ms_renderSettings.custom_final_config_max_samples)
        ms_renderSettings.addAttribute(ms_renderSettings.custom_final_config_max_ray_depth)
        ms_renderSettings.addAttribute(ms_renderSettings.custom_final_config_light_samples)
    except:
        sys.stderr.write( "Failed to create attributes of %s node\n" % ms_renderSettings_nodeTypeName )



#****************************************************************************************************************************************************************************************************
# ms_environment node *******************************************************************************************************************************************************************************
#****************************************************************************************************************************************************************************************************



ms_environment_nodeTypeName = "ms_environment"
ms_environment_nodeTypeId = OpenMaya.MTypeId(0x10211)

glRenderer = OpenMayaRender.MHardwareRenderer.theRenderer()
glFT = glRenderer.glFunctionTable()

class ms_environment(OpenMayaMPx.MPxLocatorNode):
    def __init__(self):

        OpenMayaMPx.MPxLocatorNode.__init__(self)

    def draw(self, view, path, style, status):

        view.beginGL()

        glFT.glEnable(OpenMayaRender.MGL_BLEND)
        
        #draw sphere

        glFT.glBegin(OpenMayaRender.MGL_LINE_STRIP) 
        glFT.glVertex3f(3.06161699787e-16, 5.0, -8.04061324838e-16)
        glFT.glVertex3f(-1.91341716183, 4.61939766256, -7.42855800902e-16)
        glFT.glVertex3f(-3.53553390593, 3.53553390593, -5.68557215283e-16)
        glFT.glVertex3f(-4.61939766256, 1.91341716183, -3.07700947621e-16)
        glFT.glVertex3f(-5.0, -2.48949812526e-16, 4.00341832155e-32)
        glFT.glVertex3f(-4.61939766256, -1.91341716183, 3.07700947621e-16)
        glFT.glVertex3f(-3.53553390593, -3.53553390593, 5.68557215283e-16)
        glFT.glVertex3f(-1.91341716183, -4.61939766256, 7.42855800902e-16)
        glFT.glVertex3f(5.26505568682e-16, -5.0, 8.04061324838e-16)
        glFT.glVertex3f(1.91341716183, -4.61939766256, 7.42855800902e-16)
        glFT.glVertex3f(3.53553390593, -3.53553390593, 5.68557215283e-16)
        glFT.glVertex3f(4.61939766256, -1.91341716183, 3.07700947621e-16)
        glFT.glVertex3f(5.0, 1.08161708099e-15, -1.73937292622e-31)
        glFT.glVertex3f(4.61939766256, 1.91341716183, -3.07700947621e-16)
        glFT.glVertex3f(3.53553390593, 3.53553390593, -5.68557215283e-16)
        glFT.glVertex3f(1.91341716183, 4.61939766256, -7.42855800902e-16)
        glFT.glVertex3f(-1.63672859331e-15, 5.0, -8.04061324838e-16)
        glFT.glEnd()

        glFT.glBegin(OpenMayaRender.MGL_LINE_STRIP)
        glFT.glVertex3f(-2.19184010562e-15, 5.0, -3.06161699787e-16)
        glFT.glVertex3f(-2.237428191e-15, 4.61939766256, 1.91341716183)
        glFT.glVertex3f(-1.94238811663e-15, 3.53553390593, 3.53553390593)
        glFT.glVertex3f(-1.3516370593e-15, 1.91341716183, 4.61939766256)
        glFT.glVertex3f(-5.55111512313e-16, -5.26505568682e-16, 5.0)
        glFT.glVertex3f(3.25924730327e-16, -1.91341716183, 4.61939766256)
        glFT.glVertex3f(1.15734188729e-15, -3.53553390593, 3.53553390593)
        glFT.glVertex3f(1.81256423324e-15, -4.61939766256, 1.91341716183)
        glFT.glVertex3f(2.19184010562e-15, -5.0, -5.26505568682e-16)
        glFT.glVertex3f(2.237428191e-15, -4.61939766256, -1.91341716183)
        glFT.glVertex3f(1.94238811663e-15, -3.53553390593, -3.53553390593)
        glFT.glVertex3f(1.3516370593e-15, -1.91341716183, -4.61939766256)
        glFT.glVertex3f(5.55111512313e-16, 1.35917283715e-15, -5.0)
        glFT.glVertex3f(-3.25924730327e-16, 1.91341716183, -4.61939766256)
        glFT.glVertex3f(-1.15734188729e-15, 3.53553390593, -3.53553390593)
        glFT.glVertex3f(-1.81256423324e-15, 4.61939766256, -1.91341716183)
        glFT.glVertex3f(-2.19184010562e-15, 5.0, 1.63672859331e-15)
        glFT.glEnd()
        
        glFT.glBegin(OpenMayaRender.MGL_LINE_STRIP)
        glFT.glVertex3f(3.06161699787e-16, 3.06161699787e-16, -5.0)
        glFT.glVertex3f(-1.91341716183, 2.82856528072e-16, -4.61939766256)
        glFT.glVertex3f(-3.53553390593, 2.16489014059e-16, -3.53553390593)
        glFT.glVertex3f(-4.61939766256, 1.17163010133e-16, -1.91341716183)
        glFT.glVertex3f(-5.0, -1.52437795529e-32, 2.48949812526e-16)
        glFT.glVertex3f(-4.61939766256, -1.17163010133e-16, 1.91341716183)
        glFT.glVertex3f(-3.53553390593, -2.16489014059e-16, 3.53553390593)
        glFT.glVertex3f(-1.91341716183, -2.82856528072e-16, 4.61939766256)
        glFT.glVertex3f(5.26505568682e-16, -3.06161699787e-16, 5.0)
        glFT.glVertex3f(1.91341716183, -2.82856528072e-16, 4.61939766256)
        glFT.glVertex3f(3.53553390593, -2.16489014059e-16, 3.53553390593)
        glFT.glVertex3f(4.61939766256, -1.17163010133e-16, 1.91341716183)
        glFT.glVertex3f(5.0, 6.62299448072e-32, -1.08161708099e-15)
        glFT.glVertex3f(4.61939766256, 1.17163010133e-16, -1.91341716183)
        glFT.glVertex3f(3.53553390593, 2.16489014059e-16, -3.53553390593)
        glFT.glVertex3f(1.91341716183, 2.82856528072e-16, -4.61939766256)
        glFT.glVertex3f(-1.63672859331e-15, 3.06161699787e-16, -5.0)
        glFT.glEnd()

        glFT.glBegin(OpenMayaRender.MGL_LINE_STRIP) # Axis
        glFT.glVertex3f(0.0, 0.0, 2.0)
        glFT.glVertex3f(0.0, 0.0, 0.0)
        glFT.glVertex3f(2.0, 0.0, 0.0)
        glFT.glVertex3f(0.0, 0.0, 0.0)
        glFT.glVertex3f(0.0, 2.0, 0.0)
        glFT.glEnd()

        glFT.glBegin(OpenMayaRender.MGL_LINE_STRIP) # X
        glFT.glVertex3f(2.5, 0.5, 0.0)
        glFT.glVertex3f(3.5, -0.5, 0.0)
        glFT.glVertex3f(3.0, 0.0, 0.0)
        glFT.glVertex3f(3.5, 0.5, 0.0)
        glFT.glVertex3f(2.5, -0.5, 0.0)
        glFT.glEnd()

        glFT.glBegin(OpenMayaRender.MGL_LINE_STRIP) # Y
        glFT.glVertex3f(-0.5, 3.5, 0.0)
        glFT.glVertex3f(0.0, 3.0, 0.0)
        glFT.glVertex3f(0.0, 2.5, 0.0)
        glFT.glVertex3f(0.0, 3.0, 0.0)
        glFT.glVertex3f(0.5, 3.5, 0.0)
        glFT.glEnd()

        glFT.glBegin(OpenMayaRender.MGL_LINE_STRIP) # Z
        glFT.glVertex3f(-0.5, 0.5, 3.0)
        glFT.glVertex3f(0.5, 0.5, 3.0)
        glFT.glVertex3f(-0.5, -0.5, 3.0)
        glFT.glVertex3f(0.5, -0.5, 3.0)
        glFT.glEnd()

        #Appleseed logo

        glFT.glBegin(OpenMayaRender.MGL_LINE_STRIP)
        glFT.glVertex3f(4.99999602985, 6.24639415266, 0.0)
        glFT.glVertex3f(4.84344021397, 5.95478169919, 0.0)
        glFT.glVertex3f(4.77695111567, 5.74857723447, 0.0)
        glFT.glVertex3f(4.75097549437, 5.54600980644, 0.0)
        glFT.glVertex3f(4.78516283497, 5.37147218711, 0.0)
        glFT.glVertex3f(4.87062698707, 5.26346158765, 0.0)
        glFT.glVertex3f(4.99999602985, 5.21958256572, 0.0)
        glFT.glVertex3f(5.12488923665, 5.25780446049, 0.0)
        glFT.glVertex3f(5.21482963071, 5.37147218711, 0.0)
        glFT.glVertex3f(5.24875492795, 5.54682314909, 0.0)
        glFT.glVertex3f(5.22313636337, 5.74836664521, 0.0)
        glFT.glVertex3f(5.15665391004, 5.95474029648, 0.0)
        glFT.glVertex3f(4.99999643583, 6.24639415266, 0.0)
        glFT.glEnd()
        glFT.glBegin(OpenMayaRender.MGL_LINE_STRIP) 
        glFT.glVertex3f(6.18539005392, 5.3851607507, 0.0)
        glFT.glVertex3f(5.85967172213, 5.44394097566, 0.0)
        glFT.glVertex3f(5.64301336096, 5.44345518195, 0.0)
        glFT.glVertex3f(5.44233338012, 5.40556268809, 0.0)
        glFT.glVertex3f(5.28690270915, 5.3191135045, 0.0)
        glFT.glVertex3f(5.21058840012, 5.20445515493, 0.0)
        glFT.glVertex3f(5.20883420315, 5.06785852031, 0.0)
        glFT.glVertex3f(5.28377950862, 4.96088923718, 0.0)
        glFT.glVertex3f(5.41967705096, 4.91047609857, 0.0)
        glFT.glVertex3f(5.59692921938, 4.9323976508, 0.0)
        glFT.glVertex3f(5.78069190285, 5.01904271899, 0.0)
        glFT.glVertex3f(5.95642070078, 5.14604425488, 0.0)
        glFT.glVertex3f(6.18539017937, 5.38516036459, 0.0)
        glFT.glEnd()
        glFT.glBegin(OpenMayaRender.MGL_LINE_STRIP) 
        glFT.glVertex3f(5.7326153134, 3.9916482824, 0.0)
        glFT.glVertex3f(5.68786612947, 4.31958891277, 0.0)
        glFT.glVertex3f(5.62045299662, 4.52549314046, 0.0)
        glFT.glVertex3f(5.52240156891, 4.70464171936, 0.0)
        glFT.glVertex3f(5.39215279075, 4.82575080494, 0.0)
        glFT.glVertex3f(5.25952380184, 4.86289864727, 0.0)
        glFT.glVertex3f(5.12907060571, 4.82235630626, 0.0)
        glFT.glVertex3f(5.05049614498, 4.71802375876, 0.0)
        glFT.glVertex3f(5.04454505108, 4.573198999, 0.0)
        glFT.glVertex3f(5.1201676185, 4.41139630138, 0.0)
        glFT.glVertex3f(5.25935776733, 4.26340240236, 0.0)
        glFT.glVertex3f(5.43444659058, 4.1355200169, 0.0)
        glFT.glVertex3f(5.73261498495, 3.99164804378, 0.0)
        glFT.glEnd()
        glFT.glBegin(OpenMayaRender.MGL_LINE_STRIP) 
        glFT.glVertex3f(4.26739111044, 3.99164361521, 0.0)
        glFT.glVertex3f(4.56545292559, 4.13554184611, 0.0)
        glFT.glVertex3f(4.74044767937, 4.26328345096, 0.0)
        glFT.glVertex3f(4.88052854524, 4.41189585562, 0.0)
        glFT.glVertex3f(4.95546104432, 4.57319457044, 0.0)
        glFT.glVertex3f(4.9498061303, 4.71081154918, 0.0)
        glFT.glVertex3f(4.87093581813, 4.82235163907, 0.0)
        glFT.glVertex3f(4.74742882528, 4.86483986171, 0.0)
        glFT.glVertex3f(4.60785330464, 4.82574637638, 0.0)
        glFT.glVertex3f(4.4773384532, 4.70382525755, 0.0)
        glFT.glVertex3f(4.37960001261, 4.52571492964, 0.0)
        glFT.glVertex3f(4.3120820585, 4.31967773297, 0.0)
        glFT.glVertex3f(4.267390782, 3.99164385384, 0.0)
        glFT.glEnd()
        glFT.glBegin(OpenMayaRender.MGL_LINE_STRIP) 
        glFT.glVertex3f(3.8146074924, 5.38515319902, 0.0)
        glFT.glVertex3f(4.04356900883, 5.14614656628, 0.0)
        glFT.glVertex3f(4.21913484737, 5.01919099216, 0.0)
        glFT.glVertex3f(4.40376101137, 4.93188993049, 0.0)
        glFT.glVertex3f(4.58032062081, 4.91046893301, 0.0)
        glFT.glVertex3f(4.70945468066, 4.95837306097, 0.0)
        glFT.glVertex3f(4.79116334317, 5.06785096864, 0.0)
        glFT.glVertex3f(4.79340628446, 5.19844268185, 0.0)
        glFT.glVertex3f(4.71309496261, 5.31910633894, 0.0)
        glFT.glVertex3f(4.55680978096, 5.40555764117, 0.0)
        glFT.glVertex3f(4.35721395384, 5.4434733038, 0.0)
        glFT.glVertex3f(4.1403967401, 5.44401769877, 0.0)
        glFT.glVertex3f(3.81460761785, 5.38515358513, 0.0)
        glFT.glEnd()

        glFT.glDisable(OpenMayaRender.MGL_BLEND)
        
        view.endGL()


def ms_environment_nodeCreator():
    return OpenMayaMPx.asMPxPtr(ms_environment())
 
def ms_environment_nodeInitializer():
    #environment type
    model_enumAttr = OpenMaya.MFnEnumAttribute()
    ms_environment.model = model_enumAttr.create("model", "model")
    model_enumAttr.addField("Constant Environment", 0)
    model_enumAttr.addField("Gradient Cnvironment", 1)
    model_enumAttr.addField("Latitude Longitude Map", 2)
    model_enumAttr.addField("Mirrorball Map", 3)
    #model_enumAttr.setDefault(0)
    #constant exitance
    constant_exitance_nAttr = OpenMaya.MFnNumericAttribute()
    ms_environment.constant_exitance = constant_exitance_nAttr.createColor( "constant_exitance", "const_exitance")
    constant_exitance_nAttr.setDefault(0.5,0.5,0.5)
    constant_exitance_nAttr.setKeyable(True)
    #gradient horizon exitance
    gradient_horizon_exitance_nAttr = OpenMaya.MFnNumericAttribute()
    ms_environment.gradient_horizon_exitance = gradient_horizon_exitance_nAttr.createColor( "gradient_horizon_exitance", "grad_horizon_exitance")
    gradient_horizon_exitance_nAttr.setDefault(0.5,0.5,0.5)
    gradient_horizon_exitance_nAttr.setKeyable(True)
    #gradient zenith exitance
    gradient_zenith_exitance_nAttr = OpenMaya.MFnNumericAttribute()
    ms_environment.gradient_zenith_exitance = gradient_zenith_exitance_nAttr.createColor( "gradient_zenith_exitance", "grad_zenith_exitance")
    gradient_zenith_exitance_nAttr.setDefault(0.5,0.5,0.5)
    gradient_zenith_exitance_nAttr.setKeyable(True)
    #latitude longitude exitance
    latitude_longitude_exitance_nAttr = OpenMaya.MFnNumericAttribute()
    ms_environment.latitude_longitude_exitance = latitude_longitude_exitance_nAttr.createColor( "latitude_longitude_exitance", "lat_long_exitance")
    latitude_longitude_exitance_nAttr.setDefault(0.5,0.5,0.5)
    latitude_longitude_exitance_nAttr.setKeyable(True)
    #mirror ball exitance
    mirror_ball_exitance_nAttr = OpenMaya.MFnNumericAttribute()
    ms_environment.mirror_ball_exitance = mirror_ball_exitance_nAttr.createColor( "mirror_ball_exitance", "mball_exitance")
    mirror_ball_exitance_nAttr.setDefault(0.5,0.5,0.5)
    mirror_ball_exitance_nAttr.setKeyable(True)

    # add attribute
    try:
        ms_environment.addAttribute( ms_environment.model )
        ms_environment.addAttribute( ms_environment.constant_exitance )
        ms_environment.addAttribute( ms_environment.gradient_horizon_exitance )
        ms_environment.addAttribute( ms_environment.gradient_zenith_exitance )
        ms_environment.addAttribute( ms_environment.latitude_longitude_exitance )
        ms_environment.addAttribute( ms_environment.mirror_ball_exitance )
    except:
        sys.stderr.write( "Failed to create attributes of %s node", kPluginNodeTypeName )

    return OpenMaya.MStatus.kSuccess
 
#****************************************************************************************************************************************************************************************************
# initialize nodes node *****************************************************************************************************************************************************************************
#****************************************************************************************************************************************************************************************************

def initializePlugin(obj):
    #register nodes
    plugin = OpenMayaMPx.MFnPlugin(obj)
    try:
        plugin.registerNode(ms_environment_nodeTypeName, ms_environment_nodeTypeId, ms_environment_nodeCreator, ms_environment_nodeInitializer, OpenMayaMPx.MPxNode.kLocatorNode)
    except:
        sys.stderr.write( "Failed to register node: %s" % ms_environment_nodeTypeName)
    
    try:
        plugin.registerNode( ms_renderSettings_nodeTypeName, ms_renderSettings_nodeTypeId, ms_renderSettings_nodeCreator, ms_renderSettings_nodeInitializer )
    except:
        sys.stderr.write( "Failed to register command: %s\n" % ms_renderSettings_nodeTypeName )
    
    #create menu
    ms_menu.createMenu()
    ms_menu.buildMenu()

def uninitializePlugin(obj):
    #deregister nodes
    plugin = OpenMayaMPx.MFnPlugin(obj)
    try:
        plugin.deregisterNode(ms_environment_nodeTypeId)
    except:
        sys.stderr.write( "Failed to deregister node: %s" % ms_environment_nodeTypeName)

    try:
        plugin.deregisterNode( ms_renderSettings_nodeTypeId )
    except:
        sys.stderr.write( "Failed to unregister node: %s\n" % ms_renderSettings_nodeTypeName )
    
    #delete menu
    ms_menu.deleteMenu()
    


