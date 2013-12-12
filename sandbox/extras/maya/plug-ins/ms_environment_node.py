
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

#--------------------------------------------------------------------------------------------------
# ms_environment node.
#--------------------------------------------------------------------------------------------------

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
        
        # draw sphere
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

        glFT.glBegin(OpenMayaRender.MGL_LINE_STRIP) #  Axis
        glFT.glVertex3f(0.0, 0.0, 2.0)
        glFT.glVertex3f(0.0, 0.0, 0.0)
        glFT.glVertex3f(2.0, 0.0, 0.0)
        glFT.glVertex3f(0.0, 0.0, 0.0)
        glFT.glVertex3f(0.0, 2.0, 0.0)
        glFT.glEnd()

        glFT.glBegin(OpenMayaRender.MGL_LINE_STRIP) #  X
        glFT.glVertex3f(2.5, 0.5, 0.0)
        glFT.glVertex3f(3.5, -0.5, 0.0)
        glFT.glVertex3f(3.0, 0.0, 0.0)
        glFT.glVertex3f(3.5, 0.5, 0.0)
        glFT.glVertex3f(2.5, -0.5, 0.0)
        glFT.glEnd()

        glFT.glBegin(OpenMayaRender.MGL_LINE_STRIP) #  Y
        glFT.glVertex3f(-0.5, 3.5, 0.0)
        glFT.glVertex3f(0.0, 3.0, 0.0)
        glFT.glVertex3f(0.0, 2.5, 0.0)
        glFT.glVertex3f(0.0, 3.0, 0.0)
        glFT.glVertex3f(0.5, 3.5, 0.0)
        glFT.glEnd()

        glFT.glBegin(OpenMayaRender.MGL_LINE_STRIP) #  Z
        glFT.glVertex3f(-0.5, 0.5, 3.0)
        glFT.glVertex3f(0.5, 0.5, 3.0)
        glFT.glVertex3f(-0.5, -0.5, 3.0)
        glFT.glVertex3f(0.5, -0.5, 3.0)
        glFT.glEnd()

        # appleseed logo

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
    # environment type
    model_enumAttr = OpenMaya.MFnEnumAttribute()
    ms_environment.model = model_enumAttr.create("model", "model")
    model_enumAttr.addField("Constant Environment", 0)
    model_enumAttr.addField("Gradient Environment", 1)
    model_enumAttr.addField("Latitude Longitude Map", 2)
    model_enumAttr.addField("Mirrorball Map", 3)
    ms_environment.addAttribute(ms_environment.model)
    
    # constant exitance
    constant_exitance_nAttr = OpenMaya.MFnNumericAttribute()
    ms_environment.constant_exitance = constant_exitance_nAttr.createColor("constant_exitance", "const_exitance")
    constant_exitance_nAttr.setDefault(0.5, 0.5, 0.5)
    constant_exitance_nAttr.setKeyable(True)
    ms_environment.addAttribute(ms_environment.constant_exitance)

    # gradient horizon exitance
    gradient_horizon_exitance_nAttr = OpenMaya.MFnNumericAttribute()
    ms_environment.gradient_horizon_exitance = gradient_horizon_exitance_nAttr.createColor("gradient_horizon_exitance", "grad_horizon_exitance")
    gradient_horizon_exitance_nAttr.setDefault(0.5, 0.5, 0.5)
    gradient_horizon_exitance_nAttr.setKeyable(True)
    ms_environment.addAttribute(ms_environment.gradient_horizon_exitance)

    # gradient zenith exitance
    gradient_zenith_exitance_nAttr = OpenMaya.MFnNumericAttribute()
    ms_environment.gradient_zenith_exitance = gradient_zenith_exitance_nAttr.createColor("gradient_zenith_exitance", "grad_zenith_exitance")
    gradient_zenith_exitance_nAttr.setDefault(0.5, 0.5, 0.5)
    gradient_zenith_exitance_nAttr.setKeyable(True)
    ms_environment.addAttribute(ms_environment.gradient_zenith_exitance)

    # latitude longitude exitance
    latitude_longitude_exitance_nAttr = OpenMaya.MFnNumericAttribute()
    ms_environment.latitude_longitude_exitance = latitude_longitude_exitance_nAttr.createColor("latitude_longitude_exitance", "lat_long_exitance")
    latitude_longitude_exitance_nAttr.setDefault(0.5, 0.5, 0.5)
    latitude_longitude_exitance_nAttr.setKeyable(True)
    ms_environment.addAttribute(ms_environment.latitude_longitude_exitance)

    # mirror ball exitance
    mirror_ball_exitance_nAttr = OpenMaya.MFnNumericAttribute()
    ms_environment.mirror_ball_exitance = mirror_ball_exitance_nAttr.createColor("mirror_ball_exitance", "mball_exitance")
    mirror_ball_exitance_nAttr.setDefault(0.5, 0.5, 0.5)
    mirror_ball_exitance_nAttr.setKeyable(True)
    ms_environment.addAttribute(ms_environment.mirror_ball_exitance)

    # exitance multiplier
    exitance_multiplier_AttrFloat = OpenMaya.MFnNumericAttribute()
    ms_environment.exitance_multiplier = exitance_multiplier_AttrFloat.create("exitance_multiplier", "exitance_multiplier", OpenMaya.MFnNumericData.kFloat, 1)
    exitance_multiplier_AttrFloat.setHidden(False)
    exitance_multiplier_AttrFloat.setKeyable(True)
    ms_environment.addAttribute(ms_environment.exitance_multiplier)


#--------------------------------------------------------------------------------------------------
# node initialization .
#--------------------------------------------------------------------------------------------------

def initializePlugin(obj):
    plugin = OpenMayaMPx.MFnPlugin(obj)

    try:
        plugin.registerNode(ms_environment_nodeTypeName, ms_environment_nodeTypeId, ms_environment_nodeCreator, ms_environment_nodeInitializer, OpenMayaMPx.MPxNode.kLocatorNode)
    except:
        sys.stderr.write("Failed to register node: %s" % ms_environment_nodeTypeName)

def uninitializePlugin(obj):
    plugin = OpenMayaMPx.MFnPlugin(obj)

    try:
        plugin.deregisterNode(ms_environment_nodeTypeId)
    except:
        sys.stderr.write("Failed to deregister node: %s" % ms_environment_nodeTypeName)
