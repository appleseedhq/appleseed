
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2013 Esteban Tovagliari, Jupiter Jazz Limited
// Copyright (c) 2014 Esteban Tovagliari, The appleseedhq Organization
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
//

// Interface header.
#include "rendererservices.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/texturing/texturecache.h"

// appleseed.foundation headers.
#include "foundation/image/image.h"

// Standard headers.
#include <limits>

using namespace foundation;
using namespace std;

namespace renderer
{

// Globals used by RendererServices.
namespace
{

OIIO::TypeDesc g_float_array2_typedesc(OIIO::TypeDesc::FLOAT, 2);
OIIO::TypeDesc g_float_array4_typedesc(OIIO::TypeDesc::FLOAT, 4);
OIIO::TypeDesc g_int_array2_typedesc(OIIO::TypeDesc::INT, 2);

OIIO::ustring g_empty_ustr;
OIIO::ustring g_trace_ustr("trace");
OIIO::ustring g_hit_ustr("hit");
OIIO::ustring g_hitdist_ustr("hitdist");
OIIO::ustring g_N_ustr("N");
OIIO::ustring g_Ng_ustr("Ng");
OIIO::ustring g_P_ustr("P");
OIIO::ustring g_u_ustr("u");
OIIO::ustring g_v_ustr("v");
OIIO::ustring g_perspective_ustr("perspective");
OIIO::ustring g_spherical_ustr("spherical");
OIIO::ustring g_unknown_proj_ustr("unknown");

}

//
// RendererServices class implementation.
//

RendererServices::RendererServices(
    const Project&          project,
    OIIO::TextureSystem&    texture_sys,
    TextureStore&           texture_store)
  : OSL::RendererServices()
  , m_project(project)
  , m_texture_sys(texture_sys)
  , m_trace_context(m_project.get_trace_context())
  , m_texture_store(texture_store)
{
    // Set up attribute getters.
    m_attr_getters[OIIO::ustring("camera:resolution")] = &RendererServices::get_camera_resolution;    
    m_attr_getters[OIIO::ustring("camera:projection")] = &RendererServices::get_camera_projection;
    m_attr_getters[OIIO::ustring("camera:pixelaspect")] = &RendererServices::get_camera_pixelaspect;
    m_attr_getters[OIIO::ustring("camera:screen_window")] = &RendererServices::get_camera_screen_window;
    m_attr_getters[OIIO::ustring("camera:fov")] = &RendererServices::get_camera_fov;
    m_attr_getters[OIIO::ustring("camera:clip")] = &RendererServices::get_camera_clip;
    m_attr_getters[OIIO::ustring("camera:clip_near")] = &RendererServices::get_camera_clip_near;
    m_attr_getters[OIIO::ustring("camera:clip_far")] = &RendererServices::get_camera_clip_far;
    m_attr_getters[OIIO::ustring("camera:shutter")] = &RendererServices::get_camera_shutter;
    m_attr_getters[OIIO::ustring("camera:shutter_open")] = &RendererServices::get_camera_shutter_open;
    m_attr_getters[OIIO::ustring("camera:shutter_close")] = &RendererServices::get_camera_shutter_close;
}

void RendererServices::precompute_attributes()
{
    // Camera projection string.
    {
        Camera* cam = m_project.get_scene()->get_camera();
    
        if (strcmp(cam->get_model(), "pinhole_camera") == 0)
            m_cam_projection_str = g_perspective_ustr;
        else if (strcmp(cam->get_model(), "thinlens_camera") == 0)
            m_cam_projection_str = g_perspective_ustr;
        else if (strcmp(cam->get_model(), "spherical_camera") == 0)
            m_cam_projection_str = g_spherical_ustr;
        else
            m_cam_projection_str = g_unknown_proj_ustr;
    }
}

OIIO::TextureSystem* RendererServices::texturesys() const
{
    return &m_texture_sys;
}

bool RendererServices::texture(
    OSL::ustring            filename,
    OSL::TextureOpt&        options,
    OSL::ShaderGlobals*     sg,
    float                   s,
    float                   t,
    float                   dsdx,
    float                   dtdx,
    float                   dsdy,
    float                   dtdy,
    float*                  result)
{
    const bool status =
        m_texture_sys.texture(filename, options, s, t, dsdx, dtdx, dsdy, dtdy, result);

    if (!status)
        log_error(m_texture_sys.geterror());

    return status;
}

bool RendererServices::texture3d(
    OSL::ustring            filename,
    OSL::TextureOpt&        options,
    OSL::ShaderGlobals*     sg,
    const OSL::Vec3&        P,
    const OSL::Vec3&        dPdx,
    const OSL::Vec3&        dPdy,
    const OSL::Vec3&        dPdz,
    float*                  result)
{
    const bool status =
        m_texture_sys.texture3d(filename, options, P, dPdx, dPdy, dPdz, result);

    if (!status)
        log_error(m_texture_sys.geterror());

    return status;
}

bool RendererServices::environment(
    OSL::ustring            filename,
    OSL::TextureOpt&        options,
    OSL::ShaderGlobals*     sg,
    const OSL::Vec3&        R,
    const OSL::Vec3&        dRdx,
    const OSL::Vec3&        dRdy,
    float*                  result)
{
    const bool status =
        m_texture_sys.environment(filename, options, R, dRdx, dRdy, result);

    if (!status)
        log_error(m_texture_sys.geterror());

    return status;
}

bool RendererServices::get_texture_info(
    OSL::ShaderGlobals*     sg,
    OSL::ustring            filename,
    int                     subimage,
    OSL::ustring            dataname,
    OSL::TypeDesc           datatype,
    void*                   data)
{
    const bool status =
        m_texture_sys.get_texture_info(filename, subimage, dataname, datatype, data);

    if (!status)
        log_error(m_texture_sys.geterror());

    return status;
}

bool RendererServices::get_matrix(
    OSL::ShaderGlobals*     sg,
    OSL::Matrix44&          result,
    OSL::TransformationPtr  xform,
    float                   time)
{
    if (!xform)
        return false;

    const ShadingPoint::OSLObjectTransformInfo* obj_xform =
        reinterpret_cast<const ShadingPoint::OSLObjectTransformInfo*>(xform);

    result = obj_xform->get_transform(time);
    return true;
}

bool RendererServices::get_inverse_matrix(
    OSL::ShaderGlobals*     sg,
    OSL::Matrix44&          result,
    OSL::TransformationPtr  xform,
    float                   time)
{
    if (!xform)
        return false;

    const ShadingPoint::OSLObjectTransformInfo* obj_xform =
        reinterpret_cast<const ShadingPoint::OSLObjectTransformInfo*>(xform);

    result = obj_xform->get_inverse_transform(time);
    return true;
}

bool RendererServices::get_matrix(
    OSL::ShaderGlobals*     sg,
    OSL::Matrix44&          result,
    OSL::TransformationPtr  xform)
{
    if (!xform)
        return false;

    const ShadingPoint::OSLObjectTransformInfo* obj_xform =
        reinterpret_cast<const ShadingPoint::OSLObjectTransformInfo*>(xform);

    if (obj_xform->is_animated())
        return false;

    result = obj_xform->get_transform();
    return true;
}

bool RendererServices::get_inverse_matrix(
    OSL::ShaderGlobals*     sg,
    OSL::Matrix44&          result,
    OSL::TransformationPtr  xform)
{
    if (!xform)
        return false;

    const ShadingPoint::OSLObjectTransformInfo* obj_xform =
        reinterpret_cast<const ShadingPoint::OSLObjectTransformInfo*>(xform);

    if (obj_xform->is_animated())
        return false;

    result = obj_xform->get_inverse_transform();
    return true;
}

bool RendererServices::get_matrix(
    OSL::ShaderGlobals*     sg,
    OSL::Matrix44&          result,
    OIIO::ustring           from,
    float                   time)
{
    return false;
}

bool RendererServices::get_matrix(
    OSL::ShaderGlobals*     sg,
    OSL::Matrix44&          result,
    OIIO::ustring           from)
{
    return false;
}

bool RendererServices::get_attribute(
    OSL::ShaderGlobals*     sg,
    bool                    derivatives,
    OIIO::ustring           object,
    OIIO::TypeDesc          type,
    OIIO::ustring           name,
    void*                   val)
{    
    AttrGetterMapType::const_iterator i = m_attr_getters.find (name);
    if (i != m_attr_getters.end())
    {
        AttrGetterFun getter = i->second;
        return (this->*(getter))(sg, derivatives, object, type, name, val);
    }

    if (object.empty())
        return get_userdata(derivatives, name, type, sg, val);

    return false;
}

bool RendererServices::get_array_attribute(
    OSL::ShaderGlobals*     sg,
    bool                    derivatives,
    OIIO::ustring           object,
    OIIO::TypeDesc          type,
    OIIO::ustring           name,
    int                     index,
    void*                   val)
{
    return false;
}

bool RendererServices::get_userdata(
    bool                    derivatives,
    OIIO::ustring           name,
    OIIO::TypeDesc          type,
    OSL::ShaderGlobals*     sg,
    void*                   val)
{
    return false;
}

bool RendererServices::has_userdata(
    OIIO::ustring           name,
    OIIO::TypeDesc          type,
    OSL::ShaderGlobals*     sg)
{
    return false;
}

bool RendererServices::trace(
    TraceOpt&           options,
    OSL::ShaderGlobals* sg,
    const OSL::Vec3&    P,
    const OSL::Vec3&    dPdx,
    const OSL::Vec3&    dPdy,
    const OSL::Vec3&    R,
    const OSL::Vec3&    dRdx,
    const OSL::Vec3&    dRdy)
{
    TextureCache texture_cache(m_texture_store);
    Intersector intersector(m_trace_context, texture_cache);

    const ShadingPoint* parent = reinterpret_cast<const ShadingPoint*>(sg->renderstate);

    Vector3d PP = Vector3f(P);

    if (P == sg->P)
    {
        Vector3d front = Vector3f(P);
        Vector3d back = front;
        
        intersector.fixed_offset(
            parent->get_point(),
            parent->get_geometric_normal(),
            front,
            back);

        if (sg->N.dot(R) >= 0.0f)
            PP = front;
        else
            PP = back;
    }

    const ShadingRay ray(
        PP,
        normalize(Vector3f(R)),
        options.mindist,
        options.maxdist,
        sg->time,
        ShadingRay::ProbeRay,
        parent->get_ray().m_depth + 1);

    ShadingPoint shading_point;
    intersector.trace(
        ray, 
        shading_point, 
        P == sg->P ? parent : 0);

    ShadingPoint::OSLTraceData* trace_data = 
        reinterpret_cast<ShadingPoint::OSLTraceData*>(sg->tracedata);

    trace_data->m_traced = true;

    if (shading_point.hit())
    {
        trace_data->m_hit = true;
        trace_data->m_P = Imath::V3d(shading_point.get_point());
        trace_data->m_hit_distance = (trace_data->m_P - P).length();
        trace_data->m_N = Imath::V3d(shading_point.get_shading_normal());
        trace_data->m_Ng = Imath::V3d(shading_point.get_geometric_normal());
        trace_data->m_u = shading_point.get_uv(0)[0];
        trace_data->m_v = shading_point.get_uv(0)[1];
        return true;
    }

    return false;
}

bool RendererServices::getmessage(
    OSL::ShaderGlobals* sg,
    OIIO::ustring       source,
    OIIO::ustring       name,
    OIIO::TypeDesc      type, 
    void*               val, 
    bool                derivatives)
{
    const ShadingPoint::OSLTraceData* trace_data = 
        reinterpret_cast<ShadingPoint::OSLTraceData*>(sg->tracedata);

    if (trace_data->m_traced)
    {
        if (source == g_trace_ustr)
        {
            if (name == g_hit_ustr && type == OIIO::TypeDesc::TypeInt)
                reinterpret_cast<int*>(val)[0] = trace_data->m_hit ? 1 : 0;
            else if (name == g_hitdist_ustr && type == OIIO::TypeDesc::TypeFloat)
                reinterpret_cast<float*>(val)[0] = trace_data->m_hit_distance;
            else if (name == g_N_ustr && type == OIIO::TypeDesc::TypeNormal)
                *reinterpret_cast<OSL::Vec3*>(val) = trace_data->m_N;
            else if (name == g_Ng_ustr && type == OIIO::TypeDesc::TypeNormal)
                *reinterpret_cast<OSL::Vec3*>(val) = trace_data->m_Ng;
            else if (name == g_P_ustr && type == OIIO::TypeDesc::TypePoint)
                *reinterpret_cast<OSL::Vec3*>(val) = trace_data->m_P;
            else if (name == g_u_ustr && type == OIIO::TypeDesc::TypeFloat)
                reinterpret_cast<float*>(val)[0] = trace_data->m_u;
            else if (name == g_v_ustr && type == OIIO::TypeDesc::TypeFloat)
                reinterpret_cast<float*>(val)[0] = trace_data->m_v;
            else
                return false;

            // For now, set derivatives to zero.
            if (derivatives)
                memset(reinterpret_cast<char*>(val) + type.size(), 0, 2 * type.size());

            return true;
        }
    }

    return false;
}

// Attribute getters.
#define IMPLEMENT_ATTR_GETTER(name) \
bool RendererServices::get_##name(  \
    OSL::ShaderGlobals* sg,         \
    bool                derivs,     \
    OIIO::ustring       object,     \
    OIIO::TypeDesc      type,       \
    OIIO::ustring       name,       \
    void                *val) const

IMPLEMENT_ATTR_GETTER(camera_resolution)
{
    if (type == g_int_array2_typedesc)
    {
        Image& img = m_project.get_frame()->image();
        reinterpret_cast<int*>(val)[0] = img.properties().m_canvas_width;
        reinterpret_cast<int*>(val)[1] = img.properties().m_canvas_height;
        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(camera_projection)
{
    if (type == OIIO::TypeDesc::TypeString)
    {
        reinterpret_cast<OIIO::ustring*>(val)[0] = m_cam_projection_str;
        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(camera_fov)
{
    RENDERER_LOG_WARNING("OSL: get camera fov attribute not implemented");
    return false;
}

IMPLEMENT_ATTR_GETTER(camera_pixelaspect)
{
    if (type == OIIO::TypeDesc::TypeFloat)
    {
        reinterpret_cast<float*>(val)[0] = 1.0f;
        clear_attr_derivatives(derivs, type, val);
        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(camera_clip)
{
    if (type == g_float_array2_typedesc)
    {
        reinterpret_cast<float*>(val)[0] = 0.0f;
        reinterpret_cast<float*>(val)[1] = numeric_limits<float>::max();
        clear_attr_derivatives(derivs, type, val);
        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(camera_clip_near)
{
    if (type == OIIO::TypeDesc::TypeFloat)
    {
        reinterpret_cast<float*>(val)[0] = 0.0f;
        clear_attr_derivatives(derivs, type, val);
        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(camera_clip_far)
{
    if (type == OIIO::TypeDesc::TypeFloat)
    {
        reinterpret_cast<float*>(val)[0] = numeric_limits<float>::max();
        clear_attr_derivatives(derivs, type, val);
        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(camera_shutter)
{
    if (type == g_float_array2_typedesc)
    {
        Camera* cam = m_project.get_scene()->get_camera();
        reinterpret_cast<float*>(val)[0] = cam->get_shutter_open_time();
        reinterpret_cast<float*>(val)[1] = cam->get_shutter_close_time();
        clear_attr_derivatives(derivs, type, val);
        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(camera_shutter_open)
{
    if (type == OIIO::TypeDesc::TypeFloat)
    {
        Camera* cam = m_project.get_scene()->get_camera();
        reinterpret_cast<float*>(val)[0] = cam->get_shutter_open_time();
        clear_attr_derivatives(derivs, type, val);
        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(camera_shutter_close)
{
    if (type == OIIO::TypeDesc::TypeFloat)
    {
        Camera* cam = m_project.get_scene()->get_camera();
        reinterpret_cast<float*>(val)[0] = cam->get_shutter_close_time();
        clear_attr_derivatives(derivs, type, val);
        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(camera_screen_window)
{
    if (type == g_float_array4_typedesc)
    {
        Image& img = m_project.get_frame()->image();
        const float aspect = 
            static_cast<float>(img.properties().m_canvas_width) / img.properties().m_canvas_height;

        reinterpret_cast<float*>(val)[0] = -aspect;
        reinterpret_cast<float*>(val)[1] = -1.0f;
        reinterpret_cast<float*>(val)[2] =  aspect;
        reinterpret_cast<float*>(val)[3] =  1.0f;
        clear_attr_derivatives(derivs, type, val);
        return true;
    }

    return false;
}

#undef IMPLEMENT_ATTR_GETTER

void RendererServices::clear_attr_derivatives(
    bool                    derivs,
    const OIIO::TypeDesc&   type,
    void*                   val)
{
    if (derivs)
        memset(reinterpret_cast<char*>(val) + type.size(), 0, 2 * type.size());
}

void RendererServices::log_error(const std::string& message)
{
    if (!message.empty())
        RENDERER_LOG_ERROR("%s", message.c_str());
}

}   // namespace renderer
