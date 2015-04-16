
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2013 Esteban Tovagliari, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/scene/visibilityflags.h"

// appleseed.foundation headers.
#include "foundation/core/version.h"
#include "foundation/image/image.h"

// Standard headers.
#include <limits>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// RendererServices class implementation.
//

namespace
{
    OIIO::TypeDesc g_float_array2_typedesc(OIIO::TypeDesc::FLOAT, 2);
    OIIO::TypeDesc g_float_array4_typedesc(OIIO::TypeDesc::FLOAT, 4);
    OIIO::TypeDesc g_int_array2_typedesc(OIIO::TypeDesc::INT, 2);

    OIIO::ustring g_empty_ustr;
    OIIO::ustring g_perspective_ustr("perspective");
    OIIO::ustring g_spherical_ustr("spherical");
    OIIO::ustring g_unknown_proj_ustr("unknown");
    OIIO::ustring g_trace_ustr("trace");
    OIIO::ustring g_hit_ustr("hit");
    OIIO::ustring g_hitdist_ustr("hitdist");
    OIIO::ustring g_N_ustr("N");
    OIIO::ustring g_Ng_ustr("Ng");
    OIIO::ustring g_P_ustr("P");
    OIIO::ustring g_u_ustr("u");
    OIIO::ustring g_v_ustr("v");

    // Coordinate systems.
    OIIO::ustring g_camera_ustr("camera");
    OIIO::ustring g_common_ustr("common");
    OIIO::ustring g_NDC_ustr("NDC");
    OIIO::ustring g_raster_ustr("raster");
    OIIO::ustring g_screen_ustr("screen");
    OIIO::ustring g_shader_usrt("shader");
    OIIO::ustring g_world_ustr("world");
}

RendererServices::RendererServices(
    const Project&          project,
    OIIO::TextureSystem&    texture_sys)
  : OSL::RendererServices()
  , m_project(project)
  , m_texture_sys(texture_sys)
  , m_texture_store(0)
{
    // Set up attribute getters.
    m_global_attr_getters[OIIO::ustring("object:object_instance_id")] = &RendererServices::get_object_instance_id;
    m_global_attr_getters[OIIO::ustring("object:object_instance_index")] = &RendererServices::get_object_instance_index;
    m_global_attr_getters[OIIO::ustring("object:assembly_instance_id")] = &RendererServices::get_assembly_instance_id;
    m_global_attr_getters[OIIO::ustring("camera:resolution")] = &RendererServices::get_camera_resolution;
    m_global_attr_getters[OIIO::ustring("camera:projection")] = &RendererServices::get_camera_projection;
    m_global_attr_getters[OIIO::ustring("camera:pixelaspect")] = &RendererServices::get_camera_pixelaspect;
    m_global_attr_getters[OIIO::ustring("camera:screen_window")] = &RendererServices::get_camera_screen_window;
    m_global_attr_getters[OIIO::ustring("camera:fov")] = &RendererServices::get_camera_fov;
    m_global_attr_getters[OIIO::ustring("camera:clip")] = &RendererServices::get_camera_clip;
    m_global_attr_getters[OIIO::ustring("camera:clip_near")] = &RendererServices::get_camera_clip_near;
    m_global_attr_getters[OIIO::ustring("camera:clip_far")] = &RendererServices::get_camera_clip_far;
    m_global_attr_getters[OIIO::ustring("camera:shutter")] = &RendererServices::get_camera_shutter;
    m_global_attr_getters[OIIO::ustring("camera:shutter_open")] = &RendererServices::get_camera_shutter_open;
    m_global_attr_getters[OIIO::ustring("camera:shutter_close")] = &RendererServices::get_camera_shutter_close;
    m_global_attr_getters[OIIO::ustring("path:ray_depth")] = &RendererServices::get_ray_depth;
    m_global_attr_getters[OIIO::ustring("path:ray_length")] = &RendererServices::get_ray_length;
    m_global_attr_getters[OIIO::ustring("path:ray_ior")] = &RendererServices::get_ray_ior;
    m_global_attr_getters[OIIO::ustring("path:ray_has_differentials")] = &RendererServices::get_ray_has_differentials;
    m_global_attr_getters[OIIO::ustring("appleseed:version_major")] = &RendererServices::get_appleseed_version_major;
    m_global_attr_getters[OIIO::ustring("appleseed:version_minor")] = &RendererServices::get_appleseed_version_minor;
    m_global_attr_getters[OIIO::ustring("appleseed:version_patch")] = &RendererServices::get_appleseed_version_patch;
    m_global_attr_getters[OIIO::ustring("appleseed:version")] = &RendererServices::get_appleseed_version;
}

void RendererServices::initialize(TextureStore& texture_store)
{
    m_texture_store = &texture_store;
    m_camera = m_project.get_scene()->get_camera();

    m_cam_projection_str =
        strcmp(m_camera->get_model(), "pinhole_camera") == 0 ? g_perspective_ustr :
        strcmp(m_camera->get_model(), "thinlens_camera") == 0 ? g_perspective_ustr :
        strcmp(m_camera->get_model(), "spherical_camera") == 0 ? g_spherical_ustr :
        g_unknown_proj_ustr;

    m_shutter[0] = static_cast<float>(m_camera->get_shutter_open_time());
    m_shutter[1] = static_cast<float>(m_camera->get_shutter_close_time());
    m_shutter_interval = static_cast<float>(m_camera->get_shutter_open_time_interval());
}

OIIO::TextureSystem* RendererServices::texturesys() const
{
    return &m_texture_sys;
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
    if (from == g_camera_ustr)
    {
        Transformd tmp;
        const Transformd& transform =
            m_camera->transform_sequence().evaluate(time, tmp);
        result = Matrix4f(transform.get_local_to_parent());
        return true;
    }

    return false;
}

bool RendererServices::get_inverse_matrix(
    OSL::ShaderGlobals*     sg,
    OSL::Matrix44&          result,
    OSL::ustring            to,
    float                   time)
{
    if (to == g_camera_ustr)
    {
        Transformd tmp;
        const Transformd& transform =
            m_camera->transform_sequence().evaluate(time, tmp);
        result = Matrix4f(transform.get_parent_to_local());
        return true;
    }

    return OSL::RendererServices::get_inverse_matrix(
        sg,
        result,
        to,
        time);
}

bool RendererServices::get_matrix(
    OSL::ShaderGlobals*     sg,
    OSL::Matrix44&          result,
    OIIO::ustring           from)
{
    if (from == g_camera_ustr)
    {
        if (m_camera->transform_sequence().size() > 1)
            return false;

        const Transformd& tmp =
            m_camera->transform_sequence().get_earliest_transform();
        result = Matrix4f(tmp.get_local_to_parent());
        return true;
    }

    return false;
}

bool RendererServices::get_inverse_matrix(
    OSL::ShaderGlobals*     sg,
    OSL::Matrix44&          result,
    OSL::ustring            to)
{
    if (to == g_camera_ustr)
    {
        if (m_camera->transform_sequence().size() > 1)
            return false;

        const Transformd& tmp =
            m_camera->transform_sequence().get_earliest_transform();
        result = Matrix4f(tmp.get_parent_to_local());
        return true;
    }

    return OSL::RendererServices::get_inverse_matrix(
        sg,
        result,
        to);
}

bool RendererServices::transform_points(
    OSL::ShaderGlobals*         sg,
    OSL::ustring                from,
    OSL::ustring                to,
    float                       time,
    const OSL::Vec3*            Pin,
    OSL::Vec3*                  Pout,
    int                         npoints,
    OSL::TypeDesc::VECSEMANTICS vectype)
{
    return false;
}

bool RendererServices::trace(
    TraceOpt&               options,
    OSL::ShaderGlobals*     sg,
    const OSL::Vec3&        P,
    const OSL::Vec3&        dPdx,
    const OSL::Vec3&        dPdy,
    const OSL::Vec3&        R,
    const OSL::Vec3&        dRdx,
    const OSL::Vec3&        dRdy)
{
    assert(m_texture_store);

    TextureCache texture_cache(*m_texture_store);
    Intersector intersector(m_project.get_trace_context(), texture_cache);

    const ShadingPoint* parent =
        reinterpret_cast<const ShadingPoint*>(sg->renderstate);
    const ShadingPoint* origin_shading_point = 0;

    Vector3d pos = Vector3f(P);

    if (P == sg->P)
    {
        Vector3d front = Vector3f(P);
        Vector3d back = front;

        intersector.fixed_offset(
            parent->get_point(),
            parent->get_geometric_normal(),
            front,
            back);

        pos = sg->N.dot(R) >= 0.0f ? front : back;

        origin_shading_point = parent;
    }

    const Vector3d dir = Vector3f(R);
    const ShadingRay ray(
        pos,
        normalize(dir),
        options.mindist,
        options.maxdist,
        parent->get_ray().m_time,
        VisibilityFlags::ProbeRay,
        parent->get_ray().m_depth + 1);

    ShadingPoint shading_point;
    intersector.trace(
        ray,
        shading_point,
        origin_shading_point);

    ShadingPoint::OSLTraceData* trace_data =
        reinterpret_cast<ShadingPoint::OSLTraceData*>(sg->tracedata);

    trace_data->m_traced = true;

    if (shading_point.hit())
    {
        trace_data->m_hit = true;
        trace_data->m_P = Imath::V3d(shading_point.get_point());
        trace_data->m_hit_distance = static_cast<float>(shading_point.get_distance());
        trace_data->m_N = Imath::V3d(shading_point.get_shading_normal());
        trace_data->m_Ng = Imath::V3d(shading_point.get_geometric_normal());
        const Vector2d& uv = shading_point.get_uv(0);
        trace_data->m_u = static_cast<float>(uv[0]);
        trace_data->m_v = static_cast<float>(uv[1]);
        return true;
    }

    return false;
}

bool RendererServices::getmessage(
    OSL::ShaderGlobals*     sg,
    OIIO::ustring           source,
    OIIO::ustring           name,
    OIIO::TypeDesc          type,
    void*                   val,
    bool                    derivatives)
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

bool RendererServices::get_attribute(
    OSL::ShaderGlobals*     sg,
    bool                    derivatives,
    OIIO::ustring           object,
    OIIO::TypeDesc          type,
    OIIO::ustring           name,
    void*                   val)
{
    // We don't support getting attributes from named objects, yet.
    if (object != g_empty_ustr)
        return false;

    // Try global attributes.
    AttrGetterMapType::const_iterator i = m_global_attr_getters.find(name);
    if (i != m_global_attr_getters.end())
    {
        AttrGetterFun getter = i->second;
        return (this->*(getter))(sg, derivatives, object, type, name, val);
    }

    // Try user data from the current object.
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

// Attribute getters.
#define IMPLEMENT_ATTR_GETTER(name) \
bool RendererServices::get_##name(  \
    OSL::ShaderGlobals* sg,         \
    bool                derivs,     \
    OIIO::ustring       object,     \
    OIIO::TypeDesc      type,       \
    OIIO::ustring       name,       \
    void                *val) const

IMPLEMENT_ATTR_GETTER(object_instance_id)
{
    if (type == OIIO::TypeDesc::TypeInt)
    {
        const ShadingPoint* shading_point =
            reinterpret_cast<const ShadingPoint*>(sg->renderstate);

        reinterpret_cast<int*>(val)[0] =
            static_cast<int>(shading_point->get_object_instance().get_uid());
        clear_attr_derivatives(derivs, type, val);
        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(object_instance_index)
{
    if (type == OIIO::TypeDesc::TypeInt)
    {
        const ShadingPoint* shading_point =
            reinterpret_cast<const ShadingPoint*>(sg->renderstate);

        reinterpret_cast<int*>(val)[0] =
            static_cast<int>(shading_point->get_object_instance_index());
        clear_attr_derivatives(derivs, type, val);
        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(assembly_instance_id)
{
    if (type == OIIO::TypeDesc::TypeInt)
    {
        const ShadingPoint* shading_point =
            reinterpret_cast<const ShadingPoint*>(sg->renderstate);

        reinterpret_cast<int*>(val)[0] =
            static_cast<int>(shading_point->get_assembly_instance().get_uid());
        clear_attr_derivatives(derivs, type, val);
        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(camera_resolution)
{
    if (type == g_int_array2_typedesc)
    {
        Image& img = m_project.get_frame()->image();
        reinterpret_cast<int*>(val)[0] = static_cast<int>(img.properties().m_canvas_width);
        reinterpret_cast<int*>(val)[1] = static_cast<int>(img.properties().m_canvas_height);
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
        reinterpret_cast<float*>(val)[0] = m_shutter[0];
        reinterpret_cast<float*>(val)[1] = m_shutter[1];
        clear_attr_derivatives(derivs, type, val);
        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(camera_shutter_open)
{
    if (type == OIIO::TypeDesc::TypeFloat)
    {
        reinterpret_cast<float*>(val)[0] = m_shutter[0];
        clear_attr_derivatives(derivs, type, val);
        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(camera_shutter_close)
{
    if (type == OIIO::TypeDesc::TypeFloat)
    {
        reinterpret_cast<float*>(val)[0] = m_shutter[1];
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

IMPLEMENT_ATTR_GETTER(ray_depth)
{
    if (type == OIIO::TypeDesc::TypeInt)
    {
        const ShadingPoint* shading_point =
            reinterpret_cast<const ShadingPoint*>(sg->renderstate);

        reinterpret_cast<int*>(val)[0] = static_cast<int>(shading_point->get_ray().m_depth);
        clear_attr_derivatives(derivs, type, val);
        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(ray_length)
{
    if (type == OIIO::TypeDesc::TypeFloat)
    {
        const ShadingPoint* shading_point =
            reinterpret_cast<const ShadingPoint*>(sg->renderstate);

        reinterpret_cast<float*>(val)[0] =
            static_cast<float>(norm(shading_point->get_ray().m_org - shading_point->get_point()));
        clear_attr_derivatives(derivs, type, val);
        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(ray_ior)
{
    if (type == OIIO::TypeDesc::TypeFloat)
    {
        reinterpret_cast<float*>(val)[0] = 1.0f;
        clear_attr_derivatives(derivs, type, val);
        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(ray_has_differentials)
{
    if (type == OIIO::TypeDesc::TypeInt)
    {
        const ShadingPoint* shading_point =
            reinterpret_cast<const ShadingPoint*>(sg->renderstate);

        reinterpret_cast<int*>(val)[0] = static_cast<int>(shading_point->get_ray().m_has_differentials);
        clear_attr_derivatives(derivs, type, val);
        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(appleseed_version_major)
{
    if (type == OIIO::TypeDesc::TypeInt)
    {
        reinterpret_cast<int*>(val)[0] = APPLESEED_VERSION_MAJOR;
        clear_attr_derivatives(derivs, type, val);
        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(appleseed_version_minor)
{
    if (type == OIIO::TypeDesc::TypeInt)
    {
        reinterpret_cast<int*>(val)[0] = APPLESEED_VERSION_MINOR;
        clear_attr_derivatives(derivs, type, val);
        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(appleseed_version_patch)
{
    if (type == OIIO::TypeDesc::TypeInt)
    {
        reinterpret_cast<int*>(val)[0] = APPLESEED_VERSION_PATCH;
        clear_attr_derivatives(derivs, type, val);
        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(appleseed_version)
{
    if (type == OIIO::TypeDesc::TypeInt)
    {
        reinterpret_cast<int*>(val)[0] = APPLESEED_VERSION;
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
    {
        if (type == OIIO::TypeDesc::TypeString)
        {
            reinterpret_cast<OIIO::ustring*>(val)[1] = g_empty_ustr;
            reinterpret_cast<OIIO::ustring*>(val)[2] = g_empty_ustr;
        }
        else
            memset(reinterpret_cast<char*>(val) + type.size(), 0, 2 * type.size());
    }
}

void RendererServices::log_error(const string& message)
{
    if (!message.empty())
        RENDERER_LOG_ERROR("%s", message.c_str());
}

}   // namespace renderer
