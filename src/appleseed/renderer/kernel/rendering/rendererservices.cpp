
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2013 Esteban Tovagliari, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/image/canvasproperties.h"
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
    OIIO::ustring g_object_ustr("object");
    OIIO::ustring g_raster_ustr("raster");
    OIIO::ustring g_screen_ustr("screen");
    OIIO::ustring g_shader_ustr("shader");
    OIIO::ustring g_world_ustr("world");
}

RendererServices::RendererServices(
    const Project&              project,
    OIIO::TextureSystem&        texture_sys)
  : OSL::RendererServices(&texture_sys)
  , m_texture_sys(texture_sys)
  , m_project(project)
  , m_texture_store(0)
{
    // Set up attribute getters.
    m_global_attr_getters[OIIO::ustring("object:object_instance_id")] = &RendererServices::get_attr_object_instance_id;
    m_global_attr_getters[OIIO::ustring("object:object_instance_index")] = &RendererServices::get_attr_object_instance_index;
    m_global_attr_getters[OIIO::ustring("object:assembly_instance_id")] = &RendererServices::get_attr_assembly_instance_id;
    m_global_attr_getters[OIIO::ustring("camera:resolution")] = &RendererServices::get_attr_camera_resolution;
    m_global_attr_getters[OIIO::ustring("camera:projection")] = &RendererServices::get_attr_camera_projection;
    m_global_attr_getters[OIIO::ustring("camera:pixelaspect")] = &RendererServices::get_attr_camera_pixelaspect;
    m_global_attr_getters[OIIO::ustring("camera:screen_window")] = &RendererServices::get_attr_camera_screen_window;
    m_global_attr_getters[OIIO::ustring("camera:fov")] = &RendererServices::get_attr_camera_fov;
    m_global_attr_getters[OIIO::ustring("camera:clip")] = &RendererServices::get_attr_camera_clip;
    m_global_attr_getters[OIIO::ustring("camera:clip_near")] = &RendererServices::get_attr_camera_clip_near;
    m_global_attr_getters[OIIO::ustring("camera:clip_far")] = &RendererServices::get_attr_camera_clip_far;
    m_global_attr_getters[OIIO::ustring("camera:shutter")] = &RendererServices::get_attr_camera_shutter;
    m_global_attr_getters[OIIO::ustring("camera:shutter_open")] = &RendererServices::get_attr_camera_shutter_open;
    m_global_attr_getters[OIIO::ustring("camera:shutter_close")] = &RendererServices::get_attr_camera_shutter_close;
    m_global_attr_getters[OIIO::ustring("path:ray_depth")] = &RendererServices::get_attr_ray_depth;
    m_global_attr_getters[OIIO::ustring("path:ray_length")] = &RendererServices::get_attr_ray_length;
    m_global_attr_getters[OIIO::ustring("path:ray_ior")] = &RendererServices::get_attr_ray_ior;
    m_global_attr_getters[OIIO::ustring("path:ray_has_differentials")] = &RendererServices::get_attr_ray_has_differentials;
    m_global_attr_getters[OIIO::ustring("appleseed:version_major")] = &RendererServices::get_attr_appleseed_version_major;
    m_global_attr_getters[OIIO::ustring("appleseed:version_minor")] = &RendererServices::get_attr_appleseed_version_minor;
    m_global_attr_getters[OIIO::ustring("appleseed:version_patch")] = &RendererServices::get_attr_appleseed_version_patch;
    m_global_attr_getters[OIIO::ustring("appleseed:version")] = &RendererServices::get_attr_appleseed_version;
    m_global_attr_getters[OIIO::ustring("surface_shader:color")] = &RendererServices::get_attr_surface_shader_color;
    m_global_attr_getters[OIIO::ustring("surface_shader:alpha")] = &RendererServices::get_attr_surface_shader_alpha;

    // Set up user data getters.
    m_global_user_data_getters[OIIO::ustring("Tn")] = &RendererServices::get_user_data_tn;
    m_global_user_data_getters[OIIO::ustring("Bn")] = &RendererServices::get_user_data_bn;
    m_global_user_data_getters[OIIO::ustring("dNdu")] = &RendererServices::get_user_data_dndu;
    m_global_user_data_getters[OIIO::ustring("dNdv")] = &RendererServices::get_user_data_dndv;
}

void RendererServices::initialize(TextureStore& texture_store)
{
    m_texture_store = &texture_store;
    m_camera = m_project.get_uncached_active_camera();

    if (m_camera)
    {
        m_cam_projection_str =
            strcmp(m_camera->get_model(), "pinhole_camera") == 0 ? g_perspective_ustr :
            strcmp(m_camera->get_model(), "thinlens_camera") == 0 ? g_perspective_ustr :
            strcmp(m_camera->get_model(), "spherical_camera") == 0 ? g_spherical_ustr :
            g_unknown_proj_ustr;

        m_shutter[0] = m_camera->get_shutter_open_time();
        m_shutter[1] = m_camera->get_shutter_close_time();
        m_shutter_interval = m_camera->get_shutter_open_time_interval();
    }

    const CanvasProperties& props = m_project.get_frame()->image().properties();
    m_resolution[0] = static_cast<int>(props.m_canvas_width);
    m_resolution[1] = static_cast<int>(props.m_canvas_height);
}

OIIO::TextureSystem* RendererServices::texturesys() const
{
    return &m_texture_sys;
}

bool RendererServices::get_matrix(
    OSL::ShaderGlobals*         sg,
    OSL::Matrix44&              result,
    OSL::TransformationPtr      xform,
    float                       time)
{
    if (!xform)
        return false;

    const ShadingPoint::OSLObjectTransformInfo* obj_xform =
        reinterpret_cast<const ShadingPoint::OSLObjectTransformInfo*>(xform);

    result = obj_xform->get_transform(time);
    return true;
}

bool RendererServices::get_inverse_matrix(
    OSL::ShaderGlobals*         sg,
    OSL::Matrix44&              result,
    OSL::TransformationPtr      xform,
    float                       time)
{
    if (!xform)
        return false;

    const ShadingPoint::OSLObjectTransformInfo* obj_xform =
        reinterpret_cast<const ShadingPoint::OSLObjectTransformInfo*>(xform);

    result = obj_xform->get_inverse_transform(time);
    return true;
}

bool RendererServices::get_matrix(
    OSL::ShaderGlobals*         sg,
    OSL::Matrix44&              result,
    OSL::TransformationPtr      xform)
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
    OSL::ShaderGlobals*         sg,
    OSL::Matrix44&              result,
    OSL::TransformationPtr      xform)
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
    OSL::ShaderGlobals*         sg,
    OSL::Matrix44&              result,
    OIIO::ustring               from,
    float                       time)
{
    if (from == g_camera_ustr)
    {
        Transformd scratch;
        const Transformd& transform =
            m_camera->transform_sequence().evaluate(time, scratch);
        result = Matrix4f(transform.get_local_to_parent());
        return true;
    }

    return false;
}

bool RendererServices::get_inverse_matrix(
    OSL::ShaderGlobals*         sg,
    OSL::Matrix44&              result,
    OSL::ustring                to,
    float                       time)
{
    if (to == g_camera_ustr)
    {
        Transformd scratch;
        const Transformd& transform =
            m_camera->transform_sequence().evaluate(time, scratch);
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
    OSL::ShaderGlobals*         sg,
    OSL::Matrix44&              result,
    OIIO::ustring               from)
{
    if (from == g_camera_ustr)
    {
        if (m_camera->transform_sequence().size() > 1)
            return false;

        const Transformd& scratch =
            m_camera->transform_sequence().get_earliest_transform();
        result = Matrix4f(scratch.get_local_to_parent());
        return true;
    }

    return false;
}

bool RendererServices::get_inverse_matrix(
    OSL::ShaderGlobals*         sg,
    OSL::Matrix44&              result,
    OSL::ustring                to)
{
    if (to == g_camera_ustr)
    {
        if (m_camera->transform_sequence().size() > 1)
            return false;

        const Transformd& scratch =
            m_camera->transform_sequence().get_earliest_transform();
        result = Matrix4f(scratch.get_parent_to_local());
        return true;
    }

    return OSL::RendererServices::get_inverse_matrix(sg, result, to);
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
    // We only transform points for now.
    if (vectype != OSL::TypeDesc::POINT)
        return false;

    if (to == g_NDC_ustr || to == g_raster_ustr)
    {
        if (from == g_world_ustr || from == g_common_ustr || from == g_shader_ustr)
            memcpy(Pout, Pin, npoints * sizeof(OSL::Vec3));
        else if (from == g_object_ustr)
        {
            OSL::Matrix44 m;
            get_matrix(sg, m, sg->object2common, time);

            // Convert from object to world.
            for (int i = 0; i < npoints; ++i)
                Pout[i] = Pin[i] * m;

            from = g_world_ustr;
        }
        else if (from == g_camera_ustr)
        {
            OSL::Matrix44 m;
            get_matrix(sg, m, g_camera_ustr, time);

            // Convert from camera to world.
            for (int i = 0; i < npoints; ++i)
                Pout[i] = Pin[i] * m;

            from = g_world_ustr;
        }
        else return false;

        // Transform to NDC.
        for (size_t i = 0; i < npoints; ++i)
        {
            Vector2d ndc;
            if (m_camera->project_point(time, Vector3d(Pout[0].x, Pout[0].y, Pout[0].z), ndc))
            {
                Pout[i] = OSL::Vec3(
                    static_cast<float>(ndc[0]),
                    static_cast<float>(ndc[1]),
                    0.0f);
            }
            else
                Pout[i] = OSL::Vec3(-1.0f, -1.0f, 0.0f);
        }

        if (to == g_raster_ustr)
        {
            // Transform from NDC to raster.
            for (int i = 0; i < npoints; ++i)
            {
                Pout[i] = OSL::Vec3(
                    Pout[i].x * m_resolution[0],
                    Pout[i].y * m_resolution[1],
                    Pout[i].z);
            }
        }

        return true;
    }

    return false;
}

bool RendererServices::trace(
    TraceOpt&                   options,
    OSL::ShaderGlobals*         sg,
    const OSL::Vec3&            P,
    const OSL::Vec3&            dPdx,
    const OSL::Vec3&            dPdy,
    const OSL::Vec3&            R,
    const OSL::Vec3&            dRdx,
    const OSL::Vec3&            dRdy)
{
    assert(m_texture_store);

    const ShadingPoint* parent =
        reinterpret_cast<const ShadingPoint*>(sg->renderstate);

    Vector3d pos;
    const ShadingPoint* origin_shading_point;

    if (P == sg->P)
    {
        Vector3d front(P);
        Vector3d back = front;

        Intersector::fixed_offset(
            parent->get_point(),
            parent->get_geometric_normal(),
            front,
            back);

        pos = sg->N.dot(R) >= 0.0f ? front : back;
        origin_shading_point = parent;
    }
    else
    {
        pos = Vector3d(P);
        origin_shading_point = 0;
    }

    const Vector3d dir(R);
    const ShadingRay ray(
        pos,
        normalize(dir),
        options.mindist,
        options.maxdist,
        parent->get_ray().m_time,
        VisibilityFlags::ProbeRay,
        parent->get_ray().m_depth + 1);

    // todo: move this out of the hot code path (but it must remain thread-local).
    TextureCache texture_cache(*m_texture_store);
    Intersector intersector(m_project.get_trace_context(), texture_cache);

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
        const Vector2f& uv = shading_point.get_uv(0);
        trace_data->m_u = uv[0];
        trace_data->m_v = uv[1];
        return true;
    }

    return false;
}

bool RendererServices::getmessage(
    OSL::ShaderGlobals*         sg,
    OIIO::ustring               source,
    OIIO::ustring               name,
    OIIO::TypeDesc              type,
    void*                       val,
    bool                        derivatives)
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
    OSL::ShaderGlobals*         sg,
    bool                        derivatives,
    OIIO::ustring               object,
    OIIO::TypeDesc              type,
    OIIO::ustring               name,
    void*                       val)
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
    OSL::ShaderGlobals*         sg,
    bool                        derivatives,
    OIIO::ustring               object,
    OIIO::TypeDesc              type,
    OIIO::ustring               name,
    int                         index,
    void*                       val)
{
    return false;
}

bool RendererServices::get_userdata(
    bool                        derivatives,
    OIIO::ustring               name,
    OIIO::TypeDesc              type,
    OSL::ShaderGlobals*         sg,
    void*                       val)
{
    UserDataGetterMapType::const_iterator i = m_global_user_data_getters.find(name);
    if (i != m_global_user_data_getters.end())
    {
        UserDataGetterFun getter = i->second;
        return (this->*(getter))(derivatives, name, type, sg, val);
    }

    return false;
}

#if OSL_LIBRARY_VERSION_CODE < 10700

bool RendererServices::has_userdata(
    OIIO::ustring               name,
    OIIO::TypeDesc              type,
    OSL::ShaderGlobals*         sg)
{
    UserDataGetterMapType::const_iterator i = m_global_user_data_getters.find(name);
    return i != m_global_user_data_getters.end();
}

#endif

#define IMPLEMENT_ATTR_GETTER(name)         \
    bool RendererServices::get_attr_##name( \
        OSL::ShaderGlobals*     sg,         \
        bool                    derivs,     \
        OIIO::ustring           object,     \
        OIIO::TypeDesc          type,       \
        OIIO::ustring           name,       \
        void*                   val) const

IMPLEMENT_ATTR_GETTER(object_instance_id)
{
    if (type == OIIO::TypeDesc::TypeInt)
    {
        const ShadingPoint* shading_point =
            reinterpret_cast<const ShadingPoint*>(sg->renderstate);
        reinterpret_cast<int*>(val)[0] =
            static_cast<int>(shading_point->get_object_instance().get_uid());

        if (derivs)
            clear_derivatives(type, val);

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

        if (derivs)
            clear_derivatives(type, val);

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

        if (derivs)
            clear_derivatives(type, val);

        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(camera_resolution)
{
    if (type == g_int_array2_typedesc)
    {
        reinterpret_cast<int*>(val)[0] = m_resolution[0];
        reinterpret_cast<int*>(val)[1] = m_resolution[1];

        if (derivs)
            clear_derivatives(type, val);

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

        if (derivs)
            clear_derivatives(type, val);

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

        if (derivs)
            clear_derivatives(type, val);

        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(camera_clip_near)
{
    if (type == OIIO::TypeDesc::TypeFloat)
    {
        reinterpret_cast<float*>(val)[0] = 0.0f;

        if (derivs)
            clear_derivatives(type, val);

        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(camera_clip_far)
{
    if (type == OIIO::TypeDesc::TypeFloat)
    {
        reinterpret_cast<float*>(val)[0] = numeric_limits<float>::max();

        if (derivs)
            clear_derivatives(type, val);

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

        if (derivs)
            clear_derivatives(type, val);

        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(camera_shutter_open)
{
    if (type == OIIO::TypeDesc::TypeFloat)
    {
        reinterpret_cast<float*>(val)[0] = m_shutter[0];

        if (derivs)
            clear_derivatives(type, val);

        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(camera_shutter_close)
{
    if (type == OIIO::TypeDesc::TypeFloat)
    {
        reinterpret_cast<float*>(val)[0] = m_shutter[1];

        if (derivs)
            clear_derivatives(type, val);

        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(camera_screen_window)
{
    if (type == g_float_array4_typedesc)
    {
        const float aspect = static_cast<float>(m_resolution[0]) / m_resolution[1];
        reinterpret_cast<float*>(val)[0] = -aspect;
        reinterpret_cast<float*>(val)[1] = -1.0f;
        reinterpret_cast<float*>(val)[2] =  aspect;
        reinterpret_cast<float*>(val)[3] =  1.0f;

        if (derivs)
            clear_derivatives(type, val);

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

        if (derivs)
            clear_derivatives(type, val);

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
        reinterpret_cast<float*>(val)[0] = static_cast<float>(shading_point->get_distance());

        if (derivs)
            clear_derivatives(type, val);

        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(ray_ior)
{
    if (type == OIIO::TypeDesc::TypeFloat)
    {
        const ShadingPoint* shading_point =
            reinterpret_cast<const ShadingPoint*>(sg->renderstate);
        reinterpret_cast<float*>(val)[0] = shading_point->get_ray().get_current_ior();

        if (derivs)
            clear_derivatives(type, val);

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

        if (derivs)
            clear_derivatives(type, val);

        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(appleseed_version_major)
{
    if (type == OIIO::TypeDesc::TypeInt)
    {
        reinterpret_cast<int*>(val)[0] = APPLESEED_VERSION_MAJOR;

        if (derivs)
            clear_derivatives(type, val);

        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(appleseed_version_minor)
{
    if (type == OIIO::TypeDesc::TypeInt)
    {
        reinterpret_cast<int*>(val)[0] = APPLESEED_VERSION_MINOR;

        if (derivs)
            clear_derivatives(type, val);

        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(appleseed_version_patch)
{
    if (type == OIIO::TypeDesc::TypeInt)
    {
        reinterpret_cast<int*>(val)[0] = APPLESEED_VERSION_PATCH;

        if (derivs)
            clear_derivatives(type, val);

        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(appleseed_version)
{
    if (type == OIIO::TypeDesc::TypeInt)
    {
        reinterpret_cast<int*>(val)[0] = APPLESEED_VERSION;

        if (derivs)
            clear_derivatives(type, val);

        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(surface_shader_color)
{
    if (type == OIIO::TypeDesc::TypeColor)
    {
        const ShadingPoint* shading_point =
            reinterpret_cast<const ShadingPoint*>(sg->renderstate);
        reinterpret_cast<float*>(val)[0] = shading_point->m_surface_shader_color[0];
        reinterpret_cast<float*>(val)[1] = shading_point->m_surface_shader_color[1];
        reinterpret_cast<float*>(val)[2] = shading_point->m_surface_shader_color[2];

        if (derivs)
            clear_derivatives(type, val);

        return true;
    }

    return false;
}

IMPLEMENT_ATTR_GETTER(surface_shader_alpha)
{
    if (type == OIIO::TypeDesc::TypeFloat)
    {
        const ShadingPoint* shading_point =
            reinterpret_cast<const ShadingPoint*>(sg->renderstate);
        reinterpret_cast<float*>(val)[0] = shading_point->m_surface_shader_alpha;

        if (derivs)
            clear_derivatives(type, val);

        return true;
    }

    return false;
}

#undef IMPLEMENT_ATTR_GETTER

#define IMPLEMENT_USER_DATA_GETTER(name)         \
    bool RendererServices::get_user_data_##name( \
        bool                    derivatives,     \
        OIIO::ustring           name,            \
        OIIO::TypeDesc          type,            \
        OSL::ShaderGlobals*     sg,              \
        void*                   val) const

IMPLEMENT_USER_DATA_GETTER(tn)
{
    if (type == OIIO::TypeDesc::TypeVector)
    {
        const ShadingPoint* shading_point =
            reinterpret_cast<const ShadingPoint*>(sg->renderstate);

        const Vector3d& tn = shading_point->get_shading_basis().get_tangent_u();
        OSL::Vec3 v(
            static_cast<float>(tn.x),
            static_cast<float>(tn.y),
            static_cast<float>(tn.z));

        reinterpret_cast<float*>(val)[0] = v.x;
        reinterpret_cast<float*>(val)[1] = v.y;
        reinterpret_cast<float*>(val)[2] = v.z;

        if (derivatives)
            clear_derivatives(type, val);

        return true;
    }

    return false;
}

IMPLEMENT_USER_DATA_GETTER(bn)
{
    if (type == OIIO::TypeDesc::TypeVector)
    {
        const ShadingPoint* shading_point =
            reinterpret_cast<const ShadingPoint*>(sg->renderstate);

        const Vector3d& bn = shading_point->get_shading_basis().get_tangent_v();
        OSL::Vec3 v(
            static_cast<float>(bn.x),
            static_cast<float>(bn.y),
            static_cast<float>(bn.z));

        // Make sure Bn points in the same direction as dPdv.
        if (v.dot(sg->dPdv) < 0.0f)
            v = -v;

        reinterpret_cast<float*>(val)[0] = v.x;
        reinterpret_cast<float*>(val)[1] = v.y;
        reinterpret_cast<float*>(val)[2] = v.z;

        if (derivatives)
            clear_derivatives(type, val);

        return true;
    }

    return false;
}

IMPLEMENT_USER_DATA_GETTER(dndu)
{
    if (type == OIIO::TypeDesc::TypeVector)
    {
        const ShadingPoint* shading_point =
            reinterpret_cast<const ShadingPoint*>(sg->renderstate);

        const Vector3d& dndu = shading_point->get_dndu(0);
        reinterpret_cast<float*>(val)[0] = static_cast<float>(dndu.x);
        reinterpret_cast<float*>(val)[1] = static_cast<float>(dndu.y);
        reinterpret_cast<float*>(val)[2] = static_cast<float>(dndu.z);

        if (derivatives)
            clear_derivatives(type, val);

        return true;
    }

    return false;
}

IMPLEMENT_USER_DATA_GETTER(dndv)
{
    if (type == OIIO::TypeDesc::TypeVector)
    {
        const ShadingPoint* shading_point =
            reinterpret_cast<const ShadingPoint*>(sg->renderstate);

        const Vector3d& dndv = shading_point->get_dndv(0);
        reinterpret_cast<float*>(val)[0] = static_cast<float>(dndv.x);
        reinterpret_cast<float*>(val)[1] = static_cast<float>(dndv.y);
        reinterpret_cast<float*>(val)[2] = static_cast<float>(dndv.z);

        if (derivatives)
            clear_derivatives(type, val);

        return true;
    }

    return false;
}

#undef IMPLEMENT_USER_DATA_GETTER

void RendererServices::clear_derivatives(
    const OIIO::TypeDesc&       type,
    void*                       val)
{
    if (type != OIIO::TypeDesc::TypeString)
        memset(reinterpret_cast<char*>(val) + type.size(), 0, 2 * type.size());
}

}   // namespace renderer
