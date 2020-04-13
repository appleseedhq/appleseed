
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "scenepicker.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/intersection/tracecontext.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/kernel/texturing/texturestore.h"
#include "renderer/modeling/bsdf/bsdftraits.h"
#include "renderer/modeling/bssrdf/bssrdftraits.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/edf/edftraits.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/input/inputbinder.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/material/materialtraits.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/surfaceshader/surfaceshadertraits.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"

// Standard headers.
#include <cstddef>
#include <limits>

using namespace foundation;

namespace renderer
{

struct ScenePicker::Impl
{
    const Project&      m_project;
    const TraceContext& m_trace_context;
    TextureStore        m_texture_store;
    TextureCache        m_texture_cache;
    Intersector         m_intersector;

    explicit Impl(const Project& project)
      : m_project(project)
      , m_trace_context(m_project.get_trace_context())
      , m_texture_store(m_trace_context.get_scene())
      , m_texture_cache(m_texture_store)
      , m_intersector(m_trace_context, m_texture_cache)
    {
    }
};

ScenePicker::ScenePicker(const Project& project)
  : impl(new Impl(project))
{
}

ScenePicker::~ScenePicker()
{
    delete impl;
}

ScenePicker::PickingResult ScenePicker::pick(const Vector2d& ndc) const
{
    PickingResult result;
    result.m_ndc = ndc;
    result.m_hit = false;
    result.m_primitive_type = ShadingPoint::PrimitiveNone;
    result.m_distance = std::numeric_limits<double>::max();
    result.m_bary = Vector2f(0.0);
    result.m_uv = Vector2f(0.0);
    result.m_duvdx = Vector2f(0.0);
    result.m_duvdy = Vector2f(0.0);
    result.m_point = Vector3d(0.0);
    result.m_dpdu = Vector3d(0.0);
    result.m_dpdv = Vector3d(0.0);
    result.m_dndu = Vector3d(0.0);
    result.m_dndv = Vector3d(0.0);
    result.m_dpdx = Vector3d(0.0);
    result.m_dpdy = Vector3d(0.0);
    result.m_geometric_normal = Vector3d(0.0);
    result.m_original_shading_normal = Vector3d(0.0);
    result.m_shading_basis.build(Vector3d(0.0, 1.0, 0.0), Vector3d(1.0, 0.0, 0.0));
    result.m_side = ObjectInstance::FrontSide;
    result.m_camera = impl->m_project.get_uncached_active_camera();
    result.m_assembly_instance = nullptr;
    result.m_assembly_instance_transform = Transformd::make_identity();
    result.m_assembly = nullptr;
    result.m_object_instance = nullptr;
    result.m_object = nullptr;
    result.m_material = nullptr;
    result.m_surface_shader = nullptr;
    result.m_bsdf = nullptr;
    result.m_bssrdf = nullptr;
    result.m_edf = nullptr;

    if (result.m_camera == nullptr)
        return result;

    SamplingContext::RNGType rng;
    SamplingContext sampling_context(rng, SamplingContext::QMCMode);

    ShadingRay ray;
    result.m_camera->spawn_ray(
        sampling_context,
        Dual2d(ndc),
        ray);

    ShadingPoint shading_point;
    impl->m_intersector.trace(ray, shading_point);

    result.m_hit = shading_point.hit_surface();

    if (!result.m_hit)
        return result;

    result.m_primitive_type = shading_point.get_primitive_type();
    result.m_distance = shading_point.get_distance() * norm(ray.m_dir);

    result.m_bary = shading_point.get_bary();
    result.m_uv = shading_point.get_uv(0);
    result.m_duvdx = shading_point.get_duvdx(0);
    result.m_duvdy = shading_point.get_duvdy(0);
    result.m_point = shading_point.get_point();
    result.m_dpdu = shading_point.get_dpdu(0);
    result.m_dpdv = shading_point.get_dpdv(0);
    result.m_dndu = shading_point.get_dndu(0);
    result.m_dndv = shading_point.get_dndv(0);
    result.m_dpdx = shading_point.get_dpdx();
    result.m_dpdy = shading_point.get_dpdy();
    result.m_geometric_normal = shading_point.get_geometric_normal();
    result.m_original_shading_normal = shading_point.get_original_shading_normal();
    result.m_shading_basis = shading_point.get_shading_basis();
    result.m_side = shading_point.get_side();

    result.m_assembly_instance = &shading_point.get_assembly_instance();
    result.m_assembly_instance_transform = shading_point.get_assembly_instance_transform();
    result.m_assembly = &shading_point.get_assembly();
    result.m_object_instance = &shading_point.get_object_instance();
    result.m_object = &shading_point.get_object();

    const size_t pa_index = shading_point.get_primitive_attribute_index();

    if (pa_index != Triangle::None)
    {
        const char* material_name =
            result.m_object_instance->get_material_name(pa_index, shading_point.get_side());

        if (material_name)
        {
            result.m_material =
                InputBinder::static_find_entity<Material>(
                    material_name,
                    result.m_object_instance->get_parent());
        }
    }

    if (result.m_material)
    {
        const Entity* parent = result.m_material->get_parent();

        const char* ss_name = result.m_material->get_surface_shader_name();
        result.m_surface_shader = ss_name ? InputBinder::static_find_entity<SurfaceShader>(ss_name, parent) : nullptr;

        const char* bsdf_name = result.m_material->get_bsdf_name();
        result.m_bsdf = bsdf_name ? InputBinder::static_find_entity<BSDF>(bsdf_name, parent) : nullptr;

        const char* bssrdf_name = result.m_material->get_bssrdf_name();
        result.m_bssrdf = bssrdf_name ? InputBinder::static_find_entity<BSSRDF>(bssrdf_name, parent) : nullptr;

        const char* edf = result.m_material->get_edf_name();
        result.m_edf = edf ? InputBinder::static_find_entity<EDF>(edf, parent) : nullptr;
    }

    return result;
}

}   // namespace renderer
