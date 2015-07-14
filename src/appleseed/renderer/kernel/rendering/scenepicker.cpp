
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/input/inputbinder.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/material/materialtraits.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/surfaceshader/surfaceshadertraits.h"

// Standard headers.
#include <limits>

using namespace foundation;
using namespace std;

namespace renderer
{

struct ScenePicker::Impl
{
    const TraceContext& m_trace_context;
    TextureStore        m_texture_store;
    TextureCache        m_texture_cache;
    Intersector         m_intersector;

    explicit Impl(const TraceContext& trace_context)
      : m_trace_context(trace_context)
      , m_texture_store(trace_context.get_scene())
      , m_texture_cache(m_texture_store)
      , m_intersector(trace_context, m_texture_cache)
    {
    }
};

ScenePicker::ScenePicker(const TraceContext& trace_context)
  : impl(new Impl(trace_context))
{
}

ScenePicker::~ScenePicker()
{
    delete impl;
}

ScenePicker::PickingResult ScenePicker::pick(const Vector2d& ndc) const
{
    SamplingContext::RNGType rng;
    SamplingContext sampling_context(rng, SamplingContext::QMCMode);

    const Scene& scene = impl->m_trace_context.get_scene();
    const Camera* camera = scene.get_camera();

    ShadingRay ray;
    camera->spawn_ray(sampling_context, Dual2d(ndc), ray);

    ShadingPoint shading_point;
    impl->m_intersector.trace(ray, shading_point);

    PickingResult result;
    result.m_hit = shading_point.hit();
    result.m_primitive_type = shading_point.get_primitive_type();

    if (result.m_hit)
    {
        result.m_point = shading_point.get_point();
        result.m_distance = shading_point.get_distance() * norm(ray.m_dir);
        result.m_assembly_instance_transform = shading_point.get_assembly_instance_transform();
        result.m_assembly_instance = &shading_point.get_assembly_instance();
        result.m_assembly = &shading_point.get_assembly();
        result.m_object_instance = &shading_point.get_object_instance();
        result.m_object = &shading_point.get_object();
    }
    else
    {
        result.m_point = Vector3d(0.0);
        result.m_distance = numeric_limits<double>::max();
        result.m_assembly_instance_transform = Transformd::identity();
        result.m_assembly_instance = 0;
        result.m_assembly = 0;
        result.m_object_instance = 0;
        result.m_object = 0;
    }

    result.m_camera = camera;
    result.m_material = 0;
    result.m_surface_shader = 0;
    result.m_bsdf = 0;
    result.m_bssrdf = 0;
    result.m_edf = 0;

    if (result.m_hit)
    {
        const size_t pa_index = shading_point.get_primitive_attribute_index();

        if (pa_index != Triangle::None)
        {
            const char* material_name =
                result.m_object_instance->get_material_name(pa_index, shading_point.get_side());

            if (material_name)
            {
                result.m_material =
                    InputBinder::find_entity<Material>(
                        material_name,
                        result.m_object_instance->get_parent());
            }
        }
    }

    if (result.m_material)
    {
        const Entity* parent = result.m_material->get_parent();

        const char* ss_name = result.m_material->get_surface_shader_name();
        result.m_surface_shader = ss_name ? InputBinder::find_entity<SurfaceShader>(ss_name, parent) : 0;

        const char* bsdf_name = result.m_material->get_bsdf_name();
        result.m_bsdf = bsdf_name ? InputBinder::find_entity<BSDF>(bsdf_name, parent) : 0;

        const char* bssrdf_name = result.m_material->get_bssrdf_name();
        result.m_bssrdf = bssrdf_name ? InputBinder::find_entity<BSSRDF>(bssrdf_name, parent) : 0;

        const char* edf = result.m_material->get_edf_name();
        result.m_edf = edf ? InputBinder::find_entity<EDF>(edf, parent) : 0;
    }

    return result;
}

}   // namespace renderer
