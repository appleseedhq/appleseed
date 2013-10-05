
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/kernel/texturing/texturestore.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdftraits.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/edf/edftraits.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/input/inputbinder.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/material/materialtraits.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/modeling/surfaceshader/surfaceshadertraits.h"

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
    InputBinder         m_input_binder;

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
    SamplingContext sampling_context(rng);

    const Scene& scene = impl->m_trace_context.get_scene();
    const Camera* camera = scene.get_camera();

    ShadingRay ray;
    camera->generate_ray(
        sampling_context,
        ndc,
        ray);

    ShadingPoint shading_point;
    impl->m_intersector.trace(ray, shading_point);

    const bool hit = shading_point.hit();

    PickingResult result;
    result.m_camera = camera;
    result.m_hit = hit;
    result.m_assembly_instance = hit ? &shading_point.get_assembly_instance() : 0;
    result.m_assembly = hit ? &shading_point.get_assembly() : 0;
    result.m_object_instance = hit ? &shading_point.get_object_instance() : 0;
    result.m_object = hit ? &shading_point.get_object() : 0;
    result.m_material = 0;
    result.m_surface_shader = 0;
    result.m_bsdf = 0;
    result.m_edf = 0;

    if (hit)
    {
        const size_t pa_index = shading_point.get_primitive_attribute_index();

        if (pa_index != Triangle::None)
        {
            const char* material_name = result.m_object_instance->get_material_name(pa_index, shading_point.get_side());
            result.m_material = impl->m_input_binder.find_entity<Material>(material_name, result.m_object_instance->get_parent());
        }
    }

    if (result.m_material)
    {
        const string surface_shader_name = result.m_material->get_parameters().get_optional<string>("surface_shader", "");
        result.m_surface_shader = impl->m_input_binder.find_entity<SurfaceShader>(surface_shader_name.c_str(), result.m_material->get_parent());

        const string bsdf_name = result.m_material->get_parameters().get_optional<string>("bsdf", "");
        result.m_bsdf = impl->m_input_binder.find_entity<BSDF>(bsdf_name.c_str(), result.m_material->get_parent());

        const string edf_name = result.m_material->get_parameters().get_optional<string>("edf", "");
        result.m_edf = impl->m_input_binder.find_entity<EDF>(edf_name.c_str(), result.m_material->get_parent());
    }

    return result;
}

}   // namespace renderer
