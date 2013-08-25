
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
#include "sppmpasscallback.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/lighting/sppm/sppmphotonmap.h"
#include "renderer/kernel/lighting/lightsampler.h"
#include "renderer/kernel/lighting/pathtracer.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/rng.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cassert>
#include <cstddef>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// SPPMPassCallback class implementation.
//

SPPMPassCallback::SPPMPassCallback(
    const LightSampler&     light_sampler,
    const TraceContext&     trace_context,
    TextureStore&           texture_store,
    const ParamArray&       params)
  : m_params(params)
  , m_light_sampler(light_sampler)
  , m_texture_cache(texture_store)
  , m_intersector(trace_context, m_texture_cache /*, m_params.m_report_self_intersections*/)
{
}

void SPPMPassCallback::release()
{
    delete this;
}

void SPPMPassCallback::pre_render()
{
    // Trace photons.
    PhotonVector photons;
    trace_photons(photons);

    // Build the photon map.
    m_photon_map.reset(new SPPMPhotonMap(photons));
}

void SPPMPassCallback::trace_photons(PhotonVector& photons)
{
    const size_t PhotonCount = 1000000;

    RENDERER_LOG_INFO(
        "tracing " FMT_SIZE_T " photon%s...",
        PhotonCount,
        PhotonCount > 1 ? "s" : "");

    photons.reserve(PhotonCount);

    const float flux_multiplier = 1.0f / PhotonCount;

    MersenneTwister rng;
    SamplingContext sampling_context(
        rng,
        4,
        PhotonCount,
        0);

    for (size_t i = 0; i < PhotonCount; ++i)
        trace_photon(sampling_context, flux_multiplier, photons);
}

void SPPMPassCallback::trace_photon(
    SamplingContext&        sampling_context,
    const float             flux_multiplier,
    PhotonVector&           photons)
{
    LightSample light_sample;
    m_light_sampler.sample(sampling_context.next_vector2<4>(), light_sample);

    if (light_sample.m_triangle)
    {
        trace_emitting_triangle_photon(
            sampling_context,
            light_sample,
            flux_multiplier,
            photons);
    }
    else
    {
        trace_non_physical_light_photon(
            sampling_context,
            light_sample,
            flux_multiplier,
            photons);
    }
}

void SPPMPassCallback::trace_emitting_triangle_photon(
    SamplingContext&        sampling_context,
    LightSample&            light_sample,
    const float             flux_multiplier,
    PhotonVector&           photons)
{
    // Make sure the geometric normal of the light sample is in the same hemisphere as the shading normal.
    light_sample.m_geometric_normal =
        flip_to_same_hemisphere(
            light_sample.m_geometric_normal,
            light_sample.m_shading_normal);

    const EDF* edf = light_sample.m_triangle->m_edf;

    // Evaluate the EDF inputs.
    InputEvaluator input_evaluator(m_texture_cache);
    const void* edf_data =
        input_evaluator.evaluate(edf->get_inputs(), light_sample.m_bary);

    // Sample the EDF.
    SamplingContext child_sampling_context = sampling_context.split(2, 1);
    Vector3d emission_direction;
    Spectrum edf_value;
    double edf_prob;
    edf->sample(
        edf_data,
        light_sample.m_geometric_normal,
        Basis3d(light_sample.m_shading_normal),
        child_sampling_context.next_vector2<2>(),
        emission_direction,
        edf_value,
        edf_prob);

    // Compute the initial particle weight.
    Spectrum initial_alpha = edf_value;
    initial_alpha *=
        static_cast<float>(
            flux_multiplier *
            dot(emission_direction, light_sample.m_shading_normal)
                / (light_sample.m_probability * edf_prob));

    // Manufacture a shading point at the position of the light sample.
    // It will be used to avoid self-intersections.
    ShadingPoint parent_shading_point;
    m_intersector.manufacture_hit(
        parent_shading_point,
        ShadingRay(light_sample.m_point, emission_direction, 0.0, 0.0, 0.0f, ~0),
        light_sample.m_triangle->m_assembly_instance,
        light_sample.m_triangle->m_assembly_instance->transform_sequence().get_earliest_transform(),
        light_sample.m_triangle->m_object_instance_index,
        light_sample.m_triangle->m_region_index,
        light_sample.m_triangle->m_triangle_index,
        light_sample.m_triangle->m_triangle_support_plane);

    // Build the light ray.
    child_sampling_context.split_in_place(1, 1);
    const ShadingRay light_ray(
        light_sample.m_point,
        emission_direction,
        child_sampling_context.next_double2(),
        ~0);

    // Build the path tracer.
    PathVisitor path_visitor(
        m_params,
        initial_alpha,
        photons);
    PathTracer<PathVisitor, true> path_tracer(      // true = adjoint
        path_visitor,
        3,                                          // m_params.m_rr_min_path_length
        ~0,                                         // m_params.m_max_path_length
        1000);                                      // m_params.m_max_iterations

    // Trace the light path.
    const size_t path_length =
        path_tracer.trace(
            child_sampling_context,
            m_intersector,
            m_texture_cache,
            light_ray,
            &parent_shading_point);

    // Update path statistics.
    //++m_path_count;
    //m_path_length.insert(path_length);
}

void SPPMPassCallback::trace_non_physical_light_photon(
    SamplingContext&        sampling_context,
    LightSample&            light_sample,
    const float             flux_multiplier,
    PhotonVector&           photons)
{
}


//
// SPPMPassCallback::PathVisitor class implementation.
//

SPPMPassCallback::PathVisitor::PathVisitor(
    const ParamArray&       params,
    const Spectrum&         initial_alpha,
    PhotonVector&           photons)
  : m_initial_alpha(initial_alpha)
  , m_photons(photons)
{
}

bool SPPMPassCallback::PathVisitor::accept_scattering_mode(
    const BSDF::Mode        prev_bsdf_mode,
    const BSDF::Mode        bsdf_mode) const
{
    assert(bsdf_mode != BSDF::Absorption);
    return true;
}

bool SPPMPassCallback::PathVisitor::visit_vertex(const PathVertex& vertex)
{
    SPPMPhoton photon;
    photon.m_position = vertex.get_point();

    photon.m_payload.m_incoming = -vertex.get_ray().m_dir;
    photon.m_payload.m_flux = m_initial_alpha;
    photon.m_payload.m_flux *= vertex.m_throughput;

    m_photons.push_back(photon);

    return true;
}

void SPPMPassCallback::PathVisitor::visit_environment(
    const ShadingPoint&     shading_point,
    const Vector3d&         outgoing,
    const BSDF::Mode        prev_bsdf_mode,
    const double            prev_bsdf_prob,
    const Spectrum&         throughput)
{
    // The photon escapes.
}

}   // namespace renderer
