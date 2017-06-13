
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Artem Bishev, The appleseedhq Organization
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
#include "materialsamplers.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/tracer.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfsample.h"
#include "renderer/modeling/phasefunction/phasefunction.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// BSDFSampler class implementation.
//

BSDFSampler::BSDFSampler(
    const BSDF&             bsdf,
    const void*             bsdf_data,
    const int               bsdf_sampling_modes,
    const ShadingPoint&     shading_point)
  : m_bsdf(bsdf)
  , m_bsdf_data(bsdf_data)
  , m_bsdf_sampling_modes(bsdf_sampling_modes)
  , m_shading_basis(shading_point.get_shading_basis())
  , m_geometric_normal(shading_point.get_geometric_normal())
  , m_shading_point(shading_point)
{
}

const Vector3d& BSDFSampler::get_point() const
{
    return m_shading_point.get_point();
}

bool BSDFSampler::contributes_to_light_sampling() const
{
    // There cannot be any contribution for purely specular BSDFs.
    return !m_bsdf.is_purely_specular();
}

float BSDFSampler::trace_between(
    const ShadingContext&   shading_context,
    const Vector3d&         target_position) const
{
    return
        shading_context.get_tracer().trace_between(
            m_shading_point,
            target_position,
            VisibilityFlags::ShadowRay);
}

bool BSDFSampler::sample(
    SamplingContext&        sampling_context,
    const Dual3d&           outgoing,
    Dual3f&                 incoming,
    Spectrum&               value,
    float&                  pdf) const
{
    BSDFSample sample(&m_shading_point, Dual3f(outgoing));
    m_bsdf.sample(
        sampling_context,
        m_bsdf_data,
        false,              // not adjoint
        true,               // multiply by |cos(incoming, normal)|
        m_bsdf_sampling_modes,
        sample);

    // Filter scattering modes.
    if (!(m_bsdf_sampling_modes & sample.m_mode))
        return false;

    incoming = sample.m_incoming;
    value = sample.m_value;
    pdf = sample.m_probability;

    return true;
}

float BSDFSampler::evaluate(
    const int               light_sampling_modes,
    const Vector3f&         outgoing,
    const Vector3f&         incoming,
    Spectrum&               value) const
{
    return
        m_bsdf.evaluate(
            m_bsdf_data,
            false,          // not adjoint
            true,           // multiply by |cos(incoming, normal)|
            Vector3f(m_geometric_normal),
            Basis3f(m_shading_basis),
            Vector3f(outgoing),
            Vector3f(incoming),
            light_sampling_modes,
            value);
}

const ShadingPoint& BSDFSampler::trace(
    const ShadingContext&   shading_context,
    const Vector3f&         direction,
    float&                  transmission) const
{
    return
        shading_context.get_tracer().trace(
            m_shading_point,
            Vector3d(direction),
            VisibilityFlags::ShadowRay,
            transmission);
}

bool BSDFSampler::cull_incoming_direction(const Vector3d& incoming) const
{
    // Cull light samples behind the shading surface if the BSDF
    // is either reflective or transmissive, but not both.
    if (m_bsdf.get_type() != BSDF::AllBSDFTypes)
    {
        double cos_in = dot(incoming, m_shading_basis.get_normal());
        if (m_bsdf.get_type() == BSDF::Transmissive)
            cos_in = -cos_in;
        if (cos_in <= 0.0)
            return true;
    }
    return false;
}


//
// PhaseFunctionSampler class implementation.
//

PhaseFunctionSampler::PhaseFunctionSampler(
    const ShadingRay&       volume_ray,
    const PhaseFunction&    phasefunction,
    const void*             phasefunction_data,
    const float             distance)
  : m_volume_ray(volume_ray)
  , m_phasefunction(phasefunction)
  , m_phasefunction_data(phasefunction_data)
  , m_distance(distance)
  , m_point(m_volume_ray.point_at(distance))
{
}

const Vector3d& PhaseFunctionSampler::get_point() const
{
    return m_point;
}

bool PhaseFunctionSampler::contributes_to_light_sampling() const
{
    return true;
}

bool PhaseFunctionSampler::sample(
    SamplingContext&        sampling_context,
    const Dual3d&           outgoing,
    Dual3f&                 incoming,
    Spectrum&               value,
    float&                  pdf) const
{
    Vector3f incoming_direction;

    pdf =
        m_phasefunction.sample(
            sampling_context,
            m_volume_ray,
            m_phasefunction_data,
            m_distance,
            incoming_direction);

    incoming = Dual3f(incoming_direction);

    m_phasefunction.scattering_coefficient(
        m_volume_ray, m_phasefunction_data, m_distance, value);

    Spectrum transmission;
    m_phasefunction.evaluate_transmission(
        m_volume_ray, m_phasefunction_data, m_distance, transmission);
    value *= transmission;

    return true;
}

float PhaseFunctionSampler::evaluate(
    const int               light_sampling_modes,
    const Vector3f&         outgoing,
    const Vector3f&         incoming,
    Spectrum&               value) const
{
    const float pdf =
        m_phasefunction.evaluate(
            m_volume_ray,
            m_phasefunction_data,
            m_distance,
            incoming);

    m_phasefunction.scattering_coefficient(
        m_volume_ray, m_phasefunction_data, m_distance, value);

    Spectrum transmission;
    m_phasefunction.evaluate_transmission(
        m_volume_ray, m_phasefunction_data, m_distance, transmission);
    value *= transmission;

    return pdf;
}

float PhaseFunctionSampler::trace_between(
    const ShadingContext&   shading_context,
    const Vector3d&         target_position) const
{
    return
        shading_context.get_tracer().trace_between(
            m_point,
            target_position,
            m_volume_ray.m_time,
            VisibilityFlags::ShadowRay,
            m_volume_ray.m_depth + 1);
}

const ShadingPoint& PhaseFunctionSampler::trace(
    const ShadingContext&   shading_context,
    const Vector3f&         direction,
    float&                  transmission) const
{
    return
        shading_context.get_tracer().trace(
            m_point,
            Vector3d(direction),
            m_volume_ray.m_time,
            VisibilityFlags::ShadowRay,
            m_volume_ray.m_depth + 1,
            transmission);
}

bool PhaseFunctionSampler::cull_incoming_direction(const Vector3d& incoming) const
{
    // No culling for phase function.
    return false;
}

}   // namespace renderer
