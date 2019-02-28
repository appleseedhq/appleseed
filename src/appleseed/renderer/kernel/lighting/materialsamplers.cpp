
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Artem Bishev, The appleseedhq Organization
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
#include "renderer/kernel/shading/directshadingcomponents.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfsample.h"
#include "renderer/modeling/volume/volume.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// BSDFSampler class implementation.
//

BSDFSampler::BSDFSampler(
    const BSDF&                 bsdf,
    const void*                 bsdf_data,
    const int                   bsdf_sampling_modes,
    const ShadingPoint&         shading_point)
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

const ShadingPoint& BSDFSampler::get_shading_point() const
{
    return m_shading_point;
}

bool BSDFSampler::contributes_to_light_sampling() const
{
    // There cannot be any contribution for purely specular BSDFs.
    return !m_bsdf.is_purely_specular();
}

const ShadingPoint& BSDFSampler::trace_full(
    const ShadingContext&       shading_context,
    const Vector3f&             direction,
    Spectrum&                   transmission) const
{
    ShadingRay ray(
        m_shading_point.get_point(),
        Vector3d(direction),
        m_shading_point.get_ray().m_time,
        VisibilityFlags::ShadowRay,
        m_shading_point.get_ray().m_depth + 1);
    ray.copy_media_from(m_shading_point.get_ray());

    const ShadingPoint& shading_point =
        shading_context.get_tracer().trace_full(
            shading_context,
            m_shading_point,
            ray,
            transmission);

    return shading_point;
}

void BSDFSampler::trace_simple(
    const ShadingContext&       shading_context,
    const Vector3f&             direction,
    Spectrum&                   transmission) const
{
    ShadingRay ray(
        m_shading_point.get_point(),
        Vector3d(direction),
        m_shading_point.get_ray().m_time,
        VisibilityFlags::ShadowRay,
        m_shading_point.get_ray().m_depth + 1);
    ray.copy_media_from(m_shading_point.get_ray());

    shading_context.get_tracer().trace_simple(
        shading_context,
        m_shading_point,
        ray,
        transmission);
}

void BSDFSampler::trace_between(
    const ShadingContext&       shading_context,
    const Vector3d&             target_position,
    Spectrum&                   transmission) const
{
    shading_context.get_tracer().trace_between_simple(
        shading_context,
        m_shading_point,
        target_position,
        m_shading_point.get_ray(),
        VisibilityFlags::ShadowRay,
        transmission);
}

bool BSDFSampler::sample(
    SamplingContext&            sampling_context,
    const Dual3d&               outgoing,
    Dual3f&                     incoming,
    DirectShadingComponents&    value,
    float&                      pdf) const
{
    BSDFSample sample(&m_shading_point, Dual3f(outgoing));
    m_bsdf.sample(
        sampling_context,
        m_bsdf_data,
        false,                  // not adjoint
        true,                   // multiply by |cos(incoming, normal)|
        m_bsdf_sampling_modes,
        sample);

    // Filter scattering modes.
    if (!(m_bsdf_sampling_modes & sample.get_mode()))
        return false;

    incoming = sample.m_incoming;
    value = sample.m_value;
    pdf = sample.get_probability();

    return true;
}

float BSDFSampler::evaluate(
    const int                   light_sampling_modes,
    const Vector3f&             outgoing,
    const Vector3f&             incoming,
    DirectShadingComponents&    value) const
{
    return
        m_bsdf.evaluate(
            m_bsdf_data,
            false,              // not adjoint
            true,               // multiply by |cos(incoming, normal)|
            Vector3f(m_geometric_normal),
            Basis3f(m_shading_basis),
            Vector3f(outgoing),
            Vector3f(incoming),
            light_sampling_modes,
            value);
}


//
// VolumeSampler class implementation.
//

VolumeSampler::VolumeSampler(
    const ShadingRay&           volume_ray,
    const Volume&               volume,
    const void*                 volume_data,
    const float                 distance,
    const ShadingPoint&         shading_point)
  : m_volume_ray(volume_ray)
  , m_volume(volume)
  , m_volume_data(volume_data)
  , m_distance(distance)
  , m_shading_point(shading_point)
  , m_point(m_volume_ray.point_at(distance))
{
}

const Vector3d& VolumeSampler::get_point() const
{
    return m_point;
}

const ShadingPoint& VolumeSampler::get_shading_point() const
{
    return m_shading_point;
}

bool VolumeSampler::contributes_to_light_sampling() const
{
    return true;
}

const ShadingPoint& VolumeSampler::trace_full(
    const ShadingContext&       shading_context,
    const Vector3f&             direction,
    Spectrum&                   transmission) const
{
    ShadingRay ray(
        m_point,
        Vector3d(direction),
        m_volume_ray.m_time,
        VisibilityFlags::ShadowRay,
        m_volume_ray.m_depth + 1);
    ray.copy_media_from(m_volume_ray);

    const ShadingPoint& shading_point =
        shading_context.get_tracer().trace_full(
            shading_context,
            ray,
            transmission);

    return shading_point;
}

void VolumeSampler::trace_simple(
    const ShadingContext&       shading_context,
    const Vector3f&             direction,
    Spectrum&                   transmission) const
{
    ShadingRay ray(
        m_point,
        Vector3d(direction),
        m_volume_ray.m_time,
        VisibilityFlags::ShadowRay,
        m_volume_ray.m_depth + 1);
    ray.copy_media_from(m_volume_ray);

    shading_context.get_tracer().trace_simple(
        shading_context,
        ray,
        transmission);
}

void VolumeSampler::trace_between(
    const ShadingContext&       shading_context,
    const Vector3d&             target_position,
    Spectrum&                   transmission) const
{
    shading_context.get_tracer().trace_between_simple(
        shading_context,
        m_point,
        target_position,
        m_volume_ray,
        VisibilityFlags::ShadowRay,
        transmission);
}

bool VolumeSampler::sample(
    SamplingContext&            sampling_context,
    const Dual3d&               outgoing,
    Dual3f&                     incoming,
    DirectShadingComponents&    value,
    float&                      pdf) const
{
    Vector3f incoming_direction;

    pdf =
        m_volume.sample(
            sampling_context,
            m_volume_data,
            m_volume_ray,
            m_distance,
            incoming_direction);

    incoming = Dual3f(incoming_direction);

    m_volume.scattering_coefficient(
        m_volume_data, m_volume_ray, m_distance, value.m_volume);
    value.m_volume *= pdf;
    value.m_beauty = value.m_volume;

    return true;
}

float VolumeSampler::evaluate(
    const int                   light_sampling_modes,
    const Vector3f&             outgoing,
    const Vector3f&             incoming,
    DirectShadingComponents&    value) const
{
    const float pdf =
        m_volume.evaluate(
            m_volume_data,
            m_volume_ray,
            m_distance,
            incoming);

    m_volume.scattering_coefficient(
        m_volume_data, m_volume_ray, m_distance, value.m_volume);
    value.m_volume *= pdf;
    value.m_beauty = value.m_volume;

    return pdf;
}

}   // namespace renderer
