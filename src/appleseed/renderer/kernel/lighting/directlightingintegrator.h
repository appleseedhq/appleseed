
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_DIRECTLIGHTINGINTEGRATOR_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_DIRECTLIGHTINGINTEGRATOR_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/aov/spectrumstack.h"
#include "renderer/kernel/lighting/lightsampler.h"
#include "renderer/kernel/lighting/tracer.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/scene/visibilityflags.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/dual.h"
#include "foundation/math/mis.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>

// Forward declarations.
namespace renderer  { class PathVertex; }

namespace renderer
{

//
// The direct lighting integrator allows to estimate direct lighting at a given point in the scene.
//
// Note about the methods ending with *_low_variance():
//
//   The sample_lights() and sample_bsdf_and_lights() methods have "low variance" counterparts
//   respectively called sample_lights_low_variance() and sample_bsdf_and_lights_low_variance().
//   These methods treat non-physical light sources (such as point lights) and light-emitting
//   triangles differently: every light source of the scene is sampled individually, while the
//   set of light-emitting triangles is sampled as a whole.
//
//   The number of shadow rays cast by these functions may be as high as the number of light
//   samples passed to the constructor plus the number of non-physical lights in the scene.
//

class DirectLightingIntegrator
{
  public:
    // No MIS weighting.
    static double mis_none(
        const size_t    n1,
        const size_t    n2,
        const double    q1,
        const double    q2);

    // Balance MIS weighting function.
    static double mis_balance(
        const size_t    n1,
        const size_t    n2,
        const double    q1,
        const double    q2);

    // Power-2 MIS weighting function.
    static double mis_power2(
        const size_t    n1,
        const size_t    n2,
        const double    q1,
        const double    q2);

    // Constructors.
    DirectLightingIntegrator(
        const ShadingContext&           shading_context,
        const LightSampler&             light_sampler,
        const ShadingPoint&             shading_point,
        const foundation::Dual3d&       outgoing,                   // world space outgoing direction, unit-length
        const BSDF&                     bsdf,
        const void*                     bsdf_data,
        const int                       bsdf_sampling_modes,        // permitted scattering modes during BSDF sampling
        const int                       light_sampling_modes,       // permitted scattering modes during environment sampling
        const size_t                    bsdf_sample_count,          // number of samples in BSDF sampling
        const size_t                    light_sample_count,         // number of samples in light sampling
        const bool                      indirect);                  // are we computing indirect lighting?
    DirectLightingIntegrator(
        const ShadingContext&           shading_context,
        const LightSampler&             light_sampler,
        const PathVertex&               vertex,
        const int                       bsdf_sampling_modes,        // permitted scattering modes during BSDF sampling
        const int                       light_sampling_modes,       // permitted scattering modes during environment sampling
        const size_t                    bsdf_sample_count,          // number of samples in BSDF sampling
        const size_t                    light_sample_count,         // number of samples in light sampling
        const bool                      indirect);                  // are we computing indirect lighting?

    // Evaluate direct lighting by sampling the BSDF only.
    template <typename WeightingFunction>
    void sample_bsdf(
        SamplingContext&                sampling_context,
        WeightingFunction&              weighting_function,
        Spectrum&                       radiance,
        SpectrumStack&                  aovs);

    // Evaluate direct lighting by sampling the lights only.
    template <typename WeightingFunction>
    void sample_lights(
        SamplingContext&                sampling_context,
        WeightingFunction&              weighting_function,
        Spectrum&                       radiance,
        SpectrumStack&                  aovs);

    // A low-variance but more expensive variant of sample_lights().
    template <typename WeightingFunction>
    void sample_lights_low_variance(
        SamplingContext&                sampling_context,
        WeightingFunction&              weighting_function,
        Spectrum&                       radiance,
        SpectrumStack&                  aovs);

    // Evaluate direct lighting by sampling both the BSDF and the lights.
    void sample_bsdf_and_lights(
        SamplingContext&                sampling_context,
        Spectrum&                       radiance,
        SpectrumStack&                  aovs);

    // A low-variance but more expensive variant of sample_bsdf_and_lights().
    void sample_bsdf_and_lights_low_variance(
        SamplingContext&                sampling_context,
        Spectrum&                       radiance,
        SpectrumStack&                  aovs);

    // Sample the BSDF or the lights using a single sample.
    void take_single_bsdf_or_light_sample(
        SamplingContext&                sampling_context,
        Spectrum&                       radiance,
        SpectrumStack&                  aovs);

  private:
    const ShadingContext&               m_shading_context;
    const LightSampler&                 m_light_sampler;
    const ShadingPoint&                 m_shading_point;
    const foundation::Vector3d&         m_point;
    const foundation::Vector3d&         m_geometric_normal;
    const foundation::Basis3d&          m_shading_basis;
    const ShadingRay::Time&             m_time;
    const foundation::Dual3d&           m_outgoing;
    const BSDF&                         m_bsdf;
    const void*                         m_bsdf_data;
    const int                           m_bsdf_sampling_modes;
    const int                           m_light_sampling_modes;
    const size_t                        m_bsdf_sample_count;
    const size_t                        m_light_sample_count;
    const bool                          m_indirect;

    template <typename WeightingFunction>
    void take_single_bsdf_sample(
        SamplingContext&                sampling_context,
        WeightingFunction&              weighting_function,
        Spectrum&                       radiance,
        SpectrumStack&                  aovs);

    template <typename WeightingFunction>
    void take_single_light_sample(
        SamplingContext&                sampling_context,
        WeightingFunction&              weighting_function,
        Spectrum&                       radiance,
        SpectrumStack&                  aovs);

    template <typename WeightingFunction>
    void add_emitting_triangle_sample_contribution(
        const LightSample&              sample,
        WeightingFunction&              weighting_function,
        Spectrum&                       radiance,
        SpectrumStack&                  aovs);

    void add_non_physical_light_sample_contribution(
        const LightSample&              sample,
        Spectrum&                       radiance,
        SpectrumStack&                  aovs);
};


//
// DirectLightingIntegrator class implementation.
//
// Call Graph
// ----------
//
//   sample_bsdf  -->  take_single_bsdf_sample
//
//
//                                                   .-->  add_emitting_triangle_sample_contribution
//                                                   |
//   sample_lights  -->  take_single_light_sample  --+
//                                                   |
//                                                   `-->  add_non_physical_light_sample_contribution
//
//
//                                 .-->  add_emitting_triangle_sample_contribution
//                                 |
//   sample_lights_low_variance  --+
//                                 |
//                                 `-->  add_non_physical_light_sample_contribution
//
//
//                                                                                    .-->  take_single_bsdf_sample
//                                                                                    |     (Balance MIS)
//                                          .-->  take_single_bsdf_or_light_sample  --+                                 .-->  add_emitting_triangle_sample_contribution
//                                          |                                         |                                 |     (Balance MIS)
//                                          |                                         `-->  take_single_light_sample  --+
//                                          |                                               (Balance MIS)               |
//                                          |                                                                           `-->  add_non_physical_light_sample_contribution
//   sample_bsdf_and_lights               --+-->  sample_bsdf  -->  take_single_bsdf_sample
//   sample_bsdf_and_lights_low_variance    |
//                                          `-->  sample_lights  -->  take_single_light_sample  -->  ...
//

inline double DirectLightingIntegrator::mis_none(
    const size_t                        n1,
    const size_t                        n2,
    const double                        q1,
    const double                        q2)
{
    return 1.0;
}

inline double DirectLightingIntegrator::mis_balance(
    const size_t                        n1,
    const size_t                        n2,
    const double                        q1,
    const double                        q2)
{
    assert(n1 == 0);
    assert(n2 == 0);
    return foundation::mis_balance(q1, q2);
}

inline double DirectLightingIntegrator::mis_power2(
    const size_t                        n1,
    const size_t                        n2,
    const double                        q1,
    const double                        q2)
{
    return foundation::mis_power2(n1 * q1, n2 * q2);
}

template <typename WeightingFunction>
void DirectLightingIntegrator::sample_bsdf(
    SamplingContext&                    sampling_context,
    WeightingFunction&                  weighting_function,
    Spectrum&                           radiance,
    SpectrumStack&                      aovs)
{
    radiance.set(0.0f);
    aovs.set(0.0f);

    if (m_light_sampler.get_emitting_triangle_count() == 0)
        return;

    for (size_t i = 0; i < m_bsdf_sample_count; ++i)
    {
        take_single_bsdf_sample(
            sampling_context,
            weighting_function,
            radiance,
            aovs);
    }

    if (m_bsdf_sample_count > 1)
    {
        const float rcp_bsdf_sample_count = 1.0f / m_bsdf_sample_count;
        radiance *= rcp_bsdf_sample_count;
        aovs *= rcp_bsdf_sample_count;
    }
}

template <typename WeightingFunction>
void DirectLightingIntegrator::sample_lights(
    SamplingContext&                    sampling_context,
    WeightingFunction&                  weighting_function,
    Spectrum&                           radiance,
    SpectrumStack&                      aovs)
{
    radiance.set(0.0f);
    aovs.set(0.0f);

    sampling_context.split_in_place(3, m_light_sample_count);

    for (size_t i = 0; i < m_light_sample_count; ++i)
    {
        take_single_light_sample(
            sampling_context,
            weighting_function,
            radiance,
            aovs);
    }

    if (m_light_sample_count > 1)
    {
        const float rcp_light_sample_count = 1.0f / m_light_sample_count;
        radiance *= rcp_light_sample_count;
        aovs *= rcp_light_sample_count;
    }
}

template <typename WeightingFunction>
void DirectLightingIntegrator::sample_lights_low_variance(
    SamplingContext&                    sampling_context,
    WeightingFunction&                  weighting_function,
    Spectrum&                           radiance,
    SpectrumStack&                      aovs)
{
    radiance.set(0.0f);
    aovs.set(0.0f);

    // todo: if we had a way to know that a BSDF is purely specular, we could
    // immediately return black here since there will be no contribution from
    // such a BSDF.

    // Sample emitting triangles.
    if (m_light_sampler.get_emitting_triangle_count() > 0)
    {
        sampling_context.split_in_place(3, m_light_sample_count);

        for (size_t i = 0; i < m_light_sample_count; ++i)
        {
            const foundation::Vector3d s = sampling_context.next_vector2<3>();

            LightSample sample;
            m_light_sampler.sample_emitting_triangles(m_time, s, sample);

            add_emitting_triangle_sample_contribution(
                sample,
                weighting_function,
                radiance,
                aovs);
        }

        if (m_light_sample_count > 1)
        {
            const float rcp_light_sample_count = 1.0f / m_light_sample_count;
            radiance *= rcp_light_sample_count;
            aovs *= rcp_light_sample_count;
        }
    }

    // Sample non-physical light sources.
    const size_t light_count = m_light_sampler.get_non_physical_light_count();
    if (light_count > 0)
    {
        sampling_context.split_in_place(2, light_count);

        for (size_t i = 0; i < light_count; ++i)
        {
            const foundation::Vector2d s = sampling_context.next_vector2<2>();

            LightSample sample;
            m_light_sampler.sample_non_physical_light(m_time, s, i, sample);

            add_non_physical_light_sample_contribution(
                sample,
                radiance,
                aovs);
        }
    }
}

template <typename WeightingFunction>
void DirectLightingIntegrator::take_single_bsdf_sample(
    SamplingContext&                    sampling_context,
    WeightingFunction&                  weighting_function,
    Spectrum&                           radiance,
    SpectrumStack&                      aovs)
{
    assert(m_light_sampler.get_emitting_triangle_count() > 0);

    // Sample the BSDF.
    BSDFSample sample(
        m_shading_point,
        sampling_context,
        m_outgoing);
    m_bsdf.sample(
        m_bsdf_data,
        false,                      // not adjoint
        true,                       // multiply by |cos(incoming, normal)|
        sample);

    // Filter scattering modes.
    if (!(m_bsdf_sampling_modes & sample.get_mode()))
        return;
    assert(sample.get_probability() != BSDF::DiracDelta);

    // Trace a ray in the direction of the reflection.
    double weight;
    const ShadingPoint& light_shading_point =
        m_shading_context.get_tracer().trace(
            m_shading_point,
            sample.get_incoming_vector(),
            VisibilityFlags::ShadowRay,
            weight);

    // todo: wouldn't it be more efficient to look the environment up at this point?
    if (!light_shading_point.hit())
        return;

    // Retrieve the material at the intersection point.
    const Material* material = light_shading_point.get_material();
    if (material == 0)
        return;

    // Retrieve the EDF at the intersection point.
    const EDF* edf = material->get_edf();
    if (edf == 0)
        return;

    // No contribution if we are computing indirect lighting but this light does not cast indirect light.
    if (m_indirect && !(edf->get_flags() & EDF::CastIndirectLight))
        return;

    // Cull the samples on the back side of the lights' shading surface.
    const double cos_on = foundation::dot(-sample.get_incoming_vector(), light_shading_point.get_shading_normal());
    if (cos_on <= 0.0)
        return;

    // Evaluate the input values of the EDF.
    InputEvaluator edf_input_evaluator(m_shading_context.get_texture_cache());

#ifdef APPLESEED_WITH_OSL
    if (const ShaderGroup* sg = material->get_osl_surface())
        m_shading_context.execute_osl_emission(*sg, light_shading_point);
#endif
    edf->evaluate_inputs(edf_input_evaluator, light_shading_point);

    // Evaluate emitted radiance.
    Spectrum edf_value;
    double edf_prob;
    edf->evaluate(
        edf_input_evaluator.data(),
        light_shading_point.get_geometric_normal(),
        light_shading_point.get_shading_basis(),
        -sample.get_incoming_vector(),
        edf_value,
        edf_prob);
    if (edf_prob == 0.0)
        return;

    // Compute the square distance between the light sample and the shading point.
    const double square_distance = foundation::square(light_shading_point.get_distance());

    // Don't use this sample if we're closer than the light near start value.
    if (square_distance < foundation::square(edf->get_light_near_start()))
        return;

    if (square_distance > 0.0)
    {
        // Transform bsdf_prob to surface area measure (Veach: 8.2.2.2 eq. 8.10).
        const double bsdf_point_prob = sample.get_probability() * cos_on / square_distance;

        // Compute the probability density wrt. surface area mesure of the light sample.
        const double light_point_prob = m_light_sampler.evaluate_pdf(light_shading_point);

        // Apply the weighting function.
        weight *=
            weighting_function(
                m_bsdf_sample_count,
                m_light_sample_count,
                bsdf_point_prob,
                light_point_prob);
    }

    // Add the contribution of this sample to the illumination.
    edf_value *= static_cast<float>(weight / sample.get_probability());
    edf_value *= sample.value();
    radiance += edf_value;
    aovs.add(edf->get_render_layer_index(), edf_value);
}

template <typename WeightingFunction>
void DirectLightingIntegrator::take_single_light_sample(
    SamplingContext&                    sampling_context,
    WeightingFunction&                  weighting_function,
    Spectrum&                           radiance,
    SpectrumStack&                      aovs)
{
    // todo: if we had a way to know that a BSDF is purely specular, we could
    // immediately return black here since there will be no contribution from
    // such a BSDF.

    if (!m_light_sampler.has_lights_or_emitting_triangles())
        return;

    const foundation::Vector3d s = sampling_context.next_vector2<3>();

    LightSample sample;
    m_light_sampler.sample(m_time, s, sample);

    if (sample.m_triangle)
    {
        add_emitting_triangle_sample_contribution(
            sample,
            weighting_function,
            radiance,
            aovs);
    }
    else
    {
        add_non_physical_light_sample_contribution(
            sample,
            radiance,
            aovs);
    }
}

template <typename WeightingFunction>
void DirectLightingIntegrator::add_emitting_triangle_sample_contribution(
    const LightSample&                  sample,
    WeightingFunction&                  weighting_function,
    Spectrum&                           radiance,
    SpectrumStack&                      aovs)
{
    const Material* material = sample.m_triangle->m_material;
    const EDF* edf = material->get_edf();

    // No contribution if we are computing indirect lighting but this light does not cast indirect light.
    if (m_indirect && !(edf->get_flags() & EDF::CastIndirectLight))
        return;

    // Compute the incoming direction in world space.
    foundation::Vector3d incoming = sample.m_point - m_point;

    // Cull light samples behind the shading surface
    // if the BSDF is either Reflective or Transmissive, but not both.
    if (m_bsdf.get_type() != BSDF::AllBSDFTypes)
    {
        double cos_in = foundation::dot(incoming, m_shading_basis.get_normal());

        if (m_bsdf.get_type() == BSDF::Transmissive)
            cos_in = -cos_in;

        if (cos_in <= 0.0)
            return;
    }

    // Cull samples on lights emitting in the wrong direction.
    double cos_on = foundation::dot(-incoming, sample.m_shading_normal);
    if (cos_on <= 0.0)
        return;

    // Compute the transmission factor between the light sample and the shading point.
    const double transmission =
        m_shading_context.get_tracer().trace_between(
            m_shading_point,
            sample.m_point,
            VisibilityFlags::ShadowRay);

    // Discard occluded samples.
    if (transmission == 0.0)
        return;

    // Compute the square distance between the light sample and the shading point.
    const double square_distance = foundation::square_norm(incoming);
    const double rcp_sample_square_distance = 1.0 / square_distance;
    const double rcp_sample_distance = std::sqrt(rcp_sample_square_distance);

    // Don't use this sample if we're closer than the light near start value.
    if (square_distance < foundation::square(edf->get_light_near_start()))
        return;

    // Normalize the incoming direction.
    incoming *= rcp_sample_distance;
    cos_on *= rcp_sample_distance;

    // Evaluate the BSDF.
    Spectrum bsdf_value;
    const double bsdf_prob =
        m_bsdf.evaluate(
            m_bsdf_data,
            false,                          // not adjoint
            true,                           // multiply by |cos(incoming, normal)|
            m_geometric_normal,
            m_shading_basis,
            m_outgoing.get_value(),
            incoming,
            m_light_sampling_modes,
            bsdf_value);
    if (bsdf_prob == 0.0)
        return;

    // Evaluate the input values of the EDF.
    InputEvaluator edf_input_evaluator(m_shading_context.get_texture_cache());

    // TODO: refactor this code (est.).
    ShadingPoint shading_point;
    sample.make_shading_point(
        shading_point,
        sample.m_shading_normal,
        m_shading_context.get_intersector());

#ifdef APPLESEED_WITH_OSL
    if (const ShaderGroup* sg = material->get_osl_surface())
        m_shading_context.execute_osl_emission(*sg, shading_point);
#endif
    edf->evaluate_inputs(edf_input_evaluator, shading_point);

    // Evaluate the EDF.
    Spectrum edf_value;
    edf->evaluate(
        edf_input_evaluator.data(),
        sample.m_geometric_normal,
        foundation::Basis3d(sample.m_shading_normal),
        -incoming,
        edf_value);

    // Transform bsdf_prob to surface area measure (Veach: 8.2.2.2 eq. 8.10).
    const double bsdf_point_prob = bsdf_prob * cos_on * rcp_sample_square_distance;

    // Evaluate the weighting function.
    const double mis_weight =
        weighting_function(
            m_light_sample_count,
            m_bsdf_sample_count,
            sample.m_probability,
            bsdf_point_prob);

    // Add the contribution of this sample to the illumination.
    const double weight = mis_weight * transmission * cos_on * rcp_sample_square_distance / sample.m_probability;
    edf_value *= static_cast<float>(weight);
    edf_value *= bsdf_value;
    radiance += edf_value;
    aovs.add(edf->get_render_layer_index(), edf_value);
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_DIRECTLIGHTINGINTEGRATOR_H
