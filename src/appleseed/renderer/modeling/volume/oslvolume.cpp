
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
#include "oslvolume.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/cdf.h"
#include "foundation/math/fp.h"
#include "foundation/math/mis.h"
#include "foundation/math/phasefunction.h"
#include "foundation/math/sampling/equiangularsampler.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/arena.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/makevector.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/shading/closures.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/phasefunctionbsdf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/volume/distancesample.h"
#include "renderer/modeling/volume/volume.h"

// Standard headers.
#include <cmath>
#include <limits>
#include <memory>
#include <string>

using namespace foundation;


namespace
{

float dot(const renderer::Spectrum& lhs, const renderer::Spectrum& rhs)
{
    float result = 0.0f;
    for (size_t i = 0, e = renderer::Spectrum::size(); i < e; ++i)
    {
        result += lhs[i] * rhs[i];
    }
    return result;
}
}


namespace renderer
{

namespace
{
    const char* Model = "osl_volume";
}

//
// Generic volume.
//

class OSLVolume : public Volume
{
public:
    OSLVolume(
        const char*         name,
        const ParamArray&   params)
        : Volume(name, params)
        , m_bsdf((std::string(name) + "_brdf").c_str(), ParamArray())
        , m_extinction_majorant(100.0f)
        , m_max_iterations(1000)
    {
    }

    void release() override
    {
        delete this;
    }

    const char* get_model() const override
    {
        return Model;
    }

    bool is_homogeneous() const override
    {
        return false;
    }

    void sample(
        const ShadingContext&       shading_context,
        SamplingContext&            sampling_context,
        DistanceSample&             sample) const override
    {
        CompositeVolumeClosure* composite_closure =
            shading_context.get_arena().allocate_noinit<CompositeVolumeClosure>();
        const ShadingRay& volume_ray = sample.m_incoming_point->get_ray();
        ShadingPoint volume_shading_point;
        const ShadingRay::Medium* medium = volume_ray.m_media.get_current();
        const ShaderGroup* volume_shader_group =
            medium->m_material->get_render_data().m_volume_shader_group;
        assert(volume_shader_group != nullptr);


        //
        // Delta Tracking.
        //
        // http://drz.disneyresearch.com/~jnovak/publications/SDTracking/SDTracking.pdf
        //
        sample.m_value.set(1.0f);
        sample.m_probability = 1.0f;
        bool continue_tracking = true;
        sample.m_distance = 0.0f;
        size_t n_iterations = 0;
        while (continue_tracking)
        {
            // Put a hard limit on the number of iterations.
            if (n_iterations++ == m_max_iterations)
            {
                sample.m_value.set(0.0f);
                sample.m_distance = volume_ray.get_length();
                sample.m_transmitted = true;
                break;
            }

            // Sample distance exponentially, assuming that the ray is infinite.
            sampling_context.split_in_place(1, 1);
            const float s1 = sampling_context.next2<float>();
            sample.m_distance += sample_exponential_distribution(s1, m_extinction_majorant);

            if (volume_ray.is_finite() && sample.m_distance > volume_ray.get_length())
            {
                // The ray is transmitted.
                sample.m_distance = volume_ray.get_length();
                sample.m_transmitted = true;
                continue_tracking = false;
            }
            else
            {
                // Evaluate OSL volume shader.
                volume_shading_point.clear();
                shading_context.get_intersector().make_volume_shading_point(
                    volume_shading_point,
                    volume_ray,
                    sample.m_distance);
                shading_context.execute_osl_shading(
                    *volume_shader_group,
                    volume_shading_point);
                new (composite_closure) CompositeVolumeClosure(
                    volume_shading_point.get_osl_shader_globals().Ci,
                    shading_context.get_arena());

                Spectrum scattering_coef(0.0f);
                for (size_t i = 0; i < composite_closure->get_closure_count(); ++i)
                   scattering_coef += composite_closure->get_closure_weight(i);
                const Spectrum absorption_coef = composite_closure->get_absorption();
                Spectrum null_coef;
                for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
                {
                    null_coef[i] = m_extinction_majorant - scattering_coef[i] - absorption_coef[i];
                }

                Spectrum channel_weights = sample.m_channel_sampling_weights;
                channel_weights *= sample.m_value;
                const float avg_absorption = dot(channel_weights, absorption_coef);
                const float avg_scattering = dot(channel_weights, scattering_coef);
                const float avg_extinction = avg_absorption + avg_scattering;
                const float avg_null = dot(channel_weights, null_coef);
                assert(avg_null >= 0.0f);
                assert(min_value(null_coef) >= 0.0f);

                sampling_context.split_in_place(1, 1);
                const float denominator = (avg_null + avg_extinction);
                const float s2 = sampling_context.next2<float>() * denominator;
                float cdf = 0.0f;

                const size_t num_closures = composite_closure->get_closure_count();
                for (size_t i = 0; i < num_closures && continue_tracking; ++i)
                {
                    const Spectrum& scattering_coef_closure = composite_closure->get_closure_weight(i);
                    const float avg_scattering_closure = dot(channel_weights, scattering_coef_closure);
                    if (s2 < (cdf += avg_scattering_closure))
                    {
                        // The ray is scatterred with i-th phase function.
                        sample.m_transmitted = false;

                        // Assign phase-function-based BSDF.
                        const float weight = denominator / (avg_scattering_closure * m_extinction_majorant);
                        sample.m_value *= scattering_coef_closure;
                        sample.m_value *= weight;
                        sample.m_bsdf = &m_bsdf;
                        sample.m_bsdf_data;
                        auto bsdf_inputs = shading_context.get_arena().allocate<PhaseFunctionBSDFInputValues>();
                        bsdf_inputs->m_albedo.set(1.0f);
                        bsdf_inputs->m_phase_function = composite_closure->get_closure_phase_function(i);
                        sample.m_bsdf_data = bsdf_inputs;
                        continue_tracking = false;
                    }
                }
                if (continue_tracking && s2 < (cdf += avg_absorption))
                {
                    // The ray is absorbed.
                    sample.m_value.set(0.0f);
                    sample.m_distance = volume_ray.get_length();
                    sample.m_transmitted = true;
                    continue_tracking = false;
                }
                if (continue_tracking)
                {
                    const float weight = denominator / (avg_null * m_extinction_majorant);
                    sample.m_value *= null_coef;
                    sample.m_value *= weight;
                }
            }
        }
    }

    void evaluate(
        const ShadingContext&       shading_context,
        SamplingContext&            sampling_context,
        const double                distance,
        DistanceSample&             sample) const override
    {
        throw "";
    }

    void evaluate_transmission(
        const ShadingContext&       shading_context,
        SamplingContext&            sampling_context,
        const ShadingRay&           volume_ray,
        const float                 distance,
        Spectrum&                   spectrum) const override
    {
        CompositeVolumeClosure* composite_closure =
            shading_context.get_arena().allocate_noinit<CompositeVolumeClosure>();
        ShadingPoint volume_shading_point;
        const ShadingRay::Medium* medium = volume_ray.m_media.get_current();
        const ShaderGroup* volume_shader_group =
            medium->m_material->get_render_data().m_volume_shader_group;
        assert(volume_shader_group != nullptr);

        //
        // Ratio Tracking.
        //
        // https://pdfs.semanticscholar.org/ae7c/c1bde50a069574c545e8550ffa6c6fd270be.pdf
        //
        spectrum.set(1.0f);
        float current_distance = 0.0f;
        bool continue_tracking = true;
        size_t n_iterations = 0;
        while (continue_tracking)
        {
            // Put a hard limit on the number of iterations.
            if (n_iterations++ == m_max_iterations)
            {
                spectrum.set(0.0f);
                break;
            }

            // Sample distance exponentially, assuming that the ray is infinite.
            sampling_context.split_in_place(1, 1);
            const float s1 = sampling_context.next2<float>();
            current_distance += sample_exponential_distribution(s1, m_extinction_majorant);

            if (volume_ray.is_finite() && current_distance > volume_ray.get_length())
            {
                // The ray is transmitted.
                continue_tracking = false;
            }
            else
            {
                // Evaluate OSL volume shader.
                volume_shading_point.clear();
                shading_context.get_intersector().make_volume_shading_point(
                    volume_shading_point,
                    volume_ray,
                    current_distance);
                shading_context.execute_osl_shading(
                    *volume_shader_group,
                    volume_shading_point);
                new (composite_closure) CompositeVolumeClosure(
                    volume_shading_point.get_osl_shader_globals().Ci,
                    shading_context.get_arena());

                const Spectrum& absorption_coef = composite_closure->get_absorption();
                Spectrum total_scattering_coef(0.0f);
                for (size_t i = 0; i < composite_closure->get_closure_count(); ++i)
                    total_scattering_coef += composite_closure->get_closure_weight(i);
                const Spectrum extinction_coef = absorption_coef + total_scattering_coef;

                assert(max_value(extinction_coef) <= m_extinction_majorant);
                const float rcp_extinction_majorant = rcp(m_extinction_majorant);
                for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
                {
                    spectrum[i] *= (1.0f - extinction_coef[i] * rcp_extinction_majorant);
                }
            }
        }
    }

    void evaluate_transmission(
        const ShadingContext&   shading_context,
        SamplingContext&        sampling_context,
        const ShadingRay&       volume_ray,
        Spectrum&               spectrum) const override
    {
        if (!volume_ray.is_finite())
            spectrum.set(0.0f);
        else
        {
            const float distance = static_cast<float>(volume_ray.get_length());
            evaluate_transmission(shading_context, sampling_context, volume_ray, distance, spectrum);
        }
    }

  private:
    PhaseFunctionBSDF   m_bsdf;
    float               m_extinction_majorant;
    const size_t        m_max_iterations;
};


//
// OSLVolumeFactory class implementation.
//

auto_release_ptr<Volume> OSLVolumeFactory::create() const
{
    return auto_release_ptr<Volume>(new OSLVolume("osl_volume", ParamArray()));
}

}   // namespace renderer
