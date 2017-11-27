
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
#include "genericvolume.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
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

namespace renderer
{

namespace
{
    const char* Model = "generic_volume";
}

//
// Generic volume.
//

class GenericVolume
  : public Volume
{
  public:
    GenericVolume(
        const char*         name,
        const ParamArray&   params)
      : Volume(name, params)
      , m_bsdf((std::string(name) + "_brdf").c_str(), ParamArray())
    {
        m_inputs.declare("absorption", InputFormatSpectralReflectance);
        m_inputs.declare("absorption_multiplier", InputFormatFloat, "1.0");
        m_inputs.declare("scattering", InputFormatSpectralReflectance);
        m_inputs.declare("scattering_multiplier", InputFormatFloat, "1.0");
        m_inputs.declare("average_cosine", InputFormatFloat, "0.0");
    }

    void release() override
    {
        delete this;
    }

    const char* get_model() const override
    {
        return Model;
    }

    bool on_frame_begin(
        const Project&          project,
        const BaseGroup*        parent,
        OnFrameBeginRecorder&   recorder,
        IAbortSwitch*           abort_switch) override
    {
        if (!Volume::on_frame_begin(project, parent, recorder, abort_switch))
            return false;

        const EntityDefMessageContext context("volume", this);

        const std::string phase_function =
            m_params.get_required<std::string>(
                "phase_function_model",
                "isotropic",
                make_vector("isotropic", "henyey"),
                context);

        if (phase_function == "isotropic")
            m_bsdf.m_phase_function.reset(new IsotropicPhaseFunction());
        else if (phase_function == "henyey")
        {
            const float g = clamp(
                m_params.get_optional<float>("average_cosine", 0.0f),
                -0.99f, +0.99f);
            m_bsdf.m_phase_function.reset(new HenyeyPhaseFunction(g));
        }
        else return false;

        return true;
    }

    bool is_homogeneous() const override
    {
        return true;
    }

    size_t compute_input_data_size() const override
    {
        return sizeof(InputValues);
    }

    void prepare_inputs(
        Arena&              arena,
        const ShadingRay&   volume_ray,
        void*               data) const override
    {
        InputValues* values = static_cast<InputValues*>(data);

        values->m_absorption *= values->m_absorption_multiplier;
        values->m_scattering *= values->m_scattering_multiplier;

        // Precompute extinction.
        values->m_precomputed.m_extinction = values->m_absorption + values->m_scattering;
    }

    float evaluate_exponential_sample(
        const float         distance,
        const ShadingRay&   volume_ray,
        const float         extinction) const
    {
        if (extinction == 0.0f)
            return static_cast<float>(1.0 / volume_ray.get_length());
        if (!volume_ray.is_finite())
            return exponential_distribution_pdf(distance, extinction);
        else
        {
            const float ray_length = static_cast<float>(volume_ray.get_length());
            return exponential_distribution_on_segment_pdf(
                distance, extinction, 0.0f, ray_length);
        }
    }

    void sample_distance(
        const ShadingContext&       shading_context,
        SamplingContext&            sampling_context,
        const void*                 data,
        DistanceSample&             sample) const override
    {
        if (sample.m_pivot != nullptr)
            sample_distance_combined_sampling(sampling_context, data, sample);
        else
            sample_distance_exponential_sampling(sampling_context, data, sample);

        const InputValues* values = static_cast<const InputValues*>(data);

        if (!sample.m_transmitted)
        {
            sample.m_bsdf = &m_bsdf;
            sample.m_bsdf_data;
            auto bsdf_inputs = shading_context.get_arena().allocate<PhaseFunctionBSDFInputValues>();
            for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
            {
                bsdf_inputs->m_albedo[i] = values->m_scattering[i] == 0.0f ? 0.0f :
                    values->m_scattering[i] / values->m_precomputed.m_extinction[i];
            }
            sample.m_bsdf_data = bsdf_inputs;
        }
    }

    void sample_distance_combined_sampling(
        SamplingContext&            sampling_context,
        const void*                 data,
        DistanceSample&             sample) const
    {
        const InputValues* values = static_cast<const InputValues*>(data);
        const Spectrum& extinction_coef = values->m_precomputed.m_extinction;

        sampling_context.split_in_place(1, 2);

        // Sample color channel uniformly at random.
        const float s_channel = sampling_context.next2<float>();
        const size_t channel = truncate<size_t>(s_channel * Spectrum::size());

        // Sample distance exponentially, assuming that the ray is infinite.
        const float s = sampling_context.next2<float>();
        sample.m_distance = sample_exponential_distribution(s, extinction_coef[channel]);

        // Check if the sampled point is a surface point or a point in infinity.
        if (extinction_coef[channel] == 0.0f ||
            sample.m_volume_ray->is_finite() && sample.m_distance > sample.m_volume_ray->m_tmax)
        {
            sample.m_distance = sample.m_volume_ray->m_tmax;
            evaluate_transmission(data, *sample.m_volume_ray, sample.m_value);
            sample.m_value /= foundation::average_value(sample.m_value);
            sample.m_transmitted = true;
            return;
        }
        sample.m_transmitted = false;

        // Prepare equiangular sampling.
        const EquiangularSampler<SamplingContext> equiangular_distance_sampler(
            *sample.m_pivot,
            *sample.m_volume_ray,
            sampling_context);

        sampling_context.split_in_place(1, 1);
        if (sampling_context.next2<float>() < 0.5f)
        {
            //
            // Exponential sampling.
            //

            const float exponential_sample = static_cast<float>(sample.m_distance);
            const float exponential_prob_mis = evaluate_exponential_sample(
                exponential_sample, *sample.m_volume_ray, extinction_coef[channel]);
            const float exponential_prob = exponential_distribution_pdf(
                exponential_sample, extinction_coef[channel]);
            const float equiangular_prob_mis =
                equiangular_distance_sampler.evaluate(exponential_sample);

            // Calculate MIS weight for spectral sampling.
            const float mis_weight_channel = compute_channel_mis(
                extinction_coef, exponential_sample, exponential_prob);

            // Calculate MIS weight for distance sampling.
            const float mis_weight_distance = 2.0f * mis(
                m_mis_heuristic, exponential_prob_mis, equiangular_prob_mis);

            sample.m_distance = exponential_sample;
            evaluate_transmission(data, *sample.m_volume_ray, exponential_sample, sample.m_value);
            sample.m_value *= mis_weight_channel * mis_weight_distance;
            sample.m_probability = exponential_prob;
            assert(exponential_prob > 0);
            assert(exponential_sample > 0);
        }
        else
        {
            //
            // Equiangular sampling.
            //

            const float equiangular_sample =
                equiangular_distance_sampler.sample();
            const float equiangular_prob_mis =
                equiangular_distance_sampler.evaluate(equiangular_sample);
            const float exponential_prob_mis = evaluate_exponential_sample(
                equiangular_sample, *sample.m_volume_ray, extinction_coef[channel]);

            // Calculate MIS weight for distance sampling.
            sample.m_distance = equiangular_sample;
            evaluate_transmission(data, *sample.m_volume_ray, equiangular_sample, sample.m_value);
            sample.m_value *= 2.0f * mis(m_mis_heuristic, equiangular_prob_mis, exponential_prob_mis);
            assert(equiangular_prob_mis > 0);
            assert(equiangular_sample > 0);
            const float transmission_prob =
                std::exp(-extinction_coef[channel] * static_cast<float>(sample.m_volume_ray->m_tmax));
            sample.m_probability = equiangular_prob_mis * (1.0f - transmission_prob);
        }
        sample.m_value *= extinction_coef;
    }

    void sample_distance_exponential_sampling(
        SamplingContext&            sampling_context,
        const void*                 data,
        DistanceSample&             sample) const
    {
        const InputValues* values = static_cast<const InputValues*>(data);
        const Spectrum& extinction_coef = values->m_precomputed.m_extinction;

        sampling_context.split_in_place(1, 2);

        // Sample color channel uniformly at random.
        const float s_channel = sampling_context.next2<float>();
        const size_t channel = truncate<size_t>(s_channel * Spectrum::size());

        // Sample distance exponentially, assuming that the ray is infinite.
        const float s = sampling_context.next2<float>();
        sample.m_distance = sample_exponential_distribution(s, extinction_coef[channel]);

        // Check if the sampled point is a surface point or a point in infinity.
        if (extinction_coef[channel] == 0.0f ||
            sample.m_volume_ray->is_finite() && sample.m_distance > sample.m_volume_ray->m_tmax)
        {
            sample.m_distance = sample.m_volume_ray->m_tmax;
            evaluate_transmission(data, *sample.m_volume_ray, sample.m_value);
            sample.m_value /= foundation::average_value(sample.m_value);
            sample.m_transmitted = true;
            return;
        }
        sample.m_transmitted = false;

        // Perform exponential sampling for distance.
        const float exponential_sample = static_cast<float>(sample.m_distance);
        const float exponential_probability = exponential_distribution_pdf(
            exponential_sample, extinction_coef[channel]);

        // Compute MIS for chromatic distance sampling.
        const float mis_weight_channel = compute_channel_mis(
            extinction_coef, exponential_sample, exponential_probability);

        // Compute transmission value for the distance sample.
        evaluate_transmission(data, *sample.m_volume_ray, exponential_sample, sample.m_value);
        sample.m_value *= mis_weight_channel;
        // When we compute transmission value for further direction sampling
        // it is multiplied by extinction coefficient;
        // direction sampling should then use albedo for computations
        // instead of scattering coefficient:
        sample.m_value *= extinction_coef;
        sample.m_probability = exponential_probability;
    }

    void evaluate_transmission(
        const void*         data,
        const ShadingRay&   volume_ray,
        const float         distance,
        Spectrum&           spectrum) const override
    {
        const InputValues* values = static_cast<const InputValues*>(data);
        spectrum = values->m_precomputed.m_extinction;

        for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
        {
            const float x = -distance * spectrum[i];
            assert(FP<float>::is_finite(x));
            spectrum[i] = std::exp(x);
        }
    }

    void evaluate_transmission(
        const void*         data,
        const ShadingRay&   volume_ray,
        Spectrum&           spectrum) const override
    {
        if (!volume_ray.is_finite())
            spectrum.set(0.0f);
        else
        {
            const float distance = static_cast<float>(volume_ray.get_length());
            evaluate_transmission(data, volume_ray, distance, spectrum);
        }
    }

  private:
    typedef GenericVolumeInputValues InputValues;

    PhaseFunctionBSDF m_bsdf;
    const MISHeuristic m_mis_heuristic = MISHeuristic::MISPower2;

    // Calculate MIS weight for spectral channel sampling (power heuristic).
    // One-sample estimator is used (Veach: 9.2.4 eq. 9.15).
    static float compute_channel_mis(
        const Spectrum& extinction_coef,
        const float distance_sample,
        const float probability)
    {
        float mis_weights_sum = 0.0f;
        for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
        {
            if (extinction_coef[i] > 0.0f)
            {
                const float probability = exponential_distribution_pdf(
                    distance_sample, extinction_coef[i]);
                mis_weights_sum += square(probability);
            }
        }
        return Spectrum::size() * square(probability) / mis_weights_sum;
    }
};


//
// GenericVolumeFactory class implementation.
//

void GenericVolumeFactory::release()
{
    delete this;
}

const char* GenericVolumeFactory::get_model() const
{
    return Model;
}

Dictionary GenericVolumeFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Generic Volume");
}

DictionaryArray GenericVolumeFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "absorption")
            .insert("label", "Absorption Coefficient")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("color", "Colors"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "absorption_multiplier")
            .insert("label", "Absorption Coefficient Multiplier")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "200.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "scattering")
            .insert("label", "Scattering Coefficient")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("color", "Colors"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "scattering_multiplier")
            .insert("label", "Scattering Coefficient Multiplier")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "200.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "phase_function_model")
            .insert("label", "Phase Function Model")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("Isotropic", "isotropic")
                    .insert("Henyey-Greenstein", "henyey"))
            .insert("use", "required")
            .insert("default", "isotropic")
            .insert("on_change", "rebuild_form"));

    metadata.push_back(
        Dictionary()
            .insert("name", "average_cosine")
            .insert("label", "Average Cosine (g)")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "-1.0")
                    .insert("type", "soft"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "soft"))
            .insert("use", "optional")
            .insert("default", "0.0")
            .insert("visible_if",
                Dictionary().insert("phase_function_model", "henyey")));

    return metadata;
}

auto_release_ptr<Volume> GenericVolumeFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<Volume>(new GenericVolume(name, params));
}

}   // namespace renderer
