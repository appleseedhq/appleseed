
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
#include "genericvolume.h"

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

class GenericVolume : public Volume
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

        const OnFrameBeginMessageContext context("volume", this);

        const std::string phase_function =
            m_params.get_required<std::string>(
                "phase_function_model",
                "isotropic",
                make_vector("isotropic", "henyey", "rayleigh"),
                context);

        if (phase_function == "isotropic")
            m_bsdf.m_phase_function.reset(new IsotropicPhaseFunction());
        else if (phase_function == "henyey")
        {
            const float g =
                clamp(
                    m_params.get_optional<float>("average_cosine", 0.0f),
                    -0.99f, +0.99f);
            m_bsdf.m_phase_function.reset(new HenyeyPhaseFunction(g));
        }
        else if (phase_function == "rayleigh")
        {
            m_bsdf.m_phase_function.reset(new RayleighPhaseFunction());
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

        // Precompute extinction and albedo.
        values->m_precomputed.m_extinction = values->m_absorption + values->m_scattering;
        for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
        {
            values->m_precomputed.m_albedo[i] = values->m_scattering[i] == 0.0f ? 0.0f :
                values->m_scattering[i] / values->m_precomputed.m_extinction[i];
        }
    }

    void sample(
        foundation::Arena&      arena,
        SamplingContext&        sampling_context,
        const void*             data,
        DistanceSample&         sample) const override
    {
        const InputValues* values = static_cast<const InputValues*>(data);
        const Spectrum& extinction_coef = values->m_precomputed.m_extinction;

        // Sample channel.
        sampling_context.split_in_place(1, 1);
        const float s0 = sampling_context.next2<float>();
        size_t channel = 0;
        float cdf = sample.m_channel_sampling_weights[0];
        while (s0 > cdf && channel + 1 < Spectrum::size())
        {
            cdf += sample.m_channel_sampling_weights[++channel];
        }

        // Sample distance exponentially, assuming that the ray is infinite.
        sampling_context.split_in_place(1, 1);
        const float s1 = sampling_context.next2<float>();
        const double distance = sample_exponential_distribution(s1, extinction_coef[channel]);

        // Evaluate this sample.
        evaluate(arena, data, distance, sample);
    }

    void evaluate(
        foundation::Arena&      arena,
        const void*             data,
        const double            distance,
        DistanceSample&         sample) const override
    {
        const InputValues* values = static_cast<const InputValues*>(data);

        const Spectrum& extinction_coef = values->m_precomputed.m_extinction;
        const ShadingRay& volume_ray = sample.m_incoming_point->get_ray();

        sample.m_distance = distance;
        sample.m_probability = 0.0f;

        if (volume_ray.is_finite() && sample.m_distance > volume_ray.get_length())
        {
            // The ray is transmitted.
            sample.m_distance = volume_ray.get_length();
            evaluate_transmission(data, volume_ray, sample.m_value);
            sample.m_transmitted = true;
            for (int i = 0; i < Spectrum::size(); ++i)
            {
                sample.m_probability +=
                    sample.m_channel_sampling_weights[i] * sample.m_value[i];  // MIS with balance heuristic
            }
        }
        else
        {
            // The ray is absorbed or scattered.
            sample.m_transmitted = false;

            const float exponential_sample = static_cast<float>(sample.m_distance);
            for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
            {
                sample.m_value[i] = exponential_distribution_pdf(exponential_sample, extinction_coef[i]);
                sample.m_probability +=
                    sample.m_channel_sampling_weights[i] * sample.m_value[i];  // MIS with balance heuristic
            }

            // Assign phase-function-based BSDF.
            sample.m_bsdf = &m_bsdf;
            sample.m_bsdf_data;
            auto bsdf_inputs = arena.allocate<PhaseFunctionBSDFInputValues>();
            bsdf_inputs->m_albedo = values->m_precomputed.m_albedo;
            sample.m_bsdf_data = bsdf_inputs;
        }
    }

    void evaluate_transmission(
        const void*         data,
        const ShadingRay&   volume_ray,
        const float         distance,
        Spectrum&           spectrum) const override
    {
        const InputValues* values = static_cast<const InputValues*>(data);

        for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
        {
            const float x = -distance * values->m_precomputed.m_extinction[i];
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
                    .insert("Henyey-Greenstein", "henyey")
                    .insert("Rayleigh", "rayleigh"))
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
