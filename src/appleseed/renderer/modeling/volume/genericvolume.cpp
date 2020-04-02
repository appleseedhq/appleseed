
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
#include "foundation/containers/dictionary.h"
#include "foundation/math/basis.h"
#include "foundation/math/fp.h"
#include "foundation/math/phasefunction.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/makevector.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/volume/volume.h"

// Standard headers.
#include <cmath>
#include <limits>
#include <memory>

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
                make_vector("isotropic", "henyey"),
                context);

        if (phase_function == "isotropic")
            m_phase_function.reset(new IsotropicPhaseFunction());
        else if (phase_function == "henyey")
        {
            const float g =
                clamp(
                    m_params.get_optional<float>("average_cosine", 0.0f),
                    -0.99f, +0.99f);
            m_phase_function.reset(new HenyeyPhaseFunction(g));
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

    float sample(
        SamplingContext&    sampling_context,
        const void*         data,
        const ShadingRay&   volume_ray,
        const float         distance,
        Vector3f&           incoming) const override
    {
        sampling_context.split_in_place(2, 1);
        const Vector2f s = sampling_context.next2<Vector2f>();

        const Vector3f outgoing(normalize(volume_ray.m_dir));
        return m_phase_function->sample(outgoing, s, incoming);
    }

    float evaluate(
        const void*         data,
        const ShadingRay&   volume_ray,
        const float         distance,
        const Vector3f&     incoming) const override
    {
        const Vector3f outgoing = Vector3f(normalize(volume_ray.m_dir));
        return m_phase_function->evaluate(outgoing, incoming);
    }

    void evaluate_transmission(
        const void*         data,
        const ShadingRay&   volume_ray,
        const float         distance,
        Spectrum&           spectrum) const override
    {
        extinction_coefficient(data, volume_ray, distance, spectrum);

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

    void scattering_coefficient(
        const void*         data,
        const ShadingRay&   volume_ray,
        const float         distance,
        Spectrum&           spectrum) const override
    {
        const InputValues* values = static_cast<const InputValues*>(data);
        spectrum = values->m_scattering;
    }

    const Spectrum& scattering_coefficient(
        const void*         data,
        const ShadingRay&   volume_ray) const override
    {
        const InputValues* values = static_cast<const InputValues*>(data);
        return values->m_scattering;
    }

    void absorption_coefficient(
        const void*         data,
        const ShadingRay&   volume_ray,
        const float         distance,
        Spectrum&           spectrum) const override
    {
        const InputValues* values = static_cast<const InputValues*>(data);
        spectrum = values->m_absorption;
    }

    const Spectrum& absorption_coefficient(
        const void*         data,
        const ShadingRay&   volume_ray) const override
    {
        const InputValues* values = static_cast<const InputValues*>(data);
        return values->m_absorption;
    }

    void extinction_coefficient(
        const void*         data,
        const ShadingRay&   volume_ray,
        const float         distance,
        Spectrum&           spectrum) const override
    {
        const InputValues* values = static_cast<const InputValues*>(data);
        spectrum = values->m_precomputed.m_extinction;
    }

    const Spectrum& extinction_coefficient(
        const void*         data,
        const ShadingRay&   volume_ray) const override
    {
        const InputValues* values = static_cast<const InputValues*>(data);
        return values->m_precomputed.m_extinction;
    }

  private:
    typedef GenericVolumeInputValues InputValues;

    std::unique_ptr<PhaseFunction> m_phase_function;
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
