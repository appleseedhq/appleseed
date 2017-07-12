
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
#include "henyeyphasefunction.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/phasefunction/phasefunction.h"

// Standard headers.
#include <cmath>

using namespace foundation;

namespace renderer
{

namespace
{
    const char* Model = "henyey_phasefunction";
}

//
// Henyey-Greenstein phase function.
//
// https://www.astro.umd.edu/~jph/HG_note.pdf
//

class HenyeyPhaseFunction
  : public PhaseFunction
{
  public:
    HenyeyPhaseFunction(
        const char*         name,
        const ParamArray&   params)
      : PhaseFunction(name, params)
    {
        m_inputs.declare("absorption", InputFormatSpectralReflectance);
        m_inputs.declare("absorption_multiplier", InputFormatFloat, "1.0");
        m_inputs.declare("scattering", InputFormatSpectralReflectance);
        m_inputs.declare("scattering_multiplier", InputFormatFloat, "1.0");
        m_inputs.declare("average_cosine", InputFormatFloat, "0.0");
    }

    virtual void release() override
    {
        delete this;
    }

    virtual const char* get_model() const override
    {
        return Model;
    }

    virtual bool is_homogeneous() const override
    {
        return true;
    }

    virtual size_t compute_input_data_size() const override
    {
        return sizeof(InputValues);
    }

    virtual void prepare_inputs(
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

    virtual float sample(
        SamplingContext&    sampling_context,
        const void*         data,
        const ShadingRay&   volume_ray,
        const float         distance,
        Vector3f&           incoming) const override
    {
        const InputValues* values = static_cast<const InputValues*>(data);
        const Vector3f outgoing = Vector3f(normalize(volume_ray.m_dir));
        const Basis3f basis(outgoing);

        //
        // x = 1/(2g) * (1 + g^2 - [(1 - g^2) / (1 + g*s)]^2),
        // where x is cos(phi) and s is a uniform random sample from [-1, 1).
        //

        const float g = values->m_average_cosine;
        const float sqr_g = g * g;

        sampling_context.split_in_place(1, 2);
        const float s = 2.0f * sampling_context.next2<float>() - 1.0f;

        float cosine;
        if (std::abs(g) < +1e-5f)
        {
            cosine = s; // isotropic
        }
        else
        {
            const float t = (1.0f - sqr_g) / (1.0f + g * s);
            cosine = 0.5f / g * (1.0f + sqr_g - t * t);
        }

        const float sine = std::sqrt(saturate(1.0f - cosine * cosine));

        const Vector2f tangent = sample_circle_uniform(sampling_context.next2<float>());

        incoming =
            basis.get_tangent_u() * tangent.x * sine +
            basis.get_tangent_v() * tangent.y * sine +
            basis.get_normal()    * cosine;

        assert(feq(norm(incoming), 1.0f));

        // Evaluate PDF.

        const float numerator = (1.0f - sqr_g);
        const float denominator = std::pow(1.0f + sqr_g - 2.0f * g * cosine, -1.5f);

        return RcpFourPi<float>() * numerator * denominator;
    }

    virtual float evaluate(
        const void*         data,
        const ShadingRay&   volume_ray,
        const float         distance,
        const Vector3f&     incoming) const override
    {
        const InputValues* values = static_cast<const InputValues*>(data);
        const Vector3f outgoing = Vector3f(normalize(volume_ray.m_dir));
        const float cosine = dot(incoming, outgoing);

        //
        // p(x) = 1/2 * (1 - g^2) / (1 + g^2 - 2gx)^(3/2),
        // where x is cos(phi) and g is the average cosine parameter.
        //

        const float g = values->m_average_cosine;
        const float sqr_g = g * g;

        const float numerator = (1.0f - sqr_g);
        const float denominator = std::pow(1.0f + sqr_g - 2.0f * g * cosine, -1.5f);

        // Additionally divide by TwoPi, because we sample over the sphere.
        return RcpFourPi<float>() * numerator * denominator;
    }

    virtual void evaluate_transmission(
        const void*         data,
        const ShadingRay&   volume_ray,
        const float         distance,
        Spectrum&           spectrum) const override
    {
        extinction_coefficient(data, volume_ray, distance, spectrum);
        for (size_t i = 0, e = spectrum.size(); i < e; ++i)
            spectrum[i] = std::exp(-distance * spectrum[i]);
    }

    virtual void evaluate_transmission(
        const void*         data,
        const ShadingRay&   volume_ray,
        Spectrum&           spectrum) const override
    {
        const float distance = static_cast<float>(volume_ray.get_length());
        evaluate_transmission(data, volume_ray, distance, spectrum);
    }

    virtual void scattering_coefficient(
        const void*         data,
        const ShadingRay&   volume_ray,
        const float         distance,
        Spectrum&           spectrum) const override
    {
        const InputValues* values = static_cast<const InputValues*>(data);
        spectrum = values->m_scattering;
    }

    virtual const Spectrum& scattering_coefficient(
        const void*         data,
        const ShadingRay&   volume_ray) const override
    {
        const InputValues* values = static_cast<const InputValues*>(data);
        return values->m_scattering;
    }

    virtual void absorption_coefficient(
        const void*         data,
        const ShadingRay&   volume_ray,
        const float         distance,
        Spectrum&           spectrum) const override
    {
        const InputValues* values = static_cast<const InputValues*>(data);
        spectrum = values->m_absorption;
    }

    virtual const Spectrum& absorption_coefficient(
        const void*         data,
        const ShadingRay&   volume_ray) const override
    {
        const InputValues* values = static_cast<const InputValues*>(data);
        return values->m_absorption;
    }

    virtual void extinction_coefficient(
        const void*         data,
        const ShadingRay&   volume_ray,
        const float         distance,
        Spectrum&           spectrum) const override
    {
        const InputValues* values = static_cast<const InputValues*>(data);
        spectrum = values->m_precomputed.m_extinction;
    }

    virtual const Spectrum& extinction_coefficient(
        const void*         data,
        const ShadingRay&   volume_ray) const override
    {
        const InputValues* values = static_cast<const InputValues*>(data);
        return values->m_precomputed.m_extinction;
    }

  private:
    typedef HenyeyPhaseFunctionInputValues InputValues;
};


//
// HenyeyPhaseFunctionFactory class implementation.
//

const char* HenyeyPhaseFunctionFactory::get_model() const
{
    return Model;
}

Dictionary HenyeyPhaseFunctionFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Henyey-Greenstein Phase Function");
}

DictionaryArray HenyeyPhaseFunctionFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "absorption")
            .insert("label", "Absorption Coefficient")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors"))
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
                Dictionary()
                    .insert("color", "Colors"))
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
            .insert("name", "average_cosine")
            .insert("label", "Average Cosine (g)")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "-0.9")
                    .insert("type", "soft"))
            .insert("max",
                Dictionary()
                    .insert("value", "0.9")
                    .insert("type", "soft"))
            .insert("use", "required")
            .insert("default", "0.0"));

    return metadata;
}

auto_release_ptr<PhaseFunction> HenyeyPhaseFunctionFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<PhaseFunction>(new HenyeyPhaseFunction(name, params));
}

}   // namespace renderer
