
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

#include "foundation/math/sampling/mappings.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"

// appleseed.renderer headers.
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/phasefunction/isotropicphasefunction.h"
#include "renderer/modeling/phasefunction/phasefunction.h"


namespace renderer
{

namespace
{
    const char* Model = "isotropic_phasefunction";
}

//
// Isotropic phase function.
//
class IsotropicPhaseFunction
    : public PhaseFunction
{
  public:
    IsotropicPhaseFunction(
        const char*             name,
        const ParamArray&       params)
        : PhaseFunction(name, params)
    {
        m_inputs.declare("scattering", InputFormatSpectralReflectance);
        m_inputs.declare("scattering_multiplier", InputFormatFloat, "1.0");
        m_inputs.declare("extinction", InputFormatSpectralReflectance);
        m_inputs.declare("extinction_multiplier", InputFormatFloat, "1.0");
    }

    virtual void release() APPLESEED_OVERRIDE
    {
        delete this;
    }

    virtual const char* get_model() const APPLESEED_OVERRIDE
    {
        return Model;
    }
        
    virtual bool is_homogeneous() const
    {
        return true;
    }

    virtual void prepare_inputs(
        foundation::Arena&          arena,
        const ShadingRay&           shading_point,
        void*                       data
        ) const APPLESEED_OVERRIDE
    {
        InputValues* values = static_cast<InputValues*>(data);

        // Ensure that extinction spectrum has unit norm, which is neccessary for distance sampling
        const float extinction_norm = foundation::max_value(values->m_extinction);
        values->m_extinction /= extinction_norm;
        values->m_extinction_multiplier *= extinction_norm;
    }

    virtual float sample_distance(
        SamplingContext&            sampling_context,
        const ShadingRay&           volume_ray,
        const void*                 data,
        float&                      distance
        ) const APPLESEED_OVERRIDE
    {
        const InputValues* values = static_cast<const InputValues*>(data);

        float ray_length = norm(volume_ray.m_dir) * (volume_ray.m_tmax - volume_ray.m_tmin);

        // Sample distance.
        sampling_context.split_in_place(1, 1);
        const float s = sampling_context.next2<float>();
        distance = foundation::sample_exponential_distribution_on_segment(
            s, values->m_extinction_multiplier, 0.0f, ray_length);

        // Return corresponding PDF value.
        return foundation::exponential_distribution_on_segment_pdf(
            distance, values->m_extinction_multiplier, 0.0f, ray_length);
    }

    virtual float sample(
        SamplingContext&            sampling_context,
        const ShadingRay&           volume_ray,
        const void*                 data,
        float                       distance,
        foundation::Vector3f&       incoming) const APPLESEED_OVERRIDE
    {
        const InputValues* values = static_cast<const InputValues*>(data);

        // Sample incoming direction.
        sampling_context.split_in_place(2, 1);
        const foundation::Vector2f s = sampling_context.next2<foundation::Vector2f>();
        incoming = foundation::sample_sphere_uniform(s);

        return foundation::RcpPi<float>();
    }

    virtual float evaluate(
        const ShadingRay&           volume_ray,
        const void*                 data,
        float                       distance,
        const foundation::Vector3f& incoming) const APPLESEED_OVERRIDE
    {
        return foundation::RcpPi<float>();
    }

    virtual void evaluate_transmission(
        const ShadingRay&           volume_ray,
        const void*                 data,
        float                       distance,
        Spectrum&                   spectrum) const APPLESEED_OVERRIDE
    {
        const InputValues* values = static_cast<const InputValues*>(data);

        spectrum = foundation::exp(-distance * values->m_extinction_multiplier * values->m_extinction);
    }

    virtual void evaluate_transmission(
        const ShadingRay&           volume_ray,
        const void*                 data,
        Spectrum&                   spectrum) const APPLESEED_OVERRIDE
    {
        const float distance = 
            norm(volume_ray.m_dir) *
            (volume_ray.m_tmax - volume_ray.m_tmin);
        evaluate_transmission(volume_ray, data, distance, spectrum);
    }

    virtual void scattering_coefficient(
        const ShadingRay&           volume_ray,
        const void*                 data,
        float                       distance,
        Spectrum&                   spectrum) const APPLESEED_OVERRIDE
    {
        const InputValues* values = static_cast<const InputValues*>(data);
        spectrum = values->m_scattering * values->m_scattering_multiplier;
    }

    virtual void extinction_coefficient(
        const ShadingRay&           volume_ray,
        const void*                 data,
        float                       distance,
        Spectrum&                   spectrum) const APPLESEED_OVERRIDE
    {
        const InputValues* values = static_cast<const InputValues*>(data);
        spectrum = values->m_extinction * values->m_extinction_multiplier;
    }

  private:
    typedef IsotropicPhaseFunctionInputValues InputValues;
};


//
// IsotropicPhaseFunctionFactory class implementation.
//

const char* IsotropicPhaseFunctionFactory::get_model() const
{
    return Model;
}

foundation::Dictionary IsotropicPhaseFunctionFactory::get_model_metadata() const
{
    return
        foundation::Dictionary()
        .insert("name", Model)
        .insert("label", "Isotropic Phase Function");
}

foundation::DictionaryArray IsotropicPhaseFunctionFactory::get_input_metadata() const
{
    foundation::DictionaryArray metadata;

    metadata.push_back(
        foundation::Dictionary()
            .insert("name", "scattering")
            .insert("label", "Scattering Coefficient")
            .insert("type", "colormap")
            .insert("entity_types",
                foundation::Dictionary().insert("color", "Colors"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        foundation::Dictionary()
            .insert("name", "scattering_multiplier")
            .insert("label", "Scattering Coefficient Multiplier")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "200.0")
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        foundation::Dictionary()
        .insert("name", "extinction")
        .insert("label", "Extinction Coefficient")
        .insert("type", "colormap")
        .insert("entity_types",
        foundation::Dictionary().insert("color", "Colors"))
        .insert("use", "required")
        .insert("default", "0.5"));

    metadata.push_back(
        foundation::Dictionary()
        .insert("name", "extinction_multiplier")
        .insert("label", "Extinction Coefficient Multiplier")
        .insert("type", "numeric")
        .insert("min_value", "0.0")
        .insert("max_value", "200.0")
        .insert("use", "optional")
        .insert("default", "1.0"));

    return metadata;
}

foundation::auto_release_ptr<PhaseFunction> IsotropicPhaseFunctionFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return foundation::auto_release_ptr<PhaseFunction>(
        new IsotropicPhaseFunction(name, params));
}
}