
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2018 Francois Beaune, The appleseedhq Organization
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
#include "dipolebssrdf.h"

// appleseed.renderer headers.
#include "renderer/modeling/bssrdf/dipolebssrdffactory.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/utility/api/specializedapiarrays.h"

using namespace foundation;

namespace renderer
{

//
// DipoleBSSRDF class implementation.
//

DipoleBSSRDF::DipoleBSSRDF(
    const char*             name,
    const ParamArray&       params)
  : SeparableBSSRDF(name, params)
  , m_has_sigma_sources(false)
{
    m_inputs.declare("weight", InputFormat::Float, "1.0");
    m_inputs.declare("reflectance", InputFormat::SpectralReflectance);
    m_inputs.declare("reflectance_multiplier", InputFormat::Float, "1.0");
    m_inputs.declare("mfp", InputFormat::SpectralReflectance);
    m_inputs.declare("mfp_multiplier", InputFormat::Float, "1.0");
    m_inputs.declare("sigma_a", InputFormat::SpectralReflectance, "");
    m_inputs.declare("sigma_s", InputFormat::SpectralReflectance, "");
    m_inputs.declare("g", InputFormat::Float, "0.0");
    m_inputs.declare("ior", InputFormat::Float);
    m_inputs.declare("fresnel_weight", InputFormat::Float, "1.0");
}

size_t DipoleBSSRDF::compute_input_data_size() const
{
    return sizeof(DipoleBSSRDFInputValues);
}

float DipoleBSSRDF::sample_profile(
    const void*             data,
    const size_t            channel,
    const float             u) const
{
    const DipoleBSSRDFInputValues* values =
        static_cast<const DipoleBSSRDFInputValues*>(data);

    const float sigma_tr = values->m_precomputed.m_sigma_tr[channel];

    return sample_exponential_distribution(u, sigma_tr);
}

float DipoleBSSRDF::evaluate_profile_pdf(
    const void*             data,
    const float             disk_radius) const
{
    const DipoleBSSRDFInputValues* values =
        static_cast<const DipoleBSSRDFInputValues*>(data);

    if (disk_radius > values->m_base_values.m_max_disk_radius)
        return 0.0f;

    float pdf = 0.0f;

    for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
    {
        const float channel_pdf = values->m_precomputed.m_channel_pdf[i];
        const float sigma_tr = values->m_precomputed.m_sigma_tr[i];
        pdf += channel_pdf * exponential_distribution_pdf(disk_radius, sigma_tr);
    }

    // Convert PDF to area measure (intuitively, multiply the PDF by the probability
    // of choosing a particular point on the circle of radius 'disk_radius').
    pdf /= TwoPi<float>() * disk_radius;

    return pdf;
}

bool DipoleBSSRDF::sample(
    const ShadingContext&   shading_context,
    SamplingContext&        sampling_context,
    const void*             data,
    const ShadingPoint&     outgoing_point,
    const Vector3f&         outgoing_dir,
    const int               modes,
    BSSRDFSample&           bssrdf_sample,
    BSDFSample&             bsdf_sample) const
{
    const DipoleBSSRDFInputValues* values =
        static_cast<const DipoleBSSRDFInputValues*>(data);

    return
        do_sample(
            shading_context,
            sampling_context,
            data,
            values->m_base_values,
            outgoing_point,
            outgoing_dir,
            modes,
            bssrdf_sample,
            bsdf_sample);
}

void DipoleBSSRDF::evaluate(
    const void*             data,
    const ShadingPoint&     outgoing_point,
    const Vector3f&         outgoing_dir,
    const ShadingPoint&     incoming_point,
    const Vector3f&         incoming_dir,
    const int               modes,
    Spectrum&               value) const
{
    const DipoleBSSRDFInputValues* values =
        static_cast<const DipoleBSSRDFInputValues*>(data);

    return
        do_evaluate(
            data,
            values->m_base_values,
            outgoing_point,
            outgoing_dir,
            incoming_point,
            incoming_dir,
            modes,
            value);
}


//
// DipoleBSSRDFFactory class implementation.
//

DictionaryArray DipoleBSSRDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "weight")
            .insert("label", "Weight")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "reflectance")
            .insert("label", "Diffuse Surface Reflectance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "reflectance_multiplier")
            .insert("label", "Diffuse Surface Reflectance Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "mfp")
            .insert("label", "Mean Free Path")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "mfp_multiplier")
            .insert("label", "Mean Free Path Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "ior")
            .insert("label", "Index of Refraction")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "2.5")
                    .insert("type", "hard"))
            .insert("use", "required")
            .insert("default", "1.3"));

    metadata.push_back(
        Dictionary()
            .insert("name", "fresnel_weight")
            .insert("label", "Fresnel Weight")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    return metadata;
}

}   // namespace renderer
