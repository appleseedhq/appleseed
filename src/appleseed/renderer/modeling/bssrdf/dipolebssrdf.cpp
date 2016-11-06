
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2016 Francois Beaune, The appleseedhq Organization
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
#include "dipolebssrdffactory.h"

// appleseed.renderer headers.
#include "renderer/modeling/bssrdf/bssrdfsample.h"
#include "renderer/modeling/bssrdf/sss.h"
#include "renderer/modeling/input/inputevaluator.h"

// appleseed.foundation headers.
#include "foundation/math/cdf.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// DipoleBSSRDF class implementation.
//

DipoleBSSRDF::DipoleBSSRDF(
    const char*         name,
    const ParamArray&   params)
  : SeparableBSSRDF(name, params)
{
    m_inputs.declare("weight", InputFormatFloat, "1.0");
    m_inputs.declare("reflectance", InputFormatSpectralReflectance);
    m_inputs.declare("reflectance_multiplier", InputFormatFloat, "1.0");
    m_inputs.declare("mfp", InputFormatSpectralReflectance);
    m_inputs.declare("mfp_multiplier", InputFormatFloat, "1.0");
    m_inputs.declare("sigma_a", InputFormatSpectralReflectance, "");
    m_inputs.declare("sigma_s", InputFormatSpectralReflectance, "");
    m_inputs.declare("g", InputFormatFloat, "0.0");
    m_inputs.declare("ior", InputFormatFloat);
}

bool DipoleBSSRDF::sample(
    SamplingContext&    sampling_context,
    const void*         data,
    BSSRDFSample&       sample) const
{
    const DipoleBSSRDFInputValues* values =
        reinterpret_cast<const DipoleBSSRDFInputValues*>(data);

    if (values->m_weight == 0.0)
        return false;

    sampling_context.split_in_place(3, 1);
    const Vector3f s = sampling_context.next2<Vector3f>();

    // Sample a channel.
    const size_t channel =
        sample_cdf(
            &values->m_precomputed.m_channel_cdf[0],
            &values->m_precomputed.m_channel_cdf[0] + values->m_precomputed.m_channel_cdf.size(),
            s[0]);

    // Sample a radius.
    const float sigma_tr = values->m_precomputed.m_sigma_tr[channel];
    const float radius = sample_exponential_distribution(s[1], sigma_tr);

    // Sample an angle.
    const float phi = TwoPi<float>() * s[2];

    sample.m_eta = values->m_precomputed.m_eta;
    sample.m_channel = channel;
    sample.m_point = Vector2f(radius * cos(phi), radius * sin(phi));
    sample.m_rmax2 = values->m_precomputed.m_rmax2;

    return true;
}

float DipoleBSSRDF::evaluate_pdf(
    const void*         data,
    const size_t        channel,
    const float         radius) const
{
    const DipoleBSSRDFInputValues* values =
        reinterpret_cast<const DipoleBSSRDFInputValues*>(data);

    // PDF of the sampled radius.
    float pdf_radius = 0.0f;
    for (size_t i = 0, e = values->m_precomputed.m_sigma_tr.size(); i < e; ++i)
    {
        const float sigma_tr = values->m_precomputed.m_sigma_tr[i];
        pdf_radius +=
              exponential_distribution_pdf(radius, sigma_tr)
            * values->m_precomputed.m_channel_pdf[i];
    }

    // PDF of the sampled angle.
    const float pdf_angle = RcpTwoPi<float>();

    // Compute and return the final PDF.
    return pdf_radius * pdf_angle;
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
                Dictionary().insert("texture_instance", "Textures"))
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
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "reflectance_multiplier")
            .insert("label", "Diffuse Surface Reflectance Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
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
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "mfp_multiplier")
            .insert("label", "Mean Free Path Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "ior")
            .insert("label", "Index of Refraction")
            .insert("type", "numeric")
            .insert("min_value", "1.0")
            .insert("max_value", "2.5")
            .insert("use", "required")
            .insert("default", "1.3"));

    return metadata;
}

}   // namespace renderer
