
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/bssrdf/bssrdfsample.h"
#include "renderer/modeling/bssrdf/sss.h"
#include "renderer/modeling/input/inputevaluator.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/memory.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// DipoleBSSRDF class implementation.
//

DipoleBSSRDF::DipoleBSSRDF(
    const char*             name,
    const ParamArray&       params)
  : BSSRDF(name, params)
{
    m_inputs.declare("weight", InputFormatScalar, "1.0");
    m_inputs.declare("reflectance", InputFormatSpectralReflectance);
    m_inputs.declare("reflectance_multiplier", InputFormatScalar, "1.0");
    m_inputs.declare("dmfp", InputFormatScalar, "5.0");
    m_inputs.declare("dmfp_multiplier", InputFormatScalar, "0.1");
    m_inputs.declare("anisotropy", InputFormatScalar);
    m_inputs.declare("outside_ior", InputFormatScalar);
    m_inputs.declare("inside_ior", InputFormatScalar);
}

size_t DipoleBSSRDF::compute_input_data_size(
    const Assembly&         assembly) const
{
    return align(sizeof(DipoleBSSRDFInputValues), 16);
}

void DipoleBSSRDF::prepare_inputs(void* data) const
{
    DipoleBSSRDFInputValues* values =
        reinterpret_cast<DipoleBSSRDFInputValues*>(data);

    // Apply multipliers.
    values->m_reflectance *= static_cast<float>(values->m_reflectance_multiplier);
    values->m_dmfp *= values->m_dmfp_multiplier;

    // Clamp reflectance.
    values->m_reflectance = clamp(values->m_reflectance, 0.001f, 1.0f);

    // Compute sigma_a and sigma_s from the reflectance and dmfp parameters.
    const ComputeRdStandardDipole rd_fun(values->m_outside_ior / values->m_inside_ior);
    compute_absorption_and_scattering(
        rd_fun,
        values->m_reflectance,
        values->m_dmfp,
        values->m_anisotropy,
        values->m_sigma_a,
        values->m_sigma_s);

    // Precompute the (square of the) max radius.
    values->m_max_radius2 = square(dipole_max_radius(1.0 / values->m_dmfp));
}

bool DipoleBSSRDF::sample(
    const void*             data,
    BSSRDFSample&           sample) const
{
    const DipoleBSSRDFInputValues* values =
        reinterpret_cast<const DipoleBSSRDFInputValues*>(data);

    if (values->m_weight == 0.0)
        return false;

    sample.set_eta(values->m_outside_ior / values->m_inside_ior);
    sample.set_channel(0);

    sample.get_sampling_context().split_in_place(2, 1);
    const Vector2d s = sample.get_sampling_context().next_vector2<2>();

    // Sample a radius.
    const double sigma_tr = 1.0 / values->m_dmfp;
    const double radius = dipole_profile_sample(s[0], sigma_tr);

    // Set the max radius.
    sample.set_rmax2(values->m_max_radius2);

    // Sample an angle.
    const double phi = TwoPi * s[1];

    // Set the sampled point.
    sample.set_point(Vector2d(radius * cos(phi), radius * sin(phi)));

    return true;
}

double DipoleBSSRDF::evaluate_pdf(
    const void*             data,
    const size_t            channel,
    const double            radius) const
{
    const DipoleBSSRDFInputValues* values =
        reinterpret_cast<const DipoleBSSRDFInputValues*>(data);

    const double reflectance = values->m_reflectance[channel];
    if (reflectance == 0.0)
        return 0.0;

    // PDF of the sampled radius.
    const double sigma_tr = 1.0 / values->m_dmfp;
    const double pdf_radius = dipole_profile_pdf(radius, sigma_tr);

    // PDF of the sampled angle.
    const double pdf_angle = RcpTwoPi;

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
            .insert("name", "dmfp")
            .insert("label", "Diffuse Mean Free Path")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "dmfp_multiplier")
            .insert("label", "Diffuse Mean Free Path Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "0.1"));

    metadata.push_back(
        Dictionary()
            .insert("name", "anisotropy")
            .insert("label", "Anisotropy")
            .insert("type", "numeric")
            .insert("min_value", "-1.0")
            .insert("max_value", "1.0")
            .insert("use", "required")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "outside_ior")
            .insert("label", "Outside Index of Refraction")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "5.0")
            .insert("use", "required")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "inside_ior")
            .insert("label", "Inside Index of Refraction")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "5.0")
            .insert("use", "required")
            .insert("default", "1.3"));

    return metadata;
}

}   // namespace renderer
