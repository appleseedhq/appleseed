
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
#include "standarddipolebssrdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bssrdf/bssrdf.h"
#include "renderer/modeling/bssrdf/bssrdfsample.h"
#include "renderer/modeling/bssrdf/sss.h"
#include "renderer/modeling/input/inputevaluator.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/memory.h"

// Standard headers.
#include <cmath>
#include <cstddef>

// Forward declarations.
namespace renderer  { class Assembly; }
namespace renderer  { class ShadingContext; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Standard dipole BSSRDF.
    //
    // Reference:
    //
    //   A Practical Model for Subsurface Light Transport
    //   https://graphics.stanford.edu/papers/bssrdf/bssrdf.pdf
    //

    const char* Model = "standard_dipole_bssrdf";

    class StandardDipoleBSSRDF
      : public BSSRDF
    {
      public:
        StandardDipoleBSSRDF(
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

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        virtual size_t compute_input_data_size(
            const Assembly&         assembly) const APPLESEED_OVERRIDE
        {
            return align(sizeof(StandardDipoleBSSRDFInputValues), 16);
        }

        virtual void evaluate_inputs(
            const ShadingContext&   shading_context,
            InputEvaluator&         input_evaluator,
            const ShadingPoint&     shading_point,
            const size_t            offset = 0) const APPLESEED_OVERRIDE
        {
            BSSRDF::evaluate_inputs(shading_context, input_evaluator, shading_point, offset);

            StandardDipoleBSSRDFInputValues* values =
                reinterpret_cast<StandardDipoleBSSRDFInputValues*>(input_evaluator.data() + offset);

            // Apply multipliers.
            values->m_reflectance *= static_cast<float>(values->m_reflectance_multiplier);
            values->m_dmfp *= values->m_dmfp_multiplier;

            // Clamp reflectance to [0, 1].
            values->m_reflectance = saturate(values->m_reflectance);

#if 1
            // Compute sigma_a and sigma_s from the reflectance and dmfp parameters.
            compute_absorption_and_scattering(
                values->m_reflectance,
                values->m_dmfp,
                values->m_inside_ior / values->m_outside_ior,
                values->m_anisotropy,
                values->m_sigma_a,
                values->m_sigma_s);
#else
            // Skim milk.
            values->m_sigma_a = Color3f(0.0014f, 0.0025f, 0.0142f) * 1000.0f;
            values->m_sigma_s = Color3f(0.70f, 1.22f, 1.90f) * 1000.0f;
#endif
        }

        virtual bool sample(
            const void*             data,
            BSSRDFSample&           sample) const APPLESEED_OVERRIDE
        {
            const StandardDipoleBSSRDFInputValues* values =
                reinterpret_cast<const StandardDipoleBSSRDFInputValues*>(data);

            if (values->m_weight == 0.0)
                return false;

            sample.set_is_directional(false);
            sample.set_eta(values->m_inside_ior / values->m_outside_ior);

            // Select the channel leading to the strongest scattering.
            const size_t channel = min_index(values->m_reflectance);
            sample.set_channel(channel);

            // todo: fix.
            const double reflectance = values->m_reflectance[channel];
            if (reflectance == 0.0)
                return false;

            sample.get_sampling_context().split_in_place(2, 1);
            const Vector2d s = sample.get_sampling_context().next_vector2<2>();

            // Sample a radius.
            const double sigma_a = values->m_sigma_a[channel];
            const double sigma_s = values->m_sigma_s[channel];
            const double sigma_s_prime = sigma_s * (1.0 - values->m_anisotropy);
            const double sigma_t_prime = sigma_s_prime + sigma_a;
            const double sigma_tr = sqrt(3.0 * sigma_a * sigma_t_prime);
            const double radius = sample_attenuation(sigma_tr, s[0]);

            // Set the max radius.
            sample.set_rmax2(square(max_attenuation_distance(sigma_tr)));

            // Sample an angle.
            const double phi = TwoPi * s[1];

            // Set the sampled point.
            sample.set_point(Vector2d(radius * cos(phi), radius * sin(phi)));

            return true;
        }

        virtual void evaluate(
            const void*             data,
            const ShadingPoint&     outgoing_point,
            const Vector3d&         outgoing_dir,
            const ShadingPoint&     incoming_point,
            const Vector3d&         incoming_dir,
            Spectrum&               value) const APPLESEED_OVERRIDE
        {
            const StandardDipoleBSSRDFInputValues* values =
                reinterpret_cast<const StandardDipoleBSSRDFInputValues*>(data);

            const Vector3d& x = outgoing_point.get_point();
            const double eta = values->m_inside_ior / values->m_outside_ior;
            const double Fdr = -1.440 / square(eta) + 0.710 / eta + 0.668 + 0.0636 * eta;
            const double A = (1.0 + Fdr) / (1.0 - Fdr);

            value.resize(values->m_sigma_a.size());

            for (size_t i = 0, e = value.size(); i < e; ++i)
            {
                const double sigma_a = values->m_sigma_a[i];
                const double sigma_s = values->m_sigma_s[i];
                const double sigma_s_prime = sigma_s * (1.0 - values->m_anisotropy);
                const double sigma_t_prime = sigma_s_prime + sigma_a;
                const double alpha_prime = sigma_s_prime / sigma_t_prime;
                const double sigma_tr = sqrt(3.0 * sigma_a * sigma_t_prime);
                const double D = 1.0 / (3.0 * sigma_t_prime);
                const double zr = 1.0 / sigma_t_prime;
                const double zv = zr + 4.0 * A * D;
                const Vector3d xr = incoming_point.get_point() - incoming_point.get_shading_normal() * zr;
                const Vector3d xv = incoming_point.get_point() + incoming_point.get_shading_normal() * zv;
                const double dr = norm(x - xr);
                const double dv = norm(x - xv);
                const double value_r = (sigma_tr * dr + 1.0) * exp(-sigma_tr * dr) / (sigma_t_prime * dr * dr * dr);
                const double value_v = (sigma_tr * dv + 1.0) * exp(-sigma_tr * dv) / (sigma_t_prime * dv * dv * dv);
                value[i] = static_cast<float>(alpha_prime / (4.0 * Pi) * (value_r + zv * value_v));
            }

            const double radius = norm(incoming_point.get_point() - outgoing_point.get_point());
            value *= static_cast<float>(radius * values->m_weight);
        }

        virtual double evaluate_pdf(
            const void*             data,
            const size_t            channel,
            const double            radius) const APPLESEED_OVERRIDE
        {
            const StandardDipoleBSSRDFInputValues* values =
                reinterpret_cast<const StandardDipoleBSSRDFInputValues*>(data);

            const double reflectance = values->m_reflectance[channel];
            if (reflectance == 0.0)
                return 0.0;

            // PDF of the sampled radius.
            const double sigma_a = values->m_sigma_a[channel];
            const double sigma_s = values->m_sigma_s[channel];
            const double sigma_s_prime = sigma_s * (1.0 - values->m_anisotropy);
            const double sigma_t_prime = sigma_s_prime + sigma_a;
            const double sigma_tr = sqrt(3.0 * sigma_a * sigma_t_prime);
            const double pdf_radius = pdf_attenuation(radius, sigma_tr);

            // PDF of the sampled angle.
            const double pdf_angle = RcpTwoPi;

            // Compute and return the final PDF.
            return pdf_radius * pdf_angle;
        }
    };
}


//
// StandardDipoleBSSRDFFactory class implementation.
//

const char* StandardDipoleBSSRDFFactory::get_model() const
{
    return Model;
}

Dictionary StandardDipoleBSSRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Standard Dipole BSSRDF");
}

DictionaryArray StandardDipoleBSSRDFFactory::get_input_metadata() const
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

auto_release_ptr<BSSRDF> StandardDipoleBSSRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSSRDF>(new StandardDipoleBSSRDF(name, params));
}

}   // namespace renderer
