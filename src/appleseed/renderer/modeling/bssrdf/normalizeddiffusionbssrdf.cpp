
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015 Esteban Tovagliari, The appleseedhq Organization
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
#include "normalizeddiffusionbssrdf.h"

// appleseed.renderer headers.
#include "renderer/modeling/bssrdf/bssrdfsample.h"
#include "renderer/modeling/bssrdf/separablebssrdf.h"
#include "renderer/modeling/bssrdf/sss.h"
#include "renderer/modeling/input/inputevaluator.h"

// appleseed.foundation headers.
#include "foundation/math/cdf.h"
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
    // Normalized diffusion BSSRDF.
    //
    // Reference:
    //
    //   Approximate Reflectance Profiles for Efficient Subsurface Scattering
    //   Per H. Christensen, Brent Burley
    //   http://graphics.pixar.com/library/ApproxBSSRDF/paper.pdf
    //

    const char* Model = "normalized_diffusion_bssrdf";

    class NormalizedDiffusionBSSRDF
      : public SeparableBSSRDF
    {
      public:
        NormalizedDiffusionBSSRDF(
            const char*         name,
            const ParamArray&   params)
          : SeparableBSSRDF(name, params)
        {
            m_inputs.declare("weight", InputFormatScalar, "1.0");
            m_inputs.declare("reflectance", InputFormatSpectralReflectance);
            m_inputs.declare("reflectance_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("dmfp", InputFormatScalar, "5.0");
            m_inputs.declare("dmfp_multiplier", InputFormatScalar, "0.1");
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
            const Assembly&     assembly) const APPLESEED_OVERRIDE
        {
            return align(sizeof(NormalizedDiffusionBSSRDFInputValues), 16);
        }

        virtual void prepare_inputs(void* data) const APPLESEED_OVERRIDE
        {
            NormalizedDiffusionBSSRDFInputValues* values =
                reinterpret_cast<NormalizedDiffusionBSSRDFInputValues*>(data);

            // Precompute the relative index of refraction.
            values->m_eta = values->m_outside_ior / values->m_inside_ior;

            // Apply multipliers.
            values->m_reflectance *= static_cast<float>(values->m_reflectance_multiplier);
            values->m_dmfp *= static_cast<float>(values->m_dmfp_multiplier);

            // Clamp reflectance.
            values->m_reflectance = clamp(values->m_reflectance, 0.001f, 1.0f);

            // Build a CDF for channel sampling.

            values->m_s.resize(values->m_reflectance.size());
            values->m_channel_pdf.resize(values->m_reflectance.size());
            values->m_channel_cdf.resize(values->m_reflectance.size());

            float cumulated_pdf = 0.0f;
            for (size_t i = 0, e = values->m_channel_pdf.size(); i < e; ++i)
            {
                const double a = static_cast<double>(values->m_reflectance[i]);
                const double s = normalized_diffusion_s(a);
                values->m_s[i] = static_cast<float>(s);

                const double l = values->m_dmfp;
                const float pdf = static_cast<float>(s / l);
                values->m_channel_pdf[i] = pdf;

                cumulated_pdf += pdf;
                values->m_channel_cdf[i] = cumulated_pdf;
            }

            const float rcp_cumulated_pdf = 1.0f / cumulated_pdf;
            values->m_channel_pdf *= rcp_cumulated_pdf;
            values->m_channel_cdf *= rcp_cumulated_pdf;
            values->m_channel_cdf[values->m_channel_cdf.size() - 1] = 1.0f;

            // Precompute the (square of the) max radius.
            const size_t channel = min_index(values->m_reflectance);
            values->m_rmax2 =
                square(
                    normalized_diffusion_max_radius(
                        values->m_dmfp,
                        values->m_s[channel]));
        }

        virtual bool sample(
            SamplingContext&    sampling_context,
            const void*         data,
            BSSRDFSample&       sample) const APPLESEED_OVERRIDE
        {
            const NormalizedDiffusionBSSRDFInputValues* values =
                reinterpret_cast<const NormalizedDiffusionBSSRDFInputValues*>(data);

            if (values->m_weight == 0.0)
                return false;

            sampling_context.split_in_place(3, 1);
            const Vector3d s = sampling_context.next_vector2<3>();

            // Sample a channel.
            const float* cdf_begin = &values->m_channel_cdf[0];
            const size_t channel =
                sample_cdf(
                    cdf_begin,
                    cdf_begin + values->m_channel_cdf.size(),
                    s[0]);

            // Sample a radius.
            const double radius =
                normalized_diffusion_sample(s[1], values->m_dmfp, values->m_s[channel]);

            // Sample an angle.
            const double phi = TwoPi * s[2];

            sample.m_eta = values->m_eta;
            sample.m_channel = channel;
            sample.m_point = Vector2d(radius * cos(phi), radius * sin(phi));
            sample.m_rmax2 = values->m_rmax2;

            return true;
        }

        virtual double get_eta(
            const void*         data) const APPLESEED_OVERRIDE
        {
            return reinterpret_cast<const NormalizedDiffusionBSSRDFInputValues*>(data)->m_eta;
        }

        virtual void evaluate_profile(
            const void*         data,
            const double        square_radius,
            Spectrum&           value) const APPLESEED_OVERRIDE
        {
            const NormalizedDiffusionBSSRDFInputValues* values =
                reinterpret_cast<const NormalizedDiffusionBSSRDFInputValues*>(data);

            const double radius = sqrt(square_radius);

            value.resize(values->m_reflectance.size());

            for (size_t i = 0, e = value.size(); i < e; ++i)
            {
                const double a = values->m_reflectance[i];
                const double s = values->m_s[i];
                value[i] = static_cast<float>(normalized_diffusion_profile(radius, values->m_dmfp, s, a));
            }

            // Return r * R(r) * weight.
            value *= static_cast<float>(radius * values->m_weight);
        }

        virtual double evaluate_pdf(
            const void*         data,
            const size_t        channel,
            const double        radius) const APPLESEED_OVERRIDE
        {
            const NormalizedDiffusionBSSRDFInputValues* values =
                reinterpret_cast<const NormalizedDiffusionBSSRDFInputValues*>(data);

            // PDF of the sampled radius.
            double pdf_radius = 0.0;
            for (size_t i = 0, e = values->m_reflectance.size(); i < e; ++i)
            {
                pdf_radius +=
                    normalized_diffusion_pdf(
                        radius,
                        values->m_dmfp,
                        values->m_s[i])
                    * values->m_channel_pdf[i];
            }

            // PDF of the sampled angle.
            const double pdf_angle = RcpTwoPi;

            // Compute and return the final PDF.
            return pdf_radius * pdf_angle;
        }
    };
}


//
// NormalizedDiffusionBSSRDFFactory class implementation.
//

const char* NormalizedDiffusionBSSRDFFactory::get_model() const
{
    return Model;
}

Dictionary NormalizedDiffusionBSSRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Normalized Diffusion BSSRDF");
}

DictionaryArray NormalizedDiffusionBSSRDFFactory::get_input_metadata() const
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

auto_release_ptr<BSSRDF> NormalizedDiffusionBSSRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSSRDF>(new NormalizedDiffusionBSSRDF(name, params));
}

}   // namespace renderer
