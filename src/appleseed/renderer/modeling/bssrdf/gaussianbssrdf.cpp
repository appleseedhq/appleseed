
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2017 Francois Beaune, The appleseedhq Organization
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
#include "gaussianbssrdf.h"

// appleseed.renderer headers.
#include "renderer/modeling/bssrdf/bssrdfsample.h"
#include "renderer/modeling/bssrdf/separablebssrdf.h"
#include "renderer/modeling/bssrdf/sss.h"
#include "renderer/modeling/input/inputevaluator.h"

// appleseed.foundation headers.
#include "foundation/math/cdf.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"
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
    // Gaussian BSSRDF.
    //
    // References:
    //
    //   BSSRDF Importance Sampling
    //   http://library.imageworks.com/pdfs/imageworks-library-BSSRDF-sampling.pdf
    //
    //   BSSRDF Importance Sampling
    //   http://rendering-memo.blogspot.fr/2015/01/bssrdf-importance-sampling-4-multiple.html
    //
    // Derivation:
    //
    //   The profile function is a simple Gaussian:
    //
    //            exp(-r^2 / (2*v))
    //     R(r) = -----------------
    //               2 * Pi * v
    //
    //   The integral of this function over R^2 is equal to 1. Since we want to truncate
    //   this function to a disk of radius Rmax and still have it integrate to 1, we'll
    //   need an additional scaling factor.
    //
    //   Let's compute the value of this integral for a disk of radius Rmax:
    //
    //     /              / 2 Pi   / Rmax
    //     |              |        |
    //     |  R(r) dA  =  |        |  R(r) r dr dtheta  =  1 - exp(-Rmax^2 / (2*v))
    //     |              |        |
    //     / disk         / 0      / 0
    //
    //   (Recall that dA = r dr dtheta.)
    //
    //   Our (multiplicative) normalization factor is then
    //
    //                    1
    //     K = ------------------------
    //         1 - exp(-Rmax^2 / (2*v))
    //
    //   Rmax is the radius at which the value of the integral of the Gaussian profile
    //   is low enough, i.e. when the integral over the disk is close enough to one.
    //   For instance, if we pick 0.999 as a threshold:
    //
    //     1 - exp(-Rmax^2 / (2*v)) = 0.999
    //
    //   We find that
    //
    //     Rmax = sqrt(-2 * v * ln(0.001)) = sqrt(v * 13.815510558)
    //

    const char* Model = "gaussian_bssrdf";

    const float RIntegralThreshold = 0.999f;
    const float RMax2Constant = -2.0f * log(1.0f - RIntegralThreshold);

    class GaussianBSSRDF
      : public SeparableBSSRDF
    {
      public:
        GaussianBSSRDF(
            const char*         name,
            const ParamArray&   params)
          : SeparableBSSRDF(name, params)
        {
            m_inputs.declare("weight", InputFormatFloat, "1.0");
            m_inputs.declare("reflectance", InputFormatSpectralReflectance);
            m_inputs.declare("reflectance_multiplier", InputFormatFloat, "1.0");
            m_inputs.declare("mfp", InputFormatSpectralReflectance);
            m_inputs.declare("mfp_multiplier", InputFormatFloat, "1.0");
            m_inputs.declare("ior", InputFormatFloat);
            m_inputs.declare("fresnel_weight", InputFormatFloat, "1.0");
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
            return align(sizeof(GaussianBSSRDFInputValues), 16);
        }

        virtual void prepare_inputs(
            const ShadingPoint& shading_point,
            void*               data) const APPLESEED_OVERRIDE
        {
            GaussianBSSRDFInputValues* values =
                reinterpret_cast<GaussianBSSRDFInputValues*>(data);

            new (&values->m_precomputed) GaussianBSSRDFInputValues::Precomputed();

            // Precompute the relative index of refraction.
            values->m_precomputed.m_eta = compute_eta(shading_point, values->m_ior);

            make_reflectance_and_mfp_compatible(values->m_reflectance, values->m_mfp);

            // Apply multipliers to input values.
            values->m_reflectance *= values->m_reflectance_multiplier * values->m_weight;
            values->m_mfp *= values->m_mfp_multiplier;

            // Build a CDF and PDF for channel sampling.
            build_cdf_and_pdf(
                values->m_reflectance,
                values->m_precomputed.m_channel_cdf,
                values->m_precomputed.m_channel_pdf);

            // Precompute v and the (square of the) max radius.
            float max_v = 0.0f;
            values->m_precomputed.m_v.resize(values->m_mfp.size());
            for (size_t i = 0, e = values->m_precomputed.m_channel_pdf.size(); i < e; ++i)
            {
                // The remapping from mfp to radius comes from alSurface.
                const float radius = max(values->m_mfp[i], 0.0001f) * 7.0f;

                // The remapping from radius to v comes from Cycles.
                const float v = square(radius) * square(0.25f);
                values->m_precomputed.m_v[i] = v;
                max_v = max(max_v, v);
            }

            values->m_precomputed.m_rmax2 = max_v * RMax2Constant;
        }

        virtual bool sample(
            SamplingContext&    sampling_context,
            const void*         data,
            BSSRDFSample&       sample) const APPLESEED_OVERRIDE
        {
            const GaussianBSSRDFInputValues* values =
                reinterpret_cast<const GaussianBSSRDFInputValues*>(data);

            const float rmax2 = values->m_precomputed.m_rmax2;

            sampling_context.split_in_place(3, 1);
            const Vector3f s = sampling_context.next2<Vector3f>();

            // Sample a channel.
            const float* cdf_begin = &values->m_precomputed.m_channel_cdf[0];
            const size_t channel =
                sample_cdf(
                    cdf_begin,
                    cdf_begin + values->m_precomputed.m_channel_cdf.size(),
                    s[0]);

            const float v = values->m_precomputed.m_v[channel];
            const float radius = sqrt(-2.0f * v * log(1.0f - s[0] * (1.0f - exp(-rmax2 / (2.0f * v)))));
            const float phi = TwoPi<float>() * s[1];

            sample.m_eta = values->m_precomputed.m_eta;
            sample.m_channel = channel;
            sample.m_point = Vector2f(radius * cos(phi), radius * sin(phi));
            sample.m_rmax2 = rmax2;

            return true;
        }

        virtual float get_eta(
            const void*         data) const APPLESEED_OVERRIDE
        {
            return reinterpret_cast<const GaussianBSSRDFInputValues*>(data)->m_precomputed.m_eta;
        }

        virtual float get_fresnel_weight(
            const void*         data) const APPLESEED_OVERRIDE
        {
            return reinterpret_cast<const GaussianBSSRDFInputValues*>(data)->m_fresnel_weight;
        }

        virtual void evaluate_profile(
            const void*         data,
            const float         square_radius,
            Spectrum&           value) const APPLESEED_OVERRIDE
        {
            const GaussianBSSRDFInputValues* values =
                reinterpret_cast<const GaussianBSSRDFInputValues*>(data);

            const float rmax2 = values->m_precomputed.m_rmax2;

            if (square_radius > rmax2)
            {
                value.set(0.0f);
                return;
            }

            value = values->m_reflectance;
            for (size_t i = 0, e = value.size(); i < e; ++i)
            {
                const float v = values->m_precomputed.m_v[i];
                const float rd = exp(-square_radius / (2.0f * v)) / (TwoPi<float>() * v * RIntegralThreshold);
                value[i] *= rd;
            }
        }

        virtual float evaluate_pdf(
            const void*         data,
            const size_t        channel,
            const float         radius) const APPLESEED_OVERRIDE
        {
            const GaussianBSSRDFInputValues* values =
                reinterpret_cast<const GaussianBSSRDFInputValues*>(data);

            const float rmax2 = values->m_precomputed.m_rmax2;
            const float r2 = radius * radius;

            if (r2 > rmax2)
                return 0.0f;

            float pdf = 0.0f;
            for (size_t i = 0, e = values->m_reflectance.size(); i < e; ++i)
            {
                const float v = values->m_precomputed.m_v[i];
                pdf +=
                      exp(-r2 / (2.0f * v)) / (TwoPi<float>() * v * RIntegralThreshold)
                    * values->m_precomputed.m_channel_pdf[i];
            }

            return pdf;
        }
    };
}


//
// GaussianBSSRDFFactory class implementation.
//

const char* GaussianBSSRDFFactory::get_model() const
{
    return Model;
}

Dictionary GaussianBSSRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Gaussian BSSRDF");
}

DictionaryArray GaussianBSSRDFFactory::get_input_metadata() const
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
            .insert("label", "Reflectance")
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
            .insert("label", "Reflectance Multiplier")
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

    metadata.push_back(
        Dictionary()
            .insert("name", "fresnel_weight")
            .insert("label", "Fresnel Weight")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "1.0")
            .insert("use", "optional")
            .insert("default", "1.0"));

    return metadata;
}

auto_release_ptr<BSSRDF> GaussianBSSRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSSRDF>(new GaussianBSSRDF(name, params));
}

auto_release_ptr<BSSRDF> GaussianBSSRDFFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<BSSRDF>(new GaussianBSSRDF(name, params));
}

}   // namespace renderer
