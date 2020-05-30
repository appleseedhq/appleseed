
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
#include "gaussianbssrdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/shading/shadingpoint.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <algorithm>
#include <cmath>
#include <cstddef>

// Forward declarations.
namespace foundation    { class Arena; }
namespace renderer      { class BSDFSample; }
namespace renderer      { class BSSRDFSample; }
namespace renderer      { class ShadingContext; }

using namespace foundation;

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
    //            a
    //          = -- * exp(-a * r^2)   with a = 1 / (2*v)
    //            Pi
    //
    //   Unlike its 1D counterpart for which the PDF has no closed form solution,
    //   this function can be importance-sampled analytically.
    //
    //   R(r) is its own PDF, and as such it integrates to 1 over the plane.
    //
    //   Let's express this integral as a function of r:
    //
    //     /              / 2 Pi   / r
    //     |              |        |
    //     |  R(r) dA  =  |        |  R(s) s ds dtheta  =  1 - exp(-r^2 / (2*v))
    //     |              |        |
    //     / disk         / 0      / 0
    //
    //   In order to improve sampling efficiency, we will limit our sampling to
    //   a disk of radius Rmax such that the integral of the PDF over this disk
    //   is close enough to 1. If we pick 0.999 as a threshold, solving
    //
    //     1 - exp(-Rmax^2 / (2*v)) = 0.999
    //
    //   for Rmax leads to
    //
    //     Rmax = sqrt(-2 * v * ln(0.001))
    //          = sqrt(v) * sqrt(-2 * ln(0.001))
    //          = sqrt(v) * RmaxFactor
    //
    //   with RmaxFactor = sqrt(-2 * ln(0.001))
    //
    //   In order for the PDF to continue integrating to 1, we must then rescale it by
    //
    //                     1
    //     ScaleFactor = -----
    //                   0.999
    //

    const char* Model = "gaussian_bssrdf";

    const float IntegralThreshold = 0.999f;
    const float ScaleFactor = 1.0f / IntegralThreshold;
    const float RmaxFactor = std::sqrt(-2.0f * std::log(1.0f - IntegralThreshold));

    class GaussianBSSRDF
      : public SeparableBSSRDF
    {
      public:
        GaussianBSSRDF(
            const char*             name,
            const ParamArray&       params)
          : SeparableBSSRDF(name, params)
        {
            m_inputs.declare("weight", InputFormat::Float, "1.0");
            m_inputs.declare("reflectance", InputFormat::SpectralReflectance);
            m_inputs.declare("reflectance_multiplier", InputFormat::Float, "1.0");
            m_inputs.declare("mfp", InputFormat::SpectralReflectance);
            m_inputs.declare("mfp_multiplier", InputFormat::Float, "1.0");
            m_inputs.declare("ior", InputFormat::Float);
            m_inputs.declare("fresnel_weight", InputFormat::Float, "1.0");
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return Model;
        }

        size_t compute_input_data_size() const override
        {
            return sizeof(GaussianBSSRDFInputValues);
        }

        void prepare_inputs(
            Arena&                  arena,
            const ShadingPoint&     shading_point,
            void*                   data) const override
        {
            GaussianBSSRDFInputValues* values =
                static_cast<GaussianBSSRDFInputValues*>(data);

            new (&values->m_precomputed) GaussianBSSRDFInputValues::Precomputed();

            new (&values->m_base_values) SeparableBSSRDF::InputValues();
            values->m_base_values.m_weight = values->m_weight;
            values->m_base_values.m_fresnel_weight = values->m_fresnel_weight;

            // Precompute the relative index of refraction.
            values->m_base_values.m_eta = compute_eta(shading_point, values->m_ior);

            // Apply multipliers to input values.
            values->m_reflectance *= values->m_reflectance_multiplier * values->m_weight;
            values->m_mfp *= values->m_mfp_multiplier;

            // Clamp input values.
            clamp_in_place(values->m_reflectance, 0.001f, 0.999f);
            clamp_low_in_place(values->m_mfp, 1.0e-6f);

            // Build a CDF and PDF for channel sampling.
            build_cdf_and_pdf(
                values->m_reflectance,
                values->m_base_values.m_channel_cdf,
                values->m_precomputed.m_channel_pdf);

            // Precompute 1/(2v).
            float max_sqrt_v = 0.0f;
            for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
            {
                // The remapping from mfp to radius comes from alSurface.
                const float radius = values->m_mfp[i] * 7.0f;

                // The remapping from radius to v comes from Cycles.
                const float sqrt_v = radius * 0.25f;
                max_sqrt_v = std::max(max_sqrt_v, sqrt_v);

                // Precompute 1/(2v).
                const float v = square(sqrt_v);
                values->m_precomputed.m_k[i] = 1.0f / (2.0f * v);
            }

            // Precompute the radius of the sampling disk.
            values->m_base_values.m_max_disk_radius = max_sqrt_v * RmaxFactor;
        }

        float sample_profile(
            const void*             data,
            const size_t            channel,
            const float             u) const override
        {
            const GaussianBSSRDFInputValues* values =
                static_cast<const GaussianBSSRDFInputValues*>(data);

            //
            // We restrict sampling to the disk of radius Rmax, so we restrict the sampling
            // parameter u originally in [0, 1) to [0, Umax) such that
            //
            //        | -ln(1 - Umax) |
            //   sqrt | ------------- | = Rmax
            //        |       a       |
            //
            // which leads to
            //
            //   Umax = 1 - exp(-a * Rmax^2)
            //

            const float k = values->m_precomputed.m_k[channel];
            const float umax = 1.0f - std::exp(-k * square(values->m_base_values.m_max_disk_radius));

            return sample_disk_gaussian(u * umax, k);
        }

        float evaluate_profile_pdf(
            const void*             data,
            const float             disk_radius) const override
        {
            const GaussianBSSRDFInputValues* values =
                static_cast<const GaussianBSSRDFInputValues*>(data);

            if (disk_radius > values->m_base_values.m_max_disk_radius)
                return 0.0f;

            float pdf = 0.0f;

            for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
            {
                const float channel_pdf = values->m_precomputed.m_channel_pdf[i];
                const float k = values->m_precomputed.m_k[i];
                pdf += channel_pdf * sample_disk_gaussian_area_pdf(disk_radius, k) * ScaleFactor;
            }

            return pdf;
        }

        void evaluate_profile(
            const void*             data,
            const ShadingPoint&     outgoing_point,
            const Vector3f&         outgoing_dir,
            const ShadingPoint&     incoming_point,
            const Vector3f&         incoming_dir,
            Spectrum&               value) const override
        {
            const GaussianBSSRDFInputValues* values =
                static_cast<const GaussianBSSRDFInputValues*>(data);

            const float radius =
                static_cast<float>(
                    norm(outgoing_point.get_point() - incoming_point.get_point()));

            for (size_t i = 0, e = Spectrum::size(); i < e; ++i)
            {
                const float a = values->m_reflectance[i];
                const float k = values->m_precomputed.m_k[i];
                value[i] = a * sample_disk_gaussian_area_pdf(radius, k);
            }
        }

        bool sample(
            const ShadingContext&   shading_context,
            SamplingContext&        sampling_context,
            const void*             data,
            const ShadingPoint&     outgoing_point,
            const Vector3f&         outgoing_dir,
            const int               modes,
            BSSRDFSample&           bssrdf_sample,
            BSDFSample&             bsdf_sample) const override
        {
            const GaussianBSSRDFInputValues* values =
                static_cast<const GaussianBSSRDFInputValues*>(data);

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

        void evaluate(
            const void*             data,
            const ShadingPoint&     outgoing_point,
            const Vector3f&         outgoing_dir,
            const ShadingPoint&     incoming_point,
            const Vector3f&         incoming_dir,
            const int               modes,
            Spectrum&               value) const override
        {
            const GaussianBSSRDFInputValues* values =
                static_cast<const GaussianBSSRDFInputValues*>(data);

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
    };
}


//
// GaussianBSSRDFFactory class implementation.
//

void GaussianBSSRDFFactory::release()
{
    delete this;
}

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
                Dictionary().insert("texture_instance", "Texture Instances"))
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
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "reflectance_multiplier")
            .insert("label", "Reflectance Multiplier")
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

auto_release_ptr<BSSRDF> GaussianBSSRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSSRDF>(new GaussianBSSRDF(name, params));
}

}   // namespace renderer
