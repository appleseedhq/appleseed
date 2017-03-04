
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bssrdf/sss.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <algorithm>
#include <cstddef>

// Forward declarations.
namespace foundation    { class Arena; }
namespace renderer      { class BSDFSample; }
namespace renderer      { class BSSRDFSample; }
namespace renderer      { class ShadingContext; }

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
            const char*             name,
            const ParamArray&       params)
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

        virtual size_t compute_input_data_size() const APPLESEED_OVERRIDE
        {
            return sizeof(NormalizedDiffusionBSSRDFInputValues);
        }

        virtual void prepare_inputs(
            Arena&                  arena,
            const ShadingPoint&     shading_point,
            void*                   data) const APPLESEED_OVERRIDE
        {
            NormalizedDiffusionBSSRDFInputValues* values =
                static_cast<NormalizedDiffusionBSSRDFInputValues*>(data);

            new (&values->m_precomputed) NormalizedDiffusionBSSRDFInputValues::Precomputed();

            new (&values->m_base_values) SeparableBSSRDF::InputValues();
            values->m_base_values.m_weight = values->m_weight;
            values->m_base_values.m_fresnel_weight = values->m_fresnel_weight;

            // Precompute the relative index of refraction.
            values->m_base_values.m_eta = compute_eta(shading_point, values->m_ior);

            make_reflectance_and_mfp_compatible(values->m_reflectance, values->m_mfp);

            // Apply multipliers to input values.
            values->m_reflectance *= values->m_reflectance_multiplier;
            values->m_mfp *= values->m_mfp_multiplier;

            // Clamp input values.
            clamp_in_place(values->m_reflectance, 0.001f, 0.999f);
            clamp_low_in_place(values->m_mfp, 1.0e-6f);

            // Build a CDF and PDF for channel sampling.
            build_cdf_and_pdf(
                values->m_reflectance,
                values->m_base_values.m_channel_cdf,
                values->m_precomputed.m_channel_pdf);

            // Precompute scaling factor.
            values->m_precomputed.m_s.resize(values->m_reflectance.size());
            for (size_t i = 0, e = values->m_reflectance.size(); i < e; ++i)
            {
                const float a = values->m_reflectance[i];
                values->m_precomputed.m_s[i] = normalized_diffusion_s_mfp(a);
            }

            // Precompute the radius of the sampling disk.
            float max_radius = 0.0f;
            for (size_t i = 0, e = values->m_mfp.size(); i < e; ++i)
            {
                const float l = values->m_mfp[i];
                const float s = values->m_precomputed.m_s[i];
                max_radius = max(max_radius, normalized_diffusion_max_radius(l, s));
            }
            values->m_base_values.m_max_disk_radius = max_radius;
        }

        virtual bool sample(
            const ShadingContext&   shading_context,
            SamplingContext&        sampling_context,
            const void*             data,
            const ShadingPoint&     outgoing_point,
            const Vector3f&         outgoing_dir,
            BSSRDFSample&           bssrdf_sample,
            BSDFSample&             bsdf_sample) const APPLESEED_OVERRIDE
        {
            const NormalizedDiffusionBSSRDFInputValues* values =
                static_cast<const NormalizedDiffusionBSSRDFInputValues*>(data);

            return do_sample(
                shading_context,
                sampling_context,
                data,
                values->m_base_values,
                outgoing_point,
                outgoing_dir,
                bssrdf_sample,
                bsdf_sample);
        }

        virtual float sample_profile(
            const void*             data,
            const size_t            channel,
            const float             u) const APPLESEED_OVERRIDE
        {
            const NormalizedDiffusionBSSRDFInputValues* values =
                static_cast<const NormalizedDiffusionBSSRDFInputValues*>(data);

            const float l = values->m_mfp[channel];
            const float s = values->m_precomputed.m_s[channel];

            return normalized_diffusion_sample(u, l, s);
        }

        virtual float evaluate_profile_pdf(
            const void*             data,
            const float             disk_radius) const APPLESEED_OVERRIDE
        {
            const NormalizedDiffusionBSSRDFInputValues* values =
                static_cast<const NormalizedDiffusionBSSRDFInputValues*>(data);

            if (disk_radius > values->m_base_values.m_max_disk_radius)
                return 0.0f;

            float pdf = 0.0f;

            for (size_t i = 0, e = values->m_reflectance.size(); i < e; ++i)
            {
                const float channel_pdf = values->m_precomputed.m_channel_pdf[i];
                const float l = values->m_mfp[i];
                const float s = values->m_precomputed.m_s[i];
                pdf += channel_pdf * normalized_diffusion_pdf(disk_radius, l, s);
            }

            return pdf;
        }

        virtual void evaluate_profile(
            const void*             data,
            const ShadingPoint&     outgoing_point,
            const Vector3f&         outgoing_dir,
            const ShadingPoint&     incoming_point,
            const Vector3f&         incoming_dir,
            Spectrum&               value) const APPLESEED_OVERRIDE
        {
            const NormalizedDiffusionBSSRDFInputValues* values =
                static_cast<const NormalizedDiffusionBSSRDFInputValues*>(data);

            value.resize(values->m_reflectance.size());

            const float radius =
                static_cast<float>(
                    norm(outgoing_point.get_point() - incoming_point.get_point()));

            for (size_t i = 0, e = value.size(); i < e; ++i)
            {
                const float l = values->m_mfp[i];
                const float s = values->m_precomputed.m_s[i];
                const float a = values->m_reflectance[i];
                value[i] = normalized_diffusion_profile(radius, l, s, a);
            }
        }

        virtual void evaluate(
            const void*             data,
            const ShadingPoint&     outgoing_point,
            const Vector3f&         outgoing_dir,
            const ShadingPoint&     incoming_point,
            const Vector3f&         incoming_dir,
            Spectrum&               value) const APPLESEED_OVERRIDE
        {
            const NormalizedDiffusionBSSRDFInputValues* values =
                static_cast<const NormalizedDiffusionBSSRDFInputValues*>(data);

            return do_evaluate(
                data,
                values->m_base_values,
                outgoing_point,
                outgoing_dir,
                incoming_point,
                incoming_dir,
                value);
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

auto_release_ptr<BSSRDF> NormalizedDiffusionBSSRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSSRDF>(new NormalizedDiffusionBSSRDF(name, params));
}

auto_release_ptr<BSSRDF> NormalizedDiffusionBSSRDFFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<BSSRDF>(new NormalizedDiffusionBSSRDF(name, params));
}

}   // namespace renderer
