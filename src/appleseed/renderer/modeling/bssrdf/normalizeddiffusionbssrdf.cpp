
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
      : public BSSRDF
    {
      public:
        NormalizedDiffusionBSSRDF(
            const char*             name,
            const ParamArray&       params)
          : BSSRDF(name, params)
        {
            m_inputs.declare("weight", InputFormatScalar, "1.0");
            m_inputs.declare("reflectance", InputFormatSpectralReflectance);
            m_inputs.declare("reflectance_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("dmfp", InputFormatSpectralReflectance);
            m_inputs.declare("dmfp_multiplier", InputFormatScalar, "1.0");
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
            return align(sizeof(NormalizedDiffusionBSSRDFInputValues), 16);
        }

        virtual void evaluate_inputs(
            const ShadingContext&   shading_context,
            InputEvaluator&         input_evaluator,
            const ShadingPoint&     shading_point,
            const size_t            offset = 0) const APPLESEED_OVERRIDE
        {
            BSSRDF::evaluate_inputs(shading_context, input_evaluator, shading_point, offset);

            char* ptr = reinterpret_cast<char*>(input_evaluator.data());
            NormalizedDiffusionBSSRDFInputValues* values =
                reinterpret_cast<NormalizedDiffusionBSSRDFInputValues*>(ptr + offset);

            values->m_reflectance *= static_cast<float>(values->m_reflectance_multiplier);
            values->m_dmfp *= static_cast<float>(values->m_dmfp_multiplier);

            if (values->m_dmfp.size() != values->m_reflectance.size())
            {
                if (values->m_dmfp.is_spectral())
                    Spectrum::upgrade(values->m_reflectance, values->m_reflectance);
                else
                    values->m_reflectance = values->m_reflectance.convert_to_rgb(*m_lighting_conditions);
            }
        }

        virtual void evaluate(
            const void*             data,
            const ShadingPoint&     outgoing_point,
            const Vector3d&         outgoing_dir,
            const ShadingPoint&     incoming_point,
            const Vector3d&         incoming_dir,
            Spectrum&               value) const APPLESEED_OVERRIDE
        {
            const NormalizedDiffusionBSSRDFInputValues* values =
                reinterpret_cast<const NormalizedDiffusionBSSRDFInputValues*>(data);

            const double dist = norm(incoming_point.get_point() - outgoing_point.get_point());

            value.resize(values->m_reflectance.size());
            for (size_t i = 0, e = value.size(); i < e; ++i)
            {
                const double a = values->m_reflectance[i];
                const double s = normalized_diffusion_s(a);
                const double ld = values->m_dmfp[i];
                value[i] = static_cast<float>(normalized_diffusion_r(dist, ld, s, a));
            }

            value *= static_cast<float>(values->m_weight);
        }

      private:
        virtual bool do_sample(
            const void*             data,
            BSSRDFSample&           sample,
            Vector2d&               point) const APPLESEED_OVERRIDE
        {
            const NormalizedDiffusionBSSRDFInputValues* values =
                reinterpret_cast<const NormalizedDiffusionBSSRDFInputValues*>(data);

            if (values->m_weight == 0.0)
                return false;

            sample.set_is_directional(false);
            sample.set_eta(values->m_inside_ior / values->m_outside_ior);

            sample.get_sampling_context().split_in_place(3, 1);
            const Vector3d s = sample.get_sampling_context().next_vector2<3>();

            // Sample a color channel uniformly.
            const size_t channel = truncate<size_t>(s[0] * values->m_reflectance.size());
            sample.set_channel(channel);

            const double nd_r = values->m_reflectance[channel];
            if (nd_r == 0)
                return false;

            const double nd_s = normalized_diffusion_s(nd_r);

            // Sample a radius.
            const double radius =
                normalized_diffusion_sample(
                    s[1],
                    values->m_dmfp[channel],
                    nd_s);

            // Sample an angle.
            const double phi = TwoPi * s[2];

            // Set the max distance.
            sample.set_max_distance(
                normalized_diffusion_max_distance(
                    values->m_dmfp[channel],
                    nd_s));

            // Return point on disk.
            point = Vector2d(radius * cos(phi), radius * sin(phi));

            return true;
        }

        virtual double do_pdf(
            const void*             data,
            const size_t            channel,
            const double            dist) const APPLESEED_OVERRIDE
        {
            const NormalizedDiffusionBSSRDFInputValues* values =
                reinterpret_cast<const NormalizedDiffusionBSSRDFInputValues*>(data);

            // PDF of the sampled channel.
            const double pdf_channel = 1.0 / values->m_reflectance.size();

            // PDF of the sampled radius.
            const double pdf_radius =
                normalized_diffusion_pdf(
                    dist,
                    values->m_dmfp[channel],
                    normalized_diffusion_s(values->m_reflectance[channel]));

            // PDF of the sampled angle.
            const double pdf_angle = RcpTwoPi;

            // Compute and return the final PDF.
            return pdf_channel * pdf_radius * pdf_angle;
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
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "dmfp_multiplier")
            .insert("label", "Diffuse Mean Free Path Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

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
