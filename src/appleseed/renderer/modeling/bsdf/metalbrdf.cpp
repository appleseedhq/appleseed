
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016 Esteban Tovagliari, The appleseedhq Organization
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
#include "metalbrdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"
#include "renderer/modeling/bsdf/microfacethelper.h"
#include "renderer/utility/messagecontext.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/microfacet.h"
#include "foundation/math/minmax.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/makevector.h"
#include "foundation/utility/otherwise.h"

// Standard headers.
#include <algorithm>
#include <cmath>
#include <string>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class Project; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    template <typename T>
    class FresnelFriendlyConductorFun
    {
      public:
        FresnelFriendlyConductorFun(
            const Spectrum& normal_reflectance,
            const Spectrum& edge_tint,
            const T         reflectance_multiplier)
          : m_r(normal_reflectance)
          , m_g(edge_tint)
          , m_reflectance_multiplier(reflectance_multiplier)
        {
        }

        void operator()(
            const foundation::Vector<T, 3>& o,
            const foundation::Vector<T, 3>& h,
            const foundation::Vector<T, 3>& n,
            Spectrum&                       value) const
        {
            artist_friendly_fresnel_reflectance_conductor(
                value,
                m_r,
                m_g,
                foundation::dot(o, h));
            value *= static_cast<float>(m_reflectance_multiplier);
        }

      private:
        const Spectrum& m_r;
        const Spectrum& m_g;
        const double    m_reflectance_multiplier;
    };

    //
    // Metal BRDF.
    //
    //    A future version of this BRDF will support multiple scattering.
    //    For that reason, the only available microfacet distribution functions
    //    are those that support it (Beckmann and GGX).
    //
    // References:
    //
    //   [1] Microfacet Models for Refraction through Rough Surfaces
    //       http://www.cs.cornell.edu/~srm/publications/EGSR07-btdf.pdf
    //
    // Todo:
    //
    //  This BRDF currently assumes an air | metal interface and will not
    //  produce correct results when that's not the case, for example
    //  a metallic object inside a liquid.
    //  Reference for the correct Fresnel equation to use:
    //  https://seblagarde.wordpress.com/2013/04/29/memo-on-fresnel-equations/
    //

    const char* Model = "metal_brdf";

    class MetalBRDFImpl
      : public BSDF
    {
      public:
        MetalBRDFImpl(
            const char*         name,
            const ParamArray&   params)
          : BSDF(name, Reflective, ScatteringMode::Glossy, params)
        {
            m_inputs.declare("normal_reflectance", InputFormatSpectralReflectance);
            m_inputs.declare("edge_tint", InputFormatSpectralReflectance);
            m_inputs.declare("reflectance_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("roughness", InputFormatScalar, "0.15");
            m_inputs.declare("anisotropic", InputFormatScalar, "0.0");
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        virtual bool on_frame_begin(
            const Project&      project,
            const Assembly&     assembly,
            IAbortSwitch*       abort_switch) APPLESEED_OVERRIDE
        {
            if (!BSDF::on_frame_begin(project, assembly, abort_switch))
                return false;

            const EntityDefMessageContext context("bsdf", this);
            const string mdf =
                m_params.get_required<string>(
                    "mdf",
                    "ggx",
                    make_vector("beckmann", "ggx"),
                    context);

            if (mdf == "ggx")
                m_mdf = GGX;
            else // beckmann
                m_mdf = Beckmann;

            return true;
        }

        APPLESEED_FORCE_INLINE virtual void sample(
            SamplingContext&    sampling_context,
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            BSDFSample&         sample) const APPLESEED_OVERRIDE
        {
            const Vector3d& n = sample.get_shading_basis().get_normal();
            const double cos_on = std::min(dot(sample.m_outgoing.get_value(), n), 1.0);
            if (cos_on < 0.0)
                return;

            const InputValues* values = reinterpret_cast<const InputValues*>(data);

            double alpha_x, alpha_y;
            microfacet_alpha_from_roughness(
                values->m_roughness,
                values->m_anisotropic,
                alpha_x,
                alpha_y);

            FresnelFriendlyConductorFun<double> f(
                values->m_normal_reflectance,
                values->m_edge_tint,
                values->m_reflectance_multiplier);

            if (m_mdf == GGX)
            {
                const GGXMDF<double> mdf;
                MicrofacetBRDFHelper<double>::sample(
                    sampling_context,
                    mdf,
                    alpha_x,
                    alpha_y,
                    f,
                    cos_on,
                    sample);
            }
            else
            {
                const BeckmannMDF<double> mdf;
                MicrofacetBRDFHelper<double>::sample(
                    sampling_context,
                    mdf,
                    alpha_x,
                    alpha_y,
                    f,
                    cos_on,
                    sample);
            }
        }

        APPLESEED_FORCE_INLINE virtual double evaluate(
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            const int           modes,
            Spectrum&           value) const APPLESEED_OVERRIDE
        {
            if (!ScatteringMode::has_glossy(modes))
                return 0.0;

            // No reflection below the shading surface.
            const Vector3d& n = shading_basis.get_normal();
            const double cos_in = dot(incoming, n);
            const double cos_on = dot(outgoing, n);
            if (cos_in < 0.0 || cos_on < 0.0)
                return 0.0;

            const InputValues* values = reinterpret_cast<const InputValues*>(data);

            double alpha_x, alpha_y;
            microfacet_alpha_from_roughness(
                values->m_roughness,
                values->m_anisotropic,
                alpha_x,
                alpha_y);

            FresnelFriendlyConductorFun<double> f(
                values->m_normal_reflectance,
                values->m_edge_tint,
                values->m_reflectance_multiplier);

            if (m_mdf == GGX)
            {
                const GGXMDF<double> mdf;
                return MicrofacetBRDFHelper<double>::evaluate(
                    mdf,
                    alpha_x,
                    alpha_y,
                    shading_basis,
                    outgoing,
                    incoming,
                    f,
                    cos_in,
                    cos_on,
                    value);
            }
            else
            {
                const BeckmannMDF<double> mdf;
                return MicrofacetBRDFHelper<double>::evaluate(
                    mdf,
                    alpha_x,
                    alpha_y,
                    shading_basis,
                    outgoing,
                    incoming,
                    f,
                    cos_in,
                    cos_on,
                    value);
            }
        }

        APPLESEED_FORCE_INLINE virtual double evaluate_pdf(
            const void*         data,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            const int           modes) const APPLESEED_OVERRIDE
        {
            if (!ScatteringMode::has_glossy(modes))
                return 0.0;

            // No reflection below the shading surface.
            const Vector3d& n = shading_basis.get_normal();
            const double cos_in = dot(incoming, n);
            const double cos_on = dot(outgoing, n);
            if (cos_in < 0.0 || cos_on < 0.0)
                return 0.0;

            const InputValues* values = reinterpret_cast<const InputValues*>(data);

            double alpha_x, alpha_y;
            microfacet_alpha_from_roughness(
                values->m_roughness,
                values->m_anisotropic,
                alpha_x,
                alpha_y);

            if (m_mdf == GGX)
            {
                const GGXMDF<double> mdf;
                return MicrofacetBRDFHelper<double>::pdf(
                    mdf,
                    alpha_x,
                    alpha_y,
                    shading_basis,
                    outgoing,
                    incoming);
            }
            else
            {
                const BeckmannMDF<double> mdf;
                return MicrofacetBRDFHelper<double>::pdf(
                    mdf,
                    alpha_x,
                    alpha_y,
                    shading_basis,
                    outgoing,
                    incoming);
            }
        }

      private:
        typedef MetalBRDFInputValues InputValues;

        enum MDF
        {
            GGX,
            Beckmann
        };

        MDF m_mdf;
    };

    typedef BSDFWrapper<MetalBRDFImpl> MetalBRDF;
}


//
// MetalBRDFFactory class implementation.
//

const char* MetalBRDFFactory::get_model() const
{
    return Model;
}

Dictionary MetalBRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Metal BRDF");
}

DictionaryArray MetalBRDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "mdf")
            .insert("label", "Microfacet Distribution Function")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("Beckmann", "beckmann")
                    .insert("GGX", "ggx"))
            .insert("use", "required")
            .insert("default", "ggx"));

    metadata.push_back(
        Dictionary()
            .insert("name", "normal_reflectance")
            .insert("label", "Normal Reflectance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.92"));

    metadata.push_back(
        Dictionary()
            .insert("name", "edge_tint")
            .insert("label", "Edge Tint")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.98"));

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
            .insert("name", "roughness")
            .insert("label", "Roughness")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("min_value", "0.0")
            .insert("max_value", "1.0")
            .insert("default", "0.15"));

    metadata.push_back(
        Dictionary()
            .insert("name", "anisotropic")
            .insert("label", "Anisotropic")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("min_value", "-1.0")
            .insert("max_value", "1.0")
            .insert("default", "0.0"));

    return metadata;
}

auto_release_ptr<BSDF> MetalBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new MetalBRDF(name, params));
}

auto_release_ptr<BSDF> MetalBRDFFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<BSDF>(new MetalBRDF(name, params));
}

}   // namespace renderer
