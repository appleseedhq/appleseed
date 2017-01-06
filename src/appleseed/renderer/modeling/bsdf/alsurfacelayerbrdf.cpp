
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "alsurfacelayerbrdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/scatteringmode.h"
#include "renderer/kernel/shading/closures.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"
#include "renderer/modeling/bsdf/fresnel.h"
#include "renderer/modeling/bsdf/microfacethelper.h"
#include "renderer/modeling/bsdf/specularhelper.h"
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
    //
    // Global ustrings.
    //

    const OIIO::ustring g_beckmann_str("beckmann");
    const OIIO::ustring g_ggx_str("ggx");

    const OIIO::ustring g_dielectric_str("dielectric");
    const OIIO::ustring g_metallic_str("metallic");

    //
    // AlSurfaceLayer BRDF.
    //
    //
    // References:
    //
    //

    const char* Model = "al_surface_layer_brdf";

    class AlSurfaceLayerBRDFImpl
      : public BSDF
    {
      public:
        AlSurfaceLayerBRDFImpl(
            const char*             name,
            const ParamArray&       params)
          : BSDF(name, Reflective, ScatteringMode::All, params)
        {
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
            return align(sizeof(InputValues), 16);
        }

        APPLESEED_FORCE_INLINE virtual void prepare_inputs(
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            void*                   data) const APPLESEED_OVERRIDE
        {
            InputValues* values = reinterpret_cast<InputValues*>(data);

            new (&values->m_precomputed) InputValues::Precomputed();
            values->m_precomputed.m_reflection_weight = clamp(max_value(values->m_reflectance), 0.0f, 1.0f);

            const float roughness = square(values->m_roughness);
            values->m_precomputed.m_alpha_x = roughness;
            values->m_precomputed.m_alpha_y = roughness;

            if (values->m_anisotropy != 0.5f)
            {
                const float t = square(2.0 * values->m_anisotropy - 1.0f);

                if (values->m_anisotropy < 0.5f)
                    values->m_precomputed.m_alpha_x = lerp(roughness, 1.0f, t);
                else
                    values->m_precomputed.m_alpha_y = lerp(roughness, 1.0f, t);
            }

            // Allocate memory and initialize the nested closure tree.
            values->m_closure_data = shading_context.osl_mem_alloc(sizeof(CompositeSurfaceClosure));

            CompositeSurfaceClosure* c = reinterpret_cast<CompositeSurfaceClosure*>(values->m_closure_data);
            new (c) CompositeSurfaceClosure(
                Basis3f(shading_point.get_shading_basis()),
                reinterpret_cast<OSL::ClosureColor*>(values->m_substrate));

            // Inject values into any children layered closures.
            assert(values->m_osl_bsdf);
            for (size_t i = 0, e = c->get_num_closures(); i < e; ++i)
            {
                const ClosureID cid = c->get_closure_type(i);

                if (cid >= FirstLayeredClosure)
                    inject_layered_closure_values(cid, values->m_osl_bsdf, c->get_closure_input_values(i));
            }

            // Prepare inputs for all children BSDFs.
            values->m_osl_bsdf->prepare_inputs(shading_context, shading_point, values->m_closure_data);
        }

        APPLESEED_FORCE_INLINE virtual void sample(
            SamplingContext&        sampling_context,
            const void*             data,
            const bool              adjoint,
            const bool              cosine_mult,
            BSDFSample&             sample) const APPLESEED_OVERRIDE
        {
            const InputValues* values = static_cast<const InputValues*>(data);

            const MDF<float>& mdf = pick_mdf(values->m_distribution);

            const Basis3f& shading_basis(sample.m_shading_basis);
            Vector3f wo = shading_basis.transform_to_local(sample.m_outgoing.get_value());

            // Compute the microfacet normal by sampling the MDF.
            sampling_context.split_in_place(4, 1);
            const Vector4f s = sampling_context.next2<Vector4f>();
            Vector3f m = mdf.sample(
                wo,
                Vector3f(s[0], s[1], s[2]),
                values->m_precomputed.m_alpha_x,
                values->m_precomputed.m_alpha_y);
            assert(m.y > 0.0f);

            const float F = fresnel_term(*values, wo, m, sample.m_value);
            const float layer_probability = F * values->m_precomputed.m_reflection_weight;

            // Choose between layer and substrate.
            if (s[3] < layer_probability)
            {
                // Compute the reflected direction.
                const Vector3f wi = improve_normalization(reflect(wo, m));

                // If incoming and outgoing are on different sides
                // of the surface, this is not a reflection.
                if (wi.y * wo.y <= 0.0f)
                    return;

                evaluate_reflection(
                    *values,
                    mdf,
                    wi,
                    wo,
                    m,
                    sample.m_value);

                sample.m_probability =
                    layer_probability *
                    reflection_pdf(*values, mdf, wo, m);

                sample.m_mode = ScatteringMode::Glossy;
                sample.m_incoming = Dual3f(shading_basis.transform_to_parent(wi));
                sample.compute_reflected_differentials();
            }
            else
            {
                // Sample the substrate.
                values->m_osl_bsdf->sample(sampling_context, values->m_closure_data, adjoint, cosine_mult, sample);
                sample.m_probability *= 1.0f - layer_probability;
            }
        }

        APPLESEED_FORCE_INLINE virtual float evaluate(
            const void*             data,
            const bool              adjoint,
            const bool              cosine_mult,
            const Vector3f&         geometric_normal,
            const Basis3f&          shading_basis,
            const Vector3f&         outgoing,
            const Vector3f&         incoming,
            const int               modes,
            Spectrum&               value) const APPLESEED_OVERRIDE
        {
            const InputValues* values = static_cast<const InputValues*>(data);

            if (!ScatteringMode::has_glossy(modes))
            {
                return values->m_osl_bsdf->evaluate(
                    values->m_closure_data,
                    adjoint,
                    cosine_mult,
                    geometric_normal,
                    shading_basis,
                    outgoing,
                    incoming,
                    modes,
                    value);
            }

            const Vector3f wi = shading_basis.transform_to_local(incoming);
            const Vector3f wo = shading_basis.transform_to_local(outgoing);

            if (wi.y * wo.y < 0.0f)
            {
                return values->m_osl_bsdf->evaluate(
                    values->m_closure_data,
                    adjoint,
                    cosine_mult,
                    geometric_normal,
                    shading_basis,
                    outgoing,
                    incoming,
                    modes,
                    value);
            }

            const Vector3f m = half_reflection_vector(wi, wo);
            const float F = fresnel_term(*values, wo, m, value);
            const float layer_probability = F * values->m_precomputed.m_reflection_weight;

            const MDF<float>& mdf = pick_mdf(values->m_distribution);
            evaluate_reflection(
                *values,
                mdf,
                wi,
                wo,
                m,
                value);

            float probability =
                layer_probability *
                reflection_pdf(*values, mdf, wo, m);

            const float substrate_probability = 1.0f - layer_probability;
            Spectrum substrate_value;

            probability += values->m_osl_bsdf->evaluate(
                values->m_closure_data,
                adjoint,
                cosine_mult,
                geometric_normal,
                shading_basis,
                outgoing,
                incoming,
                modes,
                substrate_value) * substrate_probability;

            substrate_value *= substrate_probability;
            value += substrate_value;
            return probability;
        }

        APPLESEED_FORCE_INLINE virtual float evaluate_pdf(
            const void*             data,
            const Vector3f&         geometric_normal,
            const Basis3f&          shading_basis,
            const Vector3f&         outgoing,
            const Vector3f&         incoming,
            const int               modes) const APPLESEED_OVERRIDE
        {
            const InputValues* values = static_cast<const InputValues*>(data);

            if (!ScatteringMode::has_glossy(modes))
            {
                return values->m_osl_bsdf->evaluate_pdf(
                    values->m_closure_data,
                    geometric_normal,
                    shading_basis,
                    outgoing,
                    incoming,
                    modes);
            }

            const Vector3f wi = shading_basis.transform_to_local(incoming);
            const Vector3f wo = shading_basis.transform_to_local(outgoing);

            if (wi.y * wo.y < 0.0f)
            {
                return values->m_osl_bsdf->evaluate_pdf(
                    values->m_closure_data,
                    geometric_normal,
                    shading_basis,
                    outgoing,
                    incoming,
                    modes);
            }

            const Vector3f m = half_reflection_vector(wi, wo);
            Spectrum f_value;
            const float F = fresnel_term(*values, wo, m, f_value);
            const float layer_probability = F * values->m_precomputed.m_reflection_weight;

            const MDF<float>& mdf = pick_mdf(values->m_distribution);
            float probability =
                layer_probability *
                reflection_pdf(*values, mdf, wo, m);

            const float substrate_probability = 1.0f - layer_probability;
            probability += values->m_osl_bsdf->evaluate_pdf(
                values->m_closure_data,
                geometric_normal,
                shading_basis,
                outgoing,
                incoming,
                modes) * substrate_probability;

            return probability;
        }

        float sample_ior(
            SamplingContext&        sampling_context,
            const void*             data) const APPLESEED_OVERRIDE
        {
            const InputValues* values = static_cast<const InputValues*>(data);
            return values->m_osl_bsdf->sample_ior(
                sampling_context,
                values->m_closure_data);

        }

        void compute_absorption(
            const void*             data,
            const float             distance,
            Spectrum&               absorption) const APPLESEED_OVERRIDE
        {
            const InputValues* values = static_cast<const InputValues*>(data);
            values->m_osl_bsdf->compute_absorption(
                values->m_closure_data,
                distance,
                absorption);
        }

      private:
        typedef AlSurfaceLayerBRDFInputValues InputValues;

        static const MDF<float>& pick_mdf(const OIIO::ustring& distribution)
        {
            if (distribution == g_ggx_str)
                return m_ggx_mdf;
            else
                return m_beckmann_mdf;
        }

        static float fresnel_term(
            const InputValues&  values,
            Vector3f            wo,
            const Vector3f&     m,
            Spectrum&           value)
        {
            const Vector3f n(0.0f, 1.0f, 0.0f);

            if (wo.y < 0.0f)
                wo = -wo;

            if (values.m_fresnel_mode == g_dielectric_str)
            {
                const float ior = values.m_ior0[0];
                const Spectrum one(1.0f);
                const FresnelDielectricFun f(
                    one,
                    1.0f,
                    1.0f / ior);
                f(wo, m, n, value);
            }
            else
            {
                const FresnelFriendlyConductorFun f(values.m_ior0, values.m_ior1, 1.0f);
                f(wo, m, n, value);
            }

            const float weight = clamp(max_value(value), 0.0f, 1.0f);
            value *= values.m_reflectance;
            return weight;
        }

        static Vector3f half_reflection_vector(
            const Vector3f&         wi,
            const Vector3f&         wo)
        {
            // [1] eq. 13.
            const Vector3f h = normalize(wi + wo);
            return h.y < 0.0f ? -h : h;
        }

        static void evaluate_reflection(
            const InputValues&      values,
            const MDF<float>&       mdf,
            const Vector3f&         wi,
            const Vector3f&         wo,
            const Vector3f&         m,
            Spectrum&               value)
        {
            // [1] eq. 20.
            const float denom = abs(4.0f * wo.y * wi.y);
            if (denom == 0.0f)
            {
                value.set(0.0f);
                return;
            }

            const float D = mdf.D(m, values.m_precomputed.m_alpha_x, values.m_precomputed.m_alpha_y);
            const float G = mdf.G(wi, wo, m, values.m_precomputed.m_alpha_x, values.m_precomputed.m_alpha_y);
            value *= D * G / denom;
        }

        static float reflection_pdf(
            const InputValues&      values,
            const MDF<float>&       mdf,
            const Vector3f&         wo,
            const Vector3f&         m)
        {
            const float cos_wom = dot(wo, m);
            if (cos_wom == 0.0f)
                return 0.0f;

            const float jacobian = 1.0f / (4.0f * abs(cos_wom));
            return jacobian * mdf.pdf(wo, m, values.m_precomputed.m_alpha_x, values.m_precomputed.m_alpha_y);
        }

        static GGXMDF<float>        m_ggx_mdf;
        static BeckmannMDF<float>   m_beckmann_mdf;
    };

    GGXMDF<float>        AlSurfaceLayerBRDFImpl::m_ggx_mdf;
    BeckmannMDF<float>   AlSurfaceLayerBRDFImpl::m_beckmann_mdf;

    typedef BSDFWrapper<AlSurfaceLayerBRDFImpl> AlSurfaceLayerBRDF;
}


//
// AlSurfaceLayerBRDFFactory class implementation.
//

const char* AlSurfaceLayerBRDFFactory::get_model() const
{
    return Model;
}

Dictionary AlSurfaceLayerBRDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "AlSurface Layer BRDF");
}

DictionaryArray AlSurfaceLayerBRDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;
    return metadata;
}

auto_release_ptr<BSDF> AlSurfaceLayerBRDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new AlSurfaceLayerBRDF(name, params));
}

auto_release_ptr<BSDF> AlSurfaceLayerBRDFFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<BSDF>(new AlSurfaceLayerBRDF(name, params));
}

}   // namespace renderer
