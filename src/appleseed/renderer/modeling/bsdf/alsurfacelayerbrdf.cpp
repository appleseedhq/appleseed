
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
#include "foundation/utility/arena.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/makevector.h"
#include "foundation/utility/otherwise.h"

// Standard headers.
#include <algorithm>
#include <cmath>
#include <cstddef>
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
    // AlSurfaceLayer BRDF.
    //
    // References:
    //
    //  https://bitbucket.org/anderslanglands/alshaders/wiki/Home
    //  http://blog.selfshadow.com/publications/s2014-shading-course/langlands/s2014_pbs_alshaders_notes.pdf
    //

    const char* Model = "al_surface_layer_brdf";

    class AlSurfaceLayerBRDFImpl
      : public BSDF
    {
      public:
        AlSurfaceLayerBRDFImpl(
            const char*             name,
            const ParamArray&       params)
          : BSDF(name, AllBSDFTypes, ScatteringMode::All, params)
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

        virtual size_t compute_input_data_size() const APPLESEED_OVERRIDE
        {
            return sizeof(InputValues);
        }

        virtual void prepare_inputs(
            Arena&                  arena,
            const ShadingPoint&     shading_point,
            void*                   data) const APPLESEED_OVERRIDE
        {
            InputValues* values = static_cast<InputValues*>(data);

            new (&values->m_precomputed) InputValues::Precomputed();

            const float alpha = square(values->m_roughness);
            values->m_precomputed.m_alpha_x = alpha;
            values->m_precomputed.m_alpha_y = alpha;

            if (values->m_anisotropy != 0.5f)
            {
                const float t = square(2.0f * values->m_anisotropy - 1.0f);

                if (values->m_anisotropy < 0.5f)
                    values->m_precomputed.m_alpha_x = lerp(alpha, 1.0f, t);
                else
                    values->m_precomputed.m_alpha_y = lerp(alpha, 1.0f, t);
            }

            values->m_precomputed.m_alpha_x = clamp(values->m_precomputed.m_alpha_x, 0.001f, 0.999f);
            values->m_precomputed.m_alpha_y = clamp(values->m_precomputed.m_alpha_y, 0.001f, 0.999f);

            // Allocate memory and initialize the nested closure tree.
            values->m_substrate_closure_data = arena.allocate(sizeof(CompositeSurfaceClosure));
            CompositeSurfaceClosure* c = static_cast<CompositeSurfaceClosure*>(values->m_substrate_closure_data);
            new (c) CompositeSurfaceClosure(
                Basis3f(shading_point.get_shading_basis()),
                static_cast<OSL::ClosureColor*>(values->m_substrate),
                arena);

            // Inject values into any children layered closures.
            assert(values->m_osl_bsdf);
            for (size_t i = 0, e = c->get_closure_count(); i < e; ++i)
            {
                const ClosureID cid = c->get_closure_type(i);

                if (cid >= FirstLayeredClosure)
                    inject_layered_closure_values(cid, values->m_osl_bsdf, c->get_closure_input_values(i));
            }

            // Prepare the inputs of children BSDFs.
            values->m_osl_bsdf->prepare_inputs(
                arena,
                shading_point,
                values->m_substrate_closure_data);
        }

        virtual void sample(
            SamplingContext&        sampling_context,
            const void*             data,
            const bool              adjoint,
            const bool              cosine_mult,
            BSDFSample&             sample) const APPLESEED_OVERRIDE
        {
            const InputValues* values = static_cast<const InputValues*>(data);

            const Basis3f& shading_basis(sample.m_shading_basis);
            Vector3f wo = shading_basis.transform_to_local(sample.m_outgoing.get_value());

            // Compute the microfacet normal by sampling the MDF.
            const MDF& mdf = pick_mdf(values->m_distribution);
            sampling_context.split_in_place(4, 1);
            const Vector4f s = sampling_context.next2<Vector4f>();
            Vector3f m = mdf.sample(
                wo,
                Vector3f(s[0], s[1], s[2]),
                values->m_precomputed.m_alpha_x,
                values->m_precomputed.m_alpha_y,
                0.0f);
            assert(m.y > 0.0f);

            float layer_weight;
            fresnel_term(*values, wo, m, sample.m_value, layer_weight);

            // Choose between layer and substrate.
            if (s[3] < layer_weight)
            {
                // Compute the reflected direction.
                const Vector3f wi = improve_normalization(reflect(wo, m));
                if (wi.y * wo.y <= 0.0f)
                    return;

                evaluate_reflection(
                    *values,
                    mdf,
                    wi,
                    wo,
                    m,
                    sample.m_value);

                sample.m_probability = reflection_pdf(*values, mdf, wo, m) * layer_weight;
                sample.m_mode = ScatteringMode::Glossy;
                sample.m_incoming = Dual3f(shading_basis.transform_to_parent(wi));
                sample.compute_reflected_differentials();
            }
            else
            {
                values->m_osl_bsdf->sample(
                    sampling_context,
                    values->m_substrate_closure_data,
                    adjoint,
                    false, // do not multiply by |cos(incoming, normal)|
                    sample);
                sample.m_value *= 1.0f - layer_weight;
                sample.m_probability *= 1.0f - layer_weight;
            }
        }

        virtual float evaluate(
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

            const Vector3f wi = shading_basis.transform_to_local(incoming);
            const Vector3f wo = shading_basis.transform_to_local(outgoing);

            const Vector3f m = half_reflection_vector(wi, wo);

            float probability = 0.0f;
            value.resize(values->m_reflectance.size());
            value.set(0.0f);

            float layer_weight;
            Spectrum f_value;
            fresnel_term(*values, wo, m, f_value, layer_weight);

            if (ScatteringMode::has_glossy(modes) && (wi.y * wo.y >= 0.0f))
            {
                const MDF& mdf = pick_mdf(values->m_distribution);
                value = f_value;
                evaluate_reflection(*values, mdf, wi, wo, m, value);
                probability = layer_weight * reflection_pdf(*values, mdf, wo, m);
            }

            const float substrate_weight = 1.0f - layer_weight;

            if (substrate_weight > 0.0f)
            {
                Spectrum substrate_value;
                probability += values->m_osl_bsdf->evaluate(
                    values->m_substrate_closure_data,
                    adjoint,
                    false, // do not multiply by |cos(incoming, normal)|
                    geometric_normal,
                    shading_basis,
                    outgoing,
                    incoming,
                    modes,
                    substrate_value) * substrate_weight;
                madd(value, substrate_value, substrate_weight);
            }

            return probability;
        }

        virtual float evaluate_pdf(
            const void*             data,
            const Vector3f&         geometric_normal,
            const Basis3f&          shading_basis,
            const Vector3f&         outgoing,
            const Vector3f&         incoming,
            const int               modes) const APPLESEED_OVERRIDE
        {
            const InputValues* values = static_cast<const InputValues*>(data);

            const Vector3f wi = shading_basis.transform_to_local(incoming);
            const Vector3f wo = shading_basis.transform_to_local(outgoing);
            const Vector3f m = half_reflection_vector(wi, wo);

            float layer_weight;
            Spectrum value;
            fresnel_term(*values, wo, m, value, layer_weight);

            float probability = 0.0f;

            if (ScatteringMode::has_glossy(modes) && (wi.y * wo.y >= 0.0f))
            {
                const MDF& mdf = pick_mdf(values->m_distribution);
                probability = layer_weight * reflection_pdf(*values, mdf, wo, m);
            }

            const float substrate_weight = 1.0f - layer_weight;

            if (substrate_weight > 0.0f)
            {
                probability += values->m_osl_bsdf->evaluate_pdf(
                    values->m_substrate_closure_data,
                    geometric_normal,
                    shading_basis,
                    outgoing,
                    incoming,
                    modes) * substrate_weight;
            }

            return probability;
        }

        float sample_ior(
            SamplingContext&        sampling_context,
            const void*             data) const APPLESEED_OVERRIDE
        {
            // Forward to substrate.
            const InputValues* values = static_cast<const InputValues*>(data);
            return values->m_osl_bsdf->sample_ior(
                sampling_context,
                values->m_substrate_closure_data);
        }

        void compute_absorption(
            const void*             data,
            const float             distance,
            Spectrum&               absorption) const APPLESEED_OVERRIDE
        {
            // Forward to substrate.
            const InputValues* values = static_cast<const InputValues*>(data);
            values->m_osl_bsdf->compute_absorption(
                values->m_substrate_closure_data,
                distance,
                absorption);
        }

      private:
        typedef AlSurfaceLayerBRDFInputValues InputValues;

        enum MicrofacetDistribution
        {
            Beckmann = 0,
            GGX
        };

        enum FresnelMode
        {
            Dielectric = 0,
            Metallic
        };

        static const MDF& pick_mdf(int distribution)
        {
            if (distribution == Beckmann)
                return m_beckmann_mdf;
            else return m_ggx_mdf;
        }

        static void fresnel_term(
            const InputValues&      values,
            Vector3f                wo,
            const Vector3f&         m,
            Spectrum&               value,
            float&                  weight)
        {
            if (wo.y < 0.0f)
                wo = -wo;

            if (values.m_fresnel_mode == Dielectric)
            {
                value.resize(values.m_reflectance.size());

                const float cos_wom = dot(wo, m);
                if (cos_wom < 0.0f)
                {
                    value.set(0.0f);
                    weight = 0.0f;
                    return;
                }

                float f;
                fresnel_reflectance_dielectric(
                    f,
                    1.0f / values.m_ior,
                    min(cos_wom, 1.0f));

                value = values.m_reflectance;
                value *= f;
            }
            else
            {
                const Vector3f n(0.0f, 1.0f, 0.0f);
                const FresnelFriendlyConductorFun f(
                    values.m_normal_reflectance,
                    values.m_edge_tint,
                    1.0f);
                f(wo, m, n, value);
                value *= values.m_reflectance;
            }

            weight = saturate(max_value(value));
        }

        static Vector3f half_reflection_vector(
            const Vector3f&         wi,
            const Vector3f&         wo)
        {
            const Vector3f h = normalize(wi + wo);
            return h.y < 0.0f ? -h : h;
        }

        static void evaluate_reflection(
            const InputValues&      values,
            const MDF&              mdf,
            const Vector3f&         wi,
            const Vector3f&         wo,
            const Vector3f&         m,
            Spectrum&               value)
        {
            const float denom = abs(4.0f * wo.y * wi.y);
            if (denom == 0.0f)
            {
                value.set(0.0f);
                return;
            }

            const float D = mdf.D(m, values.m_precomputed.m_alpha_x, values.m_precomputed.m_alpha_y, 0.0f);
            const float G = mdf.G(wi, wo, m, values.m_precomputed.m_alpha_x, values.m_precomputed.m_alpha_y, 0.0f);
            value *= D * G / denom;
        }

        static float reflection_pdf(
            const InputValues&      values,
            const MDF&              mdf,
            const Vector3f&         wo,
            const Vector3f&         m)
        {
            const float cos_wom = dot(wo, m);
            if (cos_wom == 0.0f)
                return 0.0f;

            const float jacobian = 1.0f / (4.0f * abs(cos_wom));
            return jacobian * mdf.pdf(wo, m, values.m_precomputed.m_alpha_x, values.m_precomputed.m_alpha_y, 0.0f);
        }

        static GGXMDF        m_ggx_mdf;
        static BeckmannMDF   m_beckmann_mdf;
    };

    GGXMDF        AlSurfaceLayerBRDFImpl::m_ggx_mdf;
    BeckmannMDF   AlSurfaceLayerBRDFImpl::m_beckmann_mdf;

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
