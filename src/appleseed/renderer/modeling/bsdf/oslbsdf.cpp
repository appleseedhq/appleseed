
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "oslbsdf.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/closures.h"
#include "renderer/kernel/shading/directshadingcomponents.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/blinnbrdf.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdffactoryregistrar.h"
#include "renderer/modeling/bsdf/bsdfsample.h"
#include "renderer/modeling/bsdf/glossylayerbsdf.h"
#include "renderer/modeling/bsdf/ibsdffactory.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/dual.h"
#include "foundation/math/vector.h"
#include "foundation/memory/arena.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstring>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // OSL closure tree -> appleseed BSDFs adapter.
    //

    class OSLBSDF
      : public BSDF
    {
      public:
        OSLBSDF(
            const char*                 name,
            const ParamArray&           params)
          : BSDF(name, AllBSDFTypes, ScatteringMode::All, params)
        {
            memset(m_all_bsdfs, 0, sizeof(BSDF*) * NumClosuresIDs);

            // Register Common BSDFs.
            m_ashikhmin_shirley_brdf = create_and_register_bsdf(AshikhminShirleyID, "ashikhmin_brdf");
            m_blinn_brdf = create_and_register_bsdf(BlinnID, "blinn_brdf");
            m_diffuse_btdf = create_and_register_bsdf(TranslucentID, "diffuse_btdf");
            m_disney_brdf = create_and_register_bsdf(DisneyID, "disney_brdf");
            m_glass_bsdf =
                create_and_register_bsdf(
                    GlassID,
                    "glass_bsdf",
                    ParamArray().insert("volume_parameterization", "transmittance"));
            m_glossy_brdf = create_and_register_bsdf(GlossyID, "glossy_brdf");
            m_hair_bsdf = create_and_register_bsdf(HairID, "hair_bsdf");
            m_metal_brdf = create_and_register_bsdf(MetalID, "metal_brdf");
            m_microfacet_blinn_brdf = create_and_register_bsdf(MicrofacetBlinnID, "microfacet_normal_mapping_blinn_brdf");
            m_microfacet_glossy_brdf = create_and_register_bsdf(MicrofacetGlossyID, "microfacet_normal_mapping_glossy_brdf");
            m_microfacet_metal_brdf = create_and_register_bsdf(MicrofacetMetalID, "microfacet_normal_mapping_metal_brdf");
            m_microfacet_plastic_brdf = create_and_register_bsdf(MicrofacetPlasticID, "microfacet_normal_mapping_plastic_brdf");
            m_microfacet_sheen_brdf = create_and_register_bsdf(MicrofacetSheenID, "microfacet_normal_mapping_sheen_brdf");
            m_orennayar_brdf = create_and_register_bsdf(OrenNayarID, "orennayar_brdf");
            m_plastic_brdf = create_and_register_bsdf(PlasticID, "plastic_brdf");
            m_sheen_brdf = create_and_register_bsdf(SheenID, "sheen_brdf");

            // Register OSL exclusive BSDFs.
            m_glossy_layer_bsdf = GlossyLayerBSDFFactory::create("glossy_layer_bsdf", ParamArray());
            m_all_bsdfs[GlossyLayerID] = m_glossy_layer_bsdf.get();
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return "osl_bsdf";
        }

        bool on_render_begin(
            const Project&              project,
            const BaseGroup*            parent,
            OnRenderBeginRecorder&      recorder,
            IAbortSwitch*               abort_switch) override
        {
            if (!BSDF::on_render_begin(project, parent, recorder, abort_switch))
                return false;

            for (int i = 0; i < NumClosuresIDs; ++i)
            {
                if (BSDF* bsdf = m_all_bsdfs[i])
                {
                    if (!bsdf->on_render_begin(project, parent, recorder, abort_switch))
                        return false;
                }
            }

            return true;
        }

        bool on_frame_begin(
            const Project&              project,
            const BaseGroup*            parent,
            OnFrameBeginRecorder&       recorder,
            IAbortSwitch*               abort_switch) override
        {
            if (!BSDF::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            for (int i = 0; i < NumClosuresIDs; ++i)
            {
                if (BSDF* bsdf = m_all_bsdfs[i])
                {
                    if (!bsdf->on_frame_begin(project, parent, recorder, abort_switch))
                        return false;
                }
            }

            return true;
        }

        void* evaluate_inputs(
            const ShadingContext&       shading_context,
            const ShadingPoint&         shading_point) const override
        {
            Arena& arena = shading_context.get_arena();

            CompositeSurfaceClosure* c =
                arena.allocate_noinit<CompositeSurfaceClosure>();

            new (c) CompositeSurfaceClosure(
                Basis3f(shading_point.get_shading_basis()),
                shading_point.get_osl_shader_globals().Ci,
                arena);

            for (size_t i = 0, e = c->get_closure_count(); i < e; ++i)
            {
                bsdf_from_closure_id(c->get_closure_type(i))
                    .prepare_inputs(
                        arena,
                        shading_point,
                        c->get_closure_input_values(i));
            }

            return c;
        }

        void sample(
            SamplingContext&            sampling_context,
            const void*                 data,
            const bool                  adjoint,
            const bool                  cosine_mult,
            const LocalGeometry&        local_geometry,
            const Dual3f&               outgoing,
            const int                   modes,
            BSDFSample&                 sample) const override
        {
            const CompositeSurfaceClosure* c = static_cast<const CompositeSurfaceClosure*>(data);

            float pdfs[CompositeSurfaceClosure::MaxClosureEntries];
            const int num_matching_closures = c->compute_pdfs(modes, pdfs);

            if (num_matching_closures == 0)
                return;

            sampling_context.split_in_place(1, 1);
            const size_t closure_index = c->choose_closure(
                sampling_context.next2<float>(),
                num_matching_closures,
                pdfs);

            const Basis3f& modified_basis = c->get_closure_shading_basis(closure_index);

            LocalGeometry closure_geometry = local_geometry;
            closure_geometry.m_shading_basis = modified_basis;
            closure_geometry.m_shading_point->set_shading_basis(Basis3d(modified_basis));

            bsdf_from_closure_id(c->get_closure_type(closure_index))
                .sample(
                    sampling_context,
                    c->get_closure_input_values(closure_index),
                    adjoint,
                    cosine_mult,
                    closure_geometry,
                    outgoing,
                    modes,
                    sample);

            apply_layers_attenuation(
                *c,
                closure_index,
                outgoing.get_value(),
                sample.m_incoming.get_value(),
                sample.m_value);

            sample.m_value *= c->get_closure_weight(closure_index);
            sample.m_aov_components.m_albedo *= c->get_closure_weight(closure_index);

            if (sample.get_mode() == ScatteringMode::None ||
                sample.get_mode() == ScatteringMode::Specular ||
                sample.get_probability() < 1.0e-6f)
                return;

            float probability = sample.get_probability() * pdfs[closure_index];
            pdfs[closure_index] = 0.0f;

            // Evaluate the closures we didn't sample.
            for (size_t i = 0, e = c->get_closure_count(); i < e; ++i)
            {
                if (pdfs[i] > 0.0f)
                {
                    DirectShadingComponents s;
                    const float pdf =
                        pdfs[i] *
                        bsdf_from_closure_id(c->get_closure_type(i))
                            .evaluate(
                                c->get_closure_input_values(i),
                                adjoint,
                                cosine_mult,
                                closure_geometry,
                                outgoing.get_value(),
                                sample.m_incoming.get_value(),
                                modes,
                                s);

                    if (pdf > 0.0f)
                    {
                        apply_layers_attenuation(*c, i, outgoing.get_value(), sample.m_incoming.get_value(), s);
                        madd(sample.m_value, s, c->get_closure_weight(i));
                        probability += pdf;
                    }
                }
            }

            if (probability > 1.0e-6f)
                sample.set_to_scattering(sample.get_mode(), probability);
            else sample.set_to_absorption();
        }

        float evaluate(
            const void*                 data,
            const bool                  adjoint,
            const bool                  cosine_mult,
            const LocalGeometry&        local_geometry,
            const Vector3f&             outgoing,
            const Vector3f&             incoming,
            const int                   modes,
            DirectShadingComponents&    value) const override
        {
            const CompositeSurfaceClosure* c = static_cast<const CompositeSurfaceClosure*>(data);

            LocalGeometry closure_geometry = local_geometry;

            float pdfs[CompositeSurfaceClosure::MaxClosureEntries];
            c->compute_pdfs(modes, pdfs);

            float pdf = 0.0f;

            for (size_t i = 0, e = c->get_closure_count(); i < e; ++i)
            {
                if (pdfs[i] > 0.0f)
                {
                    closure_geometry.m_shading_basis = c->get_closure_shading_basis(i);

                    DirectShadingComponents s;
                    const float closure_pdf =
                        pdfs[i] *
                        bsdf_from_closure_id(c->get_closure_type(i))
                            .evaluate(
                                c->get_closure_input_values(i),
                                adjoint,
                                cosine_mult,
                                closure_geometry,
                                outgoing,
                                incoming,
                                modes,
                                s);

                    if (closure_pdf > 0.0f)
                    {
                        apply_layers_attenuation(*c, i, outgoing, incoming, s);
                        madd(value, s, c->get_closure_weight(i));
                        pdf += closure_pdf;
                    }
                }
            }

            assert(pdf >= 0.0f);
            return pdf;
        }

        float evaluate_pdf(
            const void*                 data,
            const bool                  adjoint,
            const LocalGeometry&        local_geometry,
            const Vector3f&             outgoing,
            const Vector3f&             incoming,
            const int                   modes) const override
        {
            const CompositeSurfaceClosure* c = static_cast<const CompositeSurfaceClosure*>(data);

            LocalGeometry closure_geometry = local_geometry;

            float pdfs[CompositeSurfaceClosure::MaxClosureEntries];
            c->compute_pdfs(modes, pdfs);

            float pdf = 0.0f;

            for (size_t i = 0, e = c->get_closure_count(); i < e; ++i)
            {
                if (pdfs[i] > 0.0f)
                {
                    closure_geometry.m_shading_basis = c->get_closure_shading_basis(i);

                    const float closure_pdf =
                        pdfs[i] *
                        bsdf_from_closure_id(c->get_closure_type(i))
                            .evaluate_pdf(
                                c->get_closure_input_values(i),
                                adjoint,
                                closure_geometry,
                                outgoing,
                                incoming,
                                modes);

                    pdf += closure_pdf;
                }
            }

            assert(pdf >= 0.0f);
            return pdf;
        }

        float sample_ior(
            SamplingContext&            sampling_context,
            const void*                 data) const override
        {
            const CompositeSurfaceClosure* c = static_cast<const CompositeSurfaceClosure*>(data);
            sampling_context.split_in_place(1, 1);
            return c->choose_ior(sampling_context.next2<float>());
        }

        void compute_absorption(
            const void*                 data,
            const float                 distance,
            Spectrum&                   absorption) const override
        {
            const CompositeSurfaceClosure* c = static_cast<const CompositeSurfaceClosure*>(data);

            absorption.set(1.0f);

            for (size_t i = 0, e = c->get_closure_count(); i < e; ++i)
            {
                const ClosureID cid = c->get_closure_type(i);
                if (cid == GlassID)
                {
                    const float w = c->get_closure_scalar_weight(i);
                    Spectrum a;
                    bsdf_from_closure_id(cid)
                        .compute_absorption(
                            c->get_closure_input_values(i),
                            distance * w,
                            a);
                    absorption *= a;
                }
            }
        }

      private:
        BSDF*                       m_all_bsdfs[NumClosuresIDs];
        auto_release_ptr<BSDF>      m_ashikhmin_shirley_brdf;
        auto_release_ptr<BSDF>      m_blinn_brdf;
        auto_release_ptr<BSDF>      m_diffuse_btdf;
        auto_release_ptr<BSDF>      m_disney_brdf;
        auto_release_ptr<BSDF>      m_glass_bsdf;
        auto_release_ptr<BSDF>      m_glossy_brdf;
        auto_release_ptr<BSDF>      m_glossy_layer_bsdf;
        auto_release_ptr<BSDF>      m_hair_bsdf;
        auto_release_ptr<BSDF>      m_metal_brdf;
        auto_release_ptr<BSDF>      m_microfacet_blinn_brdf;
        auto_release_ptr<BSDF>      m_microfacet_glossy_brdf;
        auto_release_ptr<BSDF>      m_microfacet_metal_brdf;
        auto_release_ptr<BSDF>      m_microfacet_plastic_brdf;
        auto_release_ptr<BSDF>      m_microfacet_sheen_brdf;
        auto_release_ptr<BSDF>      m_orennayar_brdf;
        auto_release_ptr<BSDF>      m_plastic_brdf;
        auto_release_ptr<BSDF>      m_sheen_brdf;

        void apply_layers_attenuation(
            const CompositeClosure&     c,
            const size_t                closure_index,
            const Vector3f&             outgoing,
            const Vector3f&             incoming,
            DirectShadingComponents&    value) const
        {
            const std::int8_t* layers = c.get_closure_layers(closure_index);
            for (size_t i = 0; i < CompositeClosure::MaxClosureLayers; ++i)
            {
                if (layers[i] < 0)
                    break;

                const size_t layer = static_cast<size_t>(layers[i]);
                assert(c.get_closure_type(layer) >= FirstLayeredClosure);

                bsdf_from_closure_id(c.get_closure_type(layer)).attenuate_substrate(
                    c.get_closure_input_values(layer),
                    c.get_closure_shading_basis(layer),
                    outgoing,
                    incoming,
                    value);
            }
        }

        auto_release_ptr<BSDF> create_and_register_bsdf(
            const ClosureID         cid,
            const char*             model,
            const ParamArray&       params = ParamArray())
        {
            auto_release_ptr<BSDF> bsdf =
                BSDFFactoryRegistrar().lookup(model)->create(model, params);

            m_all_bsdfs[cid] = bsdf.get();

            return bsdf;
        }

        const BSDF& bsdf_from_closure_id(const ClosureID cid) const
        {
            const BSDF* bsdf = m_all_bsdfs[cid];
            assert(bsdf);
            return *bsdf;
        }

        BSDF& bsdf_from_closure_id(const ClosureID cid)
        {
            BSDF* bsdf = m_all_bsdfs[cid];
            assert(bsdf);
            return *bsdf;
        }
    };
}


//
// OSLBSDFFactory class implementation.
//

auto_release_ptr<BSDF> OSLBSDFFactory::create() const
{
    return auto_release_ptr<BSDF>(new OSLBSDF("osl_bsdf", ParamArray()));
}

}   // namespace renderer
