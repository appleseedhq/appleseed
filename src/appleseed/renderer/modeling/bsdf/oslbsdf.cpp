
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
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/closures.h"
#include "renderer/kernel/shading/directshadingcomponents.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/blinnbrdf.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdffactoryregistrar.h"
#include "renderer/modeling/bsdf/bsdfsample.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"
#include "renderer/modeling/bsdf/diffusebtdf.h"
#include "renderer/modeling/bsdf/glassbsdf.h"
#include "renderer/modeling/bsdf/glossybrdf.h"
#include "renderer/modeling/bsdf/metalbrdf.h"
#include "renderer/modeling/bsdf/plasticbrdf.h"
#include "renderer/modeling/bsdf/ibsdffactory.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/types.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/arena.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstring>
#include <string>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // OSL closure tree -> appleseed BSDFs adapter.
    //

    class OSLBSDFImpl
      : public BSDF
    {
      public:
        OSLBSDFImpl(
            const char*                 name,
            const ParamArray&           params)
          : BSDF(name, AllBSDFTypes, ScatteringMode::All, params)
        {
            memset(m_all_bsdfs, 0, sizeof(BSDF*) * NumClosuresIDs);

            m_ashikhmin_shirley_brdf = create_and_register_bsdf(AshikhminShirleyID, "ashikhmin_brdf");
            m_blinn_brdf = create_and_register_bsdf(BlinnID, "blinn_brdf");
            m_diffuse_btdf = create_and_register_bsdf(TranslucentID, "diffuse_btdf");
            m_disney_brdf = create_and_register_bsdf(DisneyID, "disney_brdf");

            m_glass_beckmann_bsdf = create_and_register_glass_bsdf(GlassBeckmannID, "beckmann");
            m_glass_ggx_bsdf = create_and_register_glass_bsdf(GlassGGXID, "ggx");
            m_glass_std_bsdf = create_and_register_glass_bsdf(GlassSTDID, "std");

            m_glossy_beckmann_brdf = create_and_register_glossy_brdf(GlossyBeckmannID, "beckmann");
            m_glossy_ggx_brdf = create_and_register_glossy_brdf(GlossyGGXID, "ggx");
            m_glossy_std_brdf = create_and_register_glossy_brdf(GlossySTDID, "std");

            m_metal_beckmann_brdf = create_and_register_metal_brdf(MetalBeckmannID, "beckmann");
            m_metal_ggx_brdf = create_and_register_metal_brdf(MetalGGXID, "ggx");
            m_metal_std_brdf = create_and_register_metal_brdf(MetalSTDID, "std");

            m_orennayar_brdf = create_and_register_bsdf(OrenNayarID, "orennayar_brdf");

            m_plastic_beckmann_brdf = create_and_register_plastic_brdf(PlasticBeckmannID, "beckmann");
            m_plastic_ggx_brdf = create_and_register_plastic_brdf(PlasticGGXID, "ggx");
            m_plastic_gtr1_brdf = create_and_register_plastic_brdf(PlasticGTR1ID, "gtr1");
            m_plastic_std_brdf = create_and_register_plastic_brdf(PlasticSTDID, "std");

            m_sheen_brdf = create_and_register_bsdf(SheenID, "sheen_brdf");
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return "osl_bsdf";
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

            sample.m_shading_basis = modified_basis;
            sample.m_shading_point->set_shading_basis(Basis3d(modified_basis));

            bsdf_from_closure_id(c->get_closure_type(closure_index))
                .sample(
                    sampling_context,
                    c->get_closure_input_values(closure_index),
                    adjoint,
                    false,
                    modes,
                    sample);
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
                                false,
                                sample.m_geometric_normal,
                                c->get_closure_shading_basis(closure_index),
                                sample.m_outgoing.get_value(),
                                sample.m_incoming.get_value(),
                                modes,
                                s);

                    if (pdf > 0.0f)
                    {
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
            const Vector3f&             geometric_normal,
            const Basis3f&              shading_basis,
            const Vector3f&             outgoing,
            const Vector3f&             incoming,
            const int                   modes,
            DirectShadingComponents&    value) const override
        {
            const CompositeSurfaceClosure* c = static_cast<const CompositeSurfaceClosure*>(data);

            float pdfs[CompositeSurfaceClosure::MaxClosureEntries];
            c->compute_pdfs(modes, pdfs);

            float pdf = 0.0f;

            for (size_t i = 0, e = c->get_closure_count(); i < e; ++i)
            {
                if (pdfs[i] > 0.0f)
                {
                    DirectShadingComponents s;
                    const float closure_pdf =
                        pdfs[i] *
                        bsdf_from_closure_id(c->get_closure_type(i))
                            .evaluate(
                                c->get_closure_input_values(i),
                                adjoint,
                                false,
                                geometric_normal,
                                c->get_closure_shading_basis(i),
                                outgoing,
                                incoming,
                                modes,
                                s);

                    if (closure_pdf > 0.0f)
                    {
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
            const Vector3f&             geometric_normal,
            const Basis3f&              shading_basis,
            const Vector3f&             outgoing,
            const Vector3f&             incoming,
            const int                   modes) const override
        {
            const CompositeSurfaceClosure* c = static_cast<const CompositeSurfaceClosure*>(data);

            float pdfs[CompositeSurfaceClosure::MaxClosureEntries];
            c->compute_pdfs(modes, pdfs);

            float pdf = 0.0f;

            for (size_t i = 0, e = c->get_closure_count(); i < e; ++i)
            {
                if (pdfs[i] > 0.0f)
                {
                    const float closure_pdf =
                        pdfs[i] *
                        bsdf_from_closure_id(c->get_closure_type(i))
                            .evaluate_pdf(
                                c->get_closure_input_values(i),
                                adjoint,
                                geometric_normal,
                                c->get_closure_shading_basis(i),
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
                if (cid > GlassID && cid <= LastGlassClosure)
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
        auto_release_ptr<BSDF>      m_glass_beckmann_bsdf;
        auto_release_ptr<BSDF>      m_glass_ggx_bsdf;
        auto_release_ptr<BSDF>      m_glass_std_bsdf;
        auto_release_ptr<BSDF>      m_glossy_beckmann_brdf;
        auto_release_ptr<BSDF>      m_glossy_ggx_brdf;
        auto_release_ptr<BSDF>      m_glossy_std_brdf;
        auto_release_ptr<BSDF>      m_metal_beckmann_brdf;
        auto_release_ptr<BSDF>      m_metal_ggx_brdf;
        auto_release_ptr<BSDF>      m_metal_std_brdf;
        auto_release_ptr<BSDF>      m_orennayar_brdf;
        auto_release_ptr<BSDF>      m_plastic_beckmann_brdf;
        auto_release_ptr<BSDF>      m_plastic_ggx_brdf;
        auto_release_ptr<BSDF>      m_plastic_gtr1_brdf;
        auto_release_ptr<BSDF>      m_plastic_std_brdf;
        auto_release_ptr<BSDF>      m_sheen_brdf;

        auto_release_ptr<BSDF> create_and_register_bsdf(
            const ClosureID         cid,
            const char*             model)
        {
            auto_release_ptr<BSDF> bsdf =
                BSDFFactoryRegistrar().lookup(model)->create(model, ParamArray());

            m_all_bsdfs[cid] = bsdf.get();

            return bsdf;
        }

        auto_release_ptr<BSDF> create_and_register_glass_bsdf(
            const ClosureID         cid,
            const char*             mdf_name)
        {
            auto_release_ptr<BSDF> bsdf =
                GlassBSDFFactory().create(
                    "glass_bsdf",
                    ParamArray()
                        .insert("mdf", mdf_name)
                        .insert("volume_parameterization", "transmittance"));

            m_all_bsdfs[cid] = bsdf.get();

            return bsdf;
        }

        auto_release_ptr<BSDF> create_and_register_glossy_brdf(
            const ClosureID         cid,
            const char*             mdf_name)
        {
            auto_release_ptr<BSDF> bsdf =
                GlossyBRDFFactory().create(
                    "glossy_brdf",
                    ParamArray().insert("mdf", mdf_name));

            m_all_bsdfs[cid] = bsdf.get();

            return bsdf;
        }

        auto_release_ptr<BSDF> create_and_register_metal_brdf(
            const ClosureID         cid,
            const char*             mdf_name)
        {
            auto_release_ptr<BSDF> bsdf =
                MetalBRDFFactory().create(
                    "metal_brdf",
                    ParamArray().insert("mdf", mdf_name));

            m_all_bsdfs[cid] = bsdf.get();

            return bsdf;
        }

        auto_release_ptr<BSDF> create_and_register_plastic_brdf(
            const ClosureID         cid,
            const char*             mdf_name)
        {
            auto_release_ptr<BSDF> bsdf =
                PlasticBRDFFactory().create(
                    "plastic_brdf",
                    ParamArray().insert("mdf", mdf_name));

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

    typedef BSDFWrapper<OSLBSDFImpl, false> OSLBSDF;
}


//
// OSLBSDFFactory class implementation.
//

auto_release_ptr<BSDF> OSLBSDFFactory::create() const
{
    return auto_release_ptr<BSDF>(new OSLBSDF("osl_bsdf", ParamArray()));
}

}   // namespace renderer
