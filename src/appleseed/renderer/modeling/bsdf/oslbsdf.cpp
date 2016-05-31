
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2016 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/glassbsdf.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdffactoryregistrar.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"
#include "renderer/modeling/bsdf/ibsdffactory.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/types.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

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
            const char*             name,
            const ParamArray&       params)
          : BSDF(name, AllBSDFTypes, ScatteringMode::All, params)
        {
            memset(m_all_bsdfs, 0, sizeof(BSDF*) * NumClosuresIDs);

            m_ashikhmin_shirley_brdf =
                AshikhminBRDFFactory().create(
                    "ashikhmin_brdf",
                    ParamArray());

            m_all_bsdfs[AshikhminShirleyID] = m_ashikhmin_shirley_brdf.get();

            m_diffuse_btdf =
                create_and_register_bsdf(
                    TranslucentID,
                    "diffuse_btdf",
                    "osl_translucent");

            m_disney_brdf =
                create_and_register_bsdf(
                    DisneyID,
                    "disney_brdf",
                    "osl_disney_brdf");

            m_glossy_beckmann_brdf =
                create_and_register_glossy_brdf(
                    GlossyBeckmannID,
                    "beckmann",
                    "osl_glossy_beckmann");

            m_glass_ggx_bsdf =
                create_and_register_glass_bsdf(
                    GlassGGXID,
                    "ggx",
                    "osl_glass_ggx");

            m_glass_beckmann_bsdf =
                create_and_register_glass_bsdf(
                    GlassBeckmannID,
                    "beckmann",
                    "osl_glass_beckmann");

            m_glossy_ggx_brdf =
                create_and_register_glossy_brdf(
                    GlossyGGXID,
                    "ggx",
                    "osl_glossy_ggx");

            m_metal_beckmann_brdf =
                create_and_register_metal_brdf(
                    MetalBeckmannID,
                    "beckmann",
                    "osl_metal_beckmann");

            m_metal_ggx_brdf =
                create_and_register_metal_brdf(
                    MetalGGXID,
                    "ggx",
                    "osl_metal_ggx");

            m_orennayar_brdf =
                create_and_register_bsdf(
                    OrenNayarID,
                    "orennayar_brdf",
                    "osl_orennayar");

            m_sheen_brdf =
                create_and_register_bsdf(
                    SheenID,
                    "sheen_brdf",
                    "osl_sheen");
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return "osl_bsdf";
        }

        virtual bool on_frame_begin(
            const Project&          project,
            const Assembly&         assembly,
            IAbortSwitch*           abort_switch) APPLESEED_OVERRIDE
        {
            if (!BSDF::on_frame_begin(project, assembly))
                return false;

            for (int i = 0; i < NumClosuresIDs; ++i)
            {
                if (BSDF* bsdf = m_all_bsdfs[i])
                {
                    if (!bsdf->on_frame_begin(project, assembly))
                        return false;
                }
            }

            return true;
        }

        virtual void on_frame_end(
            const Project&          project,
            const Assembly&         assembly) APPLESEED_OVERRIDE
        {
            for (int i = 0; i < NumClosuresIDs; ++i)
            {
                if (BSDF* bsdf = m_all_bsdfs[i])
                    bsdf->on_frame_end(project, assembly);
            }

            BSDF::on_frame_end(project, assembly);
        }

        virtual size_t compute_input_data_size(
            const Assembly&         assembly) const APPLESEED_OVERRIDE
        {
            return sizeof(CompositeSurfaceClosure);
        }

        virtual void evaluate_inputs(
            const ShadingContext&   shading_context,
            InputEvaluator&         input_evaluator,
            const ShadingPoint&     shading_point,
            const size_t            offset) const APPLESEED_OVERRIDE
        {
            CompositeSurfaceClosure* c = reinterpret_cast<CompositeSurfaceClosure*>(input_evaluator.data());
            new (c) CompositeSurfaceClosure(
                shading_point.get_shading_basis(),
                shading_point.get_osl_shader_globals().Ci);

            for (size_t i = 0, e = c->get_num_closures(); i < e; ++i)
            {
                bsdf_from_closure_id(c->get_closure_type(i)).prepare_inputs(
                    shading_point,
                    c->get_closure_input_values(i));
            }
        }

        APPLESEED_FORCE_INLINE virtual void sample(
            SamplingContext&        sampling_context,
            const void*             data,
            const bool              adjoint,
            const bool              cosine_mult,
            BSDFSample&             sample) const APPLESEED_OVERRIDE
        {
            const CompositeSurfaceClosure* c = static_cast<const CompositeSurfaceClosure*>(data);

            if (c->get_num_closures() > 0)
            {
                sampling_context.split_in_place(1, 1);
                const double s = sampling_context.next_double2();

                const size_t closure_index = c->choose_closure(s);
                sample.set_shading_basis(c->get_closure_shading_basis(closure_index));
                bsdf_from_closure_id(
                    c->get_closure_type(closure_index)).sample(
                        sampling_context,
                        c->get_closure_input_values(closure_index),
                        adjoint,
                        false,
                        sample);

                sample.m_value *= c->get_closure_weight(closure_index);
            }
        }

        APPLESEED_FORCE_INLINE virtual double evaluate(
            const void*             data,
            const bool              adjoint,
            const bool              cosine_mult,
            const Vector3d&         geometric_normal,
            const Basis3d&          shading_basis,
            const Vector3d&         outgoing,
            const Vector3d&         incoming,
            const int               modes,
            Spectrum&               value) const APPLESEED_OVERRIDE
        {
            double prob = 0.0;
            value.set(0.0f);

            const CompositeSurfaceClosure* c = static_cast<const CompositeSurfaceClosure*>(data);

            for (size_t i = 0, e = c->get_num_closures(); i < e; ++i)
            {
                Spectrum s;
                const double bsdf_prob =
                    bsdf_from_closure_id(c->get_closure_type(i)).evaluate(
                        c->get_closure_input_values(i),
                        adjoint,
                        false,
                        geometric_normal,
                        c->get_closure_shading_basis(i),
                        outgoing,
                        incoming,
                        modes,
                        s);

                if (bsdf_prob > 0.0)
                {
                    s *= c->get_closure_weight(i);
                    value += s;
                    prob += bsdf_prob * c->get_closure_pdf_weight(i);
                }
            }

            return prob;
        }

        APPLESEED_FORCE_INLINE virtual double evaluate_pdf(
            const void*             data,
            const Vector3d&         geometric_normal,
            const Basis3d&          shading_basis,
            const Vector3d&         outgoing,
            const Vector3d&         incoming,
            const int               modes) const APPLESEED_OVERRIDE
        {
            const CompositeSurfaceClosure* c = static_cast<const CompositeSurfaceClosure*>(data);
            double prob = 0.0;

            for (size_t i = 0, e = c->get_num_closures(); i < e; ++i)
            {
                const double bsdf_prob =
                    bsdf_from_closure_id(c->get_closure_type(i)).evaluate_pdf(
                        c->get_closure_input_values(i),
                        geometric_normal,
                        c->get_closure_shading_basis(i),
                        outgoing,
                        incoming,
                        modes);

                if (bsdf_prob > 0.0)
                    prob += bsdf_prob * c->get_closure_pdf_weight(i);
            }

            return prob;
        }

        virtual double sample_ior(
            SamplingContext&            sampling_context,
            const void*                 data) const APPLESEED_OVERRIDE
        {
            const CompositeSurfaceClosure* c = static_cast<const CompositeSurfaceClosure*>(data);
            sampling_context.split_in_place(1, 1);
            return c->choose_ior(sampling_context.next_double2());
        }

        virtual void compute_absorption(
            const void*                 data,
            const double                distance,
            Spectrum&                   absorption) const APPLESEED_OVERRIDE
        {
            const CompositeSurfaceClosure* c = static_cast<const CompositeSurfaceClosure*>(data);
            absorption.set(0.0f);
            for (size_t i = 0, e = c->get_num_closures(); i < e; ++i)
            {
                Spectrum a;
                bsdf_from_closure_id(c->get_closure_type(i)).compute_absorption(
                    c->get_closure_input_values(i),
                    distance,
                    a);
                const float w = static_cast<float>(c->get_closure_pdf_weight(i));

                // absorption += lerp(1.0, a, w)
                a *= w;
                absorption += a;
                a.set(1.0f - w);
                absorption += a;
            }
        }

      private:
        auto_release_ptr<BSDF>      m_ashikhmin_shirley_brdf;
        auto_release_ptr<BSDF>      m_diffuse_btdf;
        auto_release_ptr<BSDF>      m_disney_brdf;
        auto_release_ptr<BSDF>      m_glass_beckmann_bsdf;
        auto_release_ptr<BSDF>      m_glass_ggx_bsdf;
        auto_release_ptr<BSDF>      m_glossy_beckmann_brdf;
        auto_release_ptr<BSDF>      m_glossy_ggx_brdf;
        auto_release_ptr<BSDF>      m_metal_beckmann_brdf;
        auto_release_ptr<BSDF>      m_metal_ggx_brdf;
        auto_release_ptr<BSDF>      m_orennayar_brdf;
        auto_release_ptr<BSDF>      m_sheen_brdf;
        BSDF*                       m_all_bsdfs[NumClosuresIDs];

        auto_release_ptr<BSDF> create_and_register_bsdf(
            const ClosureID         cid,
            const char*             model,
            const char*             name,
            const ParamArray&       params = ParamArray())
        {
            auto_release_ptr<BSDF> bsdf =
                BSDFFactoryRegistrar().lookup(model)->create(name, params);

            m_all_bsdfs[cid] = bsdf.get();

            return bsdf;
        }

        auto_release_ptr<BSDF> create_and_register_glass_bsdf(
            const ClosureID         cid,
            const char*             mdf_name,
            const char*             name)
        {
            auto_release_ptr<BSDF> bsdf =
                GlassBSDFFactory().create_osl(
                    name,
                    ParamArray().insert("mdf", mdf_name));

            m_all_bsdfs[cid] = bsdf.get();
            return bsdf;
        }

        auto_release_ptr<BSDF> create_and_register_glossy_brdf(
            const ClosureID         cid,
            const char*             mdf_name,
            const char*             name)
        {
            auto_release_ptr<BSDF> bsdf =
                GlossyBRDFFactory().create(
                    name,
                    ParamArray().insert("mdf", mdf_name));

            m_all_bsdfs[cid] = bsdf.get();
            return bsdf;
        }

        auto_release_ptr<BSDF> create_and_register_metal_brdf(
            const ClosureID         cid,
            const char*             mdf_name,
            const char*             name)
        {
            auto_release_ptr<BSDF> bsdf =
                MetalBRDFFactory().create(
                    name,
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

    typedef BSDFWrapper<OSLBSDFImpl> OSLBSDF;
}


//
// OSLBSDFFactory class implementation.
//

auto_release_ptr<BSDF> OSLBSDFFactory::create() const
{
    return auto_release_ptr<BSDF>(new OSLBSDF("osl_bsdf", ParamArray()));
}

}   // namespace renderer
