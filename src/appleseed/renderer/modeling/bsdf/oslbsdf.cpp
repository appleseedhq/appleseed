
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdffactoryregistrar.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"
#include "renderer/modeling/bsdf/ibsdffactory.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
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
namespace foundation    { class AbortSwitch; }

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
          : BSDF(name, Reflective, AllScatteringModes, params)
        {
            memset(m_all_bsdfs, 0, sizeof(BSDF*) * NumClosuresIDs);

            m_ashikhmin_shirley_brdf =
                create_and_register_bsdf(
                    AshikhminShirleyID,
                    "ashikhmin_brdf",
                    "osl_ashikhmin_shirley");

            m_diffuse_btdf =
                create_and_register_bsdf(
                    TranslucentID,
                    "diffuse_btdf",
                    "osl_translucent");

            m_lambertian_brdf =
                create_and_register_bsdf(
                    LambertID,
                    "lambertian_brdf",
                    "osl_lambert");

            m_specular_brdf =
                create_and_register_bsdf(
                    ReflectionID,
                    "specular_brdf",
                    "osl_reflection");

            m_specular_btdf =
                create_and_register_bsdf(
                    RefractionID,
                    "specular_btdf",
                    "osl_refraction");

            m_microfacet_beckmann_brdf =
                create_and_register_bsdf(
                    MicrofacetBeckmannReflectionID,
                    "microfacet2_brdf",
                    "osl_beckmann_refl",
                    ParamArray().insert("mdf", "beckmann"));

            m_microfacet_ggx_brdf =
                create_and_register_bsdf(
                    MicrofacetGGXReflectionID,
                    "microfacet2_brdf",
                    "osl_ggx_refl",
                    ParamArray().insert("mdf", "ggx"));

            /*
            m_microfacet_beckmann_btdf =
                create_and_register_bsdf(
                    MicrofacetBeckmannRefractionID,
                    "microfacet2_btdf",
                    "osl_beckmann_refr",
                    ParamArray().insert("mdf", "beckmann"));

            m_microfacet_ggx_btdf =
                create_and_register_bsdf(
                    MicrofacetGGXRefractionID,
                    "microfacet2_btdf",
                    "osl_ggx_refr",
                    ParamArray().insert("mdf", "ggx"));
            */
            
            m_orennayar_brdf =
                create_and_register_bsdf(
                    OrenNayarID,
                    "orennayar_brdf",
                    "osl_orennayar");
        }

        virtual void release() OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const OVERRIDE
        {
            return "osl_bsdf";
        }

        virtual bool on_frame_begin(
            const Project&          project,
            const Assembly&         assembly,
            AbortSwitch*            abort_switch) OVERRIDE
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
            const Assembly&         assembly) OVERRIDE
        {
            BSDF::on_frame_end(project, assembly);

            for (int i = 0; i < NumClosuresIDs; ++i)
            {
                if (BSDF* bsdf = m_all_bsdfs[i])
                    bsdf->on_frame_end(project, assembly);
            }
        }

        virtual size_t compute_input_data_size(
            const Assembly&         assembly) const OVERRIDE
        {
            return sizeof(CompositeClosure);
        }

        virtual void evaluate_inputs(
            InputEvaluator&         input_evaluator,
            const ShadingPoint&     shading_point,
            const size_t            offset) const OVERRIDE
        {
            CompositeClosure* c = reinterpret_cast<CompositeClosure*>(input_evaluator.data());
            new (c) CompositeClosure(shading_point.get_osl_shader_globals().Ci);
        }

        FORCE_INLINE virtual Mode sample(
            SamplingContext&        sampling_context,
            const void*             data,
            const bool              adjoint,
            const bool              cosine_mult,
            const Vector3d&         geometric_normal,
            const Basis3d&          shading_basis,
            const Vector3d&         outgoing,
            Vector3d&               incoming,
            Spectrum&               value,
            double&                 probability) const
        {
            const CompositeClosure* c = reinterpret_cast<const CompositeClosure*>(data);

            if (c->get_num_closures() > 0)
            {
                sampling_context.split_in_place(1, 1);
                const double s = sampling_context.next_double2();

                const int closure_index = c->choose_closure(s);
                const Basis3d new_shading_basis = make_osl_basis(c, closure_index, shading_basis);

                const Mode result =
                    bsdf_to_closure_id(c->get_closure_type(closure_index)).sample(
                        sampling_context,
                        c->get_closure_input_values(closure_index),
                        adjoint,
                        false,
                        geometric_normal,
                        new_shading_basis,
                        outgoing,
                        incoming,
                        value,
                        probability);

                value *= c->get_closure_spectrum_multiplier(closure_index);
                return result;
            }
            else
            {
                value.set(0.0f);
                probability = 0.0;
                return Absorption;
            }
        }

        FORCE_INLINE virtual double evaluate(
            const void*             data,
            const bool              adjoint,
            const bool              cosine_mult,
            const Vector3d&         geometric_normal,
            const Basis3d&          shading_basis,
            const Vector3d&         outgoing,
            const Vector3d&         incoming,
            const int               modes,
            Spectrum&               value) const
        {
            double prob = 0.0;
            value.set(0.0f);

            const CompositeClosure* c = reinterpret_cast<const CompositeClosure*>(data);

            for (size_t i = 0, e = c->get_num_closures(); i < e; ++i)
            {
                Spectrum s;
                const Basis3d new_shading_basis = make_osl_basis(c, i, shading_basis);

                const double bsdf_prob =
                    bsdf_to_closure_id(c->get_closure_type(i)).evaluate(
                        c->get_closure_input_values(i),
                        adjoint,
                        false,
                        geometric_normal,
                        new_shading_basis,
                        outgoing,
                        incoming,
                        modes,
                        s);

                if (bsdf_prob > 0.0)
                {
                    s *= c->get_closure_spectrum_multiplier(i);
                    value += s;
                }

                prob += bsdf_prob * c->get_closure_cdf_weight(i);
            }

            return prob;
        }

        FORCE_INLINE virtual double evaluate_pdf(
            const void*             data,
            const Vector3d&         geometric_normal,
            const Basis3d&          shading_basis,
            const Vector3d&         outgoing,
            const Vector3d&         incoming,
            const int               modes) const
        {
            const CompositeClosure* c = reinterpret_cast<const CompositeClosure*>(data);
            double prob = 0.0;

            for (size_t i = 0, e = c->get_num_closures(); i < e; ++i)
            {
                const Basis3d new_shading_basis = make_osl_basis(c, i, shading_basis);

                const double bsdf_prob =
                    bsdf_to_closure_id(c->get_closure_type(i)).evaluate_pdf(
                        c->get_closure_input_values(i),
                        geometric_normal,
                        new_shading_basis,
                        outgoing,
                        incoming,
                        modes);

                prob += bsdf_prob * c->get_closure_cdf_weight(i);
            }

            return prob;
        }

      private:
        auto_release_ptr<BSDF>      m_ashikhmin_shirley_brdf;
        auto_release_ptr<BSDF>      m_diffuse_btdf;
        auto_release_ptr<BSDF>      m_lambertian_brdf;
        auto_release_ptr<BSDF>      m_microfacet_beckmann_brdf;
        auto_release_ptr<BSDF>      m_microfacet_ggx_brdf;
        auto_release_ptr<BSDF>      m_microfacet_beckmann_btdf;
        auto_release_ptr<BSDF>      m_microfacet_ggx_btdf;
        auto_release_ptr<BSDF>      m_orennayar_brdf;
        auto_release_ptr<BSDF>      m_specular_brdf;
        auto_release_ptr<BSDF>      m_specular_btdf;
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

        const BSDF& bsdf_to_closure_id(const ClosureID cid) const
        {
            const BSDF* bsdf = m_all_bsdfs[cid];
            assert(bsdf);
            return *bsdf;
        }

        BSDF& bsdf_to_closure_id(const ClosureID cid)
        {
            BSDF* bsdf = m_all_bsdfs[cid];
            assert(bsdf);
            return *bsdf;
        }

        Basis3d make_osl_basis(
            const CompositeClosure* c,
            const size_t            index,
            const Basis3d&          original_basis) const
        {
            return
                Basis3d(
                    c->get_closure_normal(index),
                    c->closure_has_tangent(index)
                        ? c->get_closure_tangent(index)
                        : original_basis.get_tangent_u());
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
