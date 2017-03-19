
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
#include "oslbssrdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/shading/closures.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bssrdf/betterdipolebssrdf.h"
#include "renderer/modeling/bssrdf/bssrdf.h"
#include "renderer/modeling/bssrdf/bssrdfsample.h"
#include "renderer/modeling/bssrdf/directionaldipolebssrdf.h"
#include "renderer/modeling/bssrdf/gaussianbssrdf.h"
#ifdef APPLESEED_WITH_NORMALIZED_DIFFUSION_BSSRDF
#include "renderer/modeling/bssrdf/normalizeddiffusionbssrdf.h"
#endif
#include "renderer/modeling/bssrdf/standarddipolebssrdf.h"

// appleseed.foundation headers.
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/arena.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <cassert>

using namespace foundation;

namespace renderer
{

namespace
{

    //
    // OSL BSSRDF.
    //

    class OSLBSSRDF
      : public BSSRDF
    {
      public:
        OSLBSSRDF(
            const char*             name,
            const ParamArray&       params)
          : BSSRDF(name, params)
        {
            memset(m_all_bssrdfs, 0, sizeof(BSSRDF*) * NumClosuresIDs);

            m_better_dipole =
                create_and_register_bssrdf<BetterDipoleBSSRDFFactory>(
                    SubsurfaceBetterDipoleID,
                    "better_dipole");

            m_dir_dipole =
                create_and_register_bssrdf<DirectionalDipoleBSSRDFFactory>(
                    SubsurfaceDirectionalDipoleID,
                    "directional_dipole");

            m_gaussian =
                create_and_register_bssrdf<GaussianBSSRDFFactory>(
                    SubsurfaceGaussianID,
                    "gaussian");

#ifdef APPLESEED_WITH_NORMALIZED_DIFFUSION_BSSRDF
            m_normalized =
                create_and_register_bssrdf<NormalizedDiffusionBSSRDFFactory>(
                    SubsurfaceNormalizedDiffusionID,
                    "normalized_diffusion");
#endif

            m_std_dipole =
                create_and_register_bssrdf<StandardDipoleBSSRDFFactory>(
                    SubsurfaceStandardDipoleID,
                    "standard_dipole");
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return "osl_bssrdf";
        }

        virtual bool on_frame_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnFrameBeginRecorder&   recorder,
            IAbortSwitch*           abort_switch = 0) APPLESEED_OVERRIDE
        {
            if (!BSSRDF::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            for (int i = 0; i < NumClosuresIDs; ++i)
            {
                if (BSSRDF* bsrsdf = m_all_bssrdfs[i])
                {
                    if (!bsrsdf->on_frame_begin(project, parent, recorder, abort_switch))
                        return false;
                }
            }

            return true;
        }

        virtual void* evaluate_inputs(
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point) const APPLESEED_OVERRIDE
        {
            CompositeSubsurfaceClosure* c =
                shading_context.get_arena().allocate_noinit<CompositeSubsurfaceClosure>();

            new (c) CompositeSubsurfaceClosure(
                Basis3f(shading_point.get_shading_basis()),
                shading_point.get_osl_shader_globals().Ci,
                shading_context.get_arena());

            for (size_t i = 0, e = c->get_closure_count(); i < e; ++i)
            {
                bssrdf_from_closure_id(c->get_closure_type(i))
                    .prepare_inputs(
                        shading_context.get_arena(),
                        shading_point,
                        c->get_closure_input_values(i));
            }

            return c;
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
            const CompositeSubsurfaceClosure* c =
                static_cast<const CompositeSubsurfaceClosure*>(data);

            if (c->get_closure_count() > 0)
            {
                const size_t closure_index = c->choose_closure(sampling_context);
                const Basis3f& modified_basis = c->get_closure_shading_basis(closure_index);

                outgoing_point.set_shading_basis(Basis3d(modified_basis));

                const BSSRDF& bssrdf = bssrdf_from_closure_id(c->get_closure_type(closure_index));
                const bool result =
                        bssrdf.sample(
                            shading_context,
                            sampling_context,
                            c->get_closure_input_values(closure_index),
                            outgoing_point,
                            outgoing_dir,
                            bssrdf_sample,
                            bsdf_sample);

                if (result)
                    bssrdf_sample.m_value *= c->get_closure_weight(closure_index);;

                return result;
            }

            return false;
        }

        virtual void evaluate(
            const void*             data,
            const ShadingPoint&     outgoing_point,
            const Vector3f&         outgoing_dir,
            const ShadingPoint&     incoming_point,
            const Vector3f&         incoming_dir,
            Spectrum&               value) const APPLESEED_OVERRIDE
        {
            const CompositeSubsurfaceClosure* c =
                static_cast<const CompositeSubsurfaceClosure*>(data);

            value.set(0.0f);

            for (size_t i = 0, e = c->get_closure_count(); i < e; ++i)
            {
                Spectrum s;
                bssrdf_from_closure_id(c->get_closure_type(i))
                    .evaluate(
                        c->get_closure_input_values(i),
                        outgoing_point,
                        outgoing_dir,
                        incoming_point,
                        incoming_dir,
                        s);

                madd(value, s, c->get_closure_weight(i));
            }
        }

      private:
        template <typename BSSRDFFactory>
        auto_release_ptr<BSSRDF> create_and_register_bssrdf(
            const ClosureID         cid,
            const char*             name)
        {
            auto_release_ptr<BSSRDF> bssrdf = BSSRDFFactory().create(name, ParamArray());
            m_all_bssrdfs[cid] = bssrdf.get();
            return bssrdf;
        }

        const BSSRDF& bssrdf_from_closure_id(const ClosureID cid) const
        {
            const BSSRDF* bssrdf = m_all_bssrdfs[cid];
            assert(bssrdf);
            return *bssrdf;
        }

        auto_release_ptr<BSSRDF>    m_better_dipole;
        auto_release_ptr<BSSRDF>    m_dir_dipole;
        auto_release_ptr<BSSRDF>    m_gaussian;
#ifdef APPLESEED_WITH_NORMALIZED_DIFFUSION_BSSRDF
        auto_release_ptr<BSSRDF>    m_normalized;
#endif
        auto_release_ptr<BSSRDF>    m_std_dipole;

        BSSRDF*                     m_all_bssrdfs[NumClosuresIDs];
    };
}


//
// OSLBSSRDFFactory class implementation.
//

auto_release_ptr<BSSRDF> OSLBSSRDFFactory::create() const
{
    return auto_release_ptr<BSSRDF>(new OSLBSSRDF("osl_bssrdf", ParamArray()));
}

}   // namespace renderer
