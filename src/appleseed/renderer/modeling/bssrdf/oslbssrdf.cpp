
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
#include "oslbssrdf.h"

// appleseed.renderer headers.
#include "renderer/kernel/shading/closures.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bssrdf/betterdipolebssrdf.h"
#include "renderer/modeling/bssrdf/bssrdf.h"
#include "renderer/modeling/bssrdf/bssrdfsample.h"
#include "renderer/modeling/bssrdf/directionaldipolebssrdf.h"
#ifdef APPLESEED_WITH_NORMALIZED_DIFFUSION_BSSRDF
#include "renderer/modeling/bssrdf/normalizeddiffusionbssrdf.h"
#endif
#include "renderer/modeling/bssrdf/standarddipolebssrdf.h"
#include "renderer/modeling/input/inputevaluator.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

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
                    "dir_dipole");

#ifdef APPLESEED_WITH_NORMALIZED_DIFFUSION_BSSRDF
            m_normalized =
                create_and_register_bssrdf<NormalizedDiffusionBSSRDFFactory>(
                    SubsurfaceNormalizedDiffusionID,
                    "normalized");
#endif

            m_std_dipole =
                create_and_register_bssrdf<StandardDipoleBSSRDFFactory>(
                    SubsurfaceStandardDipoleID,
                    "std_dipole");
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
            const Assembly&         assembly,
            IAbortSwitch*           abort_switch = 0) APPLESEED_OVERRIDE
        {
            if (!BSSRDF::on_frame_begin(project, assembly, abort_switch))
                return false;

            for (int i = 0; i < NumClosuresIDs; ++i)
            {
                if (BSSRDF* bsrsdf = m_all_bssrdfs[i])
                {
                    if (!bsrsdf->on_frame_begin(project, assembly))
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
                if (BSSRDF* bsrsdf = m_all_bssrdfs[i])
                    bsrsdf->on_frame_end(project, assembly);
            }

            BSSRDF::on_frame_end(project, assembly);
        }

        virtual size_t compute_input_data_size(
            const Assembly&         assembly) const
        {
            return sizeof(CompositeSubsurfaceClosure);
        }

        virtual void evaluate_inputs(
            const ShadingContext&   shading_context,
            InputEvaluator&         input_evaluator,
            const ShadingPoint&     shading_point,
            const size_t            offset = 0) const APPLESEED_OVERRIDE
        {
            CompositeSubsurfaceClosure* c = reinterpret_cast<CompositeSubsurfaceClosure*>(input_evaluator.data());
            new (c) CompositeSubsurfaceClosure(
                shading_point.get_shading_basis(),
                shading_point.get_osl_shader_globals().Ci);

            for (size_t i = 0, e = c->get_num_closures(); i < e; ++i)
            {
                bssrdf_from_closure_id(c->get_closure_type(i)).prepare_inputs(
                    c->get_closure_input_values(i));
            }
        }

        virtual bool sample(
            SamplingContext&        sampling_context,
            const void*             data,
            BSSRDFSample&           sample) const APPLESEED_OVERRIDE
        {
            const CompositeSubsurfaceClosure* c =
                reinterpret_cast<const CompositeSubsurfaceClosure*>(data);

            if (c->get_num_closures() > 0)
            {
                sampling_context.split_in_place(1, 1);
                const double s = sampling_context.next_double2();
                const size_t closure_index = c->choose_closure(s);

                sample.m_shading_basis =
                    &c->get_closure_shading_basis(closure_index);

                return
                    bssrdf_from_closure_id(c->get_closure_type(closure_index)).sample(
                        sampling_context,
                        c->get_closure_input_values(closure_index),
                        sample);
            }

            return false;
        }

        virtual void evaluate(
            const void*             data,
            const ShadingPoint&     outgoing_point,
            const Vector3d&         outgoing_dir,
            const ShadingPoint&     incoming_point,
            const Vector3d&         incoming_dir,
            Spectrum&               value) const APPLESEED_OVERRIDE
        {
            const CompositeSubsurfaceClosure* c =
                reinterpret_cast<const CompositeSubsurfaceClosure*>(data);

            value.set(0.0f);

            for (size_t i = 0, e = c->get_num_closures(); i < e; ++i)
            {
                Spectrum s;
                bssrdf_from_closure_id(c->get_closure_type(i)).evaluate(
                    c->get_closure_input_values(i),
                    outgoing_point,
                    outgoing_dir,
                    incoming_point,
                    incoming_dir,
                    s);

                s *= c->get_closure_weight(i);
                value += s;
            }
        }

        virtual double evaluate_pdf(
            const void*             data,
            const size_t            channel,
            const double            dist) const APPLESEED_OVERRIDE
        {
            const CompositeSubsurfaceClosure* c =
                reinterpret_cast<const CompositeSubsurfaceClosure*>(data);

            double pdf = 0.0;

            for (size_t i = 0, e = c->get_num_closures(); i < e; ++i)
            {
                pdf +=
                    bssrdf_from_closure_id(c->get_closure_type(i)).evaluate_pdf(
                        c->get_closure_input_values(i),
                        channel,
                        dist) * c->get_closure_pdf_weight(i);
            }

            return pdf;
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

        BSSRDF& bssrdf_from_closure_id(const ClosureID cid)
        {
            BSSRDF* bssrdf = m_all_bssrdfs[cid];
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
