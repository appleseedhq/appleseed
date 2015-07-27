
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
#include "renderer/modeling/bssrdf/bssrdf.h"
#include "renderer/modeling/bssrdf/directionaldipolebssrdf.h"
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
            const char*                 name,
            const ParamArray&           params)
          : BSSRDF(name, params)
        {
            m_directional_bssrdf =
                DirectionalDipoleBSSRDFFactory().create(
                    "osl_dir_bssrdf",
                    ParamArray());
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
            const Project&              project,
            const Assembly&             assembly,
            foundation::IAbortSwitch*   abort_switch = 0) APPLESEED_OVERRIDE
        {
            if (!BSSRDF::on_frame_begin(project, assembly, abort_switch))
                return false;

            if (!m_directional_bssrdf->on_frame_begin(project, assembly, abort_switch))
                return false;

            return true;
        }

        virtual void on_frame_end(
            const Project&              project,
            const Assembly&             assembly) APPLESEED_OVERRIDE
        {
            m_directional_bssrdf->on_frame_end(project, assembly);
            BSSRDF::on_frame_end(project, assembly);
        }

        virtual size_t compute_input_data_size(
            const Assembly&             assembly) const
        {
            return sizeof(CompositeSubsurfaceClosure);
        }

        virtual void evaluate_inputs(
            const ShadingContext&       shading_context,
            InputEvaluator&             input_evaluator,
            const ShadingPoint&         shading_point,
            const size_t                offset = 0) const APPLESEED_OVERRIDE
        {
            CompositeSubsurfaceClosure* c = reinterpret_cast<CompositeSubsurfaceClosure*>(input_evaluator.data());
            new (c) CompositeSubsurfaceClosure(shading_point.get_osl_shader_globals().Ci);

            for (size_t i = 0, e = c->get_num_closures(); i < e; ++i)
            {
                if (c->get_closure_type(i) == SubsurfaceDirectionalID)
                {
                    m_directional_bssrdf->evaluate_inputs(
                        shading_context,
                        input_evaluator,
                        shading_point,
                        c->get_closure_input_offset(i));
                }
                else
                    assert(false);
            }
        }

        virtual bool sample(
            const void*                 data,
            BSSRDFSample&               sample) const APPLESEED_OVERRIDE
        {
            const CompositeSubsurfaceClosure* c =
                reinterpret_cast<const CompositeSubsurfaceClosure*>(data);

            if (c->get_num_closures() > 0)
            {
                // TODO: implement this...
            }

            return false;
        }

        virtual void evaluate(
            const void*                 data,
            const ShadingPoint&         outgoing_point,
            const Vector3d&             outgoing_dir,
            const ShadingPoint&         incoming_point,
            const Vector3d&             incoming_dir,
            Spectrum&                   value) const APPLESEED_OVERRIDE
        {
            const CompositeSubsurfaceClosure* c =
                reinterpret_cast<const CompositeSubsurfaceClosure*>(data);

            // TODO: implement this...
            value.set(0.0f);
        }

        virtual double evaluate_pdf(
            const void*                 data,
            const size_t                channel,
            const double                dist) const APPLESEED_OVERRIDE
        {
            const CompositeSubsurfaceClosure* c =
                reinterpret_cast<const CompositeSubsurfaceClosure*>(data);

            // TODO: implement this...
            return 0.0;
        }

      private:
        auto_release_ptr<BSSRDF> m_directional_bssrdf;
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
