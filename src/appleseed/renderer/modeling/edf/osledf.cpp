
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2015 Esteban Tovagliari, The appleseedhq Organization
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
#include "osledf.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/closures.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/edf/diffuseedf.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"

using namespace foundation;

namespace renderer
{

//
// OSL EDF class.
//

namespace
{
    const char* Model = "osl_edf";

    class OSLEDF
      : public EDF
    {
      public:
        OSLEDF()
          : EDF("osl_edf", ParamArray())
        {
            m_diffuse_edf = DiffuseEDFFactory().create("osl_diff_edf", ParamArray());
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        virtual void evaluate_inputs(
            InputEvaluator&         input_evaluator,
            const ShadingPoint&     shading_point) const APPLESEED_OVERRIDE
        {
            CompositeEmissionClosure* c =
                reinterpret_cast<CompositeEmissionClosure*>(input_evaluator.data());
            new (c) CompositeEmissionClosure(shading_point.get_osl_shader_globals().Ci);
        }

        virtual void sample(
            SamplingContext&        sampling_context,
            const void*             data,
            const Vector3d&         geometric_normal,
            const Basis3d&          shading_basis,
            const Vector2d&         s,
            Vector3d&               outgoing,
            Spectrum&               value,
            double&                 probability) const APPLESEED_OVERRIDE
        {
            const CompositeEmissionClosure* c =
                reinterpret_cast<const CompositeEmissionClosure*>(data);

            m_diffuse_edf->sample(
                sampling_context,
                &c->edf_input_values(),
                geometric_normal,
                shading_basis,
                s,
                outgoing,
                value,
                probability);
        }

        virtual void evaluate(
            const void*             data,
            const Vector3d&         geometric_normal,
            const Basis3d&          shading_basis,
            const Vector3d&         outgoing,
            Spectrum&               value) const APPLESEED_OVERRIDE
        {
            const CompositeEmissionClosure* c =
                reinterpret_cast<const CompositeEmissionClosure*>(data);

            m_diffuse_edf->evaluate(
                &c->edf_input_values(),
                geometric_normal,
                shading_basis,
                outgoing,
                value);
        }

        virtual void evaluate(
            const void*             data,
            const Vector3d&         geometric_normal,
            const Basis3d&          shading_basis,
            const Vector3d&         outgoing,
            Spectrum&               value,
            double&                 probability) const APPLESEED_OVERRIDE
        {
            const CompositeEmissionClosure* c =
                reinterpret_cast<const CompositeEmissionClosure*>(data);

            m_diffuse_edf->evaluate(
                &c->edf_input_values(),
                geometric_normal,
                shading_basis,
                outgoing,
                value,
                probability);
        }

        virtual double evaluate_pdf(
            const void*             data,
            const Vector3d&         geometric_normal,
            const Basis3d&          shading_basis,
            const Vector3d&         outgoing) const APPLESEED_OVERRIDE
        {
            const CompositeEmissionClosure* c =
                reinterpret_cast<const CompositeEmissionClosure*>(data);

            return
                m_diffuse_edf->evaluate_pdf(
                    &c->edf_input_values(),
                    geometric_normal,
                    shading_basis,
                    outgoing);
        }

      private:
        auto_release_ptr<EDF> m_diffuse_edf;
    };
}


//
// OSLEDFFactory class implementation.
//

auto_release_ptr<EDF> OSLEDFFactory::create() const
{
    return auto_release_ptr<EDF>(new OSLEDF());
}

}   // namespace renderer
