
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/edf/diffuseedf.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/arena.h"

// Standard headers.
#include <limits>

using namespace foundation;
using namespace std;

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

        virtual void* evaluate_inputs(
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point) const APPLESEED_OVERRIDE
        {
            CompositeEmissionClosure* c =
                shading_context.get_arena().allocate_noinit<CompositeEmissionClosure>();

            new (c) CompositeEmissionClosure(
                shading_point.get_osl_shader_globals().Ci,
                shading_context.get_arena());

            return c;
        }

        virtual void sample(
            SamplingContext&        sampling_context,
            const void*             data,
            const Vector3f&         geometric_normal,
            const Basis3f&          shading_basis,
            const Vector2f&         s,
            Vector3f&               outgoing,
            Spectrum&               value,
            float&                  probability) const APPLESEED_OVERRIDE
        {
            const CompositeEmissionClosure* c =
                static_cast<const CompositeEmissionClosure*>(data);

            if (c->get_closure_count() > 0)
            {
                const size_t closure_index = c->choose_closure(sampling_context);
                const EDF& edf = edf_from_closure_id(c->get_closure_type(closure_index));
                edf.sample(
                    sampling_context,
                    c->get_closure_input_values(closure_index),
                    geometric_normal,
                    shading_basis,
                    s,
                    outgoing,
                    value,
                    probability);
            }
        }

        virtual void evaluate(
            const void*             data,
            const Vector3f&         geometric_normal,
            const Basis3f&          shading_basis,
            const Vector3f&         outgoing,
            Spectrum&               value) const APPLESEED_OVERRIDE
        {
            const CompositeEmissionClosure* c =
                static_cast<const CompositeEmissionClosure*>(data);

            value.set(0.0f);

            for (size_t i = 0, e = c->get_closure_count(); i < e; ++i)
            {
                Spectrum s(Spectrum::Illuminance);

                const EDF& edf = edf_from_closure_id(c->get_closure_type(i));
                edf.evaluate(
                    c->get_closure_input_values(i),
                    geometric_normal,
                    shading_basis,
                    outgoing,
                    s);

                value += s;
            }
        }

        virtual void evaluate(
            const void*             data,
            const Vector3f&         geometric_normal,
            const Basis3f&          shading_basis,
            const Vector3f&         outgoing,
            Spectrum&               value,
            float&                  probability) const APPLESEED_OVERRIDE
        {
            const CompositeEmissionClosure* c =
                static_cast<const CompositeEmissionClosure*>(data);

            value.set(0.0f);
            probability = 0.0f;

            for (size_t i = 0, e = c->get_closure_count(); i < e; ++i)
            {
                Spectrum s(Spectrum::Illuminance);
                float edf_prob = 0.0f;

                const EDF& edf = edf_from_closure_id(c->get_closure_type(i));
                edf.evaluate(
                    c->get_closure_input_values(i),
                    geometric_normal,
                    shading_basis,
                    outgoing,
                    s,
                    edf_prob);

                if (edf_prob > 0.0f)
                {
                    value += s;
                    probability += edf_prob * c->get_closure_pdf_weight(i);
                }
            }
        }

        virtual float evaluate_pdf(
            const void*             data,
            const Vector3f&         geometric_normal,
            const Basis3f&          shading_basis,
            const Vector3f&         outgoing) const APPLESEED_OVERRIDE
        {
            const CompositeEmissionClosure* c =
                static_cast<const CompositeEmissionClosure*>(data);

            float probability = 0.0f;

            for (size_t i = 0, e = c->get_closure_count(); i < e; ++i)
            {
                const EDF& edf = edf_from_closure_id(c->get_closure_type(i));
                const float edf_prob =
                    edf.evaluate_pdf(
                        c->get_closure_input_values(i),
                        geometric_normal,
                        shading_basis,
                        outgoing);

                if (edf_prob > 0.0f)
                    probability += edf_prob * c->get_closure_pdf_weight(i);
            }

            return probability;
        }

        virtual float get_uncached_max_contribution() const APPLESEED_OVERRIDE
        {
            // We can't know the max contribution of OSL EDFs.
            return numeric_limits<float>::max();
        }

      private:
        auto_release_ptr<EDF> m_diffuse_edf;

        const EDF& edf_from_closure_id(const ClosureID cid) const
        {
            assert(cid == EmissionID);
            return *m_diffuse_edf;
        }
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
