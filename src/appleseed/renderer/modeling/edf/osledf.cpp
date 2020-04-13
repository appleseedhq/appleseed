
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
#include "osledf.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/closures.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/edf/diffuseedf.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/glossylayerbsdf.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/input/scalarsource.h"
#include "renderer/modeling/input/source.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/vector.h"
#include "foundation/memory/arena.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cstdint>
#include <limits>

using namespace foundation;

namespace renderer
{

//
// OSL EDF class.
//

namespace
{
    const char* Model = "osl_edf";

    struct DummyNonUniformSource
      : public Source
    {
        DummyNonUniformSource()
          : Source(false)
        {
        }

        std::uint64_t compute_signature() const override
        {
            return 0;
        }

        Hints get_hints() const override
        {
            Hints hints;
            hints.m_width = 1;
            hints.m_height = 1;
            return hints;
        }
    };

    class OSLEDF
      : public EDF
    {
      public:
        OSLEDF()
          : EDF("osl_edf", ParamArray())
        {
            m_diffuse_edf = DiffuseEDFFactory().create("osl_diff_edf", ParamArray());
            m_diffuse_edf->get_inputs().find("radiance").bind(new DummyNonUniformSource());
            m_diffuse_edf->get_inputs().find("radiance_multiplier").bind(new DummyNonUniformSource());
            m_diffuse_edf->get_inputs().find("exposure").bind(new ScalarSource(0.0f));

           m_glossy_layer_bsdf = GlossyLayerBSDFFactory::create("glossy_layer_bsdf", ParamArray());
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return Model;
        }

        void* evaluate_inputs(
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point) const override
        {
            CompositeEmissionClosure* c =
                shading_context.get_arena().allocate_noinit<CompositeEmissionClosure>();

            new (c) CompositeEmissionClosure(
                Basis3f(shading_point.get_shading_basis()),
                shading_point.get_osl_shader_globals().Ci,
                shading_context.get_arena());

            for (size_t i = 0, e = c->get_closure_count(); i < e; ++i)
            {
                if (c->get_closure_type(i) >= FirstLayeredClosure)
                {
                    bsdf_from_closure_id(c->get_closure_type(i))
                        .prepare_inputs(
                            shading_context.get_arena(),
                            shading_point,
                            c->get_closure_input_values(i));
                }
            }

            return c;
        }

        bool on_render_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnRenderBeginRecorder&  recorder,
            IAbortSwitch*           abort_switch) override
        {
            if (!EDF::on_render_begin(project, parent, recorder, abort_switch))
                return false;

            if (!m_diffuse_edf->on_render_begin(project, parent, recorder, abort_switch))
                return false;

            if (!m_glossy_layer_bsdf->on_render_begin(project, parent, recorder, abort_switch))
                return false;

            return true;
        }

        bool on_frame_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnFrameBeginRecorder&   recorder,
            IAbortSwitch*           abort_switch) override
        {
            if (!EDF::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            if (!m_diffuse_edf->on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            if (!m_glossy_layer_bsdf->on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            return true;
        }

        void sample(
            SamplingContext&        sampling_context,
            const void*             data,
            const Vector3f&         geometric_normal,
            const Basis3f&          shading_basis,
            const Vector2f&         s,
            Vector3f&               outgoing,
            Spectrum&               value,
            float&                  probability) const override
        {
            const CompositeEmissionClosure* c =
                static_cast<const CompositeEmissionClosure*>(data);

            if (c->get_closure_count() > 0)
            {
                sampling_context.split_in_place(1, 1);
                const size_t closure_index = c->choose_closure(
                    sampling_context.next2<float>());

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
                attenuate_emission(*c, closure_index, outgoing, value);
            }
        }

        void evaluate(
            const void*             data,
            const Vector3f&         geometric_normal,
            const Basis3f&          shading_basis,
            const Vector3f&         outgoing,
            Spectrum&               value) const override
        {
            const CompositeEmissionClosure* c =
                static_cast<const CompositeEmissionClosure*>(data);

            value.set(0.0f);

            for (size_t i = 0, e = c->get_closure_count(); i < e; ++i)
            {
                // Skip closure layers.
                if (c->get_closure_type(i) >= FirstLayeredClosure)
                    continue;

                Spectrum s(Spectrum::Illuminance);

                const EDF& edf = edf_from_closure_id(c->get_closure_type(i));
                edf.evaluate(
                    c->get_closure_input_values(i),
                    geometric_normal,
                    shading_basis,
                    outgoing,
                    s);
                attenuate_emission(*c, i, outgoing, s);
                value += s;
            }
        }

        void evaluate(
            const void*             data,
            const Vector3f&         geometric_normal,
            const Basis3f&          shading_basis,
            const Vector3f&         outgoing,
            Spectrum&               value,
            float&                  probability) const override
        {
            const CompositeEmissionClosure* c =
                static_cast<const CompositeEmissionClosure*>(data);

            value.set(0.0f);
            probability = 0.0f;

            for (size_t i = 0, e = c->get_closure_count(); i < e; ++i)
            {
                // Skip closure layers.
                if (c->get_closure_type(i) >= FirstLayeredClosure)
                    continue;

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
                assert(edf_prob >= 0.0f);

                if (edf_prob > 0.0f)
                {
                    attenuate_emission(*c, i, outgoing, s);
                    value += s;
                    probability += edf_prob * c->get_closure_pdf(i);
                }
            }

            assert(probability >= 0.0f);
        }

        float evaluate_pdf(
            const void*             data,
            const Vector3f&         geometric_normal,
            const Basis3f&          shading_basis,
            const Vector3f&         outgoing) const override
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
                assert(edf_prob >= 0.0f);

                if (edf_prob > 0.0f)
                    probability += edf_prob * c->get_closure_pdf(i);
            }

            assert(probability >= 0.0f);
            return probability;
        }

        float get_uncached_max_contribution() const override
        {
            // We can't know the max contribution of OSL EDFs.
            return std::numeric_limits<float>::max();
        }

      private:
        void attenuate_emission(
            const CompositeClosure& c,
            const size_t            closure_index,
            const Vector3f&         outgoing,
            Spectrum&               value) const
        {
            const std::int8_t* layers = c.get_closure_layers(closure_index);

            for (size_t i = 0; i < CompositeClosure::MaxClosureLayers; ++i)
            {
                if (layers[i] < 0)
                    break;

                const size_t layer = static_cast<size_t>(layers[i]);
                bsdf_from_closure_id(c.get_closure_type(layer)).attenuate_emission(
                    c.get_closure_input_values(layer),
                    c.get_closure_shading_basis(layer),
                    outgoing,
                    value);
            }
        }

        const BSDF& bsdf_from_closure_id(const ClosureID cid) const
        {
            assert(cid >= FirstLayeredClosure);
            return *m_glossy_layer_bsdf;
        }

        const EDF& edf_from_closure_id(const ClosureID cid) const
        {
            assert(cid == EmissionID);
            return *m_diffuse_edf;
        }

        auto_release_ptr<EDF>   m_diffuse_edf;
        auto_release_ptr<BSDF>  m_glossy_layer_bsdf;
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
