
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include "bsdfblend.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"
#include "renderer/modeling/scene/assembly.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <string>

// Forward declarations.
namespace renderer  { class InputEvaluator; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // A blend of two BSDFs, with a single weight controlling the amount of each.
    //

    const char* Model = "bsdf_blend";

    class BSDFBlendImpl
      : public BSDF
    {
      public:
        BSDFBlendImpl(
            const char*         name,
            const ParamArray&   params)
          : BSDF(name, Reflective, AllScatteringModes, params)
        {
            m_inputs.declare("weight", InputFormatScalar);
        }

        virtual void release() OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const OVERRIDE
        {
            return Model;
        }

        virtual bool on_frame_begin(
            const Project&      project,
            const Assembly&     assembly) OVERRIDE
        {
            if (!BSDF::on_frame_begin(project, assembly))
                return false;

            m_bsdf[0] = retrieve_bsdf(assembly, "bsdf0");
            m_bsdf[1] = retrieve_bsdf(assembly, "bsdf1");

            if (m_bsdf[0] == 0 || m_bsdf[1] == 0)
                return false;

            m_bsdf_data_offset[0] = get_inputs().compute_data_size();
            m_bsdf_data_offset[1] = m_bsdf_data_offset[0] + m_bsdf[0]->compute_input_data_size(assembly);

            return true;
        }

        virtual size_t compute_input_data_size(
            const Assembly&     assembly) const OVERRIDE
        {
            size_t size = get_inputs().compute_data_size();

            {
                const BSDF* bsdf0 = retrieve_bsdf(assembly, "bsdf0");
                if (bsdf0)
                    size += bsdf0->compute_input_data_size(assembly);
            }

            {
                const BSDF* bsdf1 = retrieve_bsdf(assembly, "bsdf1");
                if (bsdf1)
                    size += bsdf1->compute_input_data_size(assembly);
            }

            return size;
        }

        virtual void evaluate_inputs(
            InputEvaluator&     input_evaluator,
            const Vector2d&     uv,
            const size_t        offset) const OVERRIDE
        {
            assert(m_bsdf[0] && m_bsdf[1]);

            BSDF::evaluate_inputs(input_evaluator, uv, offset);

            m_bsdf[0]->evaluate_inputs(input_evaluator, uv, offset + m_bsdf_data_offset[0]);
            m_bsdf[1]->evaluate_inputs(input_evaluator, uv, offset + m_bsdf_data_offset[1]);
        }

        FORCE_INLINE virtual Mode sample(
            SamplingContext&    sampling_context,
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            Vector3d&           incoming,
            Spectrum&           value,
            double&             probability) const
        {
            assert(m_bsdf[0] && m_bsdf[1]);

            // Retrieve the blending weights.
            const InputValues* values = static_cast<const InputValues*>(data);
            double w[2] = { values->m_weight, 1.0 - values->m_weight };

            // Choose which of the two BSDFs to sample.
            sampling_context.split_in_place(1, 1);
            const double s = sampling_context.next_double2();
            const size_t bsdf0_index = s < w[0] ? 0 : 1;
            const size_t bsdf1_index = 1 - bsdf0_index;

            // Sample this BSDF.
            Spectrum bsdf0_value;
            double bsdf0_prob;
            const Mode bsdf0_mode =
                m_bsdf[bsdf0_index]->sample(
                    sampling_context,
                    get_bsdf_data(data, bsdf0_index),
                    adjoint,
                    false,      // do not multiply by |cos(incoming, normal)|
                    geometric_normal,
                    shading_basis,
                    outgoing,
                    incoming,
                    bsdf0_value,
                    bsdf0_prob);
            if (bsdf0_mode == Absorption)
                return Absorption;

            // Evaluate the other BSDF.
            Spectrum bsdf1_value;
            const double bsdf1_prob =
                m_bsdf[bsdf1_index]->evaluate(
                    get_bsdf_data(data, bsdf1_index),
                    adjoint,
                    false,      // do not multiply by |cos(incoming, normal)|
                    geometric_normal,
                    shading_basis,
                    outgoing,
                    incoming,
                    BSDF::AllScatteringModes,
                    bsdf1_value);

            // Apply blending weights.
            if (bsdf1_prob > 0.0)
            {
                bsdf0_value *= static_cast<float>(w[bsdf0_index]);
                bsdf1_value *= static_cast<float>(w[bsdf1_index]);
            }

            // Blend BSDF values.
            value.set(0.0f);
            if (bsdf0_prob == BSDF::DiracDelta || bsdf0_prob > 0.0)
                value += bsdf0_value;
            if (bsdf1_prob > 0.0)
                value += bsdf1_value;

            // Blend PDF values.
            probability =
                bsdf0_prob == BSDF::DiracDelta
                    ? BSDF::DiracDelta
                    : bsdf0_prob * w[bsdf0_index] + bsdf1_prob * w[bsdf1_index];

            // Return the scattering mode.
            return bsdf0_mode;
        }

        FORCE_INLINE virtual double evaluate(
            const void*         data,
            const bool          adjoint,
            const bool          cosine_mult,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            const int           modes,
            Spectrum&           value) const
        {
            assert(m_bsdf[0] && m_bsdf[1]);

            // Retrieve the blending weights.
            const InputValues* values = static_cast<const InputValues*>(data);
            double w0 = values->m_weight;
            double w1 = 1.0 - w0;

            // Evaluate the first BSDF.
            Spectrum bsdf0_value;
            const double bsdf0_prob =
                m_bsdf[0]->evaluate(
                    get_bsdf_data(data, 0),
                    adjoint,
                    false,      // do not multiply by |cos(incoming, normal)|
                    geometric_normal,
                    shading_basis,
                    outgoing,
                    incoming,
                    modes,
                    bsdf0_value);

            // Evaluate the second BSDF.
            Spectrum bsdf1_value;
            const double bsdf1_prob =
                m_bsdf[1]->evaluate(
                    get_bsdf_data(data, 1),
                    adjoint,
                    false,      // do not multiply by |cos(incoming, normal)|
                    geometric_normal,
                    shading_basis,
                    outgoing,
                    incoming,
                    modes,
                    bsdf1_value);

            // Blend BSDF values.
            value.set(0.0f);
            if (bsdf0_prob > 0.0)
            {
                bsdf0_value *= static_cast<float>(w0);
                value += bsdf0_value;
            }
            if (bsdf1_prob > 0.0)
            {
                bsdf1_value *= static_cast<float>(w1);
                value += bsdf1_value;
            }

            // Blend PDF values.
            return bsdf0_prob * w0 + bsdf1_prob * w1;
        }

        FORCE_INLINE virtual double evaluate_pdf(
            const void*         data,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            const int           modes) const
        {
            assert(m_bsdf[0] && m_bsdf[1]);

            // Retrieve the blending weights.
            const InputValues* values = static_cast<const InputValues*>(data);
            double w0 = values->m_weight;
            double w1 = 1.0 - w0;

            // Evaluate the PDF of the first BSDF.
            const double bsdf0_prob =
                m_bsdf[0]->evaluate_pdf(
                    get_bsdf_data(data, 0),
                    geometric_normal,
                    shading_basis,
                    outgoing,
                    incoming,
                    modes);

            // Evaluate the PDF of the second BSDF.
            const double bsdf1_prob =
                m_bsdf[1]->evaluate_pdf(
                    get_bsdf_data(data, 1),
                    geometric_normal,
                    shading_basis,
                    outgoing,
                    incoming,
                    modes);

            // Blend PDF values.
            return bsdf0_prob * w0 + bsdf1_prob * w1;
        }

      private:
        DECLARE_INPUT_VALUES(InputValues)
        {
            double  m_weight;
        };

        const BSDF* m_bsdf[2];
        size_t      m_bsdf_data_offset[2];

        const BSDF* retrieve_bsdf(const Assembly& assembly, const char* param_name) const
        {
            const string bsdf_name = m_params.get_required<string>(param_name, "");
            if (bsdf_name.empty())
            {
                RENDERER_LOG_ERROR("while preparing bsdf \"%s\": no bsdf bound to \"%s\".", get_path().c_str(), param_name);
                return 0;
            }

            const BSDF* bsdf = assembly.bsdfs().get_by_name(bsdf_name.c_str());
            if (bsdf == 0)
                RENDERER_LOG_ERROR("while preparing bsdf \"%s\": cannot find bsdf \"%s\".", get_path().c_str(), bsdf_name.c_str());

            return bsdf;
        }

        const void* get_bsdf_data(const void* data, const size_t bsdf_index) const
        {
            assert(bsdf_index < 2);
            return static_cast<const uint8*>(data) + m_bsdf_data_offset[bsdf_index];
        }
    };

    typedef BSDFWrapper<BSDFBlendImpl> BSDFBlend;
}


//
// BSDFBlendFactory class implementation.
//

const char* BSDFBlendFactory::get_model() const
{
    return Model;
}

const char* BSDFBlendFactory::get_human_readable_model() const
{
    return "BSDF Blend";
}

DictionaryArray BSDFBlendFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "bsdf0")
            .insert("label", "BSDF 1")
            .insert("type", "entity")
            .insert("entity_types",
                Dictionary().insert("bsdf", "BSDF"))
            .insert("use", "required"));

    metadata.push_back(
        Dictionary()
            .insert("name", "bsdf1")
            .insert("label", "BSDF 2")
            .insert("type", "entity")
            .insert("entity_types",
                Dictionary().insert("bsdf", "BSDF"))
            .insert("use", "required"));

    metadata.push_back(
        Dictionary()
            .insert("name", "weight")
            .insert("label", "Weight")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.5"));

    return metadata;
}

auto_release_ptr<BSDF> BSDFBlendFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new BSDFBlend(name, params));
}

}   // namespace renderer
