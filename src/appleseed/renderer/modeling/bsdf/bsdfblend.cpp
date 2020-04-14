
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "renderer/kernel/shading/directshadingcomponents.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bsdf/bsdfwrapper.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/dual.h"
#include "foundation/math/vector.h"
#include "foundation/memory/arena.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <string>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class BaseGroup; }
namespace renderer      { class BSDFSample; }
namespace renderer      { class Project; }
namespace renderer      { class ShadingPoint; }

using namespace foundation;

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
            const char*                 name,
            const ParamArray&           params)
          : BSDF(name, Reflective, ScatteringMode::All, params)
        {
            m_inputs.declare("bsdf0", InputFormatEntity);
            m_inputs.declare("bsdf1", InputFormatEntity);
            m_inputs.declare("weight", InputFormatFloat);
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return Model;
        }

        bool on_frame_begin(
            const Project&              project,
            const BaseGroup*            parent,
            OnFrameBeginRecorder&       recorder,
            IAbortSwitch*               abort_switch) override
        {
            if (!BSDF::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            // Cache bound BSDFs.
            m_bsdf[0] = dynamic_cast<const BSDF*>(m_inputs.get_entity("bsdf0"));
            m_bsdf[1] = dynamic_cast<const BSDF*>(m_inputs.get_entity("bsdf1"));

            if (m_bsdf[0] == nullptr)
            {
                RENDERER_LOG_ERROR("while preparing bsdf \"%s\": cannot find bsdf \"%s\".",
                    get_path().c_str(), m_params.get_optional<std::string>("bsdf0", "").c_str());
            }

            if (m_bsdf[1] == nullptr)
            {
                RENDERER_LOG_ERROR("while preparing bsdf \"%s\": cannot find bsdf \"%s\".",
                    get_path().c_str(), m_params.get_optional<std::string>("bsdf1", "").c_str());
            }

            if (m_bsdf[0] == nullptr || m_bsdf[1] == nullptr)
                return false;

            return true;
        }

        void* evaluate_inputs(
            const ShadingContext&       shading_context,
            const ShadingPoint&         shading_point) const override
        {
            assert(m_bsdf[0] && m_bsdf[1]);

            Values* values = shading_context.get_arena().allocate<Values>();

            values->m_inputs =
                static_cast<Values::Inputs*>(
                    BSDF::evaluate_inputs(shading_context, shading_point));

            values->m_child_inputs[0] = m_bsdf[0]->evaluate_inputs(shading_context, shading_point);
            values->m_child_inputs[1] = m_bsdf[1]->evaluate_inputs(shading_context, shading_point);

            return values;
        }

        void sample(
            SamplingContext&            sampling_context,
            const void*                 data,
            const bool                  adjoint,
            const bool                  cosine_mult,
            const LocalGeometry&        local_geometry,
            const Dual3f&               outgoing,
            const int                   modes,
            BSDFSample&                 sample) const override
        {
            assert(m_bsdf[0] && m_bsdf[1]);

            const Values* values = static_cast<const Values*>(data);

            // Choose which of the two BSDFs to sample.
            sampling_context.split_in_place(1, 1);
            const float s = sampling_context.next2<float>();
            const size_t bsdf_index = s < values->m_inputs->m_weight ? 0 : 1;

            // Sample the chosen BSDF.
            m_bsdf[bsdf_index]->sample(
                sampling_context,
                values->m_child_inputs[bsdf_index],
                adjoint,
                false,                  // do not multiply by |cos(incoming, normal)|
                local_geometry,
                outgoing,
                modes,
                sample);
        }

        float evaluate(
            const void*                 data,
            const bool                  adjoint,
            const bool                  cosine_mult,
            const LocalGeometry&        local_geometry,
            const Vector3f&             outgoing,
            const Vector3f&             incoming,
            const int                   modes,
            DirectShadingComponents&    value) const override
        {
            assert(m_bsdf[0] && m_bsdf[1]);

            const Values* values = static_cast<const Values*>(data);

            // Retrieve blending weights.
            const float w0 = values->m_inputs->m_weight;
            const float w1 = 1.0f - w0;

            // Evaluate the first BSDF.
            DirectShadingComponents bsdf0_value;
            const float bsdf0_prob =
                w0 > 0.0f
                    ? m_bsdf[0]->evaluate(
                          values->m_child_inputs[0],
                          adjoint,
                          false,                // do not multiply by |cos(incoming, normal)|
                          local_geometry,
                          outgoing,
                          incoming,
                          modes,
                          bsdf0_value)
                    : 0.0f;

            // Evaluate the second BSDF.
            DirectShadingComponents bsdf1_value;
            const float bsdf1_prob =
                w1 > 0.0f
                    ? m_bsdf[1]->evaluate(
                          values->m_child_inputs[1],
                          adjoint,
                          false,                // do not multiply by |cos(incoming, normal)|
                          local_geometry,
                          outgoing,
                          incoming,
                          modes,
                          bsdf1_value)
                    : 0.0f;

            // Blend BSDF values.
            if (bsdf0_prob > 0.0f) madd(value, bsdf0_value, w0);
            if (bsdf1_prob > 0.0f) madd(value, bsdf1_value, w1);

            // Compute the final PDF.
            const float probability = bsdf0_prob * w0 + bsdf1_prob * w1;
            assert(probability >= 0.0f);

            return probability;
        }

        float evaluate_pdf(
            const void*                 data,
            const bool                  adjoint,
            const LocalGeometry&        local_geometry,
            const Vector3f&             outgoing,
            const Vector3f&             incoming,
            const int                   modes) const override
        {
            assert(m_bsdf[0] && m_bsdf[1]);

            const Values* values = static_cast<const Values*>(data);

            // Retrieve blending weights.
            const float w0 = values->m_inputs->m_weight;
            const float w1 = 1.0f - w0;

            // Evaluate the PDF of the first BSDF.
            const float bsdf0_prob =
                w0 > 0.0f
                    ? m_bsdf[0]->evaluate_pdf(
                          values->m_child_inputs[0],
                          adjoint,
                          local_geometry,
                          outgoing,
                          incoming,
                          modes)
                    : 0.0f;

            // Evaluate the PDF of the second BSDF.
            const float bsdf1_prob =
                w1 > 0.0f
                    ? m_bsdf[1]->evaluate_pdf(
                          values->m_child_inputs[1],
                          adjoint,
                          local_geometry,
                          outgoing,
                          incoming,
                          modes)
                    : 0.0f;

            // Compute the final PDF.
            const float probability = bsdf0_prob * w0 + bsdf1_prob * w1;
            assert(probability >= 0.0f);

            return probability;
        }

      private:
        struct Values
        {
            APPLESEED_DECLARE_INPUT_VALUES(Inputs)
            {
                float       m_weight;
            };

            const Inputs*   m_inputs;
            const void*     m_child_inputs[2];
        };

        const BSDF* m_bsdf[2];
    };

    typedef BSDFWrapper<BSDFBlendImpl, false> BSDFBlend;
}


//
// BSDFBlendFactory class implementation.
//

void BSDFBlendFactory::release()
{
    delete this;
}

const char* BSDFBlendFactory::get_model() const
{
    return Model;
}

Dictionary BSDFBlendFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "BSDF Blend");
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
                Dictionary().insert("texture_instance", "Texture Instances"))
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
