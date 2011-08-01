
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "bsdfmix.h"

// appleseed.renderer headers.
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/scene/assembly.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"
#include "foundation/utility/containers/specializedarrays.h"

// Standard headers.
#include <cstddef>
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // A blend of BSDF.
    //

    const char* Model = "bsdf_mix";

    class BSDFMix
      : public BSDF
    {
      public:
        BSDFMix(
            const char*         name,
            const ParamArray&   params)
          : BSDF(name, params)
        {
            m_inputs.declare("weight0", InputFormatScalar);
            m_inputs.declare("weight1", InputFormatScalar);
        }

        virtual void release()
        {
            delete this;
        }

        virtual const char* get_model() const
        {
            return Model;
        }

        virtual void on_frame_begin(
            const Project&      project,
            const Assembly&     assembly,
            const void*         data)
        {
            m_bsdf[0] = retrieve_bsdf(assembly, "bsdf0");
            m_bsdf[1] = retrieve_bsdf(assembly, "bsdf1");

            m_bsdf_data_offset[0] = get_inputs().compute_data_size();
            m_bsdf_data_offset[1] = m_bsdf_data_offset[0] + m_bsdf[0]->get_inputs().compute_data_size();
        }

        virtual void evaluate_inputs(
            InputEvaluator&     input_evaluator,
            const InputParams&  input_params) const
        {
            input_evaluator.evaluate(get_inputs(), input_params);
            input_evaluator.evaluate(m_bsdf[0]->get_inputs(), input_params, m_bsdf_data_offset[0]);
            input_evaluator.evaluate(m_bsdf[1]->get_inputs(), input_params, m_bsdf_data_offset[1]);
        }

        FORCE_INLINE virtual void sample(
            const void*         data,
            const bool          adjoint,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     s,
            const Vector3d&     outgoing,
            Vector3d&           incoming,
            Spectrum&           value,
            double&             probability,
            Mode&               mode) const
        {
            const InputValues* values = static_cast<const InputValues*>(data);
            const double w0 = values->m_weight[0];
            const double w1 = values->m_weight[1];
            const double total_weight = w0 + w1;

            if (total_weight == 0.0)
            {
                mode = BSDF::None;
                return;
            }

            const size_t bsdf_index = s[2] < (w0 / total_weight) ? 0 : 1;

            m_bsdf[bsdf_index]->sample(
                get_bsdf_data(data, bsdf_index),
                adjoint,
                geometric_normal,
                shading_basis,
                s,
                outgoing,
                incoming,
                value,
                probability,
                mode);
        }

        FORCE_INLINE virtual bool evaluate(
            const void*         data,
            const bool          adjoint,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming,
            Spectrum&           value,
            double*             probability) const
        {
            const InputValues* values = static_cast<const InputValues*>(data);
            double w0 = values->m_weight[0];
            double w1 = values->m_weight[1];
            const double total_weight = w0 + w1;

            if (total_weight == 0.0)
                return false;

            const double rcp_total_weight = 1.0 / total_weight;
            w0 *= rcp_total_weight;
            w1 *= rcp_total_weight;

            double bsdf0_prob = 0.0;
            if (m_bsdf[0]->evaluate(
                    get_bsdf_data(data, 0),
                    adjoint,
                    geometric_normal,
                    shading_basis,
                    outgoing,
                    incoming,
                    value,
                    probability ? &bsdf0_prob : 0))
                value *= static_cast<float>(w0);
            else value.set(0.0f);

            Spectrum bsdf1_value;
            double bsdf1_prob = 0.0;
            if (m_bsdf[1]->evaluate(
                    get_bsdf_data(data, 1),
                    adjoint,
                    geometric_normal,
                    shading_basis,
                    outgoing,
                    incoming,
                    bsdf1_value,
                    probability ? &bsdf1_prob : 0))
                bsdf1_value *= static_cast<float>(w1);
            else bsdf1_value.set(0.0f);

            value += bsdf1_value;

            if (probability)
                *probability = bsdf0_prob * w0 + bsdf1_prob * w1;

            return true;
        }

        FORCE_INLINE virtual double evaluate_pdf(
            const void*         data,
            const Vector3d&     geometric_normal,
            const Basis3d&      shading_basis,
            const Vector3d&     outgoing,
            const Vector3d&     incoming) const
        {
            const InputValues* values = static_cast<const InputValues*>(data);
            double w0 = values->m_weight[0];
            double w1 = values->m_weight[1];
            const double total_weight = w0 + w1;

            if (total_weight == 0.0)
                return 0.0;

            const double pdf0 =
                m_bsdf[0]->evaluate_pdf(
                    get_bsdf_data(data, 0),
                    geometric_normal,
                    shading_basis,
                    outgoing,
                    incoming);

            const double pdf1 =
                m_bsdf[1]->evaluate_pdf(
                    get_bsdf_data(data, 1),
                    geometric_normal,
                    shading_basis,
                    outgoing,
                    incoming);

            return (pdf0 * w0 + pdf1 * w1) / total_weight;
        }

      private:
        struct InputValues
        {
            double  m_weight[2];
        };

        const BSDF* m_bsdf[2];
        size_t      m_bsdf_data_offset[2];

        const BSDF* retrieve_bsdf(const Assembly& assembly, const char* param_name) const
        {
            const string bsdf_name = m_params.get_required<string>(param_name, "");
            if (bsdf_name.empty())
                return 0;

            const BSDF* bsdf = assembly.bsdfs().get_by_name(bsdf_name.c_str());
            if (bsdf == 0)
                RENDERER_LOG_ERROR("while preparing bsdf \"%s\": cannot find bsdf \"%s\"", get_name(), bsdf_name.c_str());

            return bsdf;
        }

        const void* get_bsdf_data(const void* data, const size_t bsdf_index) const
        {
            assert(bsdf_index < 2);
            return static_cast<const uint8*>(data) + m_bsdf_data_offset[bsdf_index];
        }
    };
}


//
// BSDFMixFactory class implementation.
//

const char* BSDFMixFactory::get_model() const
{
    return Model;
}

const char* BSDFMixFactory::get_human_readable_model() const
{
    return "BSDF Mix";
}

DictionaryArray BSDFMixFactory::get_widget_definitions() const
{
    DictionaryArray definitions;

    definitions.push_back(
        Dictionary()
            .insert("name", "bsdf0")
            .insert("label", "BSDF 1")
            .insert("widget", "entity_picker")
            .insert("entity_types", Dictionary().insert("bsdf", "BSDF")));

    definitions.push_back(
        Dictionary()
            .insert("name", "weight0")
            .insert("label", "Weight 1")
            .insert("widget", "entity_picker")
            .insert("entity_types", Dictionary().insert("texture_instance", "Textures"))
            .insert("default", "0.5"));

    definitions.push_back(
        Dictionary()
            .insert("name", "bsdf1")
            .insert("label", "BSDF 2")
            .insert("widget", "entity_picker")
            .insert("entity_types", Dictionary().insert("bsdf", "BSDF")));

    definitions.push_back(
        Dictionary()
            .insert("name", "weight1")
            .insert("label", "Weight 2")
            .insert("widget", "entity_picker")
            .insert("entity_types", Dictionary().insert("texture_instance", "Textures"))
            .insert("default", "0.5"));

    return definitions;
}

auto_release_ptr<BSDF> BSDFMixFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<BSDF>(new BSDFMix(name, params));
}

}   // namespace renderer
