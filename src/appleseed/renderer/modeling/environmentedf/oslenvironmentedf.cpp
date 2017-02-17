
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
#include "oslenvironmentedf.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/shadergroup/shadergroup.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/math/matrix.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <cassert>
#include <cstring>
#include <string>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Project; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // OSL environment EDF.
    //
    // todo: importance sampling
    //

    const char* Model = "osl_environment_edf";

    class OSLEnvironmentEDF
      : public EnvironmentEDF
    {
      public:
        OSLEnvironmentEDF(
            const char*             name,
            const ParamArray&       params)
          : EnvironmentEDF(name, params)
        {
            m_inputs.declare("osl_background", InputFormatEntity, "");
        }

        virtual void release() APPLESEED_OVERRIDE
        {
            delete this;
        }

        virtual const char* get_model() const APPLESEED_OVERRIDE
        {
            return Model;
        }

        virtual bool on_frame_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnFrameBeginRecorder&   recorder,
            IAbortSwitch*           abort_switch) APPLESEED_OVERRIDE
        {
            if (!EnvironmentEDF::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            m_shader_group =
                static_cast<ShaderGroup*>(m_inputs.get_entity("osl_background"));

            return true;
        }

        virtual void on_frame_end(
            const Project&          project,
            const BaseGroup*        parent) APPLESEED_OVERRIDE
        {
            m_shader_group = 0;

            EnvironmentEDF::on_frame_end(project, parent);
        }

        virtual void sample(
            const ShadingContext&   shading_context,
            const Vector2f&         s,
            Vector3f&               outgoing,
            Spectrum&               value,
            float&                  probability) const APPLESEED_OVERRIDE
        {
            const Vector3f local_outgoing = sample_sphere_uniform(s);
            probability = RcpFourPi<float>();

            Transformd scratch;
            const Transformd& transform = m_transform_sequence.evaluate(0.0f, scratch);
            outgoing = transform.vector_to_parent(local_outgoing);

            evaluate_osl_background(shading_context, local_outgoing, value);
        }

        virtual void evaluate(
            const ShadingContext&   shading_context,
            const Vector3f&         outgoing,
            Spectrum&               value) const APPLESEED_OVERRIDE
        {
            assert(is_normalized(outgoing));

            Transformd scratch;
            const Transformd& transform = m_transform_sequence.evaluate(0.0f, scratch);
            const Vector3f local_outgoing = transform.vector_to_local(outgoing);

            evaluate_osl_background(shading_context, local_outgoing, value);
        }

        virtual void evaluate(
            const ShadingContext&   shading_context,
            const Vector3f&         outgoing,
            Spectrum&               value,
            float&                  probability) const APPLESEED_OVERRIDE
        {
            assert(is_normalized(outgoing));

            Transformd scratch;
            const Transformd& transform = m_transform_sequence.evaluate(0.0f, scratch);
            const Vector3f local_outgoing = transform.vector_to_local(outgoing);

            evaluate_osl_background(shading_context, local_outgoing, value);
            probability = RcpFourPi<float>();
        }

        virtual float evaluate_pdf(
            const Vector3f&         outgoing) const APPLESEED_OVERRIDE
        {
            assert(is_normalized(outgoing));
            return RcpFourPi<float>();
        }

      private:
        void evaluate_osl_background(
            const ShadingContext&   shading_context,
            const Vector3f&         local_outgoing,
            Spectrum&               value) const
        {
            if (m_shader_group)
                shading_context.execute_osl_background(*m_shader_group, local_outgoing, value);
            else value.set(0.0f);
        }

        ShaderGroup* m_shader_group;
    };
}


//
// OSLEnvironmentEDFFactory class implementation.
//

const char* OSLEnvironmentEDFFactory::get_model() const
{
    return Model;
}

Dictionary OSLEnvironmentEDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "OSL Environment EDF");
}

DictionaryArray OSLEnvironmentEDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "osl_background")
            .insert("label", "OSL Background")
            .insert("type", "entity")
            .insert("entity_types",
                Dictionary()
                    .insert("shader_group", "Shader Groups"))
            .insert("use", "optional"));

    return metadata;
}

auto_release_ptr<EnvironmentEDF> OSLEnvironmentEDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<EnvironmentEDF>(
            new OSLEnvironmentEDF(name, params));
}

auto_release_ptr<EnvironmentEDF> OSLEnvironmentEDFFactory::static_create(
    const char*         name,
    const ParamArray&   params)
{
    return
        auto_release_ptr<EnvironmentEDF>(
            new OSLEnvironmentEDF(name, params));
}

}   // namespace renderer
