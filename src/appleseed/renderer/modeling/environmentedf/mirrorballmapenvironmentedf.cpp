
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
#include "mirrorballmapenvironmentedf.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/inputevaluator.h"

// appleseed.foundation headers.
#include "foundation/math/sampling.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/containers/dictionary.h"

// Standard headers.
#include <cassert>
#include <cmath>

// Forward declarations.
namespace foundation    { class AbortSwitch; }
namespace renderer      { class Project; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Mirror ball environment map EDF.
    //
    // References:
    //
    //   http://www.debevec.org/probes/
    //   http://gl.ict.usc.edu/Data/HighResProbes/
    //

    const char* Model = "mirrorball_map_environment_edf";

    class MirrorBallMapEnvironmentEDF
      : public EnvironmentEDF
    {
      public:
        MirrorBallMapEnvironmentEDF(
            const char*         name,
            const ParamArray&   params)
          : EnvironmentEDF(name, params)
        {
            m_inputs.declare("radiance", InputFormatSpectralIlluminance);
            m_inputs.declare("radiance_multiplier", InputFormatScalar, "1.0");
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
            AbortSwitch*        abort_switch) OVERRIDE
        {
            if (!EnvironmentEDF::on_frame_begin(project, abort_switch))
                return false;

            check_non_zero_radiance("radiance", "radiance_multiplier");

            return true;
        }

        virtual void sample(
            InputEvaluator&     input_evaluator,
            const Vector2d&     s,
            Vector3d&           outgoing,
            Spectrum&           value,
            double&             probability) const OVERRIDE
        {
            outgoing = sample_sphere_uniform(s);
            lookup_envmap(input_evaluator, outgoing, value);
            probability = 1.0 / (4.0 * Pi);
        }

        virtual void evaluate(
            InputEvaluator&     input_evaluator,
            const Vector3d&     outgoing,
            Spectrum&           value) const OVERRIDE
        {
            assert(is_normalized(outgoing));
            lookup_envmap(input_evaluator, outgoing, value);
        }

        virtual void evaluate(
            InputEvaluator&     input_evaluator,
            const Vector3d&     outgoing,
            Spectrum&           value,
            double&             probability) const OVERRIDE
        {
            assert(is_normalized(outgoing));
            lookup_envmap(input_evaluator, outgoing, value);
            probability = 1.0 / (4.0 * Pi);
        }

        virtual double evaluate_pdf(
            InputEvaluator&     input_evaluator,
            const Vector3d&     outgoing) const OVERRIDE
        {
            assert(is_normalized(outgoing));
            return 1.0 / (4.0 * Pi);
        }

      private:
        DECLARE_INPUT_VALUES(InputValues)
        {
            Spectrum    m_radiance;             // emitted radiance in W.m^-2.sr^-1
            Alpha       m_radiance_alpha;       // unused
            double      m_radiance_multiplier;  // emitted radiance multiplier
        };

        void lookup_envmap(
            InputEvaluator&     input_evaluator,
            const Vector3d&     direction,
            Spectrum&           value) const
        {
            // Compute the texture coordinates corresponding to this direction.
            const double d = sqrt(square(direction[0]) + square(direction[1]));
            const double r = (0.5 / Pi) * acos(direction[2]) / d;
            const Vector2d uv(0.5 + direction[0] * r, 0.5 + direction[1] * r);

            // Evaluate the input.
            const InputValues* values = input_evaluator.evaluate<InputValues>(m_inputs, uv);
            value = values->m_radiance;
            value *= static_cast<float>(values->m_radiance_multiplier);
        }
    };
}


//
// MirrorBallMapEnvironmentEDFFactory class implementation.
//

const char* MirrorBallMapEnvironmentEDFFactory::get_model() const
{
    return Model;
}

const char* MirrorBallMapEnvironmentEDFFactory::get_human_readable_model() const
{
    return "Mirror Ball Map Environment EDF";
}

DictionaryArray MirrorBallMapEnvironmentEDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "radiance")
            .insert("label", "Radiance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "radiance_multiplier")
            .insert("label", "Radiance Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    return metadata;
}

auto_release_ptr<EnvironmentEDF> MirrorBallMapEnvironmentEDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<EnvironmentEDF>(
            new MirrorBallMapEnvironmentEDF(name, params));
}

}   // namespace renderer
