
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
#include "nishita93environmentedf.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/environmentedf/sphericalcoordinates.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/input/sourceinputs.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/regularspectrum.h"
#include "foundation/math/fastmath.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>

// Forward declarations.
namespace foundation { class IAbortSwitch; }
namespace renderer { class Project; }

using namespace foundation;

namespace renderer
{

    namespace
    {

//
// An environment EDF implementing the Nishita93 day sky model.
//
// Reference:
//
//   http://nishitalab.org/user/nis/cdrom/sig93_nis.pdf
//

        const char* Model = "nishita93_environment_edf";

        class Nishita93EnvironmentEDF
            : public EnvironmentEDF
        {
        public:
            Nishita93EnvironmentEDF(
                const char*             name,
                const ParamArray&       params)
                : EnvironmentEDF(name, params)
            {
                m_inputs.declare("sun_theta", InputFormat::Float);
                m_inputs.declare("sun_phi", InputFormat::Float);
                m_inputs.declare("horizon_shift", InputFormat::Float, "0.0");
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
                const Project&          project,
                const BaseGroup*        parent,
                OnFrameBeginRecorder&   recorder,
                IAbortSwitch*           abort_switch) override
            {
                if (!EnvironmentEDF::on_frame_begin(project, parent, recorder, abort_switch))
                    return false;

                // Evaluate uniform values.
                m_inputs.evaluate_uniforms(&m_uniform_values);

                // Compute the sun direction.
                m_sun_theta = deg_to_rad(m_uniform_values.m_sun_theta);
                m_sun_phi = deg_to_rad(m_uniform_values.m_sun_phi);
                m_sun_dir = Vector3f::make_unit_vector(m_sun_theta, m_sun_phi);

                // Precompute nishitas lookup table for optical depths
                precompute_optical_depths();

                return true;
            }

            void sample(
                const ShadingContext&   shading_context,
                const Vector2f&         s,
                Vector3f&               outgoing,
                Spectrum&               value,
                float&                  probability) const override
            {
                const Vector3f local_outgoing = sample_hemisphere_cosine(s);

                Transformd scratch;
                const Transformd& transform = m_transform_sequence.evaluate(0.0f, scratch);
                outgoing = transform.vector_to_parent(local_outgoing);
                const Vector3f shifted_outgoing = shift(local_outgoing);

                RegularSpectrum31f radiance;
                if (shifted_outgoing.y > 0.0f)
                    compute_sky_radiance(shading_context, shifted_outgoing, radiance);
                else radiance.set(0.0f);

                value.set(radiance, g_std_lighting_conditions, Spectrum::Illuminance);
                probability = shifted_outgoing.y > 0.0f ? shifted_outgoing.y * RcpPi<float>() : 0.0f;
                assert(probability >= 0.0f);
            }

            void evaluate(
                const ShadingContext&   shading_context,
                const Vector3f&         outgoing,
                Spectrum&               value) const override
            {
                assert(is_normalized(outgoing));

                Transformd scratch;
                const Transformd& transform = m_transform_sequence.evaluate(0.0f, scratch);
                const Vector3f local_outgoing = transform.vector_to_local(outgoing);
                const Vector3f shifted_outgoing = shift(local_outgoing);

                RegularSpectrum31f radiance;
                if (shifted_outgoing.y > 0.0f)
                    compute_sky_radiance(shading_context, shifted_outgoing, radiance);
                else radiance.set(0.0f);

                value.set(radiance, g_std_lighting_conditions, Spectrum::Illuminance);
            }

            void evaluate(
                const ShadingContext&   shading_context,
                const Vector3f&         outgoing,
                Spectrum&               value,
                float&                  probability) const override
            {
                assert(is_normalized(outgoing));

                Transformd scratch;
                const Transformd& transform = m_transform_sequence.evaluate(0.0f, scratch);
                const Vector3f local_outgoing = transform.vector_to_local(outgoing);
                const Vector3f shifted_outgoing = shift(local_outgoing);

                RegularSpectrum31f radiance;
                if (shifted_outgoing.y > 0.0f)
                    compute_sky_radiance(shading_context, shifted_outgoing, radiance);
                else radiance.set(0.0f);

                value.set(radiance, g_std_lighting_conditions, Spectrum::Illuminance);
                probability = shifted_outgoing.y > 0.0f ? shifted_outgoing.y * RcpPi<float>() : 0.0f;
                assert(probability >= 0.0f);
            }

            float evaluate_pdf(
                const Vector3f&         outgoing) const override
            {
                assert(is_normalized(outgoing));

                Transformd scratch;
                const Transformd& transform = m_transform_sequence.evaluate(0.0f, scratch);
                const Vector3f local_outgoing = transform.vector_to_local(outgoing);
                const Vector3f shifted_outgoing = shift(local_outgoing);

                const float probability = shifted_outgoing.y > 0.0f ? shifted_outgoing.y * RcpPi<float>() : 0.0f;
                assert(probability >= 0.0f);

                return probability;
            }

        private:
            APPLESEED_DECLARE_INPUT_VALUES(InputValues)
            {
                float   m_sun_theta;                    // sun zenith angle in degrees, 0=zenith
                float   m_sun_phi;                      // degrees
                float   m_horizon_shift;
            };

            InputValues                 m_uniform_values;

            float                       m_sun_theta;    // sun zenith angle in radians, 0=zenith
            float                       m_sun_phi;      // radians
            Vector3f                    m_sun_dir;

            // Fills a 3D precomputation table with optical depths value along the sun direction
            void precompute_optical_depths() {

            }

            // Compute the sky radiance along a given direction.
            void compute_sky_radiance(
                const ShadingContext&   shading_context,
                const Vector3f&         outgoing,
                RegularSpectrum31f&     radiance) const
            {
                
                // Compute the final sky radiance.
                radiance *= RegularSpectrum31f(0.8f);
            }

            Vector3f shift(Vector3f v) const
            {
                v.y -= m_uniform_values.m_horizon_shift;
                return normalize(v);
            }
        };
    }


    //
    // Nishita93EnvironmentEDFFactory class implementation.
    //

    void Nishita93EnvironmentEDFFactory::release()
    {
        delete this;
    }

    const char* Nishita93EnvironmentEDFFactory::get_model() const
    {
        return Model;
    }

    Dictionary Nishita93EnvironmentEDFFactory::get_model_metadata() const
    {
        return
            Dictionary()
            .insert("name", Model)
            .insert("label", "Nishita93 Environment EDF")
            .insert("help", "Physical sky with single scattering environment");
    }

    DictionaryArray Nishita93EnvironmentEDFFactory::get_input_metadata() const
    {
        DictionaryArray metadata;

        add_common_sky_input_metadata(metadata);
        add_common_input_metadata(metadata);

        return metadata;
    }

    auto_release_ptr<EnvironmentEDF> Nishita93EnvironmentEDFFactory::create(
        const char*         name,
        const ParamArray&   params) const
    {
        return
            auto_release_ptr<EnvironmentEDF>(
                new Nishita93EnvironmentEDF(name, params));
    }

}   // namespace renderer
