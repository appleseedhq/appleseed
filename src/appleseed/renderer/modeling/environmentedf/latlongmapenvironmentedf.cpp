
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
#include "latlongmapenvironmentedf.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/kernel/texturing/texturestore.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/environmentedf/sphericalcoordinates.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/input/sourceinputs.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/utility/paramarray.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/math/matrix.h"
#include "foundation/math/sampling/imageimportancesampler.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/defaulttimers.h"
#include "foundation/platform/types.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/job/abortswitch.h"
#include "foundation/utility/stopwatch.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>
#include <memory>

// Forward declarations.
namespace renderer  { class OnFrameBeginRecorder; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Latitude-longitude environment map EDF.
    //
    // Reference:
    //
    //   http://www.cs.virginia.edu/~gfx/courses/2007/ImageSynthesis/assignments/envsample.pdf
    //
    // Light probes:
    //
    //   http://gl.ict.usc.edu/Data/HighResProbes/
    //   http://www.cs.kuleuven.be/~graphics/index.php/environment-maps
    //

    typedef ImageImportanceSampler<Color3f, float> ImageImportanceSamplerType;

    class ImageSampler
    {
      public:
        ImageSampler(
            TextureCache&   texture_cache,
            const Source*   radiance_source,
            const Source*   multiplier_source,
            const Source*   exposure_source,
            const Source*   exposure_multiplier_source,
            const size_t    width,
            const size_t    height)
          : m_texture_cache(texture_cache)
          , m_radiance_source(radiance_source)
          , m_multiplier_source(multiplier_source)
          , m_exposure_source(exposure_source)
          , m_exposure_multiplier_source(exposure_multiplier_source) 
          , m_rcp_width(1.0f / width)
          , m_rcp_height(1.0f / height)
        {
        }

        void sample(const size_t x, const size_t y, Color3f& payload, float& importance)
        {
            if (m_radiance_source == nullptr)
            {
                payload.set(0.0f);
                importance = 0.0f;
                return;
            }

            const Vector2f uv(
                (x + 0.5f) * m_rcp_width,
                1.0f - (y + 0.5f) * m_rcp_height);

            m_radiance_source->evaluate(m_texture_cache, SourceInputs(uv), payload);

            if (is_finite(payload))
            {
                float multiplier;
                m_multiplier_source->evaluate(m_texture_cache, SourceInputs(uv), multiplier);

                float exposure;
                m_exposure_source->evaluate(m_texture_cache, SourceInputs(uv), exposure);

                float exposure_multiplier;
                m_exposure_multiplier_source->evaluate_uniform(exposure_multiplier);

                payload *= multiplier * pow(2.0f, exposure * exposure_multiplier);
                importance = luminance(payload);
            }
            else
            {
                payload.set(0.0f);
                importance = 0.0f;
            }
        }

      private:
        TextureCache&   m_texture_cache;
        const Source*   m_radiance_source;
        const Source*   m_multiplier_source;
        const Source*   m_exposure_source;
        const Source*   m_exposure_multiplier_source;
        const float     m_rcp_width;
        const float     m_rcp_height;
    };

    const char* Model = "latlong_map_environment_edf";

    class LatLongMapEnvironmentEDF
      : public EnvironmentEDF
    {
      public:
        LatLongMapEnvironmentEDF(
            const char*             name,
            const ParamArray&       params)
          : EnvironmentEDF(name, params)
          , m_importance_map_width(0)
          , m_importance_map_height(0)
          , m_probability_scale(0.0f)
        {
            m_inputs.declare("radiance", InputFormatSpectralIlluminance);
            m_inputs.declare("radiance_multiplier", InputFormatFloat, "1.0");
            m_inputs.declare("exposure", InputFormatFloat, "0.0");
            m_inputs.declare("exposure_multiplier", InputFormatFloat, "1.0");

            m_phi_shift = deg_to_rad(m_params.get_optional<float>("horizontal_shift", 0.0f));
            m_theta_shift = deg_to_rad(m_params.get_optional<float>("vertical_shift", 0.0f));
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

            // Do not build an importance map if the environment EDF is not the active one.
            const Environment* environment = project.get_scene()->get_environment();
            if (environment->get_uncached_environment_edf() == this)
            {
                check_non_zero_emission("radiance", "radiance_multiplier");

                if (m_importance_sampler.get() == nullptr)
                    build_importance_map(*project.get_scene(), abort_switch);
            }

            return true;
        }

        void sample(
            const ShadingContext&   shading_context,
            const Vector2f&         s,
            Vector3f&               outgoing,
            Spectrum&               value,
            float&                  probability) const override
        {
            if (m_importance_sampler.get() == nullptr)
            {
                RENDERER_LOG_WARNING(
                    "cannot sample environment edf \"%s\" because it is not bound to the environment.",
                    get_path().c_str());
                value.set(0.0f);
                probability = 0.0f;
                return;
            }

            // Sample the importance map.
            size_t x, y;
            Color3f payload;
            float prob_xy;
            m_importance_sampler->sample(s, x, y, payload, prob_xy);
            assert(prob_xy >= 0.0f);

            // Compute the coordinates in [0,1)^2 of the sample.
            const float jitter_x = frac(s[0] * m_importance_map_width);
            const float jitter_y = frac(s[1] * m_importance_map_height);
            const float u = (x + jitter_x) * m_rcp_importance_map_width;
            const float v = (y + jitter_y) * m_rcp_importance_map_height;
            assert(u >= 0.0f && u < 1.0f);
            assert(v >= 0.0f && v < 1.0f);

            // Compute the spherical coordinates of the sample.
            float theta, phi;
            unit_square_to_angles(u, v, theta, phi);
            shift_angles(theta, phi, m_theta_shift, m_phi_shift);

            // Compute the local space emission direction.
            const float cos_theta = cos(theta);
            const float sin_theta = sin(theta);
            const float cos_phi = cos(phi);
            const float sin_phi = sin(phi);
            const Vector3f local_outgoing =
                Vector3f::make_unit_vector(cos_theta, sin_theta, cos_phi, sin_phi);

            // Transform the emission direction to world space.
            Transformd scratch;
            const Transformd& transform = m_transform_sequence.evaluate(0.0f, scratch);
            outgoing = transform.vector_to_parent(local_outgoing);

            // Return the emitted radiance.
            value.set(payload, g_std_lighting_conditions, Spectrum::Illuminance);

            // Compute the probability density of this direction.
            probability = prob_xy * m_probability_scale / sin_theta;
            assert(probability >= 0.0f);
        }

        void evaluate(
            const ShadingContext&   shading_context,
            const Vector3f&         outgoing,
            Spectrum&               value) const override
        {
            assert(is_normalized(outgoing));

            // Transform the emission direction to local space.
            Transformd scratch;
            const Transformd& transform = m_transform_sequence.evaluate(0.0f, scratch);
            const Vector3f local_outgoing = transform.vector_to_local(outgoing);

            // Compute the spherical coordinates of the outgoing direction.
            float theta, phi;
            unit_vector_to_angles(local_outgoing, theta, phi);
            shift_angles(theta, phi, -m_theta_shift, -m_phi_shift);

            // Convert the spherical coordinates to [0,1]^2.
            float u, v;
            angles_to_unit_square(theta, phi, u, v);

            // Compute and return the environment color.
            lookup_environment_map(shading_context, u, v, value);
        }

        void evaluate(
            const ShadingContext&   shading_context,
            const Vector3f&         outgoing,
            Spectrum&               value,
            float&                  probability) const override
        {
            assert(is_normalized(outgoing));

            if (m_importance_sampler.get() == nullptr)
            {
                RENDERER_LOG_WARNING(
                    "cannot compute pdf for environment edf \"%s\" because it is not bound to the environment.",
                    get_path().c_str());
                value.set(0.0f);
                probability = 0.0f;
                return;
            }

            // Transform the emission direction to local space.
            Transformd scratch;
            const Transformd& transform = m_transform_sequence.evaluate(0.0f, scratch);
            const Vector3f local_outgoing = transform.vector_to_local(outgoing);

            // Compute the spherical coordinates of the outgoing direction.
            float theta, phi;
            unit_vector_to_angles(local_outgoing, theta, phi);
            shift_angles(theta, phi, -m_theta_shift, -m_phi_shift);

            // Convert the spherical coordinates to [0,1]^2.
            float u, v;
            angles_to_unit_square(theta, phi, u, v);

            // Compute and return the environment color and the PDF value.
            lookup_environment_map(shading_context, u, v, value);
            probability = compute_pdf(u, v, theta);
            assert(probability >= 0.0f);
        }

        float evaluate_pdf(
            const Vector3f&         outgoing) const override
        {
            assert(is_normalized(outgoing));

            if (m_importance_sampler.get() == nullptr)
            {
                RENDERER_LOG_WARNING(
                    "cannot compute pdf for environment edf \"%s\" because it is not bound to the environment.",
                    get_path().c_str());
                return 0.0f;
            }

            // Transform the emission direction to local space.
            Transformd scratch;
            const Transformd& transform = m_transform_sequence.evaluate(0.0f, scratch);
            const Vector3f local_outgoing = transform.vector_to_local(outgoing);

            // Compute the spherical coordinates of the outgoing direction.
            float theta, phi;
            unit_vector_to_angles(local_outgoing, theta, phi);
            shift_angles(theta, phi, -m_theta_shift, -m_phi_shift);

            // Convert the spherical coordinates to [0,1]^2.
            float u, v;
            angles_to_unit_square(theta, phi, u, v);

            // Compute and return the PDF value.
            return compute_pdf(u, v, theta);
        }

      private:
        APPLESEED_DECLARE_INPUT_VALUES(InputValues)
        {
            Spectrum    m_radiance;                 // emitted radiance in W.m^-2.sr^-1
            float       m_radiance_multiplier;      // emitted radiance multiplier
            float       m_exposure;                 // emitted radiance multiplier in f-stops
            float       m_exposure_multiplier;      // emitted radiance exposure multiplier
        };

        float   m_phi_shift;                        // horizontal shift in radians
        float   m_theta_shift;                      // vertical shift in radians

        size_t  m_importance_map_width;
        size_t  m_importance_map_height;

        float   m_rcp_importance_map_width;
        float   m_rcp_importance_map_height;
        float   m_probability_scale;

        unique_ptr<ImageImportanceSamplerType> m_importance_sampler;

        void build_importance_map(const Scene& scene, IAbortSwitch*abort_switch)
        {
            Stopwatch<DefaultWallclockTimer> stopwatch;
            stopwatch.start();

            const Source* radiance_source = m_inputs.source("radiance");
            assert(radiance_source);

            const Source::Hints radiance_source_hints = radiance_source->get_hints();
            m_importance_map_width = radiance_source_hints.m_width;
            m_importance_map_height = radiance_source_hints.m_height;

            m_rcp_importance_map_width = 1.0f / m_importance_map_width;
            m_rcp_importance_map_height = 1.0f / m_importance_map_height;

            const size_t texel_count = m_importance_map_width * m_importance_map_height;
            m_probability_scale = texel_count / (2.0f * PiSquare<float>());

            TextureStore texture_store(scene);
            TextureCache texture_cache(texture_store);
            ImageSampler sampler(
                texture_cache,
                radiance_source,
                m_inputs.source("radiance_multiplier"),
                m_inputs.source("exposure"),
                m_inputs.source("exposure_multiplier"),
                m_importance_map_width,
                m_importance_map_height);

            m_importance_sampler.reset(
                new ImageImportanceSamplerType(
                    m_importance_map_width,
                    m_importance_map_height));

            RENDERER_LOG_INFO(
                "building " FMT_SIZE_T "x" FMT_SIZE_T " importance map "
                "for environment edf \"%s\"...",
                m_importance_map_width,
                m_importance_map_height,
                get_path().c_str());

            m_importance_sampler->rebuild(sampler, abort_switch);

            if (is_aborted(abort_switch))
                m_importance_sampler.reset();
            else
            {
                stopwatch.measure();

                RENDERER_LOG_INFO(
                    "built importance map for environment edf \"%s\" in %s.",
                    get_path().c_str(),
                    pretty_time(stopwatch.get_seconds()).c_str());
            }
        }

        void lookup_environment_map(
            const ShadingContext&   shading_context,
            const float             u,
            const float             v,
            Spectrum&               value) const
        {
            assert(u >= 0.0f && u < 1.0f);
            assert(v >= 0.0f && v < 1.0f);

            InputValues values;
            m_inputs.evaluate(shading_context.get_texture_cache(), SourceInputs(Vector2f(u, 1.0f - v)), &values);

            if (is_finite(values.m_radiance))
            {
                value = values.m_radiance;
                value *= values.m_radiance_multiplier * pow(2.0f, values.m_exposure * values.m_exposure_multiplier);
            }
            else value.set(0.0f);
        }

        float compute_pdf(
            const float             u,
            const float             v,
            const float             theta) const
        {
            assert(u >= 0.0f && u < 1.0f);
            assert(v >= 0.0f && v < 1.0f);
            assert(m_importance_sampler.get());

            // Compute the probability density of this sample in the importance map.
            const size_t x = truncate<size_t>(m_importance_map_width * u);
            const size_t y = truncate<size_t>(m_importance_map_height * v);
            const float prob_xy = m_importance_sampler->get_pdf(x, y);
            assert(prob_xy >= 0.0f);

            // Compute the probability density of the emission direction.
            const float pdf = prob_xy > 0.0f ? prob_xy * m_probability_scale / sin(theta) : 0.0f;
            assert(pdf >= 0.0f);

            return pdf;
        }
    };
}


//
// LatLongMapEnvironmentEDFFactory class implementation.
//

void LatLongMapEnvironmentEDFFactory::release()
{
    delete this;
}

const char* LatLongMapEnvironmentEDFFactory::get_model() const
{
    return Model;
}

Dictionary LatLongMapEnvironmentEDFFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Latitude-Longitude Map Environment EDF")
            .insert("help", "Sky dome environment");
}

DictionaryArray LatLongMapEnvironmentEDFFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "radiance")
            .insert("label", "Radiance")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("texture_instance", "Texture Instances"))
            .insert("use", "required")
            .insert("default", "1.0")
            .insert("help", "Environment texture"));

    metadata.push_back(
        Dictionary()
            .insert("name", "radiance_multiplier")
            .insert("label", "Radiance Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("default", "1.0")
            .insert("help", "Environment texture radiance multiplier"));

    metadata.push_back(
        Dictionary()
            .insert("name", "exposure")
            .insert("label", "Exposure")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Texture Instances"))
            .insert("use", "optional")
            .insert("default", "0.0")
            .insert("help", "Environment exposure"));

    metadata.push_back(
        Dictionary()
            .insert("name", "exposure_multiplier")
            .insert("label", "Exposure Multiplier")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "-64.0")
                    .insert("type", "soft"))
            .insert("max",
                Dictionary()
                    .insert("value", "64.0")
                    .insert("type", "soft"))
            .insert("default", "1.0")
            .insert("use", "optional")
            .insert("help", "Environment exposure multiplier"));

    metadata.push_back(
        Dictionary()
            .insert("name", "horizontal_shift")
            .insert("label", "Horizontal Shift")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "-360.0")
                    .insert("type", "soft"))
            .insert("max",
                Dictionary()
                    .insert("value", "360.0")
                    .insert("type", "soft"))
            .insert("default", "0.0")
            .insert("use", "optional")
            .insert("help", "Environment texture horizontal shift in degrees"));

    metadata.push_back(
        Dictionary()
            .insert("name", "vertical_shift")
            .insert("label", "Vertical Shift")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "-360.0")
                    .insert("type", "soft"))
            .insert("max",
                Dictionary()
                    .insert("value", "360.0")
                    .insert("type", "soft"))
            .insert("default", "0.0")
            .insert("use", "optional")
            .insert("help", "Environment texture vertical shift in degrees"));

    return metadata;
}

auto_release_ptr<EnvironmentEDF> LatLongMapEnvironmentEDFFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<EnvironmentEDF>(
            new LatLongMapEnvironmentEDF(name, params));
}

}   // namespace renderer
