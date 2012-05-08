
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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
#include "renderer/kernel/lighting/imageimportancesampler.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/input/inputparams.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/input/texturesource.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/texture/texture.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/colorspace.h"
#include "foundation/math/sampling.h"

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
    //   http://www.debevec.org/probes/
    //   http://gl.ict.usc.edu/Data/HighResProbes/
    //   http://www.cs.kuleuven.be/~graphics/index.php/environment-maps
    //

    class ImageSampler
    {
      public:
        ImageSampler(
            TextureCache&   texture_cache,
            Source*         source,
            const size_t    width,
            const size_t    height,
            const double    u_shift,
            const double    v_shift)
          : m_texture_cache(texture_cache)
          , m_source(source)
          , m_rcp_width(1.0 / width)
          , m_rcp_height(1.0 / height)
          , m_u_shift(u_shift)
          , m_v_shift(v_shift)
          , m_out_of_range_luminance_error_count(0)
        {
        }

        double operator()(const size_t x, const size_t y)
        {
            if (m_source == 0)
                return 0.0;

            InputParams input_params;
            input_params.m_uv[0] = x * m_rcp_width + m_u_shift;
            input_params.m_uv[1] = 1.0 - y * m_rcp_height + m_v_shift;

            Color3f linear_rgb;
            Alpha alpha;

            m_source->evaluate(
                m_texture_cache,
                input_params,
                linear_rgb,
                alpha);

            const double MaxLuminance = 1.0e4;

            double lum = static_cast<double>(luminance(linear_rgb));

            if (lum < 0.0)
            {
                lum = 0.0;
                ++m_out_of_range_luminance_error_count;
            }

            if (lum > MaxLuminance)
            {
                lum = MaxLuminance;
                ++m_out_of_range_luminance_error_count;
            }

            return lum;
        }

        size_t get_out_of_range_luminance_error_count() const
        {
            return m_out_of_range_luminance_error_count;
        }

      private:
        TextureCache&   m_texture_cache;
        Source*         m_source;
        const double    m_rcp_width;
        const double    m_rcp_height;
        const double    m_u_shift;
        const double    m_v_shift;
        size_t          m_out_of_range_luminance_error_count;
    };

    const char* Model = "latlong_map_environment_edf";

    class LatLongMapEnvironmentEDF
      : public EnvironmentEDF
    {
      public:
        LatLongMapEnvironmentEDF(
            const char*         name,
            const ParamArray&   params)
          : EnvironmentEDF(name, params)
          , m_importance_map_width(0)
          , m_importance_map_height(0)
          , m_probability_scale(0.0)
        {
            m_inputs.declare("exitance", InputFormatSpectrum);

            m_u_shift = m_params.get_optional<double>("horizontal_shift", 0.0) / 360.0;
            m_v_shift = m_params.get_optional<double>("vertical_shift", 0.0) / 360.0;
        }

        virtual void release()
        {
            delete this;
        }

        virtual const char* get_model() const
        {
            return Model;
        }

        virtual void on_frame_begin(const Project& project)
        {
            EnvironmentEDF::on_frame_begin(project);

            if (m_importance_sampler.get() == 0)
                build_importance_map(*project.get_scene());
        }

        virtual void sample(
            InputEvaluator&     input_evaluator,
            const Vector2d&     s,
            Vector3d&           outgoing,
            Spectrum&           value,
            double&             probability) const
        {
            // Sample the importance map.
            size_t x, y;
            double prob_xy;
            m_importance_sampler->sample(s, x, y, prob_xy);

            // Compute the coordinates in [0,1]^2 of the sample.
            const double u = (2.0 * x + 1.0) / (2.0 * m_importance_map_width);
            const double v = (2.0 * y + 1.0) / (2.0 * m_importance_map_height);

            double theta, phi;
            unit_square_to_angles(u, v, theta, phi);

            // Compute the world space emission direction.
            outgoing = Vector3d::unit_vector(theta, phi);

            lookup_environment_map(input_evaluator, u, v, value);

            probability = prob_xy * m_probability_scale / sin(theta);
        }

        virtual void evaluate(
            InputEvaluator&     input_evaluator,
            const Vector3d&     outgoing,
            Spectrum&           value) const
        {
            assert(is_normalized(outgoing));

            double theta, phi;
            unit_vector_to_angles(outgoing, theta, phi);

            double u, v;
            angles_to_unit_square(theta, phi, u, v);

            lookup_environment_map(input_evaluator, u, v, value);
        }

        virtual void evaluate(
            InputEvaluator&     input_evaluator,
            const Vector3d&     outgoing,
            Spectrum&           value,
            double&             probability) const
        {
            assert(is_normalized(outgoing));

            double theta, phi;
            unit_vector_to_angles(outgoing, theta, phi);

            double u, v;
            angles_to_unit_square(theta, phi, u, v);

            lookup_environment_map(input_evaluator, u, v, value);

            probability = compute_pdf(u, v, theta);
        }

        virtual double evaluate_pdf(
            InputEvaluator&     input_evaluator,
            const Vector3d&     outgoing) const
        {
            assert(is_normalized(outgoing));

            double theta, phi;
            unit_vector_to_angles(outgoing, theta, phi);

            double u, v;
            angles_to_unit_square(theta, phi, u, v);

            return compute_pdf(u, v, theta);
        }

      private:
        struct InputValues
        {
            Spectrum    m_exitance;
            Alpha       m_exitance_alpha;   // unused
        };

        typedef ImageImportanceSampler<double> ImageImportanceSamplerType;

        double                                  m_u_shift;
        double                                  m_v_shift;

        size_t                                  m_importance_map_width;
        size_t                                  m_importance_map_height;
        double                                  m_probability_scale;
        auto_ptr<ImageImportanceSamplerType>    m_importance_sampler;

        // Compute the spherical coordinates of a given direction.
        static void unit_vector_to_angles(
            const Vector3d&     v,          // unit length
            double&             theta,      // in [0, Pi]
            double&             phi)        // in [-Pi, Pi]
        {
            assert(is_normalized(v));

            theta = acos(v[1]);
            phi = atan2(v[2], v[0]);
        }

        // Convert a given direction from spherical coordinates to [0,1]^2.
        static void angles_to_unit_square(
            const double        theta,      // in [0, Pi]
            const double        phi,        // in [-Pi, Pi]
            double&             u,          // in [0, 1]
            double&             v)          // in [0, 1]
        {
            assert(theta >= 0.0);
            assert(theta <= Pi);
            assert(phi >= -Pi);
            assert(phi <= Pi);

            u = (0.5 / Pi) * (phi + Pi);
            v = (1.0 / Pi) * theta;
        }

        // Convert a given direction from [0,1]^2 to spherical coordinates.
        static void unit_square_to_angles(
            const double        u,          // in [0, 1]
            const double        v,          // in [0, 1]
            double&             theta,      // in [0, Pi]
            double&             phi)        // in [-Pi, Pi]
        {
            assert(u >= 0.0);
            assert(u <= 1.0);
            assert(v >= 0.0);
            assert(v <= 1.0);

            theta = Pi * v;
            phi = Pi * (2.0 * u - 1.0);
        }

        void build_importance_map(const Scene& scene)
        {
            const TextureSource* exitance_source =
                dynamic_cast<const TextureSource*>(m_inputs.source("exitance"));

            if (exitance_source)
            {
                const CanvasProperties& texture_props = exitance_source->get_texture(scene).properties();
                m_importance_map_width = texture_props.m_canvas_width;
                m_importance_map_height = texture_props.m_canvas_height;
            }
            else
            {
                m_importance_map_width = 1;
                m_importance_map_height = 1;
            }

            const size_t texel_count = m_importance_map_width * m_importance_map_height;
            m_probability_scale = texel_count / (2.0 * Pi * Pi);

            TextureCache texture_cache(scene);
            ImageSampler sampler(
                texture_cache,
                m_inputs.source("exitance"),
                m_importance_map_width,
                m_importance_map_height,
                m_u_shift,
                m_v_shift);

            RENDERER_LOG_INFO(
                "building " FMT_SIZE_T "x" FMT_SIZE_T " importance map "
                "for environment edf \"%s\"...",
                m_importance_map_width,
                m_importance_map_height,
                get_name());

            m_importance_sampler.reset(
                new ImageImportanceSamplerType(
                    m_importance_map_width,
                    m_importance_map_height,
                    sampler));

            const size_t oor_error_count = sampler.get_out_of_range_luminance_error_count();

            if (oor_error_count > 0)
            {
                RENDERER_LOG_WARNING(
                    "while building importance map for environment edf \"%s\": "
                    "found %s pixel%s with out-of-range luminance; rendering artifacts are to be expected.",
                    get_name(),
                    pretty_uint(oor_error_count).c_str(),
                    oor_error_count > 1 ? "s" : "");
            }

            RENDERER_LOG_INFO(
                "built importance map for environment edf \"%s\".",
                get_name());
        }

        void lookup_environment_map(
            InputEvaluator&     input_evaluator,
            const double        u,
            const double        v,
            Spectrum&           value) const
        {
            InputParams input_params;
            input_params.m_uv[0] = u + m_u_shift;
            input_params.m_uv[1] = 1.0 - v + m_v_shift;

            const InputValues* values =
                input_evaluator.evaluate<InputValues>(m_inputs, input_params);

            value = values->m_exitance;
        }

        double compute_pdf(
            const double        u,
            const double        v,
            const double        theta) const
        {
            // Compute the probability density of this sample in the importance map.
            const size_t x = truncate<size_t>(m_importance_map_width * u);
            const size_t y = truncate<size_t>(m_importance_map_height * v);
            const double prob_xy = m_importance_sampler->get_pdf(x, y);

            // Compute the probability density of the emission direction.
            return prob_xy * m_probability_scale / sin(theta);
        }
    };
}


//
// LatLongMapEnvironmentEDFFactory class implementation.
//

const char* LatLongMapEnvironmentEDFFactory::get_model() const
{
    return Model;
}

const char* LatLongMapEnvironmentEDFFactory::get_human_readable_model() const
{
    return "Latitude-Longitude Map Environment EDF";
}

DictionaryArray LatLongMapEnvironmentEDFFactory::get_widget_definitions() const
{
    DictionaryArray definitions;

    definitions.push_back(
        Dictionary()
            .insert("name", "exitance")
            .insert("label", "Exitance")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary()
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", ""));

    definitions.push_back(
        Dictionary()
            .insert("name", "horizontal_shift")
            .insert("label", "Horizontal Shift")
            .insert("widget", "text_box")
            .insert("default", "0.0")
            .insert("use", "optional"));

    definitions.push_back(
        Dictionary()
            .insert("name", "vertical_shift")
            .insert("label", "Vertical Shift")
            .insert("widget", "text_box")
            .insert("default", "0.0")
            .insert("use", "optional"));

    return definitions;
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
