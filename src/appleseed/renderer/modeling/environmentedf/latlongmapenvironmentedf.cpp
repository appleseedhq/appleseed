
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "renderer/modeling/scene/containers.h"
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
            TextureSource*  source,
            const size_t    width,
            const size_t    height)
          : m_texture_cache(texture_cache)
          , m_source(source)
          , m_rcp_width(1.0 / width)
          , m_rcp_height(1.0 / height)
        {
        }

        double operator()(const size_t x, const size_t y) const
        {
            if (m_source == 0)
                return 0.0;

            InputParams input_params;
            input_params.m_uv[0] = x * m_rcp_width;
            input_params.m_uv[1] = 1.0 - y * m_rcp_height;

            Color3f linear_rgb;
            Alpha alpha;

            m_source->evaluate(
                m_texture_cache,
                input_params,
                linear_rgb,
                alpha);

            return static_cast<double>(luminance(linear_rgb));
        }

      private:
        TextureCache&   m_texture_cache;
        TextureSource*  m_source;
        const double    m_rcp_width;
        const double    m_rcp_height;
    };

    class LatLongMapEnvironmentEDF
      : public EnvironmentEDF
    {
      public:
        LatLongMapEnvironmentEDF(
            const char*         name,
            const ParamArray&   params)
          : EnvironmentEDF(params)
          , m_name(name)
          , m_importance_map_width(0)
          , m_importance_map_height(0)
          , m_probability_scale(0.0)
        {
            m_inputs.declare("exitance", InputFormatSpectrum);
        }

        virtual void release()
        {
            delete this;
        }

        virtual const char* get_name() const
        {
            return m_name.c_str();
        }

        virtual const char* get_model() const
        {
            return LatLongMapEnvironmentEDFFactory::get_model();
        }

        virtual void on_frame_begin(const Scene& scene)
        {
            if (m_importance_sampler.get() == 0)
                build_importance_map(scene);
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

        typedef ImageImportanceSampler<double, ImageSampler> ImageImportanceSampler;

        const string                        m_name;
        size_t                              m_importance_map_width;
        size_t                              m_importance_map_height;
        double                              m_probability_scale;
        auto_ptr<ImageImportanceSampler>    m_importance_sampler;

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
            TextureSource* exitance =
                dynamic_cast<TextureSource*>(m_inputs.source("exitance"));

            if (exitance == 0)
            {
                m_importance_map_width = 512;
                m_importance_map_height = 256;

                RENDERER_LOG_ERROR(
                    "while building importance map for environment edf \"%s\": "
                    "the input \"%s\" is not bound to a texture, using default "
                    "importance map resolution " FMT_SIZE_T "x" FMT_SIZE_T,
                    m_name.c_str(),
                    "exitance",
                    m_importance_map_width,
                    m_importance_map_height);
            }
            else
            {
                const TextureInstance* texture_instance =
                    get_required_entity<TextureInstance>(
                        scene.texture_instances(),
                        m_params,
                        "exitance");

                Texture* texture =
                    scene.textures().get(texture_instance->get_texture_index());

                const CanvasProperties& texture_props = texture->properties();

                m_importance_map_width = texture_props.m_canvas_width;
                m_importance_map_height = texture_props.m_canvas_height;
            }

            const size_t texel_count = m_importance_map_width * m_importance_map_height;
            m_probability_scale = texel_count / (2.0 * Pi * Pi);

            TextureCache texture_cache(scene, 1024 * 1024);
            ImageSampler sampler(
                texture_cache,
                exitance,
                m_importance_map_width,
                m_importance_map_height);

            RENDERER_LOG_INFO(
                "building " FMT_SIZE_T "x" FMT_SIZE_T " importance map "
                "for environment edf \"%s\"...",
                m_importance_map_width,
                m_importance_map_height,
                m_name.c_str());

            m_importance_sampler.reset(
                new ImageImportanceSampler(
                    m_importance_map_width,
                    m_importance_map_height,
                    sampler));

            RENDERER_LOG_INFO(
                "built importance map for environment edf \"%s\"",
                m_name.c_str());
        }

        void lookup_environment_map(
            InputEvaluator&     input_evaluator,
            const double        u,
            const double        v,
            Spectrum&           value) const
        {
            InputParams input_params;
            input_params.m_uv[0] = u;
            input_params.m_uv[1] = 1.0 - v;

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

}   // anonymous namespace


//
// LatLongMapEnvironmentEDFFactory class implementation.
//

const char* LatLongMapEnvironmentEDFFactory::get_model()
{
    return "latlong_map_environment_edf";
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
