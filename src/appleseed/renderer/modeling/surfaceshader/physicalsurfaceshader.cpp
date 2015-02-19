
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "physicalsurfaceshader.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/aov/aovsettings.h"
#include "renderer/kernel/aov/imagestack.h"
#include "renderer/kernel/aov/shadingfragmentstack.h"
#include "renderer/kernel/aov/spectrumstack.h"
#include "renderer/kernel/lighting/ilightingengine.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingfragment.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/environmentshader/environmentshader.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/colorspace.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

// Standard headers.
#include <algorithm>
#include <cmath>
#include <cstddef>

// Forward declarations.
namespace renderer  { class Assembly; }
namespace renderer  { class PixelContext; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Physical surface shader for physically-based rendering.
    //

    const char* Model = "physical_surface_shader";

    class PhysicalSurfaceShader
      : public SurfaceShader
    {
      public:
        PhysicalSurfaceShader(
            const char*             name,
            const ParamArray&       params)
          : SurfaceShader(name, params)
          , m_lighting_conditions(IlluminantCIED65, XYZCMFCIE196410Deg)
        {
            m_inputs.declare("color_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("alpha_multiplier", InputFormatScalar, "1.0");
            m_inputs.declare("translucency", InputFormatSpectralReflectance, "0.0");
            m_inputs.declare("aerial_persp_sky_color", InputFormatSpectralIlluminance, "");

            const string aerial_persp_mode = m_params.get_optional<string>("aerial_persp_mode", "none");
            if (aerial_persp_mode == "none")
                m_aerial_persp_mode = AerialPerspNone;
            else if (aerial_persp_mode == "environment_shader")
                m_aerial_persp_mode = AerialPerspEnvironmentShader;
            else if (aerial_persp_mode == "sky_color")
                m_aerial_persp_mode = AerialPerspSkyColor;
            else
            {
                RENDERER_LOG_ERROR(
                    "invalid value \"%s\" for parameter \"aerial_persp_mode\", "
                    "using default value \"none\".",
                    aerial_persp_mode.c_str());
                m_aerial_persp_mode = AerialPerspNone;
            }

            m_aerial_persp_rcp_distance = 1.0 / m_params.get_optional<double>("aerial_persp_distance", 1000.0);
            m_aerial_persp_intensity = m_params.get_optional<double>("aerial_persp_intensity", 0.01);

            m_front_lighting_samples = m_params.get_optional<size_t>("front_lighting_samples", 1);
            m_back_lighting_samples = m_params.get_optional<size_t>("back_lighting_samples", 1);
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
            const Assembly&         assembly,
            IAbortSwitch*           abort_switch) APPLESEED_OVERRIDE
        {
            const ImageStack& aov_images = project.get_frame()->aov_images();

            for (size_t i = 0; i < aov_images.size(); ++i)
                m_is_contribution_aov[i] = aov_images.get_type(i) == ImageStack::ContributionType;

            return true;
        }

        virtual void evaluate(
            SamplingContext&        sampling_context,
            const PixelContext&     pixel_context,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            ShadingResult&          shading_result) const APPLESEED_OVERRIDE
        {
            // Evaluate the shader inputs.
            InputValues values;
            m_inputs.evaluate(
                shading_context.get_texture_cache(),
                shading_point.get_uv(0),
                &values);

            Spectrum radiance;
            SpectrumStack aovs(shading_result.m_aovs.size());

            // Compute front lighting.
            compute_front_lighting(
                values,
                sampling_context,
                pixel_context,
                shading_context,
                shading_point,
                radiance,
                aovs);

            // Optionally simulate translucency by adding back lighting.
            if (!is_zero(values.m_translucency))
            {
                add_back_lighting(
                    values,
                    sampling_context,
                    pixel_context,
                    shading_context,
                    shading_point,
                    radiance,
                    aovs);
            }

            // Initialize the shading result.
            shading_result.m_color_space = ColorSpaceSpectral;
            shading_result.m_main.m_color = radiance;
            shading_result.m_aovs.m_color = aovs;

            // Set alpha channel of AOVs.
            for (size_t i = 0; i < aovs.size(); ++i)
            {
                shading_result.m_aovs[i].m_alpha =
                    m_is_contribution_aov[i]
                        ? shading_result.m_main.m_alpha
                        : Alpha(0.0f);
            }

            // Apply multipliers.
            shading_result.m_main.m_color *= static_cast<float>(values.m_color_multiplier);
            shading_result.m_main.m_alpha *= static_cast<float>(values.m_alpha_multiplier);
            shading_result.m_aovs.m_color *= static_cast<float>(values.m_color_multiplier);
            shading_result.m_aovs.m_alpha *= static_cast<float>(values.m_alpha_multiplier);

            // Optionally apply fake aerial perspective.
            if (m_aerial_persp_mode != AerialPerspNone)
            {
                apply_aerial_perspective(
                    values,
                    shading_context,
                    shading_point,
                    shading_result);
            }
        }

      private:
        APPLESEED_DECLARE_INPUT_VALUES(InputValues)
        {
            double      m_color_multiplier;
            double      m_alpha_multiplier;
            Spectrum    m_translucency;
            Spectrum    m_aerial_persp_sky_color;
        };

        enum AerialPerspMode
        {
            AerialPerspNone,
            AerialPerspEnvironmentShader,
            AerialPerspSkyColor
        };

        const LightingConditions    m_lighting_conditions;
        AerialPerspMode             m_aerial_persp_mode;
        double                      m_aerial_persp_rcp_distance;
        double                      m_aerial_persp_intensity;
        size_t                      m_front_lighting_samples;
        size_t                      m_back_lighting_samples;
        bool                        m_is_contribution_aov[MaxAOVCount];

        void compute_front_lighting(
            const InputValues&      values,
            SamplingContext&        sampling_context,
            const PixelContext&     pixel_context,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            Spectrum&               radiance,
            SpectrumStack&          aovs) const
        {
            radiance.set(0.0f);
            aovs.set(0.0f);

            for (size_t i = 0; i < m_front_lighting_samples; ++i)
            {
                shading_context.get_lighting_engine()->compute_lighting(
                    sampling_context,
                    pixel_context,
                    shading_context,
                    shading_point,
                    radiance,
                    aovs);
            }

            if (m_front_lighting_samples > 1)
            {
                const float rcp_sample_count = 1.0f / static_cast<float>(m_front_lighting_samples);
                radiance *= rcp_sample_count;
                aovs *= rcp_sample_count;
            }
        }

        void add_back_lighting(
            const InputValues&      values,
            SamplingContext&        sampling_context,
            const PixelContext&     pixel_context,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            Spectrum&               radiance,
            SpectrumStack&          aovs) const
        {
            const Vector3d& p = shading_point.get_point();
            const Vector3d& n = shading_point.get_original_shading_normal();
            const Vector3d& d = shading_point.get_ray().m_dir;

            // Construct a ray perpendicular to the other side of the surface.
            ShadingRay back_ray(shading_point.get_ray());
            back_ray.m_tmax *= norm(d);
            back_ray.m_dir = dot(d, n) > 0.0 ? -n : n;
            back_ray.m_org = p - back_ray.m_tmax * back_ray.m_dir;

            ShadingPoint back_shading_point(shading_point);
            back_shading_point.set_ray(back_ray);

            Spectrum back_radiance(0.0f);
            SpectrumStack back_aovs(aovs.size(), 0.0f);

            // Compute back lighting.
            for (size_t i = 0; i < m_back_lighting_samples; ++i)
            {
                shading_context.get_lighting_engine()->compute_lighting(
                    sampling_context,
                    pixel_context,
                    shading_context,
                    back_shading_point,
                    back_radiance,
                    back_aovs);
            }

            // Apply translucency factor.
            back_radiance *= values.m_translucency;
            back_aovs *= values.m_translucency;

            // Divide by the number of samples.
            const float rcp_sample_count = 1.0f / static_cast<float>(m_back_lighting_samples);
            back_radiance *= rcp_sample_count;
            back_aovs *= rcp_sample_count;

            // Add back lighting contribution.
            radiance += back_radiance;
            aovs += back_aovs;
        }

        void apply_aerial_perspective(
            const InputValues&      values,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            ShadingResult&          shading_result) const
        {
            Spectrum sky_color;

            if (m_aerial_persp_mode == AerialPerspSkyColor)
                sky_color = values.m_aerial_persp_sky_color;
            else
            {
                // Retrieve the environment shader of the scene.
                const Scene& scene = shading_point.get_scene();
                const EnvironmentShader* environment_shader =
                    scene.get_environment()->get_environment_shader();

                if (environment_shader)
                {
                    // Execute the environment shader to obtain the sky color in the direction of the ray.
                    InputEvaluator input_evaluator(shading_context.get_texture_cache());
                    const ShadingRay& ray = shading_point.get_ray();
                    const Vector3d direction = normalize(ray.m_dir);
                    ShadingResult sky;
                    environment_shader->evaluate(shading_context, input_evaluator, direction, sky);
                    sky_color = sky.m_main.m_color;
                }
                else sky_color.set(0.0f);
            }

            // Compute the blend factor.
            const double d = shading_point.get_distance() * m_aerial_persp_rcp_distance;
            const double k = m_aerial_persp_intensity * exp(d);
            const double blend = min(k, 1.0);

            // Blend the shading result and the sky color.
            sky_color *= static_cast<float>(blend);
            shading_result.m_main.m_color *= static_cast<float>(1.0 - blend);
            shading_result.m_main.m_color += sky_color;
        }
    };
}


//
// PhysicalSurfaceShaderFactory class implementation.
//

const char* PhysicalSurfaceShaderFactory::get_model() const
{
    return Model;
}

Dictionary PhysicalSurfaceShaderFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Physical")
            .insert("default_model", "true");
}

DictionaryArray PhysicalSurfaceShaderFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "color_multiplier")
            .insert("label", "Color Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("default", "1.0")
            .insert("use", "optional"));

    metadata.push_back(
        Dictionary()
            .insert("name", "alpha_multiplier")
            .insert("label", "Alpha Multiplier")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("default", "1.0")
            .insert("use", "optional"));

    metadata.push_back(
        Dictionary()
            .insert("name", "translucency")
            .insert("label", "Translucency")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("default", "0.0")
            .insert("use", "optional"));

    metadata.push_back(
        Dictionary()
            .insert("name", "front_lighting_samples")
            .insert("label", "Front Lighting Samples")
            .insert("type", "text")
            .insert("default", "1")
            .insert("use", "optional"));

    metadata.push_back(
        Dictionary()
            .insert("name", "back_lighting_samples")
            .insert("label", "Back Lighting Samples")
            .insert("type", "text")
            .insert("default", "1")
            .insert("use", "optional"));

    metadata.push_back(
        Dictionary()
            .insert("name", "aerial_persp_mode")
            .insert("label", "Aerial Perspective Mode")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("None", "none")
                    .insert("Use Environment Shader", "environment_shader")
                    .insert("Use Sky Color", "sky_color"))
            .insert("default", "none")
            .insert("use", "optional"));

    metadata.push_back(
        Dictionary()
            .insert("name", "aerial_persp_sky_color")
            .insert("label", "Aerial Perspective Sky Color")
            .insert("type", "colormap")
            .insert("entity_types", Dictionary().insert("color", "Colors"))
            .insert("default", "0.5")
            .insert("use", "optional"));

    metadata.push_back(
        Dictionary()
            .insert("name", "aerial_persp_distance")
            .insert("label", "Aerial Perspective Distance")
            .insert("type", "text")
            .insert("default", "1000.0")
            .insert("use", "optional"));

    metadata.push_back(
        Dictionary()
            .insert("name", "aerial_persp_intensity")
            .insert("label", "Aerial Perspective Intensity")
            .insert("type", "text")
            .insert("default", "0.01")
            .insert("use", "optional"));

    return metadata;
}

auto_release_ptr<SurfaceShader> PhysicalSurfaceShaderFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<SurfaceShader>(
            new PhysicalSurfaceShader(name, params));
}

}   // namespace renderer
