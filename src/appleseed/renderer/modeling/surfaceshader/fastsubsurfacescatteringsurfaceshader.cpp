
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "fastsubsurfacescatteringsurfaceshader.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/aov/shadingfragmentstack.h"
#include "renderer/kernel/aov/spectrumstack.h"
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/lighting/lightsampler.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingfragment.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/math/basis.h"
#include "foundation/math/sampling.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/foreach.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <limits>
#include <memory>

// Forward declarations.
namespace foundation    { class AbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class PixelContext; }
namespace renderer      { class Scene; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Fast subsurface scattering surface shader.
    //
    // References:
    //
    //   http://publications.dice.se/attachments/Colin_BarreBrisebois_Programming_ApproximatingTranslucency.pdf
    //   http://publications.dice.se/attachments/Colin_BarreBrisebois_Programming_ApproximatingTranslucency.pptx
    //

    const char* Model = "fast_sss_surface_shader";

    class FastSubSurfaceScatteringSurfaceShader
      : public SurfaceShader
    {
      public:
        FastSubSurfaceScatteringSurfaceShader(
            const char*             name,
            const ParamArray&       params)
          : SurfaceShader(name, params)
          , m_light_samples(m_params.get_required<size_t>("light_samples", 1))
          , m_occlusion_samples(m_params.get_required<size_t>("occlusion_samples", 1))
        {
            m_inputs.declare("scale", InputFormatScalar);
            m_inputs.declare("ambient_sss", InputFormatScalar);
            m_inputs.declare("view_dep_sss", InputFormatScalar);
            m_inputs.declare("diffuse", InputFormatScalar);
            m_inputs.declare("power", InputFormatScalar);
            m_inputs.declare("distortion", InputFormatScalar);
            m_inputs.declare("albedo", InputFormatSpectralReflectance);
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
            const Project&          project,
            const Assembly&         assembly,
            AbortSwitch*            abort_switch) OVERRIDE
        {
            if (!SurfaceShader::on_frame_begin(project, assembly, abort_switch))
                return false;

            const Scene& scene = *project.get_scene();

            m_light_sampler.reset(new LightSampler(scene));

            return true;
        }

        virtual void evaluate(
            SamplingContext&        sampling_context,
            const PixelContext&     pixel_context,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            ShadingResult&          shading_result) const OVERRIDE
        {
            // Initialize the shading result to opaque black.
            shading_result.m_color_space = ColorSpaceSpectral;
            shading_result.m_main.m_color.set(0.0f);
            shading_result.m_main.m_alpha.set(1.0f);
            shading_result.m_aovs.m_color.set(0.0f);
            shading_result.m_aovs.m_alpha.set(0.0f);

            // Return black if there is no light in the scene.
            if (!m_light_sampler->has_lights_or_emitting_triangles())
                return;

            // Evaluate the shader inputs.
            InputValues values;
            m_inputs.evaluate(
                shading_context.get_texture_cache(),
                shading_point.get_uv(0),
                &values);

            // Retrieve the intersection point, the shading normal and the camera direction.
            const Vector3d& point = shading_point.get_point();
            const Vector3d& shading_normal = shading_point.get_shading_normal();
            const Vector3d inv_shading_normal = -shading_normal;
            const Vector3d camera_vec = normalize(-shading_point.get_ray().m_dir);                                          // toward camera

            // todo: there are possible correlation artifacts since the sampling_context
            // object is forked twice from there: once to compute the average occluder
            // distance sampler and once by the light sampler.

            // Compute the average occluder distance.
            const double avg_distance =
                compute_average_distance(
                    sampling_context,
                    shading_context.get_intersector(),
                    -shading_point.get_geometric_normal(),
                    Basis3d(inv_shading_normal),
                    m_occlusion_samples,
                    shading_point);
            assert(avg_distance >= 0.0);

            // Compute the total SSS contribution.
            const double K = 2.99573227;                                                                                    // -ln(0.05)
            const double sss_contrib = exp(-(avg_distance / values.m_scale) * K);

            sampling_context.split_in_place(3, m_light_samples);

            for (size_t i = 0; i < m_light_samples; ++i)
            {
                // Sample the light sources.
                LightSample light_sample;
                m_light_sampler->sample(
                    shading_point.get_time(),
                    sampling_context.next_vector2<3>(),
                    light_sample);

                // Compute the contribution of this light sample.
                const Vector3d light_vec = normalize(light_sample.m_point - point);                                         // toward light
                const Vector3d distorted_light_vec = normalize(light_vec + values.m_distortion * inv_shading_normal);       // normalize() not strictly necessary
                const double dot_nl = saturate(dot(shading_normal, light_vec));                                             // dot(N, L): diffuse lighting
                const double dot_vl = saturate(dot(camera_vec, -distorted_light_vec));                                      // dot(V, -L): view-dependent SSS
                const double view_dep = pow(dot_vl, values.m_power);
                const double sample_contrib =
                    (values.m_ambient_sss + view_dep * values.m_view_dep_sss) * sss_contrib +                               // subsurface scattering
                    dot_nl * values.m_diffuse;                                                                              // diffuse lighting

                Spectrum radiance;
                if (light_sample.m_triangle)
                {
                    // Evaluate the EDF's inputs.
                    InputEvaluator input_evaluator(shading_context.get_texture_cache());
                    const void* edf_data =
                        input_evaluator.evaluate(
                            light_sample.m_triangle->m_edf->get_inputs(),
                            light_sample.m_bary);

                    // Evaluate the EDF.
                    light_sample.m_triangle->m_edf->evaluate(
                        edf_data,
                        light_sample.m_geometric_normal,
                        Basis3d(light_sample.m_shading_normal),
                        -light_vec,
                        radiance);
                }
                else
                {
                    // Evaluate the light.
                    InputEvaluator input_evaluator(shading_context.get_texture_cache());
                    Vector3d sample_position, emission_direction;
                    light_sample.m_light->evaluate(
                        input_evaluator,
                        light_sample.m_light_transform.point_to_local(point),
                        sample_position,
                        emission_direction,
                        radiance);

                    // todo: transform sample_position and emission_direction
                    // to world space when they will be needed for shadowing.
                }

                // todo: missing shadowing term.

                // Compute and accumulate the contribution of this light sample.
                Spectrum result = values.m_albedo;
                result *= radiance;
                result *= static_cast<float>(sample_contrib);
                shading_result.m_main.m_color += result;
            }

            // Normalize the result.
            if (m_light_samples > 1)
                shading_result.m_main.m_color /= static_cast<float>(m_light_samples);
        }

      private:
        const size_t            m_light_samples;
        const size_t            m_occlusion_samples;
        auto_ptr<LightSampler>  m_light_sampler;

        DECLARE_INPUT_VALUES(InputValues)
        {
            double      m_scale;            // distance at which light absorption reaches 95%
            double      m_ambient_sss;
            double      m_view_dep_sss;
            double      m_diffuse;
            double      m_power;
            double      m_distortion;
            Spectrum    m_albedo;
            Alpha       m_alpha;
        };

        static double compute_average_distance(
            const SamplingContext&  sampling_context,
            const Intersector&      intersector,
            const Vector3d&         geometric_normal,
            const Basis3d&          shading_basis,
            const size_t            sample_count,
            const ShadingPoint&     parent_shading_point)
        {
            // Create a sampling context.
            SamplingContext child_sampling_context = sampling_context.split(2, sample_count);

            // Construct an ambient occlusion ray.
            ShadingRay ray;
            ray.m_tmin = 0.0;
            ray.m_tmax = numeric_limits<double>::max();
            ray.m_time = parent_shading_point.get_ray().m_time;
            ray.m_type = ShadingRay::ProbeRay;
            ray.m_depth = parent_shading_point.get_ray().m_depth + 1;

            size_t computed_samples = 0;
            double average_distance = 0.0;

            for (size_t i = 0; i < sample_count; ++i)
            {
                // Generate a cosine-weighted direction over the unit hemisphere.
                const Vector2d s = child_sampling_context.next_vector2<2>();
                ray.m_dir = sample_hemisphere_cosine(s);

                // Transform the direction to world space.
                ray.m_dir = shading_basis.transform_to_parent(ray.m_dir);

                // Don't cast rays on or below the geometric surface.
                if (dot(ray.m_dir, geometric_normal) <= 0.0)
                    continue;

                // Compute the ray origin.
                ray.m_org = parent_shading_point.get_biased_point(ray.m_dir);

                // Count the number of computed samples.
                ++computed_samples;

                // Trace the ambient occlusion ray and update the accumulated hit distance.
                ShadingPoint shading_point;
                if (intersector.trace(ray, shading_point, &parent_shading_point))
                    average_distance += shading_point.get_distance();
            }

            // Compute occlusion as a scalar between 0.0 and 1.0.
            if (computed_samples > 1)
                average_distance /= computed_samples;

            return average_distance;
        }
    };
}


//
// FastSubSurfaceScatteringSurfaceShaderFactory class implementation.
//

const char* FastSubSurfaceScatteringSurfaceShaderFactory::get_model() const
{
    return Model;
}

const char* FastSubSurfaceScatteringSurfaceShaderFactory::get_human_readable_model() const
{
    return "Fast Subsurface Scattering (experimental)";
}

DictionaryArray FastSubSurfaceScatteringSurfaceShaderFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "scale")
            .insert("label", "Geometric Scale")
            .insert("type", "text")
            .insert("use", "required")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "ambient_sss")
            .insert("label", "Ambient SSS")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "1.0")
            .insert("use", "required")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "view_dep_sss")
            .insert("label", "View-Dependent SSS")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "1.0")
            .insert("use", "required")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "diffuse")
            .insert("label", "Diffuse Lighting")
            .insert("type", "numeric")
            .insert("min_value", "0.0")
            .insert("max_value", "1.0")
            .insert("use", "required")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "power")
            .insert("label", "Power")
            .insert("type", "text")
            .insert("use", "required")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "distortion")
            .insert("label", "Normal Distortion")
            .insert("type", "text")
            .insert("use", "required")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "albedo")
            .insert("label", "Albedo")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "0.5"));

    metadata.push_back(
        Dictionary()
            .insert("name", "light_samples")
            .insert("label", "Light Samples")
            .insert("type", "text")
            .insert("use", "required")
            .insert("default", "1"));

    metadata.push_back(
        Dictionary()
            .insert("name", "occlusion_samples")
            .insert("label", "Occlusion Samples")
            .insert("type", "text")
            .insert("use", "required")
            .insert("default", "1"));

    return metadata;
}

auto_release_ptr<SurfaceShader> FastSubSurfaceScatteringSurfaceShaderFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<SurfaceShader>(
            new FastSubSurfaceScatteringSurfaceShader(name, params));
}

}   // namespace renderer
