
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
#include "fastsubsurfacescatteringsurfaceshader.h"

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/lighting/lightsampler.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/inputevaluator.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/math/basis.h"
#include "foundation/math/sampling.h"
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/dictionaryarray.h"
#include "foundation/utility/foreach.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <limits>
#include <memory>

// Forward declarations.
namespace renderer  { class Scene; }
namespace renderer  { class TextureCache; }

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
          : SurfaceShader(params)
          , m_light_samples(m_params.get_required<size_t>("light_samples", 1))
          , m_occlusion_samples(m_params.get_required<size_t>("occlusion_samples", 1))
        {
            set_name(name);

            m_inputs.declare("scale", InputFormatScalar);
            m_inputs.declare("ambient_sss", InputFormatScalar);
            m_inputs.declare("view_dep_sss", InputFormatScalar);
            m_inputs.declare("power", InputFormatScalar);
            m_inputs.declare("distortion", InputFormatScalar);
            m_inputs.declare("albedo", InputFormatSpectrum);
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
            const Scene& scene = *project.get_scene();
            m_light_sampler.reset(new LightSampler(scene));
        }

        virtual void evaluate(
            SamplingContext&        sampling_context,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            ShadingResult&          shading_result) const
        {
            // Evaluate the inputs.
            InputValues values;
            m_inputs.evaluate(
                shading_context.get_texture_cache(),
                shading_point.get_input_params(),
                &values);
            
            // Compute reversed normals.
            const Vector3d reverse_geometric_normal = -shading_point.get_geometric_normal();
            const Vector3d reverse_shading_normal = -shading_point.get_shading_normal();
            const Basis3d reverse_shading_basis(reverse_shading_normal);

            // todo: there are possible correlation artifacts since the sampling_context
            // object is forked twice from there: once to compute the average occluder
            // distance sampler and once by the light sampler.

            // Compute average occluder distance.
            const double avg_distance =
                compute_average_distance(
                    sampling_context,
                    shading_context.get_intersector(),
                    shading_point.get_point(),
                    reverse_geometric_normal,
                    reverse_shading_basis,
                    m_occlusion_samples,
                    &shading_point);
            assert(avg_distance >= 0.0);

            // Compute normalized thickness in [0, 1].
            const double thickness = min(avg_distance / values.m_scale, 1.0);

            // Initialize the shading result to opaque black.
            shading_result.m_color_space = ColorSpaceSpectral;
            shading_result.m_color.set(0.0f);
            shading_result.m_alpha.set(1.0f);

            // Sample the light sources.
            LightSampleVector light_samples;
            m_light_sampler->sample(sampling_context, m_light_samples, light_samples);

            const Vector3d camera_vec = normalize(-shading_point.get_ray().m_dir);                                          // toward camera

            for (const_each<LightSampleVector> i = light_samples; i; ++i)
            {
                // Compute the contribution of this light sample.
                const LightSample& light_sample = *i;
                const Vector3d light_vec = normalize(light_sample.m_input_params.m_point - shading_point.get_point());      // toward light
                const Vector3d distorted_light_vec = normalize(light_vec + values.m_distortion * reverse_shading_normal);   // normalize() not strictly necessary
                const double dot_lv = saturate(dot(camera_vec, -distorted_light_vec));
                const double view_dep = pow(dot_lv, values.m_power);
                const double contribution = (values.m_ambient_sss + values.m_view_dep_sss * view_dep) * (1.0 - thickness);

                // Evaluate the input values of the EDF.
                InputEvaluator edf_input_evaluator(shading_context.get_texture_cache());
                const void* edf_data =
                    edf_input_evaluator.evaluate(
                        light_sample.m_edf->get_inputs(),
                        light_sample.m_input_params);

                // Evaluate the EDF.
                Spectrum edf_value;
                light_sample.m_edf->evaluate(
                    edf_data,
                    light_sample.m_input_params.m_geometric_normal,
                    Basis3d(light_sample.m_input_params.m_shading_normal),
                    -light_vec,
                    edf_value);

                // Compute and accumulate the contribution of this light sample.
                Spectrum result = values.m_albedo;
                result *= edf_value;
                result *= static_cast<float>(contribution);
                shading_result.m_color += result;
            }

            // Normalize the result.
            if (light_samples.size() > 1)
                shading_result.m_color /= static_cast<float>(light_samples.size());
        }

      private:
        const size_t            m_light_samples;
        const size_t            m_occlusion_samples;
        auto_ptr<LightSampler>  m_light_sampler;

        struct InputValues
        {
            double      m_scale;
            double      m_ambient_sss;
            double      m_view_dep_sss;
            double      m_power;
            double      m_distortion;
            Spectrum    m_albedo;
            Alpha       m_alpha;
        };

        static double compute_average_distance(
            const SamplingContext&  sampling_context,
            const Intersector&      intersector,
            const Vector3d&         point,
            const Vector3d&         geometric_normal,
            const Basis3d&          shading_basis,
            const size_t            sample_count,
            const ShadingPoint*     parent_shading_point)
        {
            // Create a sampling context.
            SamplingContext child_context = sampling_context.split(2, sample_count);

            // Construct an ambient occlusion ray.
            ShadingRay ao_ray;
            ao_ray.m_org = point;
            ao_ray.m_tmin = 0.0;
            ao_ray.m_tmax = numeric_limits<double>::max();
            ao_ray.m_time = 0.0f;
            ao_ray.m_flags = ~0;

            size_t computed_samples = 0;
            double average_distance = 0.0;

            for (size_t i = 0; i < sample_count; ++i)
            {
                // Generate a cosine-weighted direction over the unit hemisphere.
                const Vector2d s = child_context.next_vector2<2>();
                ao_ray.m_dir = sample_hemisphere_cosine(s);

                // Transform the direction to world space.
                ao_ray.m_dir = shading_basis.transform_to_parent(ao_ray.m_dir);

                // Don't cast rays on or below the geometric surface.
                if (dot(ao_ray.m_dir, geometric_normal) <= 0.0)
                    continue;

                // Count the number of computed samples.
                ++computed_samples;

                // Trace the ambient occlusion ray and update the accumulated hit distance.
                ShadingPoint shading_point;
                if (intersector.trace(ao_ray, shading_point, parent_shading_point))
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

DictionaryArray FastSubSurfaceScatteringSurfaceShaderFactory::get_widget_definitions() const
{
    DictionaryArray definitions;

    {
        Dictionary widget;
        widget.insert("name", "scale");
        widget.insert("label", "Geometric Scale");
        widget.insert("widget", "text_box");
        widget.insert("use", "required");
        widget.insert("default", "1.0");
        definitions.push_back(widget);
    }

    {
        Dictionary widget;
        widget.insert("name", "ambient_sss");
        widget.insert("label", "Ambient SSS");
        widget.insert("widget", "text_box");
        widget.insert("use", "required");
        widget.insert("default", "0.0");
        definitions.push_back(widget);
    }

    {
        Dictionary widget;
        widget.insert("name", "view_dep_sss");
        widget.insert("label", "View-Dependent SSS");
        widget.insert("widget", "text_box");
        widget.insert("use", "required");
        widget.insert("default", "0.0");
        definitions.push_back(widget);
    }

    {
        Dictionary widget;
        widget.insert("name", "power");
        widget.insert("label", "Power");
        widget.insert("widget", "text_box");
        widget.insert("use", "required");
        widget.insert("default", "1.0");
        definitions.push_back(widget);
    }

    {
        Dictionary widget;
        widget.insert("name", "distortion");
        widget.insert("label", "Normal Distortion");
        widget.insert("widget", "text_box");
        widget.insert("use", "required");
        widget.insert("default", "0.0");
        definitions.push_back(widget);
    }

    {
        Dictionary entity_types;
        entity_types.insert("color", "Colors");
        entity_types.insert("texture_instance", "Textures");

        Dictionary widget;
        widget.insert("name", "albedo");
        widget.insert("label", "Albedo");
        widget.insert("widget", "entity_picker");
        widget.insert("entity_types", entity_types);
        widget.insert("use", "required");
        widget.insert("default", "");
        definitions.push_back(widget);
    }

    {
        Dictionary widget;
        widget.insert("name", "light_samples");
        widget.insert("label", "Light Samples");
        widget.insert("widget", "text_box");
        widget.insert("use", "required");
        widget.insert("default", "1");
        definitions.push_back(widget);
    }

    {
        Dictionary widget;
        widget.insert("name", "occlusion_samples");
        widget.insert("label", "Occlusion Samples");
        widget.insert("widget", "text_box");
        widget.insert("use", "required");
        widget.insert("default", "1");
        definitions.push_back(widget);
    }

    return definitions;
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
