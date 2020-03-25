
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/rendering/rendererservices.h"
#include "renderer/kernel/shading/closures.h"
#include "renderer/kernel/shading/oslshadergroupexec.h"
#include "renderer/kernel/shading/oslshadingsystem.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/texturing/oiiotexturesystem.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/environmentedf/sphericalcoordinates.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/scene/visibilityflags.h"
#include "renderer/modeling/shadergroup/shadergroup.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/cdf.h"
#include "foundation/math/matrix.h"
#include "foundation/math/sampling/imageimportancesampler.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/defaulttimers.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/memory/arena.h"
#include "foundation/containers/dictionary.h"
#include "foundation/utility/job/abortswitch.h"
#include "foundation/utility/stopwatch.h"
#include "foundation/utility/version.h"

// Standard headers.
#include <cassert>
#include <cstring>
#include <memory>
#include <string>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Project; }

using namespace foundation;

namespace renderer
{

namespace
{
    typedef ImageImportanceSampler<Color3f, float> ImageImportanceSamplerType;


    //
    // OSL environment sampler.
    //

    class OSLEnvironmentSampler
    {
      public:
        OSLEnvironmentSampler(
            const ShaderGroup*              shader_group,
            const OSLShaderGroupExec&       osl_shadergroup_exec,
            const std::size_t               width,
            const std::size_t               height)
          : m_shader_group(shader_group)
          , m_osl_shadergroup_exec(osl_shadergroup_exec)
          , m_rcp_width(1.0f / width)
          , m_rcp_height(1.0f / height)
        {
        }

        void sample(const std::size_t x, const std::size_t y, Color3f& payload, float& importance)
        {
            // Compute the spherical coordinates of the sample.
            float theta, phi;
            unit_square_to_angles(
                (x + 0.5f) * m_rcp_width, 
                1.0f - (y + 0.5f) * m_rcp_height,
                theta, 
                phi);

            // Compute the local space emission direction.
            const float cos_theta = std::cos(theta);
            const float sin_theta = std::sin(theta);
            const float cos_phi = std::cos(phi);
            const float sin_phi = std::sin(phi);
            const Vector3f local_outgoing =
                Vector3f::make_unit_vector(cos_theta, sin_theta, cos_phi, sin_phi);

            // OSL exectute system.
            payload = m_osl_shadergroup_exec.execute_background(
                *m_shader_group,
                local_outgoing);

            importance = luminance(payload);
        }

      private:
        const ShaderGroup*          m_shader_group;
        const OSLShaderGroupExec&   m_osl_shadergroup_exec;
        const float                 m_rcp_width;
        const float                 m_rcp_height;
    };


    //
    // OSL environment EDF.
    //

    const char* Model = "osl_environment_edf";

    class OSLEnvironmentEDF
      : public EnvironmentEDF
    {
      public:
        OSLEnvironmentEDF(
            const char*         name,
            const ParamArray&   params)
          : EnvironmentEDF(name, params)
          , m_importance_map_size(0)
          , m_probability_scale(0.0f)
        {
            m_inputs.declare("osl_background", InputFormatEntity, "");
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return Model;
        }

        bool on_render_begin(
            const Project&           project,
            const BaseGroup*         parent,
            OnRenderBeginRecorder&   recorder,
            IAbortSwitch*            abort_switch) override
        {
            if (!EnvironmentEDF::on_render_begin(project, parent, recorder, abort_switch))
                return false;

            m_importance_map_size = m_params.get_optional<std::size_t>("importance_map_size", 1024);

            m_shader_group =
                static_cast<ShaderGroup*>(m_inputs.get_entity("osl_background"));

            return true;
        }

        bool on_frame_begin(
            const Project&          project,
            const BaseGroup*        parent,
            OnFrameBeginRecorder&   recorder,
            IAbortSwitch*           abort_switch) override
        {
            if (!EnvironmentEDF::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            m_shader_group =
                static_cast<ShaderGroup*>(m_inputs.get_entity("osl_background"));

            if (!m_importance_sampler || m_shader_group->get_version_id() != m_shader_group_version_id)
            {
                m_shader_group_version_id = m_shader_group->get_version_id();

                // Build importance map only if this environment EDF is the active one.
                if (project.get_scene()->get_environment()->get_uncached_environment_edf() == this)
                    build_importance_map(project, abort_switch);
            }

            return true;
        }

        void on_frame_end(
            const Project&     project,
            const BaseGroup*   parent) override
        {
            m_shader_group = nullptr;

            EnvironmentEDF::on_frame_end(project, parent);
        }

        void sample(
            const ShadingContext&   shading_context,
            const Vector2f&         s,
            Vector3f&               outgoing,
            Spectrum&               value,
            float&                  probability) const override
        {

            if (!m_importance_sampler)
            {
                RENDERER_LOG_WARNING(
                    "cannot sample osl environment edf \"%s\" because it is not bound to the environment.",
                    get_path().c_str());
                value.set(0.0f);
                probability = 0.0f;
                return;
            }

            // Sample the importance map.
            std::size_t x, y;
            Color3f payload;
            float prob_xy;
            m_importance_sampler->sample(s, x, y, payload, prob_xy);
            assert(prob_xy >= 0.0f);

            // Compute the coordinates in [0,1)^2 of the sample.
            const std::size_t importance_map_width = m_importance_map_size;
            const std::size_t importance_map_height = m_importance_map_size / 2;
            const float jitter_x = frac(s[0] * importance_map_width);
            const float jitter_y = frac(s[1] * importance_map_height);
            const float u = (x + jitter_x) * m_rcp_importance_map_width;
            const float v = (y + jitter_y) * m_rcp_importance_map_height;
            assert(u >= 0.0f && u < 1.0f);
            assert(v >= 0.0f && v < 1.0f);

            // Compute the spherical coordinates of the sample.
            float theta, phi;
            unit_square_to_angles(u, v, theta, phi);

            // Compute the local space emission direction.
            const float cos_theta = std::cos(theta);
            const float sin_theta = std::sin(theta);
            const float cos_phi = std::cos(phi);
            const float sin_phi = std::sin(phi);
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

            evaluate_osl_background(shading_context, local_outgoing, value);
        }

        void evaluate(
            const ShadingContext&   shading_context,
            const Vector3f&         outgoing,
            Spectrum&               value,
            float&                  probability) const override
        {
            assert(is_normalized(outgoing));

            // Calculates value and probability, therefore importance map is needed.
            if (!m_importance_sampler)
            {
                RENDERER_LOG_WARNING(
                    "cannot compute pdf for osl environment edf \"%s\" because it is not bound to the environment.",
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

            // Convert the spherical coordinates to [0,1]^2.
            float u, v;
            angles_to_unit_square(theta, phi, u, v);

            // Compute value.
            evaluate_osl_background(shading_context, local_outgoing, value);

            // Compute probability.
            probability = compute_pdf(u, v, theta);
            assert(probability >= 0.0f);
        }

        float evaluate_pdf(const Vector3f& outgoing) const override
        {
            assert(is_normalized(outgoing));

            if (!m_importance_sampler)
            {
                RENDERER_LOG_WARNING(
                    "cannot compute pdf for osl environment edf \"%s\" because it is not bound to the environment.",
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

            // Convert the spherical coordinates to [0,1]^2.
            float u, v;
            angles_to_unit_square(theta, phi, u, v);

            // Compute and return the PDF value.
            return compute_pdf(u, v, theta);
        }

      private:
        ShaderGroup*                                  m_shader_group;
        VersionID                                     m_shader_group_version_id;
        std::size_t                                   m_importance_map_size;
        float                                         m_rcp_importance_map_width;
        float                                         m_rcp_importance_map_height;
        float                                         m_probability_scale;
        std::unique_ptr<ImageImportanceSamplerType>   m_importance_sampler;

        void evaluate_osl_background(
            const ShadingContext&   shading_context,
            const Vector3f&         local_outgoing,
            Spectrum&               value) const
        {
            if (m_shader_group)
                shading_context.execute_osl_background(*m_shader_group, local_outgoing, value);
            else value.set(0.0f);
        }

        float compute_pdf(
            const float   u,
            const float   v,
            const float   theta) const
        {
            assert(u >= 0.0f && u < 1.0f);
            assert(v >= 0.0f && v < 1.0f);
            assert(m_importance_sampler);

            // Compute the probability density of this sample in the importance map.
            const std::size_t importance_map_width = m_importance_map_size;
            const std::size_t importance_map_height = m_importance_map_size / 2;
            const std::size_t x = truncate<std::size_t>(importance_map_width * u);
            const std::size_t y = truncate<std::size_t>(importance_map_height * v);
            const float prob_xy = m_importance_sampler->get_pdf(x, y);
            assert(prob_xy >= 0.0f);

            // Compute the probability density of the emission direction.
            const float pdf = prob_xy > 0.0f ? prob_xy * m_probability_scale / std::sin(theta) : 0.0f;
            assert(pdf >= 0.0f);

            return pdf;
        }

        void build_importance_map(const Project& project, IAbortSwitch* abort_switch)
        {
            Stopwatch<DefaultWallclockTimer> stopwatch;
            stopwatch.start();

            const std::size_t importance_map_width = m_importance_map_size;
            const std::size_t importance_map_height = m_importance_map_size / 2;
            m_rcp_importance_map_width = 1.0f / importance_map_width;
            m_rcp_importance_map_height = 1.0f / importance_map_height;

            const std::size_t texel_count = importance_map_width * importance_map_height;
            m_probability_scale = texel_count / (2.0f * PiSquare<float>());

            // Create OSL shading system.
            auto_release_ptr<OIIOTextureSystem> texture_system(
                OIIOTextureSystemFactory().create());
            RendererServices renderer_services(project, texture_system.ref());
            auto_release_ptr<OSLShadingSystem> osl_shading_system(
                OSLShadingSystemFactory().create(
                    &renderer_services,
                    texture_system.get()));

            // Create OSL shadergroup exec.
            Arena arena;
            OSLShaderGroupExec osl_shadergroup_exec(
                osl_shading_system.ref(),
                arena);

            // Create OSL environment sampler.
            OSLEnvironmentSampler osl_environment_sampler(
                m_shader_group,
                osl_shadergroup_exec,
                importance_map_width,
                importance_map_height);

            // Create OSL environment EDF importance sampler.
            m_importance_sampler.reset(
                new ImageImportanceSamplerType(
                    importance_map_width,
                    importance_map_height));

            RENDERER_LOG_INFO(
                "building " FMT_SIZE_T "x" FMT_SIZE_T " importance map "
                "for osl environment edf \"%s\"...",
                importance_map_width,
                importance_map_height,
                get_path().c_str());

            m_importance_sampler->rebuild(osl_environment_sampler, abort_switch);

            if (is_aborted(abort_switch))
                m_importance_sampler.reset();
            else
            {
                stopwatch.measure();

                RENDERER_LOG_INFO(
                    "built importance map for osl environment edf \"%s\" in %s.",
                    get_path().c_str(),
                    pretty_time(stopwatch.get_seconds()).c_str());
            }
        }
    };
}


//
// OSLEnvironmentEDFFactory class implementation.
//

void OSLEnvironmentEDFFactory::release()
{
    delete this;
}

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

    metadata.push_back(
        Dictionary()
            .insert("name", "importance_map_size")
            .insert("label", "Importance Map size")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "1")
                    .insert("type", "soft"))
            .insert("max",
                Dictionary()
                    .insert("value", "65536")
                    .insert("type", "soft"))
            .insert("default", "1024")
            .insert("use", "optional")
            .insert("help", "Importance map size: width = size, height = size/2"));

    add_common_input_metadata(metadata);

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

}   // namespace renderer
