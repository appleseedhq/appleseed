
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Aytek Aman, The appleseedhq Organization
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
#include "bdptlightingengine.h"

// appleseed.renderer headers.
#include "renderer/kernel/lighting/forwardlightsampler.h"
#include "renderer/kernel/lighting/pathtracer.h"
#include "renderer/kernel/lighting/tracer.h"
#include "renderer/kernel/shading/shadingcomponents.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/project/project.h"

// appleseed.foundation headers.
#include "foundation/utility/arena.h"
#include "foundation/utility/statistics.h"

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Bidirectional Path Tracing lighting engine.
    //

    enum class BDPTVertexType
    {
        Camera, Light, Surface, Medium
    };

    struct BDPTVertex
    {
        Vector3d                m_position;
        Vector3d                m_geometric_normal;
        Spectrum                m_beta = Spectrum(0.0f);
        const BSDF*             m_bsdf = nullptr;
        const void*             m_bsdf_data = nullptr;
        Vector3d                m_dir_to_prev_vertex;
        const BDPTVertex*       m_prev_vertex = nullptr;
        Basis3f                 m_shading_basis;
        Spectrum                m_Le = Spectrum(0.0f);
        ShadingPoint            m_shading_point;
        bool                    m_is_light_vertex = false;

        float                   m_fwd_pdf = 0.0f;
        float                   m_rev_pdf = 0.0f;

        ///TODO:: create a proper constructor

        double convertDensity(double pdf, const BDPTVertex& vertex) const
        {
            Vector3d w = m_position - vertex.m_position;
            double dist2 = square_norm(w);
            if (dist2 == 0) return 0.0;
            double invDist2 = 1 / dist2;
            pdf *= max(dot(vertex.m_geometric_normal, w * sqrt(invDist2)), 0.0);
            return pdf * invDist2;
        }
    };

    class BDPTLightingEngine
      : public ILightingEngine
    {
      public:
        /// TODO:: get rid of num_max_vertices and read from params instead
        const size_t num_max_vertices = 9;
        struct Parameters
        {
            explicit Parameters(const ParamArray& params)
            {
            }
        };

        BDPTLightingEngine(
            const Project&              project,
            const ForwardLightSampler&  forward_light_sampler,
            const BackwardLightSampler& backward_light_sampler,
            const ParamArray&           params)
          : m_forward_light_sampler(forward_light_sampler)
          , m_backward_light_sampler(backward_light_sampler)
          , m_params(params)
        {
            const Camera* camera = project.get_uncached_active_camera();
            m_shutter_open_begin_time = camera->get_shutter_open_begin_time();
            m_shutter_close_end_time = camera->get_shutter_close_end_time();
        }

        void release() override
        {
            delete this;
        }

        void print_settings() const override
        {
        }

        void compute_lighting(
            SamplingContext&        sampling_context,
            const PixelContext&     pixel_context,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            ShadingComponents&      radiance) override      // output radiance, in W.sr^-1.m^-2
        {
            BDPTVertex camera_vertices[9];
            BDPTVertex light_vertices[9];

            size_t num_light_vertices = trace_light(sampling_context, shading_context, light_vertices);
            size_t num_camera_vertices = trace_camera(sampling_context, shading_context, shading_point, camera_vertices);

            for (size_t s = 0; s < num_light_vertices + 1; s++)
                for (size_t t = 2; t < num_camera_vertices + 2; t++)
                    if (s + t <= num_max_vertices)
                        connect(shading_context, shading_point, light_vertices, camera_vertices, s, t, radiance);

            shading_context.get_arena().clear();
        }

        Spectrum compute_geometry_term(
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            const BDPTVertex&       a,
            const BDPTVertex&       b)
        {
            double g = 1.0;

            const Vector3d v = b.m_position - a.m_position;

            /// TODO:: the special care have to be taken for these dot products when it comes to volume
            // compute unnormalied dot products
            g *= max(-dot(v, b.m_geometric_normal), 0.0);
            g *= max(dot(v, a.m_geometric_normal), 0.0);

            Spectrum result(0.0);

            // do the visibility test
            if (g > 0.0)
            {
                // compute inverse dist2 and normalize 2 dot products
                const double dist2 = square_norm(v);
                g /= (dist2 * dist2);
                shading_context.get_tracer().trace_between_simple(
                    shading_context,
                    a.m_position + (v * 1.0e-6),
                    b.m_position,
                    shading_point.get_ray().m_time,
                    VisibilityFlags::ShadowRay,
                    shading_point.get_ray().m_depth,
                    result);
            }

            return result * (float)g;
        }

        /// TODO:: precompute path density using fwd_pdf and rev_pdf.
        float compute_path_density(
            BDPTVertex*                         light_vertices,
            BDPTVertex*                         camera_vertices,
            const size_t                        s,
            const size_t                        t,
            const size_t                        p,
            const size_t                        q)
        {
            assert(p + q == s + t);

            // get i Vertex in the constructed path
            // i = 1 correspond first vertex of the full path

            auto GetVertexStartFromLight = [&](const size_t i)
            {
                if (i <= s)
                {
                    return &light_vertices[i - 1];
                }
                else
                {
                    return &camera_vertices[s + t - i - 1];
                }
            };

            auto GetVertexStartFromCamera = [&](const size_t i)
            {
                if (i <= t)
                {
                    return &camera_vertices[i - 2];
                }
                else
                {
                    return &light_vertices[s + t - i];
                }
            };

            /// TODO:: swap all pdf computation to rev_pdf and fwd_pdf
            float result = 1.0f;
            // start from light
            for (size_t i = 1; i <= p; i++)
            {
                if (i == 1) // the vertex on light source
                {
                    const BDPTVertex & vertex = *GetVertexStartFromLight(i);
                    result *= vertex.m_is_light_vertex ? m_forward_light_sampler.evaluate_pdf(vertex.m_shading_point) : 0.0f;
                }
                else if (i == 2) // the vertex after light source
                {
                    const BDPTVertex & prev_vertex = *GetVertexStartFromLight(i - 1);
                    const BDPTVertex & vertex = *GetVertexStartFromLight(i);
                    /// TODO:: fix this. This assumes diffuse light source.
                    float pdf = static_cast<float>(dot(normalize(vertex.m_position - prev_vertex.m_position), prev_vertex.m_geometric_normal) * RcpPi<float>());
                    result *= static_cast<float>(prev_vertex.convertDensity(pdf, vertex));
                }
                else
                {
                    const BDPTVertex & prev2_vertex = *GetVertexStartFromLight(i - 2);
                    const BDPTVertex & prev_vertex = *GetVertexStartFromLight(i - 1);
                    const BDPTVertex & vertex = *GetVertexStartFromLight(i);
                    float pdf = prev_vertex.m_bsdf->evaluate_pdf(
                        prev_vertex.m_bsdf_data,
                        true,
                        static_cast<Vector3f>(prev_vertex.m_geometric_normal),
                        prev_vertex.m_shading_basis,
                        static_cast<Vector3f>(normalize(vertex.m_position - prev_vertex.m_position)),
                        static_cast<Vector3f>(normalize(prev2_vertex.m_position - prev_vertex.m_position)),
                        ScatteringMode::All);
                    result *= static_cast<float>(prev_vertex.convertDensity(pdf, vertex));
                }
            }

            // start from camera
            for (size_t i = 2; i <= q; i++)
            {
                if (i == 2) // on shading point (first hit from camera ray)
                {
                    continue;
                }
                else if (i == 3) // first point after shading point
                {
                    const BDPTVertex & prev_vertex = *GetVertexStartFromCamera(i - 1);
                    const BDPTVertex & vertex = *GetVertexStartFromCamera(i);
                    float pdf = prev_vertex.m_bsdf->evaluate_pdf(
                        prev_vertex.m_bsdf_data,
                        false,
                        static_cast<Vector3f>(prev_vertex.m_geometric_normal),
                        prev_vertex.m_shading_basis,
                        static_cast<Vector3f>(normalize(vertex.m_position - prev_vertex.m_position)),
                        static_cast<Vector3f>(prev_vertex.m_dir_to_prev_vertex),
                        ScatteringMode::All);
                    result *= static_cast<float>(prev_vertex.convertDensity(pdf, vertex));
                }
                else
                {
                    const BDPTVertex & prev2_vertex = *GetVertexStartFromCamera(i - 2);
                    const BDPTVertex & prev_vertex = *GetVertexStartFromCamera(i - 1);
                    const BDPTVertex & vertex = *GetVertexStartFromCamera(i);
                    float pdf = prev_vertex.m_bsdf->evaluate_pdf(
                        prev_vertex.m_bsdf_data,
                        false,
                        static_cast<Vector3f>(prev_vertex.m_geometric_normal),
                        prev_vertex.m_shading_basis,
                        static_cast<Vector3f>(normalize(vertex.m_position - prev_vertex.m_position)),
                        static_cast<Vector3f>(normalize(prev2_vertex.m_position - prev_vertex.m_position)),
                        ScatteringMode::All);
                    result *= static_cast<float>(prev_vertex.convertDensity(pdf, vertex));
                }
            }

            return result;
        }

        void connect(
            const ShadingContext&               shading_context,
            const ShadingPoint&                 shading_point,
            BDPTVertex*                         light_vertices,
            BDPTVertex*                         camera_vertices,
            const size_t                        s,
            const size_t                        t,
            ShadingComponents&                  radiance)
        {
            assert(t >= 2);
            Spectrum result(0);

            if (s == 0)
            {
                // camera subpath is a complete path
                const BDPTVertex& camera_vertex = camera_vertices[t - 2];
                if (!camera_vertex.m_is_light_vertex)
                {
                    return;
                }
                result = camera_vertex.m_beta * camera_vertex.m_Le;
            }
            else if (s == 1)
            {
                // only one light vertex
                const BDPTVertex& light_vertex = light_vertices[s - 1];
                const BDPTVertex& camera_vertex = camera_vertices[t - 2];

                if (light_vertex.m_bsdf == nullptr || camera_vertex.m_bsdf == nullptr ||
                    light_vertex.m_bsdf_data == nullptr || camera_vertex.m_bsdf_data == nullptr)
                {
                    return;
                }

                Spectrum geometry = compute_geometry_term(shading_context, shading_point, camera_vertex, light_vertex);

                DirectShadingComponents camera_eval_bsdf;
                camera_vertex.m_bsdf->evaluate(
                    camera_vertex.m_bsdf_data,
                    false,   // Adjoint
                    false,
                    static_cast<Vector3f>(camera_vertex.m_geometric_normal),
                    camera_vertex.m_shading_basis,
                    static_cast<Vector3f>(normalize(light_vertex.m_position - camera_vertex.m_position)),
                    static_cast<Vector3f>(camera_vertex.m_dir_to_prev_vertex),
                    ScatteringMode::All,
                    camera_eval_bsdf);

                /// TODO:: need to take care of light material as well
                result = geometry * camera_eval_bsdf.m_beauty * camera_vertex.m_beta * light_vertex.m_beta;
            }
            else
            {
                const BDPTVertex& light_vertex = light_vertices[s - 1];
                const BDPTVertex& camera_vertex = camera_vertices[t - 2];

                if (light_vertex.m_bsdf == nullptr || camera_vertex.m_bsdf == nullptr ||
                    light_vertex.m_bsdf_data == nullptr || camera_vertex.m_bsdf == nullptr)
                {
                    return;
                }

                DirectShadingComponents camera_eval_bsdf;
                camera_vertex.m_bsdf->evaluate(
                    camera_vertex.m_bsdf_data,
                    false,   // Adjoint
                    false,
                    static_cast<Vector3f>(camera_vertex.m_geometric_normal),
                    camera_vertex.m_shading_basis,
                    static_cast<Vector3f>(normalize(light_vertex.m_position - camera_vertex.m_position)),
                    static_cast<Vector3f>(camera_vertex.m_dir_to_prev_vertex),
                    ScatteringMode::All,
                    camera_eval_bsdf);

                DirectShadingComponents light_eval_bsdf;
                light_vertex.m_bsdf->evaluate(
                    light_vertex.m_bsdf_data,
                    true,   // Adjoint
                    false,
                    static_cast<Vector3f>(light_vertex.m_geometric_normal),
                    light_vertex.m_shading_basis,
                    static_cast<Vector3f>(normalize(camera_vertex.m_position - light_vertex.m_position)),
                    static_cast<Vector3f>(light_vertex.m_dir_to_prev_vertex),
                    ScatteringMode::All,
                    light_eval_bsdf);

                Spectrum geometry = compute_geometry_term(shading_context, shading_point, camera_vertex, light_vertex);
                result = geometry * camera_eval_bsdf.m_beauty * light_eval_bsdf.m_beauty * camera_vertex.m_beta * light_vertex.m_beta;
            }

            // check if connection is near black or not
            bool nearBlack = true;
            for (size_t i = 0; i < result.size(); i++)
            {
                // need nearBlack() function for spectrum
                if (result[i] > 1e-9)
                {
                    nearBlack = false;
                    break;
                }
            }

            if (nearBlack) { return; }

            float numerator = compute_path_density(light_vertices, camera_vertices, s, t, s, t);
            float denominator = 0.0;
            /// TODO:: unhandled case where denominator = 0 (specular surface).

            // [p = 0, q = s + t], [p = 1, q = s + t - 1] ... [p = s + t - 2, q = 2]
            for (size_t i = 0; i <= s + t - 2; i++)
            {
                denominator += compute_path_density(light_vertices, camera_vertices, s, t, i, s + t - i);
            }

            float miWeight = numerator / denominator;

            assert(miWeight <= 1.0f);
            radiance.m_beauty += miWeight * result;
        }

        size_t trace_light(
            SamplingContext&            sampling_context,
            const ShadingContext&       shading_context,
            BDPTVertex*                 vertices)
        {
            // Sample the light sources.
            sampling_context.split_in_place(4, 1);
            const Vector4f s = sampling_context.next2<Vector4f>();
            LightSample light_sample;
            m_forward_light_sampler.sample(
                ShadingRay::Time::create_with_normalized_time(
                    s[0],
                    m_shutter_open_begin_time,
                    m_shutter_close_end_time),
                Vector3f(s[1], s[2], s[3]),
                light_sample);

            return light_sample.m_triangle
                ? trace_emitting_triangle(
                    sampling_context,
                    shading_context,
                    light_sample,
                    vertices)
                : trace_non_physical_light(
                    sampling_context,
                    shading_context,
                    light_sample);
        }

        size_t trace_emitting_triangle(
            SamplingContext&            sampling_context,
            const ShadingContext&       shading_context,
            LightSample&                light_sample,
            BDPTVertex*                 vertices)
        {
            // Make sure the geometric normal of the light sample is in the same hemisphere as the shading normal.
            light_sample.m_geometric_normal =
                flip_to_same_hemisphere(
                    light_sample.m_geometric_normal,
                    light_sample.m_shading_normal);

            const Material* material = light_sample.m_triangle->m_material;
            const Material::RenderData& material_data = material->get_render_data();

            // Build a shading point on the light source.
            ShadingPoint light_shading_point;
            light_sample.make_shading_point(
                light_shading_point,
                light_sample.m_shading_normal,
                shading_context.get_intersector());

            if (material_data.m_shader_group)
            {
                shading_context.execute_osl_emission(
                    *material_data.m_shader_group,
                    light_shading_point);
            }

            // Sample the EDF.
            sampling_context.split_in_place(2, 1);
            Vector3f emission_direction;
            Spectrum edf_value(Spectrum::Illuminance);
            float edf_prob;
            material_data.m_edf->sample(
                sampling_context,
                material_data.m_edf->evaluate_inputs(shading_context, light_shading_point),
                Vector3f(light_sample.m_geometric_normal),
                Basis3f(Vector3f(light_sample.m_shading_normal)),
                sampling_context.next2<Vector2f>(),
                emission_direction,
                edf_value,
                edf_prob);

            // Compute the initial particle weight.
            Spectrum initial_flux = edf_value / light_sample.m_probability;

            // Make a shading point that will be used to avoid self-intersections with the light sample.
            ShadingPoint parent_shading_point;
            light_sample.make_shading_point(
                parent_shading_point,
                Vector3d(emission_direction),
                shading_context.get_intersector());

            // Build the light ray.
            sampling_context.split_in_place(1, 1);
            const ShadingRay::Time time =
                ShadingRay::Time::create_with_normalized_time(
                    sampling_context.next2<float>(),
                    m_shutter_open_begin_time,
                    m_shutter_close_end_time);
            const ShadingRay light_ray(
                light_sample.m_point,
                Vector3d(emission_direction),
                time,
                VisibilityFlags::LightRay,
                0);

            BDPTVertex& bdpt_vertex = vertices[0];
            bdpt_vertex.m_beta = initial_flux;
            bdpt_vertex.m_fwd_pdf = light_sample.m_probability;
            /// CONFUSE:: why geometric normal is flipped?
            bdpt_vertex.m_geometric_normal = -light_shading_point.get_geometric_normal();
            bdpt_vertex.m_is_light_vertex = true;
            bdpt_vertex.m_position = light_shading_point.get_point();
            bdpt_vertex.m_rev_pdf = 1.0;
            bdpt_vertex.m_shading_point = light_shading_point;

            // Build the path tracer.
            size_t num_light_vertices = 0;
            PathVisitor path_visitor(initial_flux * dot(emission_direction, Vector3f(light_sample.m_shading_normal)) / edf_prob,
                                     shading_context,
                                     vertices + 1,
                                     &num_light_vertices);
            VolumeVisitor volume_visitor;
            PathTracer<PathVisitor, VolumeVisitor, true> path_tracer(
                path_visitor,
                volume_visitor,
                ~0,
                num_max_vertices - 2,
                ~0,
                ~0,
                ~0,
                ~0,
                shading_context.get_max_iterations());   // don't illuminate points closer than the light near start value

            const size_t light_path_length =
                path_tracer.trace(
                    sampling_context,
                    shading_context,
                    light_ray,
                    &parent_shading_point,
                    false);

            m_light_path_length.insert(light_path_length);
            assert(num_light_vertices <= num_max_vertices);
            return num_light_vertices;
        }

        size_t trace_non_physical_light(
            SamplingContext&        sampling_context,
            const ShadingContext&   shading_context,
            LightSample&            light_sample)
        {
            return 0;
        }

        size_t trace_camera(
            SamplingContext&        sampling_context,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            BDPTVertex*             vertices)
        {
            size_t num_camera_vertices = 0;
            PathVisitor path_visitor(Spectrum(1.0), shading_context, vertices, &num_camera_vertices);
            VolumeVisitor volume_visitor;

            PathTracer<PathVisitor, VolumeVisitor, false> path_tracer(
                path_visitor,
                volume_visitor,
                ~0,
                num_max_vertices - 2,
                ~0,
                ~0,
                ~0,
                ~0,
                shading_context.get_max_iterations());

            const size_t camera_path_length =
                path_tracer.trace(
                    sampling_context,
                    shading_context,
                    shading_point,
                    false);

            m_camera_path_length.insert(camera_path_length);
            assert(num_camera_vertices <= num_max_vertices - 1);
            return camera_path_length - 1;
        }

        StatisticsVector get_statistics() const override
        {
            Statistics stats;

            return StatisticsVector::make("bdpt statistics", stats);
        }

    private:
        const Parameters            m_params;

        const ForwardLightSampler&  m_forward_light_sampler;
        const BackwardLightSampler& m_backward_light_sampler;
        //Intersector                 m_intersector;

        float                       m_shutter_open_begin_time;
        float                       m_shutter_close_end_time;

        Population<uint64>          m_light_path_length;
        Population<uint64>          m_camera_path_length;


        struct PathVisitor
        {
            PathVisitor(const Spectrum&         initial_beta,
                        const ShadingContext&   shading_context,
                        BDPTVertex*             vertices,
                        size_t*                 num_vertices)
              : m_initial_beta(initial_beta)
              , m_shading_context(shading_context)
              , m_vertices(vertices)
              , m_num_vertices(num_vertices)
            {
            }

            bool accept_scattering(
                const ScatteringMode::Mode  prev_mode,
                const ScatteringMode::Mode  next_mode) const
            {
                return true;
            }

            void on_miss(const PathVertex& vertex)
            {
            }

            void on_hit(const PathVertex& vertex)
            {
                // create BDPT Vertex
                BDPTVertex& bdpt_vertex = m_vertices[*m_num_vertices];
                bdpt_vertex.m_beta = vertex.m_throughput * m_initial_beta;
                bdpt_vertex.m_bsdf = vertex.m_bsdf;
                bdpt_vertex.m_bsdf_data = vertex.m_bsdf_data;
                bdpt_vertex.m_dir_to_prev_vertex = normalize(vertex.m_outgoing.get_value());
                bdpt_vertex.m_fwd_pdf = vertex.m_prev_prob;
                bdpt_vertex.m_geometric_normal = vertex.get_geometric_normal();
                bdpt_vertex.m_position = vertex.get_point();
                bdpt_vertex.m_shading_basis = Basis3f(vertex.get_shading_basis());
                bdpt_vertex.m_shading_point = *vertex.m_shading_point;
                /// TODO:: compute rev_pdf here

                if (vertex.m_edf)
                {
                    vertex.compute_emitted_radiance(m_shading_context, bdpt_vertex.m_Le);
                    bdpt_vertex.m_is_light_vertex = true;
                }

                bdpt_vertex.m_prev_vertex = (*m_num_vertices == 0) ? nullptr : m_vertices - 1;
                (*m_num_vertices)++;
            }

            void on_scatter(const PathVertex& vertex)
            {
            }

            const ShadingContext&       m_shading_context;
            Spectrum                    m_initial_beta;
            BDPTVertex*                 m_vertices;
            size_t*                     m_num_vertices;
        };

        struct VolumeVisitor
        {
            VolumeVisitor()
            {
            }

            bool accept_scattering(
                const ScatteringMode::Mode  prev_mode)
            {
                return true;
            }

            void on_scatter(PathVertex& vertex) {}

            void visit_ray(PathVertex& vertex, const ShadingRay& volume_ray) {}
        };
    };
}

BDPTLightingEngineFactory::BDPTLightingEngineFactory(
    const Project&              project,
    const ForwardLightSampler&  forward_light_sampler,
    const BackwardLightSampler& backward_light_sampler,
    const ParamArray&           params)
    : m_project(project)
    , m_forward_light_sampler(forward_light_sampler)
    , m_backward_light_sampler(backward_light_sampler)
    , m_params(params)
{
}

void BDPTLightingEngineFactory::release()
{
    delete this;
}

ILightingEngine* BDPTLightingEngineFactory::create()
{
    return new BDPTLightingEngine(
        m_project,
        m_forward_light_sampler,
        m_backward_light_sampler,
        m_params);
}

Dictionary BDPTLightingEngineFactory::get_params_metadata()
{
    Dictionary metadata;
    add_common_params_metadata(metadata, true);
    return metadata;
}

}   // namespace renderer
