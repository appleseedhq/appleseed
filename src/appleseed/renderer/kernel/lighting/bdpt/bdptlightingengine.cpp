
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
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/project/project.h"
#include "renderer/kernel/shading/shadingcomponents.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/lighting/tracer.h"

// appleseed.foundation headers.
#include "foundation/utility/statistics.h"

using namespace foundation;

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
        foundation::Vector3d    m_position;
        const BDPTVertex*       m_prev_vertex = nullptr;
        foundation::Vector3d    m_dir_to_prev_vertex;
        foundation::Vector3d    m_geometric_normal;
        Spectrum                m_beta;
        const BSDF*             m_bsdf;
        const void*             m_bsdf_data;
        Basis3f                 m_shading_basis;
        float                   m_fwd_pdf = 0.0;
        float                   m_rev_pdf;
        Spectrum                m_Le = Spectrum(0.0);
        ShadingPoint            m_shading_point;
        bool                    m_is_light_vertex = false;

        ///TODO:: create a proper constructor

        double convertDensity(double pdf, const BDPTVertex& vertex) const
        {
            Vector3d w = m_position - vertex.m_position;
            double dist2 = foundation::square_norm(w);
            if (dist2 == 0) return 0.0;
            double invDist2 = 1 / dist2;
            pdf *= std::max(foundation::dot(vertex.m_geometric_normal, w * std::sqrt(invDist2)), 0.0);
            return pdf * invDist2;
        }
    };

    class BDPTLightingEngine
      : public ILightingEngine
    {
      public:
        const unsigned int num_max_vertices = 9;
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

        Spectrum ComputeGeometryTerm(
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            const BDPTVertex&       a,
            const BDPTVertex&       b)
        {
            double g = 1.0;

            const Vector3d v = b.m_position - a.m_position;

            /// TODO:: the special care have to be taken for these dot products when it comes to volume
            // compute unnormalied dot products
            g *= std::max(-foundation::dot(v, b.m_geometric_normal), 0.0);
            g *= std::max(foundation::dot(v, a.m_geometric_normal), 0.0);

            Spectrum result(0.0);

            // do the visibility test
            if (g > 0.0)
            {
                // compute inverse dist2 and normalize 2 dot products
                const double dist2 = foundation::square_norm(v);
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
        float ComputePathDensity(
            std::vector<BDPTVertex>             &light_vertices,
            std::vector<BDPTVertex>             &camera_vertices,
            const int                           s,
            const int                           t,
            const int                           p,
            const int                           q
        )
        {
            assert(p + q == s + t);

            // get i Vertex in the constructed path
            // i = 1 correspond first vertex of the full path

            auto GetVertexStartFromLight = [&](int i)
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

            auto GetVertexStartFromCamera = [&](int i)
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
            for (int i = 1; i <= p; i++)
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
                    float pdf = static_cast<float>(foundation::dot(foundation::normalize(vertex.m_position - prev_vertex.m_position), prev_vertex.m_geometric_normal) * RcpPi<float>());
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
                        static_cast<Vector3f>(foundation::normalize(vertex.m_position - prev_vertex.m_position)),
                        static_cast<Vector3f>(foundation::normalize(prev2_vertex.m_position - prev_vertex.m_position)),
                        ScatteringMode::All);
                    result *= static_cast<float>(prev_vertex.convertDensity(pdf, vertex));
                }
            }

            // start from camera
            for (int i = 2; i <= q; i++)
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
                        static_cast<Vector3f>(foundation::normalize(vertex.m_position - prev_vertex.m_position)),
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
                        static_cast<Vector3f>(foundation::normalize(vertex.m_position - prev_vertex.m_position)),
                        static_cast<Vector3f>(foundation::normalize(prev2_vertex.m_position - prev_vertex.m_position)),
                        ScatteringMode::All);
                    result *= static_cast<float>(prev_vertex.convertDensity(pdf, vertex));
                }
            }

            return result;
        }

        void Connect(
            const ShadingContext&               shading_context,
            const ShadingPoint&                 shading_point,
            std::vector<BDPTVertex>             &light_vertices,
            std::vector<BDPTVertex>             &camera_vertices,
            const int                           s,
            const int                           t,
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

                Spectrum geometry = ComputeGeometryTerm(shading_context, shading_point, camera_vertex, light_vertex);

                DirectShadingComponents camera_eval_bsdf;
                const float camera_eval_bsdf_prob = camera_vertex.m_bsdf->evaluate(
                    camera_vertex.m_bsdf_data,
                    false,   // Adjoint
                    false,
                    static_cast<Vector3f>(camera_vertex.m_geometric_normal),
                    camera_vertex.m_shading_basis,
                    static_cast<Vector3f>(foundation::normalize(light_vertex.m_position - camera_vertex.m_position)),
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
                const float camera_eval_bsdf_prob = camera_vertex.m_bsdf->evaluate(
                    camera_vertex.m_bsdf_data,
                    false,   // Adjoint
                    false,
                    static_cast<Vector3f>(camera_vertex.m_geometric_normal),
                    camera_vertex.m_shading_basis,
                    static_cast<Vector3f>(foundation::normalize(light_vertex.m_position - camera_vertex.m_position)),
                    static_cast<Vector3f>(camera_vertex.m_dir_to_prev_vertex),
                    ScatteringMode::All,
                    camera_eval_bsdf);

                DirectShadingComponents light_eval_bsdf;
                const float light_eval_bsdf_prob = light_vertex.m_bsdf->evaluate(
                    light_vertex.m_bsdf_data,
                    true,   // Adjoint
                    false,
                    static_cast<Vector3f>(light_vertex.m_geometric_normal),
                    light_vertex.m_shading_basis,
                    static_cast<Vector3f>(foundation::normalize(camera_vertex.m_position - light_vertex.m_position)),
                    static_cast<Vector3f>(light_vertex.m_dir_to_prev_vertex),
                    ScatteringMode::All,
                    light_eval_bsdf);

                Spectrum geometry = ComputeGeometryTerm(shading_context, shading_point, camera_vertex, light_vertex);
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

            float numerator = ComputePathDensity(light_vertices, camera_vertices, s, t, s, t);
            float denominator = 0.0;

            // [p = 0, q = s + t], [p = 1, q = s + t - 1] ... [p = s + t - 2, q = 2]
            for (int i = 0; i <= s + t - 2; i++)
            {
                denominator += ComputePathDensity(light_vertices, camera_vertices, s, t, i, s + t - i);
            }

            float miWeight = numerator / denominator;

            assert(miWeight <= 1.0f);
            radiance.m_beauty += miWeight * result;
        }

        void compute_lighting(
            SamplingContext&        sampling_context,
            const PixelContext&     pixel_context,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            ShadingComponents&      radiance) override      // output radiance, in W.sr^-1.m^-2
        {
            std::vector<BDPTVertex> camera_vertices;
            std::vector<BDPTVertex> light_vertices;

            // camera_vertices[0] is on shading_point
            camera_vertices.reserve(num_max_vertices - 1); 

            // light_vertices[0] is on light source
            light_vertices.reserve(num_max_vertices);

            trace_light(sampling_context, shading_context, &light_vertices);
            trace_camera(sampling_context, shading_context, shading_point, &camera_vertices);

            for (int s = 0;s < light_vertices.size() + 1;s++)
            {
                for (unsigned int t = 2;t < camera_vertices.size() + 2;t++)
                {
                    if (s + t <= num_max_vertices)
                        Connect(shading_context, shading_point, light_vertices, camera_vertices, s, t, radiance);
                }
            }

            shading_context.get_arena().clear();
        }

        void trace_light(
            SamplingContext&            sampling_context,
            const ShadingContext&       shading_context,
            std::vector<BDPTVertex>*    vertices)
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

            light_sample.m_triangle
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

        void trace_emitting_triangle(
            SamplingContext&            sampling_context,
            const ShadingContext&       shading_context,
            LightSample&                light_sample,
            std::vector<BDPTVertex>*    vertices)
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

            BDPTVertex bdpt_vertex;
            bdpt_vertex.m_position = light_shading_point.get_point();
            /// CONFUSE:: why geometric normal is flipped?
            bdpt_vertex.m_geometric_normal = -light_shading_point.get_geometric_normal();
            bdpt_vertex.m_beta = initial_flux;
            bdpt_vertex.m_fwd_pdf = light_sample.m_probability;
            bdpt_vertex.m_rev_pdf = 1.0;
            bdpt_vertex.m_is_light_vertex = true;
            bdpt_vertex.m_shading_point = light_shading_point;
            //bdpt_vertex.m_outgoing = -static_cast<Vector3d>(foundation::normalize(emission_direction));

            vertices->push_back(bdpt_vertex);

            // Build the path tracer.
            PathVisitor path_visitor(initial_flux * dot(emission_direction, Vector3f(light_sample.m_shading_normal)) / edf_prob, shading_context, vertices);
            VolumeVisitor volume_visitor;

            PathTracer<PathVisitor, VolumeVisitor, true> path_tracer(
                path_visitor,
                volume_visitor,
                ~0,
                num_max_vertices,
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
        }

        void trace_non_physical_light(
            SamplingContext&        sampling_context,
            const ShadingContext&   shading_context,
            LightSample&            light_sample)
        {
        }

        void trace_camera(
            SamplingContext&        sampling_context,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            std::vector<BDPTVertex> *vertices)
        {
            PathVisitor path_visitor(Spectrum(1.0), shading_context, vertices);
            VolumeVisitor volume_visitor;

            PathTracer<PathVisitor, VolumeVisitor, false> path_tracer(
                path_visitor,
                volume_visitor,
                ~0,
                num_max_vertices,
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


        struct PathVisitor
        {
            PathVisitor(const Spectrum& initial_beta,
                        const ShadingContext& shading_context,
                        std::vector<BDPTVertex> * vertices)
              : m_initial_beta(initial_beta)
              , m_shading_context(shading_context)
              , m_vertices(vertices)
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
                BDPTVertex bdpt_vertex;
                bdpt_vertex.m_position = vertex.get_point();
                bdpt_vertex.m_geometric_normal = vertex.get_geometric_normal();
                bdpt_vertex.m_shading_basis = Basis3f(vertex.get_shading_basis());
                bdpt_vertex.m_beta = vertex.m_throughput * m_initial_beta;
                bdpt_vertex.m_dir_to_prev_vertex = foundation::normalize(vertex.m_outgoing.get_value());
                bdpt_vertex.m_shading_point = *vertex.m_shading_point;
                bdpt_vertex.m_bsdf = vertex.m_bsdf;
                bdpt_vertex.m_bsdf_data = vertex.m_bsdf_data;
                bdpt_vertex.m_fwd_pdf = vertex.m_prev_prob;
                /// TODO:: compute rev_pdf here

                if (vertex.m_edf)
                {
                    vertex.compute_emitted_radiance(m_shading_context, bdpt_vertex.m_Le);
                    bdpt_vertex.m_is_light_vertex = true;
                }

                bdpt_vertex.m_prev_vertex = (m_vertices->size() == 0) ? nullptr : &m_vertices->at(m_vertices->size() - 1);
                m_vertices->push_back(bdpt_vertex);
            }

            void on_scatter(const PathVertex& vertex)
            {
            }

            const ShadingContext&       m_shading_context;
            Spectrum                    m_initial_beta;
            std::vector<BDPTVertex>*    m_vertices;
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
