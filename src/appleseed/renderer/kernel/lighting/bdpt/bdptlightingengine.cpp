
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

namespace renderer {
namespace {
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
        foundation::Vector3d    m_incoming;
        foundation::Vector3d    m_geometric_normal;
        Spectrum                m_beta;
        const BSDF*             m_bsdf;
        const void*             m_bsdf_data;
        Basis3f                 m_shading_basis;
        double                  m_fwd_pdf;
        double                  m_rev_pdf;
        Spectrum                m_Le = Spectrum(0.0);

        ///TODO:: create a proper constructor

        double convertDensity(double pdf, const BDPTVertex& vertex)
        {
            Vector3d w = vertex.m_position - m_position;
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
        const unsigned int num_max_vertices = 5;
        struct Parameters
        {
            explicit Parameters(const ParamArray& params)
            {

            }

            void print() const
            {
                RENDERER_LOG_INFO(
                    "bdpt settings:\n");
            }
        };

        BDPTLightingEngine(
            const Project&              project,
            const ForwardLightSampler&  light_sampler,
            const ParamArray&           params)
          : m_light_sampler(light_sampler)
          ,  m_params(params)
        {
            const Camera* camera = project.get_uncached_active_camera();

            m_shutter_open_time = camera->get_shutter_open_time();
            m_shutter_close_time = camera->get_shutter_close_time();
        }

        void release() override
        {
            delete this;
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

        void Connect(
            const ShadingContext&               shading_context,
            const ShadingPoint&                 shading_point,
            const std::vector<BDPTVertex>       &light_vertices,
            const std::vector<BDPTVertex>       &camera_vertices,
            unsigned int                        s,
            unsigned int                        t,
            ShadingComponents&                  radiance)
        {
            // camera subpath is a complete path
            if (s == 0)
            {
                const BDPTVertex& camera_vertex = camera_vertices[t - 2];
                radiance.m_beauty += camera_vertex.m_beta * camera_vertex.m_Le;
            }
            else if (s == 1)
            {
                const BDPTVertex& light_vertex = light_vertices[s - 1];
                const BDPTVertex& camera_vertex = camera_vertices[t - 2];
                Spectrum geometry = ComputeGeometryTerm(shading_context, shading_point, camera_vertex, light_vertex);

                DirectShadingComponents camera_eval_bsdf;
                const float camera_eval_bsdf_prob = camera_vertex.m_bsdf->evaluate(
                    camera_vertex.m_bsdf_data,
                    false,
                    false,
                    static_cast<Vector3f>(camera_vertex.m_geometric_normal),
                    camera_vertex.m_shading_basis,
                    static_cast<Vector3f>(camera_vertex.m_incoming),
                    static_cast<Vector3f>(foundation::normalize(light_vertex.m_position - camera_vertex.m_position)),
                    ScatteringMode::All,
                    camera_eval_bsdf);

                /// TODO:: check why inv pi is missing here
                radiance.m_beauty += geometry * camera_eval_bsdf.m_beauty * camera_vertex.m_beta * light_vertex.m_beta * RcpPi<float>();
            }
            else
            {
                const BDPTVertex& light_vertex = light_vertices[s - 1];
                const BDPTVertex& camera_vertex = camera_vertices[t - 2];
                DirectShadingComponents camera_eval_bsdf;
                const float camera_eval_bsdf_prob = camera_vertex.m_bsdf->evaluate(
                    camera_vertex.m_bsdf_data,
                    false,
                    false,
                    static_cast<Vector3f>(camera_vertex.m_geometric_normal),
                    camera_vertex.m_shading_basis,
                    static_cast<Vector3f>(camera_vertex.m_incoming),
                    static_cast<Vector3f>(foundation::normalize(light_vertex.m_position - camera_vertex.m_position)),
                    ScatteringMode::All,
                    camera_eval_bsdf);

                DirectShadingComponents light_eval_bsdf;
                const float light_eval_bsdf_prob = light_vertex.m_bsdf->evaluate(
                    light_vertex.m_bsdf_data,
                    false,
                    false,
                    static_cast<Vector3f>(light_vertex.m_geometric_normal),
                    light_vertex.m_shading_basis,
                    static_cast<Vector3f>(light_vertex.m_incoming),
                    static_cast<Vector3f>(foundation::normalize(camera_vertex.m_position - light_vertex.m_position)),
                    ScatteringMode::All,
                    light_eval_bsdf);

                Spectrum geometry = ComputeGeometryTerm(shading_context, shading_point, camera_vertex, light_vertex);
                radiance.m_beauty += geometry * camera_eval_bsdf.m_beauty * light_eval_bsdf.m_beauty * camera_vertex.m_beta * light_vertex.m_beta;
            }
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

            /*for (int s = 0;s < light_vertices.size() + 1;s++)
            {
                for (unsigned int t = 2;t < camera_vertices.size() + 2;t++)
                {
                    if (s + t <= num_max_vertices)
                    {
                        Connect(shading_context, shading_point, light_vertices, camera_vertices, s, t, radiance);
                    }
                }
            }*/

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
            m_light_sampler.sample(
                ShadingRay::Time::create_with_normalized_time(
                    s[0],
                    m_shutter_open_time,
                    m_shutter_close_time),
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
            Spectrum initial_flux = edf_value;
            initial_flux *=
                dot(emission_direction, Vector3f(light_sample.m_shading_normal)) /
                (light_sample.m_probability * edf_prob);

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
                    m_shutter_open_time,
                    m_shutter_close_time);
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
            bdpt_vertex.m_fwd_pdf = light_sample.m_probability * edf_prob;
            bdpt_vertex.m_rev_pdf = 1.0;

            vertices->push_back(bdpt_vertex);

            // Build the path tracer.
            PathVisitor path_visitor(initial_flux, shading_context, vertices);
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

        const ForwardLightSampler&  m_light_sampler;
        //Intersector                 m_intersector;

        float                       m_shutter_open_time;
        float                       m_shutter_close_time;

        Population<uint64>          m_light_path_length;

        struct PathVisitor
        {
            PathVisitor(const Spectrum & initial_beta, const ShadingContext &shading_context, std::vector<BDPTVertex> * vertices)
              : m_initial_beta(initial_beta),
                m_shading_context(shading_context),
                m_vertices(vertices)
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
                if (vertex.m_bsdf == nullptr || vertex.m_bsdf_data == nullptr)
                    return;

                if (vertex.m_bsdf->is_purely_specular())
                    return;

                BDPTVertex bdpt_vertex;
                bdpt_vertex.m_position = vertex.get_point();
                bdpt_vertex.m_incoming = normalize(vertex.m_outgoing.get_value());
                bdpt_vertex.m_geometric_normal = vertex.get_geometric_normal();
                bdpt_vertex.m_shading_basis = Basis3f(vertex.get_shading_basis());
                bdpt_vertex.m_beta = vertex.m_throughput * m_initial_beta;
                bdpt_vertex.m_bsdf = vertex.m_bsdf;
                bdpt_vertex.m_bsdf_data = vertex.m_bsdf_data;
                bdpt_vertex.m_rev_pdf = 1.0;
                bdpt_vertex.m_fwd_pdf = 1.0;

                if (!m_vertices->empty())
                {
                    // compute fwd and rev pdf
                    BDPTVertex & prev_bdpt_vertex = m_vertices->back();
                    bdpt_vertex.m_fwd_pdf = prev_bdpt_vertex.convertDensity(vertex.m_prev_prob, bdpt_vertex);

                    double rev_pdf = vertex.m_bsdf->evaluate_pdf(
                        prev_bdpt_vertex.m_bsdf_data,
                        false,
                        static_cast<Vector3f>(bdpt_vertex.m_geometric_normal),
                        bdpt_vertex.m_shading_basis,
                        static_cast<Vector3f>(foundation::normalize(prev_bdpt_vertex.m_position - bdpt_vertex.m_position)),
                        static_cast<Vector3f>(foundation::normalize(vertex.get_ray().m_dir)),
                        ScatteringMode::All);

                    prev_bdpt_vertex.m_rev_pdf = bdpt_vertex.convertDensity(rev_pdf, bdpt_vertex);
                }

                if (vertex.m_edf)
                {
                    vertex.compute_emitted_radiance(m_shading_context, bdpt_vertex.m_Le);
                }

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
    const ForwardLightSampler&  light_sampler,
    const ParamArray&           params)
    : m_project(project)
    , m_light_sampler(light_sampler)
    , m_params(params)
{
    BDPTLightingEngine::Parameters(params).print();
}

void BDPTLightingEngineFactory::release()
{
    delete this;
}

ILightingEngine* BDPTLightingEngineFactory::create()
{
    return new BDPTLightingEngine(
        m_project,
        m_light_sampler,
        m_params);
}

Dictionary BDPTLightingEngineFactory::get_params_metadata()
{
    Dictionary metadata;
    add_common_params_metadata(metadata, true);

    return metadata;
}

}   // namespace renderer
