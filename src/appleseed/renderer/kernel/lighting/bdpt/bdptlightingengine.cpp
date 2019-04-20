
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Aytek Aman, The appleseedhq Organization
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
#include "foundation/math/fp.h"
#include "foundation/math/population.h"
#include "foundation/utility/arena.h"
#include "foundation/utility/statistics.h"

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // Bidirectional Path Tracing lighting engine.
    //
    // References:
    //
    //   Robust Monte Carlo Methods For Light Transport Simulation
    //   http://graphics.stanford.edu/papers/veach_thesis/thesis.pdf
    //
    //   Bidirectional Path Tracing
    //   http://www.pbr-book.org/3ed-2018/Light_Transport_III_Bidirectional_Methods/Bidirectional_Path_Tracing.html
    //
    // TODO: Write to arbitrary pixel coordinates when t = 1.
    // TODO: Resample light when s = 1.
    // TODO: Support BSDFs with Dirac delta components.
    //

    enum class BDPTVertexType
    {
        Camera, Light, Surface, Medium
    };

    // TODO: Decide if we should use existing PathVertex (in PathVertex.h) or just keep using BDPTVertex.
    struct BDPTVertex
    {
        Vector3d                m_position;
        Vector3d                m_geometric_normal;
        Spectrum                m_beta;
        const BSDF*             m_bsdf;
        const void*             m_bsdf_data;
        Vector3d                m_dir_to_prev_vertex;
        const BDPTVertex*       m_prev_vertex;
        Basis3f                 m_shading_basis;
        Spectrum                m_Le;
        ShadingPoint            m_shading_point;
        bool                    m_is_light_vertex;

        float                   m_fwd_pdf;
        float                   m_rev_pdf;

        BDPTVertex()
          : m_beta(0.0f)
          , m_bsdf(nullptr)
          , m_bsdf_data(nullptr)
          , m_prev_vertex(nullptr)
          , m_Le(0.0f)
          , m_is_light_vertex(false)
          , m_fwd_pdf(0.0f)
          , m_rev_pdf(0.0f)
        {
        }

        // Returns the density converted from probability per steridian to probability per unit area (Veach 8.2.2.2).
        float convert_density(double pdf, const BDPTVertex& next) const
        {
            const Vector3d w = next.m_position - m_position;
            const double dist2 = square_norm(w);
            if (dist2 == 0.0)
                return 0.0;
            const double rcp_dist2 = 1.0 / dist2;
            pdf *= std::abs(dot(next.m_geometric_normal, w * sqrt(rcp_dist2)));
            return static_cast<float>(pdf * rcp_dist2);
        }

        float pdf_light(const BDPTVertex* vertex)
        {
            // TODO: Fix; this assumes diffuse light source.
            float pdf_w = static_cast<float>(std::abs(dot(normalize(vertex->m_position - m_position), m_geometric_normal))) * RcpPi<float>();
            return convert_density(pdf_w, *vertex);
        }

        float pdf(const BDPTVertex* prev, const BDPTVertex* next, const bool adjoint)
        {
            if (m_is_light_vertex && adjoint) return pdf_light(next);

            Vector3f wp = static_cast<Vector3f>(prev ? normalize(prev->m_position - m_position) : m_dir_to_prev_vertex);
            float pdf_w = m_bsdf->evaluate_pdf(
                m_bsdf_data,
                adjoint,
                static_cast<Vector3f>(m_geometric_normal),
                m_shading_basis,
                static_cast<Vector3f>(normalize(next->m_position - m_position)),
                wp,
                ScatteringMode::All);
            return convert_density(pdf_w, *next);
        }
    };

    class BDPTLightingEngine
      : public ILightingEngine
    {
      public:
        struct Parameters
        {
            const size_t    m_max_bounces;                  // maximum number of bounces, ~0 for unlimited

            explicit Parameters(const ParamArray& params)
                : m_max_bounces(fixup_bounces(params.get_optional<int>("max_bounces", 8)))
            {
            }

            static size_t fixup_bounces(const int x)
            {
                return x == -1 ? ~size_t(0) : x;
            }
        };

        BDPTLightingEngine(
            const Project&              project,
            const ForwardLightSampler&  light_sampler,
            const ParamArray&           params)
          : m_light_sampler(light_sampler)
          , m_params(params)
        {
            const Camera* camera = project.get_uncached_active_camera();
            m_shutter_open_begin_time = camera->get_shutter_open_begin_time();
            m_shutter_close_end_time = camera->get_shutter_close_end_time();

            m_num_max_vertices = m_params.m_max_bounces + 3;
        }

        void release() override
        {
            delete this;
        }

        void print_settings() const override
        {
        }

        void compute_lighting(
            SamplingContext&            sampling_context,
            const PixelContext&         pixel_context,
            const ShadingContext&       shading_context,
            const ShadingPoint&         shading_point,
            ShadingComponents&          radiance,               // output radiance, in W.sr^-1.m^-2
            AOVComponents&              aov_components) override
        {
            // TODO: Use arena to alloc BDPTVertices instead.
            BDPTVertex* camera_vertices = new BDPTVertex[m_num_max_vertices - 1];
            BDPTVertex* light_vertices = new BDPTVertex[m_num_max_vertices];

            size_t num_light_vertices = trace_light(sampling_context, shading_context, light_vertices);
            size_t num_camera_vertices = trace_camera(sampling_context, shading_context, shading_point, camera_vertices);

            assert(num_camera_vertices <= m_num_max_vertices - 1);
            assert(num_light_vertices <= m_num_max_vertices);

            for (size_t s = 0; s < num_light_vertices + 1; s++)
            {
                for (size_t t = 2; t < num_camera_vertices + 2; t++)
                {
                    if (s + t <= m_num_max_vertices)
                        connect(shading_context, shading_point, light_vertices, camera_vertices, s, t, radiance);
                }
            }

            delete[] camera_vertices;
            delete[] light_vertices;
        }

        // TODO: Use an output parameter instead of returning a spectrum.
        Spectrum compute_geometry_term(
            const ShadingContext&       shading_context,
            const ShadingPoint&         shading_point,
            const BDPTVertex&           a,
            const BDPTVertex&           b)
        {
            const Vector3d v = b.m_position - a.m_position;
            const Vector3d normalized_v = normalize(v);
            const double dist2 = square_norm(v);

            // TODO: The special care have to be taken for these dot products when it comes to volume.
            const double cos1 = std::abs(-dot(normalized_v, b.m_geometric_normal));
            const double cos2 = std::abs(dot(normalized_v, a.m_geometric_normal));

            Spectrum result(0.0f);

            // do the visibility test
            if (cos1 > 0.0 && cos2 > 0.0)
            {
                shading_context.get_tracer().trace_between_simple(
                    shading_context,
                    a.m_position + (v * 1.0e-6),
                    b.m_position,
                    shading_point.get_ray().m_time,
                    VisibilityFlags::ShadowRay,
                    shading_point.get_ray().m_depth,
                    result);
                double geometry_term = cos1 * cos2 / dist2;
                result *= (float)geometry_term;
            }

            return result;
        }

        // Compute the weight by considering hypothetical subpaths for s, t (Veach: 10.2).
        // Modify the reverse pdf for the current vertex based on the connection strategy.
        float compute_mis_weight(
            BDPTVertex*                 light_vertices,
            BDPTVertex*                 camera_vertices,
            const size_t                s,
            const size_t                t)
        {
            float sum_ri = 0.0f;  // The sum of the ratios of path densities for adjacent sampling strategies.

            BDPTVertex* qs      = s > 0 ? &light_vertices[s - 1]  : nullptr;  // The light connection vertex.
            BDPTVertex* qs_prev = s > 1 ? &light_vertices[s - 2]  : nullptr;  // The predecessor of the light connection vertex.
            BDPTVertex* pt      = t > 1 ? &camera_vertices[t - 2] : nullptr;  // The camera connection vertex.
            BDPTVertex* pt_prev = t > 2 ? &camera_vertices[t - 3] : nullptr;  // The predecessor of the camera connection vertex.

            // Consider hypothetical connection strategies along the camera subpath.
            float ri = 1.0f;
            for (int i = t - 2; i > 0; --i)
            {
                float rev_pdf;

                if (i == t - 2)
                {
                    if (s == 0)  rev_pdf = m_light_sampler.evaluate_pdf(pt->m_shading_point);
                    else         rev_pdf = qs->pdf(qs_prev, pt, true);
                }
                else if (i == t - 3)
                {
                    if (s == 0)  rev_pdf = pt->pdf_light(pt_prev);
                    else         rev_pdf = pt->pdf(qs, pt_prev, true);
                }
                else rev_pdf = camera_vertices[i].m_rev_pdf;

                ri *= rev_pdf / camera_vertices[i].m_fwd_pdf;
                sum_ri += ri;
            }

            // Consider hypothetical connection strategies along the light subpath.
            ri = 1.0f;
            for (int i = s - 1; i >= 0; --i)
            {
                float rev_pdf;

                if (i == s - 1)      rev_pdf = pt->pdf(pt_prev, qs, false);
                else if (i == s - 2) rev_pdf = qs->pdf(pt, qs_prev, false);
                else                 rev_pdf = light_vertices[i].m_rev_pdf;

                ri *= rev_pdf / light_vertices[i].m_fwd_pdf;
                sum_ri += ri;
            }

            return 1.0f / (1.0f + sum_ri);
        }

        void connect(
            const ShadingContext&       shading_context,
            const ShadingPoint&         shading_point,
            BDPTVertex*                 light_vertices,
            BDPTVertex*                 camera_vertices,
            const size_t                s,
            const size_t                t,
            ShadingComponents&          radiance)
        {
            assert(t >= 2);
            Spectrum result(0);

            if (s == 0)
            {
                // Camera subpath is a complete path.
                const BDPTVertex& camera_vertex = camera_vertices[t - 2];
                if (!camera_vertex.m_is_light_vertex)
                    return;
                result = camera_vertex.m_beta * camera_vertex.m_Le;
            }
            else if (s == 1)
            {
                // Only one light vertex.
                const BDPTVertex& light_vertex = light_vertices[s - 1];
                const BDPTVertex& camera_vertex = camera_vertices[t - 2];

                // TODO: Take care of light material as well.
                if (camera_vertex.m_bsdf == nullptr || camera_vertex.m_bsdf_data == nullptr)
                    return;

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

                result = geometry * camera_eval_bsdf.m_beauty * camera_vertex.m_beta * light_vertex.m_beta;
            }
            else
            {
                const BDPTVertex& light_vertex = light_vertices[s - 1];
                const BDPTVertex& camera_vertex = camera_vertices[t - 2];

                if (light_vertex.m_bsdf == nullptr || camera_vertex.m_bsdf == nullptr ||
                    light_vertex.m_bsdf_data == nullptr || camera_vertex.m_bsdf == nullptr)
                    return;

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

            // Check if throughput is near black or not.
            if (fz(result, 1.0e-4f))
                return;

            const float mis_weight = compute_mis_weight(light_vertices, camera_vertices, s, t);
            assert(mis_weight <= 1.0f + 0.01f /* epsilon */);
            radiance.m_beauty += mis_weight * result;
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
            m_light_sampler.sample(
                ShadingRay::Time::create_with_normalized_time(
                    s[0],
                    m_shutter_open_begin_time,
                    m_shutter_close_end_time),
                Vector3f(s[1], s[2], s[3]),
                light_sample);

            return
                light_sample.m_shape != nullptr
                    ? trace_emitting_shape(
                        sampling_context,
                        shading_context,
                        light_sample,
                        vertices)
                    : trace_non_physical_light(
                        sampling_context,
                        shading_context,
                        light_sample);
        }

        size_t trace_emitting_shape(
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

            const Material* material = light_sample.m_shape->get_material();
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
            size_t num_light_vertices = 1;
            PathVisitor path_visitor(
                true,
                initial_flux
                    * std::abs(dot(emission_direction, Vector3f(light_sample.m_shading_normal)))
                    / edf_prob,
                shading_context,
                vertices,
                &num_light_vertices);
            VolumeVisitor volume_visitor;
            PathTracer<PathVisitor, VolumeVisitor, true> path_tracer(
                path_visitor,
                volume_visitor,
                ~size_t(0),
                m_num_max_vertices - 2,
                ~size_t(0),
                ~size_t(0),
                ~size_t(0),
                ~size_t(0),
                false,                                  // don't clamp roughness
                shading_context.get_max_iterations());  // don't illuminate points closer than the light near start value

            const size_t light_path_length =
                path_tracer.trace(
                    sampling_context,
                    shading_context,
                    light_ray,
                    &parent_shading_point,
                    false);

            m_light_path_length.insert(light_path_length);
            assert(num_light_vertices <= m_num_max_vertices);
            return num_light_vertices;
        }

        size_t trace_non_physical_light(
            SamplingContext&            sampling_context,
            const ShadingContext&       shading_context,
            LightSample&                light_sample)
        {
            return 0;
        }

        size_t trace_camera(
            SamplingContext&            sampling_context,
            const ShadingContext&       shading_context,
            const ShadingPoint&         shading_point,
            BDPTVertex*                 vertices)
        {
            size_t num_camera_vertices = 0;
            PathVisitor path_visitor(
                false,
                Spectrum(1.0),
                shading_context,
                vertices,
                &num_camera_vertices);
            VolumeVisitor volume_visitor;

            PathTracer<PathVisitor, VolumeVisitor, false> path_tracer(
                path_visitor,
                volume_visitor,
                ~size_t(0),
                m_num_max_vertices - 2,
                ~size_t(0),
                ~size_t(0),
                ~size_t(0),
                ~size_t(0),
                false,                                  // don't clamp roughness
                shading_context.get_max_iterations());

            const size_t camera_path_length =
                path_tracer.trace(
                    sampling_context,
                    shading_context,
                    shading_point,
                    false);

            m_camera_path_length.insert(camera_path_length);
            assert(num_camera_vertices <= m_num_max_vertices - 1);
            return num_camera_vertices;
        }

        StatisticsVector get_statistics() const override
        {
            Statistics stats;

            return StatisticsVector::make("bdpt statistics", stats);
        }

    private:
        const Parameters            m_params;

        const ForwardLightSampler&  m_light_sampler;

        float                       m_shutter_open_begin_time;
        float                       m_shutter_close_end_time;

        Population<uint64>          m_light_path_length;
        Population<uint64>          m_camera_path_length;

        size_t                      m_num_max_vertices;

        struct PathVisitor
        {
            const bool                      m_adjoint;
            const ShadingContext&           m_shading_context;
            Spectrum                        m_initial_beta;
            BDPTVertex*                     m_vertices;
            size_t*                         m_num_vertices;

            PathVisitor(
                const bool                  adjoint,
                const Spectrum&             initial_beta,
                const ShadingContext&       shading_context,
                BDPTVertex*                 vertices,
                size_t*                     num_vertices)
              : m_adjoint(adjoint)
              , m_initial_beta(initial_beta)
              , m_shading_context(shading_context)
              , m_vertices(vertices)
              , m_num_vertices(num_vertices)
            {
            }

            void on_first_diffuse_bounce(const PathVertex& vertex)
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
                // Create BDPT Vertex.
                BDPTVertex& bdpt_vertex = m_vertices[*m_num_vertices];
                bdpt_vertex.m_beta = vertex.m_throughput * m_initial_beta;
                bdpt_vertex.m_bsdf = vertex.m_bsdf;
                bdpt_vertex.m_bsdf_data = vertex.m_bsdf_data;
                bdpt_vertex.m_dir_to_prev_vertex = normalize(vertex.m_outgoing.get_value());
                bdpt_vertex.m_geometric_normal = vertex.get_geometric_normal();
                bdpt_vertex.m_position = vertex.get_point();
                bdpt_vertex.m_shading_basis = Basis3f(vertex.get_shading_basis());
                bdpt_vertex.m_shading_point = *vertex.m_shading_point;

                if (vertex.m_edf)
                {
                    vertex.compute_emitted_radiance(m_shading_context, bdpt_vertex.m_Le);
                    bdpt_vertex.m_is_light_vertex = true;
                }

                BDPTVertex* bdpt_vertex_prev1 = *m_num_vertices > 0 ? &m_vertices[*m_num_vertices - 1] : nullptr;
                if (bdpt_vertex_prev1)
                {
                    bdpt_vertex.m_fwd_pdf = bdpt_vertex_prev1->pdf(nullptr, &bdpt_vertex, m_adjoint);

                    if (bdpt_vertex.m_fwd_pdf == 0.0f) bdpt_vertex.m_fwd_pdf = 1.0f;
                    assert(bdpt_vertex.m_fwd_pdf > 0.0f);

                    BDPTVertex* bdpt_vertex_prev2 = *m_num_vertices > 1 ? &m_vertices[*m_num_vertices - 2] : nullptr;
                    if (bdpt_vertex_prev2)
                    {
                        bdpt_vertex_prev2->m_rev_pdf = bdpt_vertex_prev1->convert_density(vertex.m_prev_prob, *bdpt_vertex_prev2);
                        assert(bdpt_vertex_prev2->m_rev_pdf >= 0.0f);
                    }
                }

                (*m_num_vertices)++;
            }

            void on_scatter(PathVertex& vertex)
            {
            }
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

            void on_scatter(PathVertex& vertex)
            {
            }

            void visit_ray(PathVertex& vertex, const ShadingRay& volume_ray)
            {
            }
        };
    };
}


//
// BDPTLightingEngineFactory class implementation.
//

Dictionary BDPTLightingEngineFactory::get_params_metadata()
{
    Dictionary metadata;

    metadata.dictionaries().insert(
        "enable_ibl",
        Dictionary()
            .insert("type", "bool")
            .insert("default", "on")
            .insert("label", "Enable IBL")
            .insert("help", "Enable image-based lighting"));

    metadata.dictionaries().insert(
        "max_bounces",
        Dictionary()
            .insert("type", "int")
            .insert("default", "8")
            .insert("unlimited", "true")
            .insert("min", "0")
            .insert("label", "Max Bounces")
            .insert("help", "Maximum number of bounces"));

    metadata.dictionaries().insert(
        "dl_light_samples",
        Dictionary()
            .insert("type", "float")
            .insert("default", "1.0")
            .insert("label", "Light Samples")
            .insert("help", "Number of samples used to estimate direct lighting"));

    metadata.dictionaries().insert(
        "dl_low_light_threshold",
        Dictionary()
            .insert("type", "float")
            .insert("default", "0.0")
            .insert("label", "Low Light Threshold")
            .insert("help", "Light contribution threshold to disable shadow rays"));

    metadata.dictionaries().insert(
        "ibl_env_samples",
        Dictionary()
            .insert("type", "float")
            .insert("default", "1.0")
            .insert("label", "IBL Samples")
            .insert("help", "Number of samples used to estimate environment lighting"));

    return metadata;
}

BDPTLightingEngineFactory::BDPTLightingEngineFactory(
    const Project&              project,
    const ForwardLightSampler&  light_sampler,
    const ParamArray&           params)
  : m_project(project)
  , m_light_sampler(light_sampler)
  , m_params(params)
{
}

void BDPTLightingEngineFactory::release()
{
    delete this;
}

ILightingEngine* BDPTLightingEngineFactory::create()
{
    return
        new BDPTLightingEngine(
            m_project,
            m_light_sampler,
            m_params);
}

}   // namespace renderer
