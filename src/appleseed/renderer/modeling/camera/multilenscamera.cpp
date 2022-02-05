
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2021-2022 Jan Willi, The appleseedhq Organization
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
#include "multilenscamera.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/kernel/texturing/texturestore.h"
#include "renderer/modeling/camera/lenscamera.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/visibilityflags.h"
#include "renderer/utility/paramarray.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/dual.h"
#include "foundation/math/matrix.h"
#include "foundation/math/intersection/raysphere.h"
#include "foundation/math/intersection/rayplane.h"
#include "foundation/math/sampling/imageimportancesampler.h"
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/types.h"
#include "foundation/string/string.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/iostreamop.h"

// Boost headers.
#include "boost/filesystem.hpp"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>
#include <fstream>
#include <limits>
#include <random>
#include <regex>
#include <vector>

// Forward declarations.
namespace foundation { class IAbortSwitch; }
namespace renderer { class OnRenderBeginRecorder; }

using namespace foundation;

namespace renderer
{

namespace
{
    //
    // A multi lens camera that uses text files of lens descriptions to simulate real lenses.
    //

    typedef ImageImportanceSampler<Vector2d, float> ImageImportanceSamplerType;

    const char* Model = "multilens_camera";

class MultiLensCamera
        : public LensCamera
    {
    public:
        MultiLensCamera(
            const char* name,
            const ParamArray& params)
            : LensCamera(name, params)
        {
            m_inputs.declare("diaphragm_map", InputFormat::SpectralReflectance, "");
        }

        void release() override
        {
            delete this;
        }

        const char* get_model() const override
        {
            return Model;
        }

        void print_settings() const override
        {
            RENDERER_LOG_INFO(
                "camera \"%s\" (#" FMT_UNIQUE_ID ") parameters:\n"
                "  model                         %s\n"
                "  lens file                     %s\n"
                "  film width                    %f\n"
                "  film height                   %f\n"
                "  original focal length         %f\n"
                "  focal length                  %f\n"
                "  original f-number             %f\n"
                "  f-number                      %f\n"
                "  autofocus                     %s\n"
                "  autofocus target              %f, %f\n"
                "  diaphragm map                 %s\n"
                "  diaphragm blades              %s\n"
                "  diaphragm tilt angle          %f\n"
                "  near z                        %f\n"
                "  shift                         %f, %f\n"
                "  shutter open begin time       %f\n"
                "  shutter open end time         %f\n"
                "  shutter close begin time      %f\n"
                "  shutter close end time        %f",
                get_path().c_str(),
                get_uid(),
                Model,
                m_lens_file.c_str(),
                m_film_dimensions[0],
                m_film_dimensions[1],
                m_lens_focal_length,
                m_focal_length,
                m_lens_f_number,
                m_f_number,
                m_autofocus_enabled ? "on" : "off",
                m_autofocus_target[0],
                m_autofocus_target[1],
                m_diaphragm_map_bound ? "on" : "off",
                pretty_uint(m_diaphragm_blade_count).c_str(),
                m_diaphragm_tilt_angle,
                m_near_z,
                m_shift.x,
                m_shift.y,
                m_shutter_open_begin_time,
                m_shutter_open_end_time,
                m_shutter_close_begin_time,
                m_shutter_close_end_time);
        }

        bool on_render_begin(
            const Project& project,
            const BaseGroup* parent,
            OnRenderBeginRecorder& recorder,
            IAbortSwitch* abort_switch) override
        {
            if (!PerspectiveCamera::on_render_begin(project, parent, recorder, abort_switch))
                return false;

            // Extract autofocus status.
            m_autofocus_enabled = m_params.get_optional<bool>("autofocus_enabled", true);

            // Extract autofocus target and focal distance.
            extract_focal_distance(
                m_autofocus_enabled,
                m_autofocus_target,
                m_focal_distance);

            // Extract diaphragm configuration.
            m_diaphragm_map_bound = build_diaphragm_importance_sampler(*project.get_scene());
            extract_diaphragm_blade_count();
            extract_diaphragm_tilt_angle();

            // Build the diaphragm polygon.
            if (!m_diaphragm_map_bound && m_diaphragm_blade_count > 0)
            {
                m_diaphragm_vertices.resize(m_diaphragm_blade_count);
                build_regular_polygon(
                    m_diaphragm_blade_count,
                    m_diaphragm_tilt_angle,
                    &m_diaphragm_vertices.front());
            }

            // Read the lens file and scale it to meters.
            const double scale = 0.001;
            if (!read_lens_file(project, scale))
                return false;

            // Extract the focal length.
            m_focal_length = extract_focal_length();

            // Compute the focal length of the lens.
            double p_film, f_film;
            if (!compute_thick_lens_film(p_film, f_film))
                return false;
            m_lens_focal_length = f_film - p_film;

            // Scale the lens if a new focal length is set by the user.
            if (m_focal_length > 0.0)
                scale_lens_elements(m_lens_focal_length, m_focal_length);
            else
                m_focal_length = m_lens_focal_length;

            // Extract F-number.
            m_f_number = m_params.get_optional<double>("f_stop", -1);

            // Calculate F-number of the lens.
            m_lens_f_number = m_focal_length / m_lens_container.at(m_aperture_index).diameter;

            // Scale the aperture if a new F-number is set by the user.
            if (m_f_number > 0.0)
                adjust_f_number();
            else
                m_f_number = m_lens_f_number;

            if (!m_diaphragm_map_bound && m_diaphragm_blade_count > 0)
                transform_diaphragm_to_camera_space();

            return true;
        }

        bool on_frame_begin(
            const Project& project,
            const BaseGroup* parent,
            OnFrameBeginRecorder& recorder,
            IAbortSwitch* abort_switch) override
        {
            if (!Camera::on_frame_begin(project, parent, recorder, abort_switch))
                return false;

            m_total_z = get_total_z_offset();

            // Perform autofocus, if enabled.
            if (m_autofocus_enabled)
            {
                TextureStore texture_store(*project.get_scene());
                TextureCache texture_cache(texture_store);
                Intersector intersector(project.get_trace_context(), texture_cache);
                m_focal_distance = get_autofocus_focal_distance(intersector);
            }

            // Focus lens using thick lens approximation.
            if (!focus_lens(m_focal_distance))
                return false;

            m_total_z = get_total_z_offset();
            m_last_z = m_total_z - m_lens_container.back().thickness;

            const double max_error = 1e-12;
            const int max_iter = 100;

            // Calculate entrance pupil or use first lens element.
            if (!compute_sample_pupil(Pupil::entrance, max_error, max_iter))
                return false;

            // Calculate exit pupil or use last lens element.
            if (!compute_sample_pupil(Pupil::exit, max_error, max_iter))
                return false;

            return true;
        }

        bool spawn_ray(
            SamplingContext& sampling_context,
            const Dual2d& ndc,
            ShadingRay& ray) const override
        {
            // Initialize the ray.
            initialize_ray(sampling_context, ray);

            // Retrieve the camera transform.
            Transformd scratch;
            const Transformd& transform =
                m_transform_sequence.evaluate(ray.m_time.m_absolute, scratch);

            // Film point in camera space.
            const Vector3d film_point = ndc_to_camera(ndc.get_value());

            // Sampled lens point in camera space.
            const Vector3d lens_point = sample_pupil(sampling_context, m_exit_pupil_radius, m_exit_pupil_center_z);

            ray.m_org = film_point;
            ray.m_dir = normalize(lens_point - film_point);

            if (!trace_ray_from_film(ray))
                return false;

            ray.m_org = transform.point_to_parent(ray.m_org);
            ray.m_dir = normalize(transform.vector_to_parent(ray.m_dir));

            if (ndc.has_derivatives())
            {
                const Vector2d px(ndc.get_value() + ndc.get_dx());
                const Vector2d py(ndc.get_value() + ndc.get_dy());
                const Vector3d film_point_px = ndc_to_camera(px);
                const Vector3d film_point_py = ndc_to_camera(py);

                Ray3d testing_ray_px(film_point_px, normalize(lens_point - film_point_px));
                Ray3d testing_ray_py(film_point_py, normalize(lens_point - film_point_py));

                if (!trace_ray_from_film(testing_ray_px))
                    return false;
                if (!trace_ray_from_film(testing_ray_py))
                    return false;

                ray.m_rx_org = transform.point_to_parent(testing_ray_px.m_org);
                ray.m_rx_dir = normalize(transform.vector_to_parent(testing_ray_px.m_dir));
                ray.m_ry_org = transform.point_to_parent(testing_ray_py.m_org);
                ray.m_ry_dir = normalize(transform.vector_to_parent(testing_ray_py.m_dir));

                ray.m_has_differentials = true;
            }

            return true;
        }

        bool connect_vertex(
            SamplingContext& sampling_context,
            const float time,
            const Vector3d& point,
            Vector2d& ndc,
            Vector3d& outgoing,
            float& importance) const override
        {
            // Retrieve the camera transform.
            Transformd scratch;
            const Transformd& transform = m_transform_sequence.evaluate(time, scratch);

            // Transform input point to camera space.
            const Vector3d input_point = transform.point_to_local(point);

            // Sample lens point in camera space.
            const Vector3d pupil_point = sample_pupil(sampling_context, m_entrance_pupil_radius, m_entrance_pupil_center_z);

            Ray3d ray(input_point, normalize(pupil_point - input_point));

            if (!trace_ray_from_scene(ray))
                return false;

            // Compute the intersection of the ray with the film plane.
            double t;
            if (!intersect(ray, Vector3d(0, 0, m_total_z), Vector3d(0, 0, -1), t))
                return false;

            const Vector3d film_point = ray.point_at(t);

            // Convert film point to normalized device coordinates.
            ndc = camera_to_ndc(film_point);

            // The connection is impossible if the film point lies outside the film.
            if (ndc[0] < 0.0 || ndc[0] >= 1.0 ||
                ndc[1] < 0.0 || ndc[1] >= 1.0)
                return false;

            // Transform the outgoing direction vector to world space.
            outgoing = transform.vector_to_parent(input_point - pupil_point);

            // Compute the emitted importance.
            const double square_dist_film_lens = square_norm(film_point);
            const double dist_film_lens = std::sqrt(square_dist_film_lens);
            const double cos_theta = m_focal_length / dist_film_lens;
            const double solid_angle = m_pixel_area * cos_theta / square_dist_film_lens;
            importance = 1.0f / static_cast<float>(square_norm(outgoing) * solid_angle);

            // The connection was possible.
            return true;
        }

    private:
        // Parameters.
        double                   m_f_number;                   // F-number
        bool                     m_autofocus_enabled;          // is autofocus enabled?
        Vector2d                 m_autofocus_target;           // autofocus target on film plane, in NDC
        double                   m_focal_distance;             // focal distance in camera space
        bool                     m_diaphragm_map_bound;        // is a diaphragm map bound to the camera
        Source::Hints            m_diaphragm_map_hints;

        std::vector<LensElement> m_lens_container;             // container of the lens elements
        std::string              m_lens_file;                  // path to file, where lens configuration is stored
        int                      m_aperture_index;             // index of the aperture in the lens container
        double                   m_lens_focal_length;          // original focal length of the lens
        double                   m_lens_f_number;              // original F-number of the lens

        double                   m_entrance_pupil_radius;      // radius of the entrance pupil
        double                   m_entrance_pupil_center_z;    // z coordinate of the center of the entrance pupil
        double                   m_exit_pupil_radius;          // list of radii of exit pupils from center to edge of film
        double                   m_exit_pupil_center_z;        // z coordinate of the center of the exit pupil
        double                   m_total_z;                    // z coordinate of the film plane
        double                   m_last_z;                     // z coordinate of the last lens element

        std::vector<Vector2d>    m_diaphragm_vertices;         // vertices of the diaphragm polygon
        std::vector<Vector2d>    m_diaphragm_vertices_camera;  // vertices of the diaphragm polygon translated to camera space coordinates

        // Importance sampler to sample the diaphragm map.
        std::unique_ptr<ImageImportanceSamplerType>
            m_importance_sampler;

        void extract_lens_file()
        {
            if (has_param("lens_file"))
                m_lens_file = m_params.get_required<std::string>("lens_file");
        }

        void extract_diaphragm_blade_count()
        {
            const int blade_count = m_params.get_optional<int>("diaphragm_blades", 0);

            if (blade_count == 0 || blade_count >= 3)
                m_diaphragm_blade_count = static_cast<size_t>(blade_count);
            else
            {
                m_diaphragm_blade_count = 0;
                RENDERER_LOG_ERROR(
                    "while defining camera \"%s\": invalid value \"%d\" for parameter \"%s\", "
                    "using default value \"" FMT_SIZE_T "\".",
                    get_path().c_str(),
                    blade_count,
                    "diaphragm_blades",
                    m_diaphragm_blade_count);
            }
        }

        double extract_focal_length() const
        {
            if (has_param("focal_length"))
            {
                if (has_param("horizontal_fov"))
                {
                    RENDERER_LOG_WARNING(
                        "while defining camera \"%s\": the parameter \"horizontal_fov\" "
                        "has precedence over \"focal_length\".",
                        get_path().c_str());

                    const double hfov = m_params.get_required<double>("horizontal_fov");
                    return hfov_to_focal_length(m_film_dimensions[0], deg_to_rad(hfov));
                }
                return m_params.get_required<double>("focal_length");
            }
            else if (has_param("horizontal_fov"))
            {
                const double hfov = m_params.get_required<double>("horizontal_fov");
                return hfov_to_focal_length(m_film_dimensions[0], deg_to_rad(hfov));
            }
            return -1;
        }

        bool build_diaphragm_importance_sampler(const Scene& scene)
        {
            const Source* diaphragm_map_source = m_inputs.source("diaphragm_map");
            if (diaphragm_map_source == nullptr)
                return false;

            m_diaphragm_map_hints = diaphragm_map_source->get_hints();

            TextureStore texture_store(scene);
            TextureCache texture_cache(texture_store);
            ImageSampler sampler(
                texture_cache,
                diaphragm_map_source,
                m_diaphragm_map_hints.m_width,
                m_diaphragm_map_hints.m_height);

            m_importance_sampler.reset(
                new ImageImportanceSamplerType(
                    m_diaphragm_map_hints.m_width,
                    m_diaphragm_map_hints.m_height));
            m_importance_sampler->rebuild(sampler);

            return true;
        }

        void transform_diaphragm_to_camera_space()
        {
            // Transform vertices from normalized coordinates to camera space coordinates.
            double aperture_diameter = m_lens_container.at(m_aperture_index).diameter;
            m_diaphragm_vertices_camera.reserve(m_diaphragm_vertices.size());
            for (Vector2d vertex : m_diaphragm_vertices)
            {
                Vector2d v(vertex.x * 0.5 * aperture_diameter - m_shift.x,
                    vertex.y * 0.5 * aperture_diameter - m_shift.y);

                m_diaphragm_vertices_camera.push_back(v);
            }
        }

        double get_autofocus_focal_distance(const Intersector& intersector) const
        {
            // The autofocus considers the scene at the middle of the shutter interval.
            const float time = get_shutter_middle_time();
            const Transformd transform = m_transform_sequence.evaluate(time);

            Vector3d film_point = ndc_to_camera(m_autofocus_target);

            Vector3d lens_point(0, 0, m_exit_pupil_center_z);

            Ray3d testing_ray(film_point, normalize(lens_point - film_point));

            if (!trace_ray_from_film(testing_ray))
            {
                RENDERER_LOG_INFO(
                    "camera \"%s\": autofocus could not be computed. Set focal distance to infinity.",
                    get_path().c_str());

                return 1.0e38;
            }

            // Create a ray that goes through the center of the lens.
            ShadingRay ray;
            ray.m_org = transform.point_to_parent(testing_ray.m_org);
            ray.m_dir = normalize(transform.vector_to_parent(testing_ray.m_dir));
            ray.m_tmin = 0.0;
            ray.m_tmax = std::numeric_limits<double>::max();
            ray.m_time =
                ShadingRay::Time::create_with_normalized_time(
                    0.5f,
                    get_shutter_open_begin_time(),
                    get_shutter_close_end_time());
            ray.m_flags = VisibilityFlags::ProbeRay;
            ray.m_depth = 0;

            // Trace the ray.
            ShadingPoint shading_point;
            intersector.trace(ray, shading_point);

            if (shading_point.hit_surface())
            {
                // Hit: compute the focal distance.
                const Vector3d v = shading_point.get_point() - ray.m_org;
                const double af_focal_distance = -transform.vector_to_local(v).z;

                RENDERER_LOG_INFO(
                    "camera \"%s\": autofocus sets focal distance to %f (using camera position at time=%.1f).",
                    get_path().c_str(),
                    af_focal_distance,
                    ray.m_time.m_absolute);

                return af_focal_distance;
            }
            else
            {
                // Miss: focus at infinity.
                RENDERER_LOG_INFO(
                    "camera \"%s\": autofocus sets focal distance to infinity (using camera position at time=%.1f).",
                    get_path().c_str(),
                    ray.m_time.m_absolute);

                return 1.0e38;
            }
        }

        Vector3d sample_pupil(SamplingContext& sampling_context, const double radius, const double center_z) const
        {
            sampling_context.split_in_place(2, 1);
            const Vector2d s = sampling_context.next2<Vector2d>();
            const Vector2d lens_point = radius * sample_disk_uniform(s);
            return Vector3d(lens_point.x, lens_point.y, center_z);
        }

        bool compute_sample_pupil(Pupil pupil, double max_err, int max_iter)
        {
            double& radius = pupil == Pupil::entrance ? m_entrance_pupil_radius : m_exit_pupil_radius;
            double& center_z = pupil == Pupil::entrance ? m_entrance_pupil_center_z : m_exit_pupil_center_z;

            Vector3d p0, pmin, pmax;
            const double image_plane_z = m_total_z;

            if (pupil == Pupil::entrance)
            {
                // If the first element is the aperture, then use it as entrance pupil.
                if (m_lens_container.front().is_aperture)
                {
                    center_z = 0;
                    radius = 0.5 * m_lens_container.front().diameter;
                    return true;
                }

                const double first_z = 0;
                p0 = Vector3d(0, 0, -image_plane_z);
                pmin = Vector3d(0, 0, first_z); // center of the front lens
                pmax = Vector3d(m_lens_container.front().diameter, 0, first_z); // marginal point of the front lens
            }
            else
            {
                // If the last element is the aperture, then use it as exit pupil.
                if (m_lens_container.back().is_aperture)
                {
                    center_z = m_last_z;
                    radius = 0.5 * m_lens_container.back().diameter;
                    return true;
                }

                p0 = Vector3d(0, 0, image_plane_z); // center of iamge plane
                pmin = Vector3d(0, 0, m_last_z); // center of the rear lens
                pmax = Vector3d(0.5 * m_lens_container.back().diameter, 0, m_last_z); // marginal point of the rear lens
            }

            Ray3d rmin(p0, normalize(pmin - p0));
            Ray3d rmax(p0, normalize(pmax - p0));
            Ray3d r1 = rmax;

            int iter = 0;
            double cos_similarity = dot(rmin.m_dir, rmax.m_dir);
            while (iter < max_iter && 1 - cos_similarity >= max_err)
            {
                // Create temporary ray to send through lens.
                Ray3d testing_ray = r1;
                bool can_pass_through;
                if (pupil == Pupil::entrance)
                    can_pass_through = trace_ray_from_scene(testing_ray, 0, true);
                else
                    can_pass_through = trace_ray_from_film(testing_ray, 0, true);

                if (can_pass_through)
                    rmin = r1;
                else
                    rmax = r1;
                r1.m_dir = normalize((rmin.m_dir + rmax.m_dir) / 2);

                ++iter;
                cos_similarity = dot(rmin.m_dir, rmax.m_dir);
            }

            if (iter >= max_iter)
                return false;

            int next_idx;
            if (pupil == Pupil::entrance)
                next_idx = m_aperture_index - 1;
            else
                next_idx = m_aperture_index + 1;

            double paraxial_offset = 0.5 * m_lens_container.at(next_idx).diameter * 1e-8;
            p0 = Vector3d(0, 0, get_z_offset(m_aperture_index)); // center of the aperture stop
            Vector3d p3 = Vector3d(paraxial_offset, 0, get_z_offset(next_idx)); // paraxial point on the lens before/after the aperture stop

            Ray3d r2(p0, normalize(p3 - p0));

            if (pupil == Pupil::entrance)
            {
                if (!trace_ray_from_film(r2, next_idx, true))
                    return false;
            }
            else
            {
                if (!trace_ray_from_scene(r2, next_idx, true))
                    return false;
            }

            double t = -r2.m_org.x / r2.m_dir.x;

            Vector3d center_point = r2.point_at(t);
            center_z = center_point.z; // z coordinate of the center of the pupil

            t = (center_z - r1.m_org.z) / r1.m_dir.z;
            pmax = r1.point_at(t);
            radius = pmax.x; // radius of the pupil

            return true;
        }

        bool trace_ray_from_scene(Ray3d& ray, const int start_index = 0, const bool ignore_aperture_shape = false) const
        {
            double current_z;
            if (start_index > 0)
                current_z = get_z_offset(start_index);
            else
                current_z = 0;

            for (auto lens_iter = m_lens_container.cbegin() + start_index; lens_iter != m_lens_container.cend(); ++lens_iter)
            {
                const LensElement current_element = *lens_iter;

                Vector3d intersection; // point at which the ray intersects the current lens element
                Vector3d normal; // intersection normal
                if (!intersect_lens(ray, current_element, current_z, ignore_aperture_shape, intersection, normal))
                    return false;

                ray.m_org = intersection;

                if (current_element.radius != 0) {
                    Vector3d t; // refracted ray

                    double prev_ior = 1;
                    if (lens_iter != m_lens_container.cbegin())
                    {
                        auto prev_iter = std::prev(lens_iter);
                        prev_ior = (!(*prev_iter).is_aperture) ? (*prev_iter).ior : 1;
                    }
                    double ior_frac = prev_ior / current_element.ior;

                    if (!refract(-ray.m_dir, normal, ior_frac, t))
                        return false;
                    ray.m_dir = t;
                }

                current_z += current_element.thickness;
            }
            return true;
        }

        bool trace_ray_from_film(Ray3d& ray, const int start_index = 0, const bool ignore_aperture_shape = false) const
        {
            double current_z;
            if (start_index > 0)
                current_z = get_z_offset(start_index);
            else
                current_z = m_total_z;

            for (auto lens_iter = m_lens_container.crbegin() + start_index; lens_iter != m_lens_container.crend(); ++lens_iter)
            {
                const LensElement current_element = *lens_iter;
                current_z -= current_element.thickness;

                Vector3d intersection; // point at which the ray intersects the current lens element
                Vector3d normal; // intersection normal
                if (!intersect_lens(ray, current_element, current_z, ignore_aperture_shape, intersection, normal))
                    return false;

                ray.m_org = intersection;

                if (current_element.radius != 0) {
                    Vector3d t; // refracted ray

                    auto next_iter = std::next(lens_iter);
                    double next_ior = 1;
                    if (next_iter != m_lens_container.crend() && !(*next_iter).is_aperture)
                        next_ior = (*next_iter).ior;
                    double ior_frac = current_element.ior / next_ior;

                    if (!refract(-ray.m_dir, normal, ior_frac, t))
                        return false;
                    ray.m_dir = t;
                }
            }
            return true;
        }

        // Intersects the ray with the current lens element, returns true if the intersection was successful.
        // If the current lens element is not the aperture, the intersection normal is additionally computed.
        bool intersect_lens(Ray3d& ray, LensElement current_element, const double current_z,
            const bool ignore_aperture_shape, Vector3d& intersection, Vector3d& normal) const
        {
            double t; // parameter, at which ray intersects element
            if (current_element.radius == 0)
            {
                // Aperture intersection.
                t = (current_z - ray.m_org.z) / ray.m_dir.z;
            }
            else
            {
                // Lens element intersection.
                const Vector3d center = Vector3d(0, 0, current_z + current_element.radius); // center of the lens element sphere

                double t_out[2] = { FP<double>::snan(), FP<double>::snan() };

                const size_t hits = intersect_sphere_unit_direction(ray, center, current_element.radius, t_out);
                if (hits == 0)
                    return false;

                if ((ray.m_dir.z > 0) ^ (current_element.radius > 0))
                    t = std::max(t_out[0], t_out[1]);
                else
                    t = std::min(t_out[0], t_out[1]);

                if (t < 0)
                    return false;

                normal = normalize((ray.point_at(t)) - center);
                normal = faceforward(normal, ray.m_dir);
            }

            // Test intersection point and set as new origin if inside bounds.
            intersection = ray.point_at(t);
            // Stop if intersection point lies outside of lens radius.
            if (!is_valid_intersection_point(current_element, intersection, ignore_aperture_shape))
                return false;

            return true;
        }

        // Tests if the intersection point p lies on the lens element.
        bool is_valid_intersection_point(const LensElement& element, Vector3d p, bool ignore_aperture_shape) const
        {
            if (!element.is_aperture || ignore_aperture_shape)
                return p.x * p.x + p.y * p.y <= 0.5 * element.diameter * 0.5 * element.diameter;
            else
            {
                bool pythag_test = p.x * p.x + p.y * p.y <= 0.5 * element.diameter * 0.5 * element.diameter;

                if (m_diaphragm_map_bound)
                {
                    // If the point lies outside the lens element, we can stop already here.
                    if (!pythag_test)
                        return false;

                    int x = static_cast<int>(std::floor((p.x + 0.5 * element.diameter) / element.diameter * m_diaphragm_map_hints.m_width));
                    int y = static_cast<int>(std::floor((p.y + 0.5 * element.diameter) / element.diameter * m_diaphragm_map_hints.m_height));

                    float probability = m_importance_sampler->get_pdf((size_t)x, (size_t)y);

                    return probability != 0;
                }
                else if (m_diaphragm_blade_count == 0)
                    return pythag_test;
                else
                {
                    // If the point lies outside the lens element, we can stop already here.
                    if (!pythag_test)
                        return false;

                    // Ray casting algorithm.
                    // Horizontal ray starting at point p.
                    // Count number of intersections with the polygon, odd = inside, even = outside.
                    // For more information, see https://wrf.ecse.rpi.edu/Research/Short_Notes/pnpoly.html
                    bool inside = false;
                    for (size_t i = 0, j = m_diaphragm_blade_count - 1; i < m_diaphragm_blade_count; j = i++)
                    {
                        if (((m_diaphragm_vertices_camera[i].y > p.y) != (m_diaphragm_vertices_camera[j].y > p.y)) &&
                            (p.x < (m_diaphragm_vertices_camera[j].x - m_diaphragm_vertices_camera[i].x) *
                                (p.y - m_diaphragm_vertices_camera[i].y) / (m_diaphragm_vertices_camera[j].y
                                    - m_diaphragm_vertices_camera[i].y) + m_diaphragm_vertices_camera[i].x))
                            inside = !inside;
                    }
                    return inside;
                }
            }
        }

        void compute_cardinal(const Ray3d& orig_ray, const Ray3d& out_ray, double& f, double& p) const
        {
            double t_focal = -out_ray.m_org.x / out_ray.m_dir.x;
            Vector3d focal_point = out_ray.point_at(t_focal);
            f = focal_point.z;

            double t_principal = (orig_ray.m_org.x - out_ray.m_org.x) / out_ray.m_dir.x;
            Vector3d principal_point = out_ray.point_at(t_principal);
            p = principal_point.z;
        }

        bool compute_thick_lens_scene(double& p_scene, double& f_scene) const
        {
            const double x = 0.01 * m_lens_container.at(m_aperture_index).diameter;
            Ray3d orig_ray(Vector3d(x, 0, m_total_z), Vector3d(0, 0, -1));
            Ray3d ray = orig_ray;

            if (!trace_ray_from_film(ray, 0, true))
                return false;

            compute_cardinal(orig_ray, ray, f_scene, p_scene);

            return true;
        }

        bool compute_thick_lens_film(double& p_film, double& f_film) const
        {
            const double x = 0.01 * m_lens_container.at(m_aperture_index).diameter;
            Ray3d orig_ray(Vector3d(x, 0, -1), Vector3d(0, 0, 1));
            Ray3d ray = orig_ray;

            if (!trace_ray_from_scene(ray, 0, true))
                return false;

            compute_cardinal(orig_ray, ray, f_film, p_film);

            return true;
        }

        bool focus_lens(const double& focal_distance)
        {
            double p_film, p_scene, f_film, f_scene;

            if (!compute_thick_lens_film(p_film, f_film) || !compute_thick_lens_scene(p_scene, f_scene))
            {
                RENDERER_LOG_ERROR(
                    "camera \"%s\": thick lens approximation could not be computed.",
                    get_path().c_str());
                return false;
            }

            double focal_length = f_film - p_film;
            double z_scene = -focal_distance;
            double z_film = m_total_z;
            double delta = 0.5 * (p_film - z_film - z_scene + p_scene -
                std::sqrt((-z_scene + p_scene + z_film - p_film) * (-z_scene + p_scene - 4 * focal_length + z_film - p_film)));

            if (std::isnan(delta))
            {
                RENDERER_LOG_ERROR(
                    "camera \"%s\": lens could not be focused.",
                    get_path().c_str());
                return false;
            }

            m_lens_container.back().thickness += delta;

            return true;
        }

        Vector3d ndc_to_camera(const Vector2d& point) const
        {
            return
                Vector3d(
                    (0.5 - point.x) * m_film_dimensions[0] - m_shift.x,
                    (point.y - 0.5) * m_film_dimensions[1] - m_shift.y,
                    m_total_z);
        }

        Vector2d camera_to_ndc(const Vector3d& point) const
        {
            return
                Vector2d(
                    0.5 - ((point.x + m_shift.x) / m_film_dimensions[0]),
                    0.5 + ((point.y + m_shift.y) / m_film_dimensions[1]));
        }


        //
        // Lens container helper functions.
        //

        // Comments are preceded by a # and can stand on their own line or at the end of a line.
        // The first line should contain the number of following lens elments.
        // Lens elements have to be space separated values of the format:
        // radius    thickness    ior    aperture
        bool read_lens_file(const Project& project, double factor)
        {
            if (!has_param("lens_file"))
            {
                RENDERER_LOG_ERROR(
                    "while defining camera \"%s\": no lens file specified",
                    get_path().c_str());
                return false;
            }

            const std::string lens_filepath = to_string(
                project.search_paths().qualify(m_params.get_required<std::string>("lens_file")));

            m_lens_container.clear();
            bool has_aperture = false;

            std::ifstream infile;
            infile.open(lens_filepath);
            if (!infile.is_open())
            {
                RENDERER_LOG_ERROR(
                    "while defining camera \"%s\": lens file not found",
                    get_path().c_str());
                return false;
            }

            int index = 0;
            std::string line;
            while (std::getline(infile, line))
            {
                if (line.empty())
                    continue;

                size_t comment_pos = line.find('#');
                if (comment_pos == 0)
                    continue;

                if (comment_pos != std::string::npos)
                    line = line.substr(0, comment_pos);

                if (std::regex_match(line, std::regex("[0-9]+\\s+")))
                {
                    m_lens_container.reserve(std::atoi(line.c_str()));
                    continue;
                }

                std::istringstream iss(line);

                LensElement element;
                iss >> element.radius;
                iss >> element.thickness;
                iss >> element.ior;
                iss >> element.diameter;

                if (iss.fail())
                {
                    RENDERER_LOG_ERROR(
                        "while defining camera \"%s\": error reading file \"%s\" (lens element %d)",
                        get_path().c_str(),
                        lens_filepath.c_str(),
                        index);
                    return false;
                }

                element.is_aperture = element.ior == 0;
                if (element.is_aperture)
                {
                    has_aperture = true;
                    m_aperture_index = index;
                }

                element.scale(factor);

                m_lens_container.push_back(element);
                ++index;
            }

            if (get_total_z_offset() == 0 || !has_aperture)
            {
                RENDERER_LOG_ERROR(
                    "while defining camera \"%s\": file \"%s\" empty or missing aperture",
                    get_path().c_str(),
                    lens_filepath.c_str());
                return false;
            }

            infile.close();
            return true;
        }

        void scale_lens_elements(const double from_focal_length, const double to_focal_length)
        {
            double scale = to_focal_length / from_focal_length;
            for (auto iter = m_lens_container.begin(); iter != m_lens_container.end(); ++iter)
            {
                (*iter).radius *= scale;
                (*iter).thickness *= scale;
                (*iter).diameter *= scale;
            }
        }

        void adjust_f_number()
        {
            m_lens_container.at(m_aperture_index).diameter = m_focal_length / m_f_number;
        }

        double get_z_offset(int i) const
        {
            double offset = 0;
            int cnt = 0;

            for (LensElement const& element : m_lens_container)
            {
                if (i == cnt)
                    return offset;

                offset += element.thickness;
                ++cnt;
            }
            return -1;
        }

        double get_total_z_offset() const
        {
            double offset = 0;

            for (LensElement const& element : m_lens_container)
                offset += element.thickness;
            return offset;
        }
    };
}

//
// MultiLensCameraFactory class implementation.
//

void MultiLensCameraFactory::release()
{
    delete this;
}

const char* MultiLensCameraFactory::get_model() const
{
    return Model;
}

Dictionary MultiLensCameraFactory::get_model_metadata() const
{
    return
        Dictionary()
        .insert("name", Model)
        .insert("label", "Multi Lens Camera");
}

DictionaryArray MultiLensCameraFactory::get_input_metadata() const
{
    DictionaryArray metadata = CameraFactory::get_input_metadata();

    metadata.push_back(
        Dictionary()
        .insert("name", "lens_file")
        .insert("label", "Camera Lens File")
        .insert("type", "file")
        .insert("file_picker_mode", "open")
        .insert("file_picker_type", "text")
        .insert("default", "")
        .insert("use", "required"));

    CameraFactory::add_film_metadata(metadata);
    LensCameraFactory::add_lens_metadata(metadata);

    CameraFactory::add_clipping_metadata(metadata);
    CameraFactory::add_shift_metadata(metadata);

    return metadata;
}

auto_release_ptr<Camera> MultiLensCameraFactory::create(
    const char* name,
    const ParamArray& params) const
{
    return auto_release_ptr<Camera>(new MultiLensCamera(name, params));
}

}   // namespace renderer
