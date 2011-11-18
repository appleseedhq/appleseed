
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
#include "smokesurfaceshader.h"

// appleseed.renderer headers.
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/kernel/volume/occupancygrid.h"
#include "renderer/kernel/volume/volume.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"

// appleseed.foundation headers.
#include "foundation/math/intersection.h"
#include "foundation/math/minmax.h"
#include "foundation/math/noise.h"
#include "foundation/math/scalar.h"
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/searchpaths.h"

// Standard headers.
#include <vector>

// Forward declarations.
namespace renderer  { class Assembly; }
namespace renderer  { class Project; }

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Various constants.
    //

    const float OccupancyThreshold = 0.0001f;
    const float MinOpacity = 0.0001f;
    const float MaxOpacity = 0.9999f;


    //
    // Smoke surface shader.
    //

    const char* Model = "smoke_surface_shader";

    class SmokeSurfaceShader
      : public SurfaceShader
    {
      public:
        SmokeSurfaceShader(
            const char*             name,
            const ParamArray&       params)
          : SurfaceShader(name, params)
          , m_first_frame(true)
        {
            extract_parameters();
        }

        virtual void release() override
        {
            delete this;
        }

        virtual const char* get_model() const override
        {
            return Model;
        }

        virtual void on_frame_begin(
            const Project&          project,
            const Assembly&         assembly) override
        {
            SurfaceShader::on_frame_begin(project, assembly);

            if (m_first_frame)
            {
                create_voxel_grid(project.get_search_paths());
                print_channel_availability();

                if (m_voxel_grid.get())
                {
                    m_occupancy_grid.reset(
                        new OccupancyGrid(
                            *m_voxel_grid.get(),
                            m_channels.m_density_index,
                            OccupancyThreshold));

                    m_rcp_bbox_extent = Vector3d(1.0) / m_bbox.extent();
                }

                m_first_frame = false;
            }
        }

        virtual void evaluate(
            SamplingContext&        sampling_context,
            const ShadingContext&   shading_context,
            const ShadingPoint&     shading_point,
            ShadingResult&          shading_result) const override
        {
            Color3f volume_color;
            float volume_opacity;

            switch (m_shading_mode)
            {
              // Compute the color and opacity of the volume along the incoming ray.
              case VolumeMode:
                compute_volume(
                    shading_point.get_ray(),
                    volume_color,
                    volume_opacity);
                volume_color *= volume_opacity;
                break;

              // Find an isosurface of the volume.
              case IsosurfaceMode:
                volume_color =
                    compute_isosurface(
                        shading_point.get_ray(),
                        m_isosurface_threshold);
                volume_opacity = 1.0f;
                break;

              // Invalid shading mode.
              default:
                assert(!"Invalid shading mode.");
                volume_color.set(0.0f);
                volume_opacity = 0.0f;
                break;
            }

            shading_result.set_to_linear_rgba(Color4f(volume_color, volume_opacity));
        }

      private:
        enum ShadingMode
        {
            VolumeMode = 0,
            IsosurfaceMode
        };

        enum InterpolationMode
        {
            NearestMode = 0,
            LinearMode,
            QuadraticMode
        };

        static const size_t MaxChannels = 14;

        bool                    m_first_frame;

        AABB3d                  m_bbox;
        ShadingMode             m_shading_mode;
        InterpolationMode       m_interpolation_mode;
        float                   m_isosurface_threshold;
        string                  m_filename;
        double                  m_step_size;
        float                   m_density_cutoff;
        float                   m_density_scale;
        Color3f                 m_smoke_color;
        Color3f                 m_fuel_color;
        float                   m_fuel_scale;
        Vector3d                m_light_direction;
        Color3f                 m_light_color;
        float                   m_color_scale;
        float                   m_volume_opacity;
        float                   m_shadow_opacity;

        auto_ptr<VoxelGrid>     m_voxel_grid;
        FluidChannels           m_channels;

        auto_ptr<OccupancyGrid> m_occupancy_grid;
        Vector3d                m_rcp_bbox_extent;

        void extract_parameters()
        {
            // Retrieve the bounding box of the volume.
            m_bbox.invalidate();
            Vector<double, 6> default_bbox;
            default_bbox[0] = m_bbox.min[0];
            default_bbox[1] = m_bbox.min[1];
            default_bbox[2] = m_bbox.min[2];
            default_bbox[3] = m_bbox.max[0];
            default_bbox[4] = m_bbox.max[1];
            default_bbox[5] = m_bbox.max[2];
            const Vector<double, 6> bbox =
                m_params.get_required<Vector<double, 6> >("bounding_box", default_bbox);
            m_bbox.min[0] = bbox[0];
            m_bbox.min[1] = bbox[1];
            m_bbox.min[2] = bbox[2];
            m_bbox.max[0] = bbox[3];
            m_bbox.max[1] = bbox[4];
            m_bbox.max[2] = bbox[5];

            // Retrieve the shading mode.
            const string shading_mode_string =
                m_params.get_optional<string>("shading_mode", "volume");
            if (shading_mode_string == "volume")
                m_shading_mode = VolumeMode;
            else if (shading_mode_string == "isosurface")
                m_shading_mode = IsosurfaceMode;
            else
            {
                RENDERER_LOG_ERROR(
                    "invalid shading mode \"%s\", using default value \"volume\"",
                    shading_mode_string.c_str());
                m_shading_mode = VolumeMode;
            }

            // Retrieve the interpolation mode.
            const string interp_mode_string =
                m_params.get_optional<string>("interpolation_mode", "linear");
            if (interp_mode_string == "nearest")
                m_interpolation_mode = NearestMode;
            else if (interp_mode_string == "linear")
                m_interpolation_mode = LinearMode;
            else if (interp_mode_string == "quadratic")
                m_interpolation_mode = QuadraticMode;
            else
            {
                RENDERER_LOG_ERROR(
                    "invalid interpolation mode \"%s\", using default value \"linear\"",
                    interp_mode_string.c_str());
                m_interpolation_mode = LinearMode;
            }

            // Retrieve the other parameters.
            m_isosurface_threshold = m_params.get_optional<float>("isosurface_threshold", 0.5f);
            m_filename = m_params.get_optional<string>("filename", "");
            m_step_size = m_params.get_optional<double>("step_size", 0.1);
            m_density_cutoff = m_params.get_optional<float>("density_cutoff", 1.0f);
            m_density_scale = m_params.get_optional<float>("density_scale", 1.0f);
            m_smoke_color = m_params.get_optional<Color3f>("smoke_color", Color3f(1.0f, 1.0f, 1.0f));
            m_fuel_color = m_params.get_optional<Color3f>("fuel_color", Color3f(1.0f, 0.5f, 0.2f));
            m_fuel_scale = m_params.get_optional<float>("fuel_scale", 1.0f);
            m_light_direction = m_params.get_optional<Vector3d>("light_direction", Vector3d(0.0, -1.0, 0.0));
            m_light_direction = normalize(m_light_direction);
            m_light_color = m_params.get_optional<Color3f>("light_color", Color3f(1.0f, 1.0f, 1.0f));
            m_color_scale = m_params.get_optional<float>("color_scale", 1.0f);
            m_volume_opacity = m_params.get_optional<float>("volume_opacity", 1.0f);
            m_shadow_opacity = m_params.get_optional<float>("shadow_opacity", 1.0f);
        }

        void create_voxel_grid(const SearchPaths& search_paths)
        {
            if (m_filename.empty())
            {
                create_sample_voxel_grid();
            }
            else
            {
                load_voxel_grid_from_disk(search_paths);
            }
        }

        // Create a voxel grid procedurally.
        void create_sample_voxel_grid()
        {
            const float ScaleX = 8.0f;
            const float ScaleY = 8.0f;
            const float ScaleZ = 8.0f;

            const size_t XRes = 20;
            const size_t YRes = 20;
            const size_t ZRes = 20;

            m_voxel_grid.reset(new VoxelGrid(XRes, YRes, ZRes, 1));

            for (size_t z = 0; z < ZRes; ++z)
            {
                for (size_t y = 0; y < YRes; ++y)
                {
                    for (size_t x = 0; x < XRes; ++x)
                    {
                        const Vector3f p(
                            static_cast<float>(x) / (XRes - 1) * ScaleX,
                            static_cast<float>(y) / (YRes - 1) * ScaleY,
                            static_cast<float>(z) / (ZRes - 1) * ScaleZ);

                        float value = fbm(p, 8, 2.0f, 0.5f);

                        if (value < 0.3f)
                            value = 0.0f;

                        m_voxel_grid->voxel(x, y, z)[0] = 0.2f * value;
                    }
                }
            }

            // We only have a density channel.
            m_channels.m_density_index = 0;
        }

        void load_voxel_grid_from_disk(const SearchPaths& search_paths)
        {
            const string filepath = search_paths.qualify(m_filename);

            RENDERER_LOG_INFO("loading fluid file %s...", filepath.c_str());

            m_voxel_grid = read_fluid_file(filepath.c_str(), m_channels);

            if (m_voxel_grid.get() == 0)
                RENDERER_LOG_ERROR("failed to load fluid file %s", filepath.c_str());

/*
            write_voxel_grid(
                m_filename + ".txt",
                *m_voxel_grid.get());
*/
        }

        void print_channel_availability()
        {
            if (m_voxel_grid.get())
            {
                RENDERER_LOG_INFO(
                    "fluid channels:\n"
                    "  color        %s\n"
                    "  density      %s\n"
                    "  temperature  %s\n"
                    "  fuel         %s\n"
                    "  falloff      %s\n"
                    "  pressure     %s\n"
                    "  coordinates  %s\n"
                    "  velocity     %s\n",
                    m_channels.m_color_index == FluidChannels::NotPresent ? "no" : "yes",
                    m_channels.m_density_index == FluidChannels::NotPresent ? "no" : "yes",
                    m_channels.m_temperature_index == FluidChannels::NotPresent ? "no" : "yes",
                    m_channels.m_fuel_index == FluidChannels::NotPresent ? "no" : "yes",
                    m_channels.m_falloff_index == FluidChannels::NotPresent ? "no" : "yes",
                    m_channels.m_pressure_index == FluidChannels::NotPresent ? "no" : "yes",
                    m_channels.m_coordinates_index == FluidChannels::NotPresent ? "no" : "yes",
                    m_channels.m_velocity_index == FluidChannels::NotPresent ? "no" : "yes");
            }
        }

        static void build_occupancy_grid(
            const VoxelGrid&    voxel_grid,
            const size_t        density_index,
            vector<int>&        occupancy_grid,
            const float         occupancy_threshold)
        {
            const int xres = static_cast<int>(voxel_grid.get_xres());
            const int yres = static_cast<int>(voxel_grid.get_yres());
            const int zres = static_cast<int>(voxel_grid.get_zres());

            occupancy_grid.resize(xres * yres * zres);

            for (int z = 0; z < zres; ++z)
            {
                for (int y = 0; y < yres; ++y)
                {
                    for (int x = 0; x < xres; ++x)
                    {
                        // Compute the sum of the densities over the 3x3x3 neighboring of (x, y, z).
                        float density_sum = 0.0f;
                        for (int dx = -1; dx <= +1; ++dx)
                        {
                            for (int dy = -1; dy <= +1; ++dy)
                            {
                                for (int dz = -1; dz <= +1; ++dz)
                                {
                                    const int ix = x + dx;
                                    const int iy = y + dy;
                                    const int iz = z + dz;

                                    if (ix < 0 || iy < 0 || iz < 0 ||
                                        ix >= xres || iy >= yres || iz >= zres)
                                        continue;

                                    const float* voxel = voxel_grid.voxel(ix, iy, iz);
                                    assert(voxel[density_index] >= 0.0f);

                                    density_sum += voxel[density_index];
                                }
                            }
                        }

                        // Mark this voxel as occupied if there's enough density.
                        const size_t index = (z * yres + y) * xres + x;
                        occupancy_grid[index] = density_sum > occupancy_threshold ? 1 : 0;
                    }
                }
            }
        }

        FORCE_INLINE static void intersect_tmax(
            const ShadingRay&               ray,
            const ShadingRay::RayInfoType&  ray_info,
            const AABB3d&                   bbox,
            double&                         tmax)
        {
            const double xmax = bbox[ray_info.m_sgn_dir[0]][0];
            const double ymax = bbox[ray_info.m_sgn_dir[1]][1];
            const double zmax = bbox[ray_info.m_sgn_dir[2]][2];
            const double txmax = (xmax - ray.m_org[0]) * ray_info.m_rcp_dir[0];
            const double tymax = (ymax - ray.m_org[1]) * ray_info.m_rcp_dir[1];
            const double tzmax = (zmax - ray.m_org[2]) * ray_info.m_rcp_dir[2];
            tmax = min(txmax, tymax, tzmax);
        }

        // Retrieve the fluid values at a given point, in world space.
        // Return true if the fluid is present at that point, false otherwise.
        bool get_fluid_values(const Vector3d& point, float values[]) const
        {
            // No fluid is defined.
            if (m_voxel_grid.get() == 0)
                return false;

            // Normalize the lookup point coordinates.
            const Vector3d normalized_point((point - m_bbox.min) * m_rcp_bbox_extent);

            // Quickly find out whether the fluid is present at this position.
            if (!m_occupancy_grid->has_fluid(normalized_point))
                return false;

            // Lookup the grid.
            switch (m_interpolation_mode)
            {
              case NearestMode:
                  m_voxel_grid->nearest_lookup(normalized_point, values);
                  return true;

              case LinearMode:
                  m_voxel_grid->linear_lookup(normalized_point, values);
                  return true;

              case QuadraticMode:
                  m_voxel_grid->quadratic_lookup(normalized_point, values);
                  return true;

              default:
                  assert(false);
                  return false;
            }
        }

        float get_density(const Vector3d& point) const
        {
            if (m_channels.m_density_index == FluidChannels::NotPresent)
                return 0.0f;

            // Get fluid values.
            float values[MaxChannels];
            if (!get_fluid_values(point, values))
                return 0.0f;

            // Fetch density.
            float density = values[m_channels.m_density_index];
            assert(density >= 0.0f);

            // Apply density scale.
            density *= m_density_scale;

            return density;
        }

        Vector3d get_density_gradient(const Vector3d& point) const
        {
            static const Vector3d Dx(1.0, 0.0, 0.0);
            static const Vector3d Dy(0.0, 1.0, 0.0);
            static const Vector3d Dz(0.0, 0.0, 1.0);

            Vector3d gradient(
                get_density(point + Dx) - get_density(point - Dx),
                get_density(point + Dy) - get_density(point - Dy),
                get_density(point + Dz) - get_density(point - Dz));

            const double n2 = square_norm(gradient);
            if (n2 > 0.0)
                gradient /= sqrt(n2);

            return gradient;
        }

        float get_sample_opacity(
            const Vector3d& point,
            const float     values[]) const
        {
            if (m_channels.m_density_index == FluidChannels::NotPresent)
                return 0.0f;

            // Fetch density.
            float density = values[m_channels.m_density_index];
            assert(density >= 0.0f);

            // Apply density scale.
            density *= m_density_scale;

            // Apply density cutoff.
            density = smoothstep(0.0f, m_density_cutoff, density);

            // Apply falloff.
            if (m_channels.m_falloff_index != FluidChannels::NotPresent)
                density *= 1.0f - values[m_channels.m_falloff_index];

/*
            // Apply turbulence.
            if (m_channels.m_coordinates_index != FluidChannels::NotPresent)
            {
                const Vector3f coords(
                    values[m_channels.m_coordinates_index + 0],
                    values[m_channels.m_coordinates_index + 1],
                    values[m_channels.m_coordinates_index + 2]);

                density *= 0.1f + turbulence(coords / 1.76854335f, 8, 1.493f, 0.75f);

                density *= turbulence(coords, 8, 2.2525f, 0.5f);
            }
*/

            return density;
        }

        FORCE_INLINE float get_sample_opacity_fast(const float values[]) const
        {
            if (m_channels.m_density_index == FluidChannels::NotPresent)
                return 0.0f;

            // Fetch density.
            float density = values[m_channels.m_density_index];
            assert(density >= 0.0f);

            // Apply density scale.
            density *= m_density_scale;

            // Apply density cutoff.
            density = smoothstep(0.0f, m_density_cutoff, density);

            // Apply falloff.
            if (m_channels.m_falloff_index != FluidChannels::NotPresent)
                density *= 1.0f - values[m_channels.m_falloff_index];

            return density;
        }

        Color3f get_sample_color(
            const Vector3d& point,
            const float     values[]) const
        {
            Color3f color;

/*
            // Shade according to the density gradient.
            const Vector3d gradient = get_density_gradient(point);
            color[0] = static_cast<float>(0.5 * (1.0 + gradient.x));
            color[1] = static_cast<float>(0.5 * (1.0 + gradient.y));
            color[2] = static_cast<float>(0.5 * (1.0 + gradient.z));
            return color;
*/

/*
            // Shade according to the fluid coordinates.
            assert(m_channels.m_coordinates_index != FluidChannels::NotPresent);
            color[0] = values[m_channels.m_coordinates_index + 0];
            color[1] = values[m_channels.m_coordinates_index + 1];
            color[2] = values[m_channels.m_coordinates_index + 2];
            return color;
*/

/*
            // Shade according to the fluid velocity.
            assert(m_channels.m_velocity_index != FluidChannels::NotPresent);
            color[0] = values[m_channels.m_velocity_index + 0];
            color[1] = values[m_channels.m_velocity_index + 1];
            color[2] = values[m_channels.m_velocity_index + 2];
            return color;
*/

            // Smoke color.
            color = m_smoke_color;

            // Lighting.
            color *= compute_lighting(point);

            // Fuel color.
            if (m_channels.m_fuel_index != FluidChannels::NotPresent)
            {
                const float fuel = values[m_channels.m_fuel_index] * m_fuel_scale;
                color += fuel * m_fuel_color;
            }

            // User color.
            if (m_channels.m_color_index != FluidChannels::NotPresent)
            {
                color[0] += values[m_channels.m_color_index + 0] * m_color_scale;
                color[1] += values[m_channels.m_color_index + 1] * m_color_scale;
                color[2] += values[m_channels.m_color_index + 2] * m_color_scale;
            }

            return color;
        }

        Color3f compute_lighting(const Vector3d& point) const
        {
            // Start with the light color.
            Color3f light_color = m_light_color;

            // Modulate according to density gradient.
//             radiance *= static_cast<float>(max(dot(LightDirection, gradient), 0.0));

            // Compute and apply self-shadowing, if enabled.
            if (m_shadow_opacity > 0.0f)
            {
                // Construct the shadow ray.
                ShadingRay ray;
                ray.m_org = point;
                ray.m_dir = -m_light_direction;

                // Find the ray segment that lies inside the bounding box of the volume.
                double tmax;
                intersect_tmax(
                    ray,
                    ShadingRay::RayInfoType(ray),
                    m_bbox,
                    tmax);

                float shadow_opacity = 0.0f;

                // Integrate the shadow opacity along the shadow ray.
                const float step_size = static_cast<float>(m_step_size);
                double t = 0.5 * m_step_size;
                while (t < tmax && shadow_opacity < MaxOpacity)
                {
                    // Compute the world space position of the sample.
                    const Vector3d sample_position = ray.point_at(t);

                    // Get the fluid values at this position.
                    float values[MaxChannels];
                    if (get_fluid_values(sample_position, values))
                    {
                        // Compute the sample opacity.
                        float sample_opacity = get_sample_opacity_fast(values);
                        sample_opacity *= m_shadow_opacity;

                        // Integrate the shadow opacity.
                        assert(shadow_opacity < 1.0f);
                        shadow_opacity += (1.0f - shadow_opacity) * sample_opacity * step_size;
                    }

                    // Move forward along the ray.
                    t += m_step_size;
                }

                // Clamp the shadow opacity to [0, 1].
                shadow_opacity = saturate(shadow_opacity);

                // Modulate the light color with the shadow opacity.
                light_color *= 1.0f - shadow_opacity;
            }

            return light_color;
        }

        void compute_volume(
            ShadingRay      ray,
            Color3f&        volume_color,
            float&          volume_opacity) const
        {
            // Normalize the direction of the incoming ray.
            ray.m_dir = normalize(ray.m_dir);

            // Find the ray segment that lies inside the bounding box of the volume.
            double tmin, tmax;
            intersect(
                ray,
                ShadingRay::RayInfoType(ray),
                m_bbox,
                tmin,
                tmax);

            volume_color.set(0.0f);
            volume_opacity = 0.0f;

            // Integrate the volume color and opacity along the incoming ray.
            const float step_size = static_cast<float>(m_step_size);
            double t = tmin + 0.5 * m_step_size;
            while (t < tmax && volume_opacity < MaxOpacity)
            {
                // Compute the world space position of the sample.
                const Vector3d sample_position = ray.point_at(t);

                // Get the fluid values at this position.
                float values[MaxChannels];
                if (get_fluid_values(sample_position, values))
                {
                    // Compute the sample opacity.
                    float sample_opacity = get_sample_opacity(sample_position, values);
                    sample_opacity *= m_volume_opacity;

                    if (sample_opacity > MinOpacity)
                    {
                        // Compute the sample color.
                        const Color3f sample_color = get_sample_color(sample_position, values);

                        // Integrate the volume color and opacity.
                        assert(volume_opacity < 1.0f);
                        volume_color += (1.0f - volume_opacity) * sample_opacity * sample_color * step_size;
                        volume_opacity += (1.0f - volume_opacity) * sample_opacity * step_size;
                    }
                }

                // Move forward along the ray.
                t += m_step_size;
            }

            // Clamp the volume opacity to [0, 1].
            volume_opacity = saturate(volume_opacity);
        }

        Color3f compute_isosurface(
            ShadingRay      ray,
            const float     density_threshold) const
        {
            // Normalize the direction of the incoming ray.
            ray.m_dir = normalize(ray.m_dir);

            // Find the ray segment that lies inside the bounding box of the volume.
            double tmin, tmax;
            intersect(
                ray,
                ShadingRay::RayInfoType(ray),
                m_bbox,
                tmin,
                tmax);

            // Integrate the volume color and opacity along the incoming ray.
            double t = tmin + 0.5 * m_step_size;
            while (t < tmax)
            {
                // Compute the world space position of the sample.
                const Vector3d sample_position = ray.point_at(t);

                // Compute the fluid density at this position.
                const float density = get_density(sample_position);

                // Find the limit surface.
                if (density > density_threshold)
                {
                    // Compute the density gradient.
                    const Vector3d gradient = get_density_gradient(sample_position);

                    // Shade according to the density gradient.
                    return Color3f(
                        static_cast<float>(0.5 * (1.0 + gradient.x)),
                        static_cast<float>(0.5 * (1.0 + gradient.y)),
                        static_cast<float>(0.5 * (1.0 + gradient.z)));
                }

                // Move forward along the ray.
                t += m_step_size;
            }

            return Color3f(0.0f);
        }
    };
}


//
// SmokeSurfaceShaderFactory class implementation.
//

const char* SmokeSurfaceShaderFactory::get_model() const
{
    return Model;
}

const char* SmokeSurfaceShaderFactory::get_human_readable_model() const
{
    return "Smoke";
}

DictionaryArray SmokeSurfaceShaderFactory::get_widget_definitions() const
{
    DictionaryArray definitions;
    return definitions;
}

auto_release_ptr<SurfaceShader> SmokeSurfaceShaderFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<SurfaceShader>(
            new SmokeSurfaceShader(name, params));
}

}   // namespace renderer
