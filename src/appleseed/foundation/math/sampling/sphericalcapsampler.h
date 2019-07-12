
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Kevin Masson, The appleseedhq Organization
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

#pragma once

// appleseed.foundation headers.
#include "foundation/math/sampling/mappings.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cmath>

#include <iostream>

namespace foundation
{

//
// References:
//
//   [1] Stratified Sampling of Projected Spherical Caps
//       http://www.iliyan.com/publications/ProjectedSphericalCaps/ProjectedSphericalCaps_EGSR2018.pdf
//
//   [2] Sampling Projected Spherical Caps in Real Time
//       http://momentsingraphics.de/Media/I3D2019/Peters2019-SamplingSphericalCaps.pdf
//
//   [3] Sampling Light Sources
//       http://www.pbr-book.org/3ed-2018/Light_Transport_I_Surface_Reflection/Sampling_Light_Sources.html#sec:sampling-lights
//

// todo: split declaration and definition.
template <typename T>
class SphericalCapSampler
{
  public:
    // Constructor.
    SphericalCapSampler(
        const Vector<T, 3>&   surface_point,
        const Vector<T, 3>&   center,
        const T               radius)
      : m_radius(radius)
    {
        const T rcp_center_distance = T(1.0) / std::sqrt(dot(center, center));

        // Construct a coordinate frame with z pointing to the sphere.
        // todo: use Basis3 here. the vector we give to basis3 is y (normal).
        m_z = normalize(center - surface_point);
        m_x =
            (std::abs(m_z.x) > std::abs(m_z.y))
            ? (Vector<T, 3>(-m_z.z, T(0.0), m_z.x) / std::sqrt(square(m_z.x) + square(m_z.z)))
            : (Vector<T, 3>(T(0.0), m_z.z, -m_z.y) / std::sqrt(square(m_z.y) + square(m_z.z)));
        m_y = cross(m_z, m_x);
        // Make sure (m_x, m_y, m_z) forms an orthonormal basis.
        assert(is_normalized(m_x));
        assert(is_normalized(m_y));
        assert(is_normalized(m_z));
        assert(fz(dot(m_x, m_y), make_eps<T>(1.0e-4f, 1.0e-6)));
        assert(fz(dot(m_x, m_z), make_eps<T>(1.0e-4f, 1.0e-6)));
        assert(fz(dot(m_y, m_z), make_eps<T>(1.0e-4f, 1.0e-6)));

        // Make sure (m_x, m_y, m_z) is right-handed.
        assert(feq(dot(cross(m_x, m_y), m_z), T(1.0), make_eps<T>(1.0e-4f, 1.0e-5)));

        // todo: cache square_distance
        // todo: rename dc to dist
        m_dc = std::sqrt(square_distance(surface_point, center));

        // Compute sampling cone parameters.
        m_sin_theta_max_2 = square(radius) / square_distance(surface_point, center);
        m_cos_theta_max = std::sqrt(std::max(T(0.0), T(1.0) - m_sin_theta_max_2));

        if (square_distance(surface_point, center) < square(radius))
        {
            std::cout << "What the actual fuck, the point is inside.\n";
        }
    }

    T get_pdf() const
    {
        // todo: handle the case where the surface_point is inside the sphere.
        return sample_cone_uniform_pdf(m_cos_theta_max);
    }

    T get_density() const
    {
        // todo: handle the case where the surface_point is inside the sphere.
        return (TwoPi<T>() * (T(1.0) - m_cos_theta_max));
        //return sample_cone_uniform_pdf(m_cos_theta_max);
    }

    // Return the normal of a point on the spherical m_
    Vector<T, 3> sample(const Vector<T, 2>& s) const
    {
        // todo: handle the case where the surface_point is inside the sphere.
        const T cos_theta = lerp(T(1.0), m_cos_theta_max, s[0]);
        const T sin_theta = safe_sqrt(T(1.0) - square(cos_theta));
        const T phi = s[1] * TwoPi<T>();

        // todo: compute ds² directly.
        const T ds = m_dc * cos_theta - safe_sqrt(square(m_radius) - square(m_dc) * square(sin_theta));
        const T cos_alpha = (square(m_dc) + square(m_radius) - square(ds)) / (T(2.0) * m_dc * m_radius);
        const T sin_alpha = safe_sqrt(T(1.0) - square(cos_alpha));

        // todo: -m_x -> m_x and -m_y -> m_y.
        const Vector<T, 3> normal =
            sin_alpha * std::cos(phi) * (m_x)
            + sin_alpha * std::sin(phi) * (m_y)
            + cos_alpha * (-m_z);

        return normal;
    }

  private:
      Vector<T, 3>      m_x;
      Vector<T, 3>      m_y;
      Vector<T, 3>      m_z;

      T                 m_radius;
      T                 m_sin_theta_max_2;
      T                 m_cos_theta_max;
      T                 m_dc;
};

namespace
{
      template <typename T>
      inline T get_cut_disk_area(T maximal_x)
      {
          const T a = -maximal_x * maximal_x + T(1.0);
          return std::sqrt(a) * maximal_x + std::asin(maximal_x);
      }

      template <typename T>
      inline T mad(const T a, const T b, const T c)
      {
          return a * b + c;
      }

      /*! Implements the inverse of getCutDiskArea().
         \note This implementation factors the inverse function into a part with a
               singularity and a quintic polynomial. The worst-case error is around
               4e-6.*/
      template <typename T>
      inline T get_area_disk_cut(const T area)
      {
          const T absArea = min(T(0.5) * Pi<T>(), abs(area));
          const T polynomial = mad(mad(mad(mad(mad(absArea, -T(0.0079908617), T(0.0238255409)), absArea, -T(0.0283903598)), absArea, T(0.0198450184)), absArea, -T(0.0574433620)), absArea, T(0.7400712465));
          const T result = T(1.0) - polynomial * std::pow(T(0.5) * Pi<T>() - absArea, T(2.0) / T(3.0));
          return (area < T(0.0)) ? (-result) : result;
      }
}

template <typename T>
class ProjectedSphericalCapSampler
{
  public:
    // Constructor.
      ProjectedSphericalCapSampler(
          const Vector<T, 3>&   surface_normal,
          const Vector<T, 3>&   sphere_center,
          const T               sphere_radius)
      {
          const T inv_center_dist = T(1.0) / std::sqrt(dot(sphere_center, sphere_center));
          m_normal = surface_normal;
          const Vector<T, 3> normalized_center = inv_center_dist * sphere_center;
          const T normalized_center_z = dot(m_normal, normalized_center);
          m_tangent = (-normalized_center_z * m_normal + normalized_center);
          const T inv_tangent_length = T(1.0) / std::sqrt(dot(m_tangent, m_tangent));
          m_tangent *= inv_tangent_length;
          const T normalized_center_x = dot(m_tangent, normalized_center);
          m_bitangent = cross(m_normal, m_tangent);
          // Compute the radius of the circle that bounds the spherical m_ It 
          // agrees with half the diameter of the ellipse, which is also the extent 
          // along y.
          m_circle_half_extent.y = sphere_radius * inv_center_dist;
          // Compute the width of the ellipse (extent along x). Negative if the 
          // sphere center is below the horizon.
          m_circle_half_extent.x = m_circle_half_extent.y*normalized_center_z;
          // Compute the location of the center of the ellipse along the x-axis
          const T plane_origin_distance = std::sqrt(mad(m_circle_half_extent.y, m_circle_half_extent.y, T(1.0)));
          m_circle_center_x = plane_origin_distance * normalized_center_x;
          // Compute where the plane of the circle bounding the spherical cap 
          // intersects the x-axis
          m_disk_cut_x = plane_origin_distance * inv_tangent_length;
          // Case 1: The spherical cap is entirely above the horizon or maybe
          // case 4: The spherical cap is entirely below the horizon and thus empty
          if (m_disk_cut_x >= T(1.0)) {
              // The projected solid angle is an ellipse
              const T projected_solid_angle = Pi<T>() * m_circle_half_extent.x*m_circle_half_extent.y;
              //m_projected_solid_angle=projected_solid_angle;
              m_density_factor = max(T(0.0), T(1.0) / projected_solid_angle);
          }
          // Cases 2 and 3, the spherical cap intersects the horizon
          else {
              // The area of the cut disk is needed for case 2 and 3
              const T cut_disk_area = T(0.5) * Pi<T>() - get_cut_disk_area(m_disk_cut_x);
              // Case 3, the spherical cap intersects the horizon but its center is 
              // below the horizon
              if (m_circle_half_extent.x <= T(0.0)) {
                  const T circle_center_z = normalized_center_z * plane_origin_distance;
                  const T circle_half_extent_z = m_circle_half_extent.y*normalized_center_x;
                  const T circle_max_z = circle_center_z + circle_half_extent_z;
                  m_inv_circle_half_extent_z = T(1.0) / circle_half_extent_z;
                  m_center_over_half_extent_z = -circle_center_z * m_inv_circle_half_extent_z;
                  m_squared_disk_cut_y = saturate(-m_disk_cut_x * m_disk_cut_x + T(1.0));
                  m_scale_z = circle_max_z * (T(1.0) / std::sqrt(m_squared_disk_cut_y));
                  m_density_factor = m_squared_disk_cut_y / (circle_max_z * circle_max_z * cut_disk_area);
                  m_random_to_cut_disk_area_factor = cut_disk_area;
                  // Optionally compute the projected solid angle. We do not need it for 
                  // sampling but it may be useful in some contexts.
                  //const T ellipse_cut_ratio_x=-saturate(-(m_disk_cut_x-m_circle_center_x)/m_circle_half_extent.x);
                  //const T normalized_ellipse_area=T(0.5)*Pi<T>()+get_cut_disk_area(ellipse_cut_ratio_x);
                  //m_projected_solid_angle=m_circle_half_extent.x*m_circle_half_extent.y*normalized_ellipse_area+cut_disk_area;
              }
              // Case 2, the spherical cap intersects the horizon but its center is 
              // above the horizon
              else {
                  // Compute the area of the cut ellipse and the total projected solid 
                  // angle
                  const T ellipse_cut_ratio_x = saturate((m_disk_cut_x - m_circle_center_x) / m_circle_half_extent.x);
                  const T normalized_ellipse_area = T(0.5) * Pi<T>() + get_cut_disk_area(ellipse_cut_ratio_x);
                  const T cut_ellipse_area = m_circle_half_extent.x * m_circle_half_extent.y * normalized_ellipse_area;
                  const T projected_solid_angle = cut_ellipse_area + cut_disk_area;
                  //m_projected_solid_angle=projected_solid_angle;
                  m_density_factor = T(1.0) / projected_solid_angle;
                  // Prepare the decision which cut disk will be sampled
                  m_cut_ellipse_area_ratio = cut_ellipse_area * m_density_factor;
                  m_random_to_cut_ellipse_area_factor = normalized_ellipse_area / m_cut_ellipse_area_ratio;
                  m_random_to_cut_disk_area_factor = cut_disk_area / (T(1.0) - m_cut_ellipse_area_ratio);
              }
          }
          // For points inside the light source, we treat the projected spherical cap 
          // as empty
          m_density_factor = (m_circle_half_extent.y >= T(1.0)) ? T(0.0) : m_density_factor;
          //m_projected

      }

      /*! \return true iff the given projected spherical cap is empty.*/
      inline bool is_projected_spherical_cap_empty() const
      {
          return m_density_factor <= T(0.0);
      }

      inline Vector<T, 3> sample(T& out_density, Vector<T, 2> s) const
      {
          T area;
          bool sample_ellipse;
          // If the sphere center is below the horizon, we want to sample the cut 
          // unit disk
          if (m_circle_half_extent.x <= T(0.0)) {
              area = mad(s.x, -m_random_to_cut_disk_area_factor, m_random_to_cut_disk_area_factor);
          }
          // If the sphere center is above the horizon but the sphere intersects it, 
          // we need to decide whether we want to sample the cut unit disk or the cut 
          // ellipse
          else if (m_disk_cut_x < T(1.0)) {
              sample_ellipse = (s.x < m_cut_ellipse_area_ratio);
              area = sample_ellipse ?
                  (s.x*m_random_to_cut_ellipse_area_factor) :
                  mad(s.x, -m_random_to_cut_disk_area_factor, m_random_to_cut_disk_area_factor);
          }
          // If the sphere is entirely above the horizon, we sample the ellipse
          else {
              area = Pi<T>() * s.x;
          }
          // Sample the cut disk
          Vector<T, 3> disk;
          disk.x = get_area_disk_cut(area - T(0.5)*Pi<T>());
          disk.y = std::sqrt(mad(-disk.x, disk.x, T(1.0)))*mad(s.y, T(2.0), -T(1.0));
          // If the sphere center is below the horizon, we need to warp the samples 
          // further and compute the density
          Vector<T, 3> local;
          out_density = m_density_factor;
          if (m_circle_half_extent.x <= T(0.0)) {
              disk.x = -disk.x;
              disk.z = std::sqrt(saturate(mad(-disk.x, disk.x, mad(-disk.y, disk.y, T(1.0)))));
              // Scale down along Z to get the appropriate maximal Z
              local.z = disk.z*m_scale_z;
              // Scale down along Y to account for the different shape of the cut disk
              const T z_quotient = mad(local.z, m_inv_circle_half_extent_z, m_center_over_half_extent_z);
              const T scale_y =
                  m_circle_half_extent.y * std::sqrt(
                      std::max(
                          T(1.0),
                          mad(-z_quotient, z_quotient, T(1.0)) / mad(-disk.z, disk.z, m_squared_disk_cut_y)));

              local.y = disk.y*scale_y;
              // Turn it into a normalized vector to get X
              local.x = std::sqrt(saturate(mad(-local.y, local.y, mad(-local.z, local.z, T(1.0)))));
              // Compute the proper density
              out_density *= local.x / (disk.x*scale_y);
          }
          // If the sphere center is above the horizon but the sphere intersects it, 
          // we may be sampling the cut disk
          else if (m_disk_cut_x < T(1.0) && !sample_ellipse) {
              local.x = -disk.x;
              local.y = disk.y;
              local.z = std::sqrt(saturate(mad(-local.x, local.x, mad(-local.y, local.y, T(1.0)))));
          }
          // Otherwise we are sampling the ellipse (either the cut ellipse or the 
          // entire ellipse, it does not make a difference here)
          else {
              local.x = mad(disk.x, m_circle_half_extent.x, m_circle_center_x);
              local.y = m_circle_half_extent.y*disk.y;
              local.z = std::sqrt(saturate(mad(-local.x, local.x, mad(-local.y, local.y, T(1.0)))));
          }
          // Go back to the original coordinate frame
          return local.x*m_tangent + local.y*m_bitangent + local.z*m_normal;
      }

  private:
      Vector<T, 3>      m_tangent, m_bitangent, m_normal;
      Vector<T, 2>      m_circle_half_extent;
      T                 m_inv_circle_half_extent_z;
      T                 m_circle_center_x;
      T                 m_center_over_half_extent_z;
      T                 m_disk_cut_x;
      T                 m_squared_disk_cut_y;
      T                 m_scale_z;
      T                 m_density_factor;
      T                 m_cut_ellipse_area_ratio;
      T                 m_random_to_cut_ellipse_area_factor;
      T                 m_random_to_cut_disk_area_factor;
};

}  // namespace foundation
