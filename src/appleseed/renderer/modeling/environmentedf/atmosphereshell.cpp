
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Joel Barmettler, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/math/intersection/raysphere.h"
#include "foundation/math/ray.h"

// Header file.
#include "atmosphereshell.h"

#include <cmath>

using namespace foundation;

static float get_rayleigh_density(float height)
{
    return expf((-height + earth_radius) / rayleigh_scale);
}

static float get_mie_density(float height)
{
    return expf((-height + earth_radius) / mie_scale);
}

static float get_ozone_density(float height)
{
    const float total_height = height - earth_radius;
    if (total_height < ozone_start)
        return 0.0f;
    if (total_height < ozone_peak)
        return (total_height - ozone_start) * (1.0f / (ozone_peak - ozone_start));
    return expf(-(total_height - ozone_peak) / ozone_start);
}

const int shell::n_atmosphere_shells;
shell shell::atmosphere_shells[shell::n_atmosphere_shells + 1];

shell::shell() {}

shell::shell(int i) : index(i)
{
    radius = shell::find_radius(index);
    densities();
}

shell::shell(int i, float r, float rd, float md, float od) :
    index(i),
    radius(r),
    rayleigh_density(rd),
    mie_density(md),
    ozone_density(od) {}

bool shell::ray_in_shell(const Ray3f& ray) const
{
    return (norm(ray.m_org) < radius);
}

shell::intersection shell::intersection_distance_inside(const Ray3f& ray) const
{
    const float radius_sqr = radius * radius;
    float b = -2.0f * dot(ray.m_dir, -ray.m_org);
    float c = square_norm(ray.m_org) - radius_sqr;
    float distance = (-b + sqrtf(b * b - 4.0f * c)) / 2.0f;
    return shell::intersection(distance, this);
}

size_t shell::intersection_distances_outside(const Ray3f& ray, shell::intersection intersections[2]) const
{
    float distances[2] = { 0.0f };
    size_t n_distances = intersect_sphere_unit_direction(ray, earth_center, radius, distances);
    intersections[0].distance = distances[0];
    intersections[1].distance = distances[1];
    intersections[1].involved_shell = this;
    return n_distances;
}

void shell::densities()
{
    const float previous_radius = shell::find_radius(index-1);
    const float shell_center_height = (radius + previous_radius) / 2.0f;
    rayleigh_density = get_rayleigh_density(shell_center_height);
    mie_density = get_mie_density(shell_center_height);
    ozone_density = get_ozone_density(shell_center_height);
}

float shell::find_radius(int shell_index)
{
    if (shell_index < 0) {
        return earth_radius;
    }
    const float scale = rayleigh_scale * 2;
    float x = static_cast<float>(shell_index) / static_cast<float>(shell::n_atmosphere_shells - 1);
    const float a = expf(-(atmosphere_radius - earth_radius) / scale) - 1.0f;
    return earth_radius - scale * logf(a * x + 1.0f);
}

float shell::find_index(float shell_radius)
{
    // Alternate way, direct inversion of find_radius
    const float scale = rayleigh_scale * 2.f;
    const float a = expf(-(atmosphere_radius - earth_radius) / scale) - 1.f;
    return (expf(-(shell_radius - earth_radius) / scale) - 1) / a * (shell::n_atmosphere_shells - 1);
    // Old way, for loop. Keep until direct calculation works.
    for (int i = 0; i < shell::n_atmosphere_shells; i++) {
        float radius_s1 = shell::atmosphere_shells[i].radius;
        float radius_s2 = shell::atmosphere_shells[i + 1].radius;
        float dist_s1 = radius_s1 - shell_radius;
        float dist_s2 = radius_s2 - shell_radius;
        if (dist_s1 <= 0 && dist_s2 > 0) {
            return static_cast<float>(i) + ((shell_radius - radius_s1) / (radius_s2 - radius_s1));
        }
    }
    return shell::n_atmosphere_shells;
}

int shell::find_intersections(const Ray3f& ray, shell::intersection intersections[]) {
    int intersct_i = 0;
    for (int k = 0; k < shell::n_atmosphere_shells; k++) {
        shell *kth_shell = &shell::atmosphere_shells[k];

        if (kth_shell->ray_in_shell(ray)) {
            intersections[intersct_i] = kth_shell->intersection_distance_inside(ray);
            intersct_i++;
        }
        else {
            shell::intersection ray_intersections[2];
            size_t n_intersections = kth_shell->intersection_distances_outside(ray, ray_intersections);
            if (n_intersections >= 1) {
                ray_intersections[0].involved_shell = &shell::atmosphere_shells[k + 1];
                intersections[intersct_i] = ray_intersections[0];
                intersct_i++;
            }
            if (n_intersections == 2) {
                intersections[intersct_i] = ray_intersections[1];
                intersct_i++;
            }
        }
    }

    std::sort(intersections, intersections + intersct_i, shell::intersection::sort_by_distance);
    return intersct_i;
}

shell::intersection::intersection() : distance(0) {}

shell::intersection::intersection(float d, const shell* s) : distance(d), involved_shell(s) {}

bool shell::intersection::sort_by_distance(const shell::intersection& i, const shell::intersection& j)
{
    return i.distance < j.distance;
}
