
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/vector.h"
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

shell::shell() {}

shell::shell(int i) : index(i), center(earth_center) {
    radius = find_shell_radius(index);
    densities();
}

shell::shell(int i, float r, float rd, float md) :
    index(i),
    radius(r),
    rayleigh_density(rd),
    mie_density(md),
    center(earth_center) {}

bool shell::ray_in_shell(Ray3f ray) {
    return (norm(ray.m_org) < radius);
}

intersection shell::intersection_distance_inside(Ray3f ray) {
    const float radius_sqr = radius * radius;
    float b = -2.0f * dot(ray.m_dir, -ray.m_org);
    float c = square_norm(ray.m_org) - radius_sqr;
    float distance = (-b + sqrtf(b * b - 4.0f * c)) / 2.0f;
    return intersection(distance, this);
}

size_t shell::intersection_distances_outside(Ray3f ray, intersection intersections[2]) {
    float distances[2] = { 0.0f };
    size_t n_distances = intersect_sphere_unit_direction(ray, center, radius, distances);
    intersections[0].distance = distances[0];
    intersections[1].distance = distances[1];
    intersections[1].involved_shell = this;
    return n_distances;
}

void shell::densities() {
    const float previous_radius = find_shell_radius(index-1);
    const float shell_center_height = (radius + previous_radius) / 2.0f;
    rayleigh_density = get_rayleigh_density(shell_center_height);
    mie_density = get_mie_density(shell_center_height);
}

float find_shell_radius(int shell_index)
{
    if (shell_index < 0) {
        return earth_radius;
    }
    float x = static_cast<float>(shell_index) / static_cast<float>(n_shells - 1);
    return earth_radius - rayleigh_scale * logf(a * x + 1.0f);
}

float find_shell_index(float shell_radius, shell shells[])
{
    // This is a shitty implementation! This can for sure be done numerically.
    for (int i = 0; i < n_shells; i++) {
        float radius_s1 = shells[i].radius;
        float radius_s2 = shells[i + 1].radius;
        float dist_s1 = radius_s1 - shell_radius;
        float dist_s2 = radius_s2 - shell_radius;
        if (dist_s1 <= 0 && dist_s2 > 0) {
            return static_cast<float>(i) + ((shell_radius - radius_s1) / (radius_s2 - radius_s1));
        }
    }
    return n_shells;
}

intersection::intersection() : distance(0) {}

intersection::intersection(float d, shell* s) : distance(d), involved_shell(s) {}

bool sort_intersections(intersection i, intersection j) {
    return i.distance < j.distance;
}
