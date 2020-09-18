
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
#include "foundation/math/ray.h"

#include <cmath>

using namespace foundation;

// Forward declarations.
struct intersection;

static const float earth_radius = 6378137.0f;                               // Radius of Earth (m) according to nssdc.gsfc.nasa.gov.
static const float atmosphere_thickness = 60000.0f;                         // Rough height of Mesosphere according to nasa.gov.
static const float atmosphere_radius = earth_radius + atmosphere_thickness; // Radius of atmosphere (m).
static const Vector3f earth_center = Vector3f(0.0f, 0.0f, 0.0f);            // Central point of the earth (m).

static const float rayleigh_scale = 7994.0f;                                // Rayleigh scale height H0 (m).
static const float mie_scale = 1200.0f;                                     // Mie scale height H0 (m).
static const float a = expf(-(atmosphere_radius - earth_radius) / rayleigh_scale) - 1.0f;


// Density of rayleigh particles at height (m) above the earths surface.
static float get_rayleigh_density(float height);

// Density of mie particles at height (m) above the earths surface.
static float get_mie_density(float height);

// Class representing a shell around the earth with constant rayleigh/mie particle density.
class shell {

public:
    int index;                  // Shell index between [0, n_shells)
    float radius;               // Radius (m) of shell around earth
    float rayleigh_density;     // Constant density of aerosol particles withing shell
    float mie_density;          // Constant density of dust particles within shell
    Vector3f center;            // Center of shell, is always earths center as well

    shell();

    // Instanciate a new shell at index i of n_shells.
    shell(int i);

    // Predefine all shell values.
    shell(int i, float r, float rd, float md);

    // Determines whether a given light ray origins inside of the shell.
    bool ray_in_shell(Ray3f ray);

    // Determines intersection from ray within a shell.
    intersection intersection_distance_inside(Ray3f ray);

    // Determines 0, 1 or 2 intersections from ray outside of a shell.
    size_t intersection_distances_outside(Ray3f ray, intersection intersections[2]);

protected:

    // Finds rayleigh and mie density for this shell.
    void densities();

};

// Determine height (m) of shell.
float find_shell_radius(int shell_index);

// Returns index of shell that matches best. A return value of 4.2 suggest that the distance to shell 4 is 20% and shell 5 is 80%.
float find_shell_index(float shell_radius, shell shells[]);

// Intersection of ray with a shell after a distance.
struct intersection {

    float distance;             // Distance before ray hits shell
    shell* involved_shell;      // Pointer to shell that was hit

    intersection();

    intersection(float d, shell* s);
};

// Comparison function used to sort intersections with increasing distance.
bool sort_intersections(intersection i, intersection j);

static const int n_shells = 64;                                             // Number of atmospheric shells around the earth
static shell shells[n_shells + 1];                                          // Precomputed atmospheric shells with constant particle density
