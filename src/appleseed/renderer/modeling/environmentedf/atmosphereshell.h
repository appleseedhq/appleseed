
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

#pragma once

#include "foundation/math/vector.h"
#include "foundation/math/ray.h"

#include <cmath>

using namespace foundation;

// Forward declarations.
struct intersection;

const float earth_radius = 6378137.0f;                               // radius of Earth (m) according to nssdc.gsfc.nasa.gov
const float atmosphere_thickness = 60000.0f;                         // rough height of Mesosphere according to nasa.gov
const float atmosphere_radius = earth_radius + atmosphere_thickness; // radius of atmosphere (m)
const Vector3f earth_center(0.0f, 0.0f, 0.0f);                       // central point of the earth (m)

const float rayleigh_scale = 7994.0f;                                // rayleigh scale height H0 (m)
const float mie_scale = 1200.0f;                                     // mie scale height H0 (m)

const float ozone_ground = 0.4f;                                      // total amount of ozone on the ground
const float ozone_start = 10000.0f;                                   // height at beginning of ozone layer in atmosphere (m)
const float ozone_peak = 32000.0f;                                    // height at peak density of ozone in atmosphere (m)

// Density of rayleigh particles at height (m) above the earths surface.
float get_rayleigh_density(float height);

// Density of mie particles at height (m) above the earths surface.
float get_mie_density(float height);

// Density of ozone particles (m) above the earths surface.
// Source: https://doi.org/10.1145/584458.584482
float get_ozone_density(float height);

//
// Class representing a shell around the earth with constant rayleigh/mie particle density.
// 
class shell {

public:

    static const int n_atmosphere_shells = 64;                  // number of atmospheric shells around the earth
    static shell atmosphere_shells[n_atmosphere_shells + 1];    // precomputed atmospheric shells with constant particle density

    //
    // Intersection of ray with a shell after a distance (m).
    // 
    struct intersection {

        float distance;                 // distance before ray hits shell
        const shell* involved_shell;    // pointer to shell that was hit

        intersection();

        intersection(float d, const shell* s);

        // Comparison function used to sort intersections with increasing distance.
        static bool sort_by_distance(const intersection& i, const intersection& j);
    };

    int index;                          // shell index between [0, n_shells)
    float radius;                       // radius (m) of shell around earth
    float rayleigh_density;             // constant density of aerosol particles withing shell
    float mie_density;                  // constant density of dust particles within shell
    float ozone_density;                // constant density of ozone particles within shell

    shell();

    // Instanciate a new shell at index i of n_shells.
    shell(int i);

    // Predefine all shell values.
    shell(int i, float r, float rd, float md, float od);

    // Determines whether a given light ray origins inside of the shell.
    bool ray_in_shell(const Ray3f& ray) const;

    // Determines intersection from ray within a shell.
    shell::intersection intersection_distance_inside(const Ray3f& ray) const;

    // Determines 0, 1 or 2 intersections from ray outside of a shell.
    size_t intersection_distances_outside(const Ray3f& ray, shell::intersection intersections[2]) const;

    // Determine height (m) of shell.
    static float find_radius(int shell_index);

    // Returns index of shell that matches best. A return value of 4.2 suggest that the distance to shell 4 is 20% and shell 5 is 80%.
    static float find_index(float shell_radius);

    // Finds the intersection distances between the ray and each atmospheric shell.
    // Returns the number N of intersections found, stores intersection objects in the intersections-array from position 0 to position N-1.
    static int find_intersections(const Ray3f& ray, shell::intersection intersections[]);

private:

    // Finds rayleigh and mie density for this shell.
    void densities();

};


