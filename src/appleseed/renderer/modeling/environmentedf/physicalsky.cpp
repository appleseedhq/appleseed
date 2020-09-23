
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
#include "foundation/math/vector.h"
#include "foundation/image/regularspectrum.h"
#include "foundation/math/ray.h"
#include "foundation/math/intersection/raysphere.h"
#include "physicalsky.h"

#include <cmath>

using namespace foundation;


float sqr(float n)
{
    return n * n;
}

void precompute_mie_g(float haze) {
    mie_g = mie_assymetricity(haze);
}

void precompute_shells() {
    for (int i = 0; i < shell::n_atmosphere_shells; i++) {
        shell::atmosphere_shells[i] = shell(i);
    }
    // Outermost "shell" is deep space with infinite radius.
    shell::atmosphere_shells[shell::n_atmosphere_shells] = shell(shell::n_atmosphere_shells, INFINITY, 0, 0);
}

void precompute_optical_depths(const Vector3f& sun_dir, float air_density, float dust_density) {
    float sqrt3 = sqrtf(3.0f);
    Vector3f unit_vector = Vector3f(sqrt3);
    Vector3f sun_dir_perpendicular = normalize(cross(sun_dir, unit_vector));

    for (int n_cylinder = 0; n_cylinder < n_cylinders; n_cylinder++) {
        float cylinder_radius = cylinder_delta * n_cylinder;
        Ray3f cylinder_border = Ray3f(sun_dir_perpendicular * cylinder_radius, sun_dir);
        for (int n_shell = 0; n_shell <= shell::n_atmosphere_shells; n_shell++) {
            if (shell::atmosphere_shells[n_shell].ray_in_shell(cylinder_border)) {
                shell::intersection shell_cylinder_intersection = shell::atmosphere_shells[n_shell].intersection_distance_inside(cylinder_border);
                Vector3f intersection_point = cylinder_border.m_org + sun_dir * shell_cylinder_intersection.distance;
                Ray3f intersection_ray = Ray3f(intersection_point, sun_dir);

                const sky::opticaldepth optical_depth = ray_optical_depth(intersection_ray, air_density, dust_density);
                optical_depths_table[n_shell][n_cylinder] = optical_depth;
            }
            else {
                optical_depths_table[n_shell][n_cylinder] = sky::opticaldepth();
            }
        }
    }
}

sky::opticaldepth lookup_optical_depth(const Ray3f& ray) {
    Vector3f sun_dir = ray.m_dir;
    float radius = sqrtf(square_distance_point_line(ray.m_org, earth_center, sun_dir));

    float cylinder_index_raw = radius / cylinder_delta;
    int cylinder_index = static_cast<int>(cylinder_index_raw);
    float second_cylinder_dominance = cylinder_index_raw - static_cast<float>(cylinder_index);
    float first_cylinder_dominance = 1.0f - second_cylinder_dominance; 

    float shell_index_raw = shell::find_index(norm(ray.m_org));
    int shell_index = static_cast<int>(shell_index_raw);
    float second_shell_dominance = shell_index_raw - static_cast<float>(shell_index);
    float first_shell_dominance = 1.0f - second_shell_dominance;

    sky::opticaldepth avg_cylinder_depths_1 = optical_depths_table[shell_index][cylinder_index] * first_cylinder_dominance
                                            + optical_depths_table[shell_index][cylinder_index + 1] * second_cylinder_dominance;
    sky::opticaldepth avg_cylinder_depths_2 = optical_depths_table[shell_index+1][cylinder_index] * first_cylinder_dominance
                                            + optical_depths_table[shell_index+1][cylinder_index + 1] * second_cylinder_dominance;
    sky::opticaldepth looked_up_depth = avg_cylinder_depths_1 * first_shell_dominance + avg_cylinder_depths_2 * second_shell_dominance;
    return looked_up_depth;
}

float rayleigh_phase(float angle)
{
    const float angle_squared = sqr(angle);
    return 3.0f / (16.0f * Pi<float>()) * (1.0f + angle_squared);
}

float mie_assymetricity(float u)
{
    const float x = 5.0f / 9.0f * u + 125.0f / 729.0f * powf(u, 3.0f) +
        powf(64.0f / 27.0f - 325.0f / 243.0f * sqr(u) + 1250.0f / 2187.0f * powf(u, 4), 1.0f / 2.0f);
    return 5.0f / 9.0f * u - (4.0f / 3.0f - 25.0f / 81.0f * sqr(u)) * powf(x, -1.0f / 3.0f) + powf(x, 1.0f / 3.0f);
}

float mie_phase(float angle)
{
    const static float mie_g_sqr = sqr(mie_g);
    return 3.0f / (8.0f * Pi<float>()) * (1.0f - mie_g_sqr) / (2.0f + mie_g_sqr) *
        (1.0f + angle * angle) / powf(1.0f + mie_g_sqr - 2.0f * mie_g * angle, 3.0f / 2.0f);
}

bool intersects_earth(const Ray3f& ray)
{
    if (ray.m_dir.y >= 0)
        return false;
    return intersect_sphere_unit_direction(ray, earth_center, earth_radius);
}

bool ray_inside_earth(const Ray3f& ray)
{
    return (norm(ray.m_org) < earth_radius);
}

float distance_to_atmosphere(const Ray3f& ray) {
    const float radius_sqr = earth_radius * earth_radius;
    float b = -2.0f * dot(ray.m_dir, -ray.m_org);
    float c = square_norm(ray.m_org) - radius_sqr;
    return (-b + sqrtf(b * b - 4.0f * c)) / 2.0f;
}

sky::opticaldepth ray_optical_depth(const Ray3f& ray, float air_density, float dust_density)
{
    shell::intersection intersections[shell::n_atmosphere_shells * 2];
    int n_intersections = shell::find_intersections(ray, intersections);
    float passed_distance = 0.0f;

    sky::opticaldepth optical_depth(0.0f, 0.0f, air_density, dust_density);

    for (int i = 0; i < n_intersections; i++) {

        shell::intersection ith_intersection = intersections[i];
        float segment_length = ith_intersection.distance - passed_distance;

        optical_depth.increase(
            segment_length,
            ith_intersection.involved_shell->rayleigh_density,
            ith_intersection.involved_shell->mie_density
        );

        passed_distance = ith_intersection.distance;
    }
    return optical_depth;
}

void single_scattering(
    const Ray3f& ray,
    const Vector3f& sun_dir,
    float air_density,
    float dust_density,
    RegularSpectrum31f& spectrum)
{
    spectrum.set(0.0f);

    shell::intersection intersections[shell::n_atmosphere_shells * 2];
    int n_intersections = shell::find_intersections(ray, intersections);
    float passed_distance = 0.0f;

    sky::opticaldepth optical_depth(0.0f, 0.0f, air_density, dust_density);

    float angle = dot(ray.m_dir, sun_dir);
    float rayleigh_phase_function = rayleigh_phase(angle);
    float mie_phase_function = mie_phase(angle);

    for (int i = 0; i < n_intersections; i++) {

        shell::intersection ith_intersection = intersections[i];
        float segment_length = ith_intersection.distance - passed_distance;
        float half_segment_length = segment_length / 2.0f;

        float rayleigh_mulecule_density = ith_intersection.involved_shell->rayleigh_density;
        float mie_molecule_density = ith_intersection.involved_shell->mie_density;

        optical_depth.increase(segment_length, rayleigh_mulecule_density, mie_molecule_density);

        Vector3f segment_middle_point = ray.m_org + (ray.m_dir * (passed_distance + half_segment_length));

        Ray3f scatter_ray = Ray3f(segment_middle_point, sun_dir);
        if (!intersects_earth(scatter_ray) && !ray_inside_earth(scatter_ray)) {
            // sky::opticaldepth ligh_optical_depth = ray_optical_depth(scatter_ray, air_density, dust_density);
            sky::opticaldepth ligh_optical_depth = lookup_optical_depth(scatter_ray);
            sky::opticaldepth total_optical_depth = optical_depth + ligh_optical_depth;

            /*
            const RegularSpectrum31f total_extinction_density = rayleigh_coeff_spectrum * total_optical_depth.rayleigh + mie_coeff_spectrum * 1.11f * total_optical_depth.mie;

            float attenuations[num_wavelengths];
            for (int wl = 0; wl < num_wavelengths; wl++) { attenuations[wl] = expf(-total_extinction_density[wl]); }
            const RegularSpectrum31f attenuation = RegularSpectrum31f::from_array(attenuations);

            const RegularSpectrum31f total_reduction = rayleigh_phase_function * rayleigh_mulecule_density * rayleigh_coeff_spectrum + mie_phase_function * mie_molecule_density * mie_coeff_spectrum;
            spectrum += attenuation * total_reduction * sun_irradiance_spectrum * segment_length;
            */
            
            for (int wl = 0; wl < num_wavelengths; wl++) {
                float rayleigh_extinction_density = total_optical_depth.rayleigh * rayleigh_coeff[wl];
                float mie_extinction_density = total_optical_depth.mie * 1.11f * mie_extinction_coeff;
                float total_extinction_density = rayleigh_extinction_density + mie_extinction_density;
                float attenuation = expf(-total_extinction_density);

                float rayleigh_scattering_density = rayleigh_mulecule_density * rayleigh_coeff[wl];
                float mie_scattering_density = mie_molecule_density * mie_extinction_coeff;

                float rayleigh_reduction = rayleigh_phase_function * rayleigh_scattering_density;
                float mie_reduction = mie_phase_function * mie_scattering_density;
                float total_reduction = rayleigh_reduction + mie_reduction;

                spectrum[wl] += attenuation * total_reduction * sun_irradiance[wl] * segment_length;
            }
            
        }

        passed_distance = ith_intersection.distance;
    }
}


void sun_disk(
    const Ray3f& ray,
    float air_density,
    float dust_density,
    float sun_radius,
    RegularSpectrum31f& spectrum)
{
    spectrum.set(0.0f);
    if (intersects_earth(ray)) {
        return;
    }
    sky::opticaldepth optical_depth = ray_optical_depth(ray, air_density, dust_density);
    float rayleigh_optical_depht = optical_depth.rayleigh;
    float mie_optical_depht = optical_depth.mie;
    float solid_angle = Pi<float>() * (1.0f - cosf(sun_radius));

    for (int i = 0; i < num_wavelengths; i++) {
        float rayleigh_transmittance = rayleigh_coeff[i] * rayleigh_optical_depht * air_density;
        float mie_transmittance = 1.11f * mie_extinction_coeff * mie_optical_depht * dust_density;
        float total_transmittance = rayleigh_transmittance + mie_transmittance;
        spectrum[i] = (sun_irradiance[i] / solid_angle) * expf(-total_transmittance);
    }
}
