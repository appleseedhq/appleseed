
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

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/image/regularspectrum.h"
#include "foundation/math/ray.h"
#include "foundation/math/distance.h"

#include "atmosphereshell.h"
#include "opticaldepth.h"

using namespace foundation;

static const int num_wavelengths = 31;                                     // Number of wavelengths per spectrum (400nm to 700nm, delta=10nm)
static float mie_g;                                                        // Mie assymetricity component g.
static const int n_cylinders = 1028;                                       // Number of cylinders for optical depth precomputation

// Lookup table storing optical dephts into the direction of the sun.
static sky::opticaldepth optical_depths_table[shell::n_atmosphere_shells+1][n_cylinders];

// Width of a single cylinder. Sum of all cylinder widths must be marginally smaller than the atmosphere radius.
static const float cylinder_delta = (atmosphere_radius - n_cylinders) / (n_cylinders - 1);

// Mie scattering coefficient and regular spectrum of mie coefficients.
const float mie_extinction_coeff = 2e-5f;                           
const RegularSpectrum31f mie_coeff_spectrum = RegularSpectrum31f(mie_extinction_coeff);

// Sun irradiance (W*m^-2*nm^-1) values at the top of the atmosphere.
// Source: https://www.nrel.gov/grid/solar-resource/spectra.html, Table SMART MODTRAN ETR Spectra.
const float sun_irradiance[num_wavelengths] = {
        1.689945f,   // 400 nm
        1.720414f,   // 410 nm
        1.693671f,   // 420 nm
        1.635621f,   // 430 nm
        1.922204f,   // 440 nm
        2.050226f,   // 450 nm
        2.017384f,   // 460 nm
        2.010281f,   // 470 nm
        1.938443f,   // 480 nm
        1.962400f,   // 490 nm
        1.920103f,   // 500 nm
        1.831570f,   // 510 nm
        1.871788f,   // 520 nm
        1.901942f,   // 530 nm
        1.866246f,   // 540 nm
        1.856125f,   // 550 nm
        1.843470f,   // 560 nm
        1.844103f,   // 570 nm
        1.826544f,   // 580 nm
        1.780521f,   // 590 nm
        1.746604f,   // 600 nm
        1.714340f,   // 610 nm
        1.696628f,   // 620 nm
        1.655753f,   // 630 nm
        1.614657f,   // 640 nm
        1.528049f,   // 650 nm
        1.558611f,   // 660 nm
        1.502000f,   // 670 nm
        1.462200f,   // 680 nm
        1.438943f,   // 690 nm
        1.383291f,   // 700 nm
};
const RegularSpectrum31f sun_irradiance_spectrum = RegularSpectrum31f::from_array(sun_irradiance);

// Rayleigh scattering coefficients (m^-1) from Rudolf Penndorf (1957) Table 3.
// Source: https://www.osapublishing.org/josa/abstract.cfm?uri=josa-47-2-176.
const float rayleigh_coeff[num_wavelengths] = {
        45.40e-6f,   // 400nm
        40.98e-6f,   // 410nm
        37.08e-6f,   // 420nm
        33.65e-6f,   // 430nm
        30.60e-6f,   // 440nm
        27.89e-6f,   // 450nm
        25.48e-6f,   // 460nm
        23.33e-6f,   // 470nm
        21.40e-6f,   // 480nm
        19.66e-6f,   // 490nm
        18.10e-6f,   // 500nm
        16.69e-6f,   // 510nm
        15.42e-6f,   // 520nm
        14.26e-6f,   // 530nm
        13.21e-6f,   // 540nm
        12.26e-6f,   // 550nm
        11.39e-6f,   // 560nm
        10.60e-6f,   // 570nm
        9.876e-6f,   // 580nm
        9.212e-6f,   // 590nm
        8.604e-6f,   // 600nm
        8.045e-6f,   // 610nm
        7.531e-6f,   // 620nm
        7.057e-6f,   // 630nm
        6.620e-6f,   // 640nm
        6.217e-6f,   // 650nm
        5.844e-6f,   // 660nm
        5.498e-6f,   // 670nm
        5.178e-6f,   // 680nm
        4.881e-6f,   // 690nm
        4.605e-6f,   // 700nm
};
const RegularSpectrum31f rayleigh_coeff_spectrum = RegularSpectrum31f::from_array(rayleigh_coeff);

// Utility function to calculate square of a number n.
inline float sqr(float n);

// Precomputes g parameter determining Mie assymetricity depending on atmospheric haze condition.
void precompute_mie_g(float haze);

// Precomputes shell values with exponentially decreasing radius.
void precompute_shells();

// Precomputes optical dephts using n cylinders.
void precompute_optical_depths(const Vector3f& sun_dir, float air_density, float dust_density);

// Finds best fitting optical depth from lookup table.
sky::opticaldepth lookup_optical_depth(const Ray3f& ray);

// Mie assymetricity value depending on atmospheric haze condition u, varies from 0.7 tp 0.85.
inline float mie_assymetricity(float u);

// Rayleigh phase function for a given angle (rad).
float rayleigh_phase(float angle);

// Mie phase function for a given angle (rad).
float mie_phase(float angle);

// Determines whether the light ray intersects with the earths surface.
bool intersects_earth(const Ray3f& ray);

// Determines whether a ray is below the earths surface.
bool ray_inside_earth(const Ray3f& ray);

// Determines the distance a ray travels before hitting the outer point of the atmosphere.
float distance_to_atmosphere(const Ray3f& ray);

// Computes optical depth along a ray considering mie and rayleigh scattering.
sky::opticaldepth ray_optical_depth(const Ray3f& ray, float air_density, float dust_density);

// Computes the irradiance spectrum of a single ray through the atmosphere, considering rayleigh
// and mie scattering.
void single_scattering(
    const Ray3f& ray,
    const Vector3f& sun_dir,
    float air_density,
    float dust_density,
    RegularSpectrum31f& spectrum);

// Returns the irradiance spectrum of the sun for rays pointing directgly at the sun.
void sun_disk(
    const Ray3f& ray,
    float air_density,
    float dust_density,
    float sun_radius,
    RegularSpectrum31f& spectrum);
