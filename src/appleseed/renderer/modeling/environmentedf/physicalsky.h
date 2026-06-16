
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
#include "foundation/image/regularspectrum.h"
#include "foundation/math/ray.h"
#include "foundation/math/distance.h"

#include "atmosphereshell.h"
#include "opticaldepth.h"

using namespace foundation;

namespace nishita {

    static const int num_wavelengths = 31;                                     // number of wavelengths per spectrum (400nm to 700nm, delta=10nm)
    static const int n_cylinders = 1024;                                       // number of cylinders for optical depth precomputation

    // Lookup table storing optical dephts into the direction of the sun.
    static sky::opticaldepth optical_depths_table[shell::n_atmosphere_shells + 1][n_cylinders];

    // Width of a single cylinder. Sum of all cylinder widths must be marginally smaller than the atmosphere radius.
    static const float cylinder_delta = (atmosphere_radius - n_cylinders) / (n_cylinders - 1);

    // Mie scattering coefficient and regular spectrum of mie coefficients.
    const float mie_extinction_coeff = 2e-5f;
    const RegularSpectrum31f mie_coeff_spectrum = RegularSpectrum31f(mie_extinction_coeff);

    // Sun irradiance (W*m^-2*nm^-1) values at the top of the atmosphere.
    // Source: https://www.nrel.gov/grid/solar-resource/spectra.html, Table SMART MODTRAN ETR Spectra.
    const float sun_radiance[num_wavelengths] = {
            2.11275f,   // 400nm
            2.58882f,   // 410nm
            2.58291f,   // 420nm
            2.42323f,   // 430nm
            2.67605f,   // 440nm
            2.96583f,   // 450nm
            3.05454f,   // 460nm
            3.00575f,   // 470nm
            3.06637f,   // 480nm
            2.88304f,   // 490nm
            2.87121f,   // 500nm
            2.78250f,   // 510nm
            2.71006f,   // 520nm
            2.72336f,   // 530nm
            2.63613f,   // 540nm
            2.55038f,   // 550nm
            2.50602f,   // 560nm
            2.53116f,   // 570nm
            2.53559f,   // 580nm
            2.51342f,   // 590nm
            2.46315f,   // 600nm
            2.41732f,   // 610nm
            2.36853f,   // 620nm
            2.32121f,   // 630nm
            2.28277f,   // 640nm
            2.23398f,   // 650nm
            2.19702f,   // 650nm
            2.15267f,   // 670nm
            2.10979f,   // 680nm
            2.07283f,   // 690nm
            2.02404f    // 700nm
    };
    const RegularSpectrum31f sun_radiance_spectrum = RegularSpectrum31f::from_array(sun_radiance);

    // Rayleigh scattering coefficients (m^-1) from Rudolf Penndorf (1957) Table 3.
    // Source: https://doi.org/10.1364/JOSA.47.000176
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

    // Ozona absorption coefficient (m^-1).
    // Source: https://www.iup.uni-bremen.de/gruppen/molspec/databases/referencespectra/o3spectra2011/index.html
    const float ozone_coeff[num_wavelengths] = {
        3.804511196879277e-09f,      // 400 nm
        6.913786897105462e-09f,      // 410 nm
        1.3852765960014552e-08f,     // 420 nm
        2.1308603627919998e-08f,     // 430 nm
        3.974417614472733e-08f,      // 440 nm
        5.779591314894535e-08f,      // 450 nm
        9.191587335498181e-08f,      // 460 nm
        1.2363721551643633e-07f,     // 470 nm
        1.9505027060647285e-07f,     // 480 nm
        2.2672051905767247e-07f,     // 490 nm
        3.716605995280002e-07f,      // 500 nm
        4.0267814468581854e-07f,     // 510 nm
        5.364069922247275e-07f,      // 520 nm
        6.912136535745463e-07f,      // 530 nm
        7.745488102370914e-07f,      // 540 nm
        8.772119777709093e-07f,      // 550 nm
        1.0680234682312722e-06f,     // 560 nm
        1.1695343279723626e-06f,     // 570 nm
        1.1011384812494534e-06f,     // 580 nm
        1.1759623019832746e-06f,     // 590 nm
        1.2552240270210935e-06f,     // 600 nm
        1.0772983295309093e-06f,     // 610 nm
        9.361428617905462e-07f,      // 620 nm
        8.052237676756349e-07f,      // 630 nm
        6.675936847221821e-07f,      // 640 nm
        5.619235334727269e-07f,      // 650 nm
        4.6550674463418176e-07f,     // 660 nm
        3.7068568738763686e-07f,     // 670 nm
        3.0466838275272715e-07f,     // 680 nm
        2.3788813137578206e-07f,     // 690 nm
        1.8836707145585476e-07f,     // 700 nm
    };
    const RegularSpectrum31f ozone_coeff_spectrum = RegularSpectrum31f::from_array(ozone_coeff);

    // Precomputes g parameter determining Mie assymetricity depending on atmospheric haze condition.
    void precompute_mie_g(float haze);

    // Precomputes shell values with exponentially decreasing radius.
    void precompute_shells();

    // Precomputes optical dephts using n cylinders.
    void precompute_optical_depths(const Vector3f& sun_dir, float air_particle_density, float dust_particle_density, float ozone_particle_density);

    // Mie assymetricity value depending on atmospheric haze condition u, varies from 0.7 tp 0.85.
    inline float mie_assymetricity(float u);

    // Rayleigh phase function for a given angle (rad).
    float rayleigh_phase(float angle);

    // Mie phase function for a given angle (rad).
    float mie_phase(float angle);

    // Determines whether the light ray intersects with the earths surface.
    bool intersects_earth(const Ray3f& ray);
    bool intersects_earth(const Ray3f& ray, float& distance);

    // Determines whether a ray is below the earths surface.
    bool ray_inside_earth(const Ray3f& ray);

    // Determines the distance a ray travels before hitting the outer point of the atmosphere.
    float distance_to_atmosphere(const Ray3f& ray);

    // Computes optical depth along a ray considering mie and rayleigh scattering.
    sky::opticaldepth ray_optical_depth(const Ray3f& ray, float air_particle_density, float dust_particle_density, float ozone_particle_density);

    // Finds best fitting optical depth from lookup table.
    sky::opticaldepth lookup_optical_depth(const Ray3f& ray);

    // Computes the irradiance spectrum of a single ray through the atmosphere, considering rayleigh and mie scattering.
    void single_scattering(
        const Ray3f& ray,
        const Vector3f& sun_dir,
        float air_particle_density,
        float dust_particle_density,
        float ozone_particle_density,
        bool is_precomputed,
        RegularSpectrum31f& spectrum);

    // Returns the irradiance spectrum of the sun for rays pointing directgly at the sun.
    bool sun_disk(
        const Ray3f& ray,
        float air_particle_density,
        float dust_particle_density,
        float ozone_particle_density,
        float sun_radius,
        RegularSpectrum31f& spectrum);


}

