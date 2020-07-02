/**
 * Copyright (c) 2016 Eric Bruneton
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of the copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived from
 *    this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
 * THE POSSIBILITY OF SUCH DAMAGE.
 */

#pragma once

// appleseed.foundation headers.
#include "foundation/math/scalar.h"

#include "b_binary_function.h"

using namespace foundation;


const float EarthRadius = 6360000.0f;
const float AtmosphereRadius = 6420000.0f;

// The height of the atmosphere if its density was uniform.
const float RayleighScaleHeight = 8000.0f;

// The height of the Mie particle layer if its density was uniform.
const float MieScaleHeight = 1200.0f;

// The Angstrom alpha coefficient for the Mie optical depth.
const float MieAngstromAlpha = 0.8f;

// The Angstrom beta coefficient for the Mie optical depth.
const float MieAngstromBeta = 0.04f;

// The g parameter of the Cornette-Shanks phase function used for Mie particles.
const float MiePhaseFunctionG = 0.7f;



RegularSpectrum31f NewSolarSpectrum() {
    // Values from "Reference Solar Spectral Irradiance: ASTM G-173", ETR column
    // (see http://rredc.nrel.gov/solar/spectra/am1.5/ASTMG173/ASTMG173.html),
    // summed and averaged in each bin (e.g. the value for 360nm is the average of
    // the ASTM G-173 values for all wavelengths between 360 and 371.75nm).
    static const float kSpectralIrradiance[31] = {
        1.561492553,    // 400 nm
        1.70652234,	    // 410 nm
        1.65946383,	    // 420 nm
        1.703174468,	// 430 nm
        1.922501702,	// 440 nm
        2.01449383,	    // 450 nm
        2.020621489,	// 460 nm
        1.987939787,	// 470 nm
        1.955086596,	// 480 nm
        1.952352766,	// 490 nm
        1.926922979,	// 500 nm
        1.845134043,	// 510 nm
        1.860563617,	// 520 nm
        1.871258511,	// 530 nm
        1.853975532,	// 540 nm
        1.846872553,	// 550 nm
        1.827945319,	// 560 nm
        1.842558085,	// 570 nm
        1.803487021,	// 580 nm
        1.775715106,	// 590 nm
        1.742042766,	// 600 nm
        1.70144383,	    // 610 nm
        1.677125957,	// 620 nm
        1.63943234,	    // 630 nm
        1.603298723,	// 640 nm
        1.545717447,	// 650 nm
        1.523477021,	// 660 nm
        1.506940426,	// 670 nm
        1.471371489,	// 680 nm
        1.444816596,	// 690 nm
        1.407982553,	// 700 nm
    };
    return RegularSpectrum31f::from_array(kSpectralIrradiance);
} // Checked

RegularSpectrum31f NewRayleighScattering() {
    // Values from Table III in Penndorf 1957 "Tables of the Refractive Index for
    // Standard Air and the Rayleigh Scattering Coefficient for the Spectral
    // Region between 0.2 and 20.0 μ and Their Application to Atmospheric Optics".
    float kPenndorf[31] = {
        45.40E-6,   // 400nm
        40.98E-6,   // 410nm
        37.08E-6,   // 420nm
        33.65E-6,   // 430nm   
        30.60E-6,   // 440nm
        27.89E-6,   // 450nm
        25.48E-6,   // 460nm
        23.33E-6,   // 470nm
        21.40E-6,   // 480nm
        19.66E-6,   // 490nm
        18.10E-6,   // 500nm
        16.69E-6,   // 510nm
        15.42E-6,   // 520nm
        14.26E-6,   // 530nm
        13.21E-6,   // 540nm
        12.26E-6,   // 550nm
        11.39E-6,   // 560nm
        10.60E-6,   // 570nm
        9.876E-6,   // 580nm
        9.212E-6,   // 590nm
        8.604E-6,   // 600nm
        8.045E-6,   // 610nm
        7.531E-6,   // 620nm
        7.057E-6,   // 630nm
        6.620E-6,   // 640nm
        6.217E-6,   // 650nm
        5.844E-6,   // 660nm
        5.498E-6,   // 670nm
        5.178E-6,   // 680nm
        4.881E-6,   // 690nm
        4.605E-6,   // 700nm
    };
    for (int i = 0; i < 31; ++i) { 
        // The above values are for T_0=0°C. For T=15°C, a correction factor
        // T_0 / T must be applied (Eq. (12) in Penndorf paper).
        constexpr double T_0 = 273.16;
        constexpr double T = T_0 + 15.0;
        kPenndorf[i] = kPenndorf[i] * (T_0 / T);
    }
    return RegularSpectrum31f::from_array(kPenndorf); 
}

RegularSpectrum31f NewMieExtinction(double angstrom_alpha, double angstrom_beta) {
    float mie[31];
    for (unsigned int i = 0; i < 31; ++i) {
        const double x = (i / 31.0);
        double lambda = (400.0 * (1.0 - x) + 700.0 * x) / 1000.0;
        mie[i] = angstrom_beta * pow(lambda, -angstrom_alpha) / MieScaleHeight;
    }
    return RegularSpectrum31f::from_array(mie);
}

RegularSpectrum31f NewMieScattering(double angstrom_alpha, double angstrom_beta) {
    const double kSingleScatteringAlbedo = 0.8;
    return NewMieExtinction(angstrom_alpha, angstrom_beta) * RegularSpectrum31f(kSingleScatteringAlbedo);
}

const RegularSpectrum31f solar_spectrum = NewSolarSpectrum();
const RegularSpectrum31f rayleigh_scattering = NewRayleighScattering();
const RegularSpectrum31f mie_extinction = NewMieExtinction(MieAngstromAlpha, MieAngstromBeta);
const RegularSpectrum31f mie_scattering = NewMieScattering(MieAngstromAlpha, MieAngstromBeta);


// Returns the spectral irradiance of the Sun at the top of the atmosphere.
const RegularSpectrum31f& SolarSpectrum() { return solar_spectrum; }

// Returns the Rayleigh scattering coefficient at sea level.
const RegularSpectrum31f& RayleighScattering() { return rayleigh_scattering; }

// Returns the Mie extinction coefficient at sea level.
const RegularSpectrum31f& MieExtinction() { return mie_extinction; }

// Returns the Mie extinction and scattering coefficients at sea level for
// the given Angstrom parameters.
const RegularSpectrum31f& MieScattering() { return mie_scattering; }

// Returns the Mie scattering coefficient at sea level.
RegularSpectrum31f MieExtinction(double angstrom_alpha, double angstrom_beta) {
    return NewMieExtinction(angstrom_alpha, angstrom_beta);
}

// Computes the Rayleigh phase function for the given scattering angle.
// The integral of this function over all solid angles is 1.
double RayleighPhaseFunctionCos(double scattering_angle_cosine) {
    const double kRayleigh = 3.0 / (16.0 * Pi<float>());
    return kRayleigh * (1.0 + scattering_angle_cosine * scattering_angle_cosine);
} 
double RayleighPhaseFunction(double scattering_angle) {
    return RayleighPhaseFunctionCos(cos(scattering_angle));
} 

// Computes the Mie phase function for the given scattering angle (using
// the Cornette-Shanks approximation).
// The integral of this function over all solid angles is 1.
double MiePhaseFunctionCos(double scattering_angle_cosine) {
    const double g = MiePhaseFunctionG;
    const double kMie = 3.0 / (8.0 * Pi<double>()) * (1.0 - g * g) / (2.0 + g * g);
    return kMie * (1.0 + scattering_angle_cosine * scattering_angle_cosine) / pow(1.0 + g * g - 2.0 * g * scattering_angle_cosine, 1.5);
} 
double MiePhaseFunction(double scattering_angle) {
    return MiePhaseFunctionCos(cos(scattering_angle));
} 

