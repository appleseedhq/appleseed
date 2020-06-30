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

#include "b_units.h"
#include "b_binary_function.h"

using namespace foundation;


const double EarthRadius = 6360.0 * 1000;
const double AtmosphereRadius = 6420.0 * 1000;

// The height of the atmosphere if its density was uniform.
const double RayleighScaleHeight = 8000.0;

// The height of the Mie particle layer if its density was uniform.
const double MieScaleHeight = 1200.0;

// The Angstrom alpha coefficient for the Mie optical depth.
const double MieAngstromAlpha = 0.8;

// The Angstrom beta coefficient for the Mie optical depth.
const double MieAngstromBeta = 0.04;

// The g parameter of the Cornette-Shanks phase function used for Mie particles.
const double MiePhaseFunctionG = 0.7;

// The Linke turbidity value, for the Preetham and Hosek models.
const double Turbidity = 2.53;



bSpectrum31f NewSolarSpectrum() {
    // Values from "Reference Solar Spectral Irradiance: ASTM G-173", ETR column
    // (see http://rredc.nrel.gov/solar/spectra/am1.5/ASTMG173/ASTMG173.html),
    // summed and averaged in each bin (e.g. the value for 360nm is the average of
    // the ASTM G-173 values for all wavelengths between 360 and 371.75nm).
    static const float kSpectralIrradiance[40] = {
        1.13419, 1.09801, 1.03541, 1.45086, 1.72453, 1.654, 1.70536, 1.97393,
        2.03543, 2.00643, 1.95531, 1.95426, 1.92438, 1.82092, 1.88517, 1.85545,
        1.85083, 1.82758, 1.84475, 1.78771, 1.76683, 1.70858, 1.68278, 1.63849,
        1.59608, 1.52211, 1.52468, 1.47836, 1.4485, 1.40522, 1.35526, 1.32788,
        1.28834, 1.26938, 1.23241, 1.20345, 1.17087, 1.1344, 1.11012, 1.07147
    };
    bSpectrum31f result;
    for (unsigned int i = 0; i < result.size(); ++i) {
        result[i] = kSpectralIrradiance[i];
    }
    return result;
}

bSpectrum31f NewRayleighScattering() {
    // Values from Table III in Penndorf 1957 "Tables of the Refractive Index for
    // Standard Air and the Rayleigh Scattering Coefficient for the Spectral
    // Region between 0.2 and 20.0 μ and Their Application to Atmospheric Optics".
    static const float kPenndorf[48] = {
        70.45E-6, 62.82E-6, 56.20E-6, 50.43E-6, 45.40E-6, 40.98E-6, 37.08E-6,
        33.65E-6, 30.60E-6, 27.89E-6, 25.48E-6, 23.33E-6, 21.40E-6, 19.66E-6,
        18.10E-6, 16.69E-6, 15.42E-6, 14.26E-6, 13.21E-6, 12.26E-6, 11.39E-6,
        10.60E-6, 9.876E-6, 9.212E-6, 8.604E-6, 8.045E-6, 7.531E-6, 7.057E-6,
        6.620E-6, 6.217E-6, 5.844E-6, 5.498E-6, 5.178E-6, 4.881E-6, 4.605E-6,
        4.348E-6, 4.109E-6, 3.886E-6, 3.678E-6, 3.484E-6, 3.302E-6, 3.132E-6,
        2.973E-6, 2.824E-6, 2.684E-6, 2.583E-6, 2.481E-6, 2.380E-6
    };
    std::vector<double> penndorf_samples;
    for (int i = 0; i < 48; ++i) {
        // The above values are for T_0=0°C. For T=15°C, a correction factor
        // T_0 / T must be applied (Eq. (12) in Penndorf paper).
        constexpr double T_0 = 273.16;
        constexpr double T = T_0 + 15.0;
        penndorf_samples.push_back(kPenndorf[i] * (T_0 / T));
    }
    return bSpectrum31f(400.0, 700.0, penndorf_samples);
}

bSpectrum31f NewMieExtinction(double angstrom_alpha, double angstrom_beta) {
    bSpectrum31f mie;
    for (unsigned int i = 0; i < mie.size(); ++i) {
        double lambda = mie.GetSample(i) * 1000;
        mie[i] = angstrom_beta * pow(lambda, -angstrom_alpha) / MieScaleHeight;
    }
    return mie;
}

bSpectrum31f NewMieScattering(double angstrom_alpha, double angstrom_beta) {
    const double kSingleScatteringAlbedo = 0.8;
    return NewMieExtinction(angstrom_alpha, angstrom_beta) * kSingleScatteringAlbedo;
}

const bSpectrum31f solar_spectrum = NewSolarSpectrum();
const bSpectrum31f rayleigh_scattering = NewRayleighScattering();
const bSpectrum31f mie_extinction = NewMieExtinction(MieAngstromAlpha, MieAngstromBeta);
const bSpectrum31f mie_scattering = NewMieScattering(MieAngstromAlpha, MieAngstromBeta);


// Returns the spectral irradiance of the Sun at the top of the atmosphere.
const bSpectrum31f& SolarSpectrum() { return solar_spectrum; }

// Returns the Rayleigh scattering coefficient at sea level.
const bSpectrum31f& RayleighScattering() { return rayleigh_scattering; }

// Returns the Mie extinction coefficient at sea level.
const bSpectrum31f& MieExtinction() { return mie_extinction; }

// Returns the Mie extinction and scattering coefficients at sea level for
// the given Angstrom parameters.
const bSpectrum31f& MieScattering() { return mie_scattering; }

// Returns the Mie scattering coefficient at sea level.

bSpectrum31f MieExtinction(double angstrom_alpha, double angstrom_beta) {
    return NewMieExtinction(angstrom_alpha, angstrom_beta);
}
bSpectrum31f MieScattering(double angstrom_alpha, double angstrom_beta) {
    return NewMieScattering(angstrom_alpha, angstrom_beta);
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
    const double kMie = 3.0 / (8.0 * Pi<float>()) * (1.0 - g * g) / (2.0 + g * g);
    return kMie * (1.0 + scattering_angle_cosine * scattering_angle_cosine) / pow(1.0 + g * g - 2.0 * g * scattering_angle_cosine, 1.5);
}
double MiePhaseFunction(double scattering_angle) {
    return MiePhaseFunctionCos(cos(scattering_angle));
}
double MiePhaseFunction(double g, double scattering_angle_cosine) {
    const double kMie = 3.0 / (8.0 * Pi<float>()) * (1.0 - g * g) / (2.0 + g * g);
    return kMie * (1.0 + scattering_angle_cosine * scattering_angle_cosine) / pow(1.0 + g * g - 2.0 * g * scattering_angle_cosine, 1.5);
}

