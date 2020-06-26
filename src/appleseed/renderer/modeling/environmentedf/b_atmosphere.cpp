/**
 Copyright (c) 2015 Eric Bruneton
 All rights reserved.
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:
 1. Redistributions of source code must retain the above copyright notice, this
 list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright notice,
 this list of conditions and the following disclaimer in the documentation
 and/or other materials provided with the distribution.
 3. Neither the name of the copyright holder nor the names of its contributors
 may be used to endorse or promote products derived from this software without
 specific prior written permission.
 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
#include "b_atmosphere.h"

#include <vector>

namespace {

    IrradianceSpectrum NewSolarSpectrum() {
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
        IrradianceSpectrum result;
        for (unsigned int i = 0; i < result.size(); ++i) {
            result[i] = kSpectralIrradiance[i] * watt_per_square_meter_per_nm;
        }
        return result;
    }

    ScatteringSpectrum NewRayleighScattering() {
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
        std::vector<ScatteringCoefficient> penndorf_samples;
        for (int i = 0; i < 48; ++i) {
            // The above values are for T_0=0°C. For T=15°C, a correction factor
            // T_0 / T must be applied (Eq. (12) in Penndorf paper).
            constexpr double T_0 = 273.16;
            constexpr double T = T_0 + 15.0;
            penndorf_samples.push_back(kPenndorf[i] * (T_0 / T) / m);
        }
        return ScatteringSpectrum(360.0 * nm, 830.0 * nm, penndorf_samples);
    }

    ScatteringSpectrum NewMieExtinction(double angstrom_alpha,
        double angstrom_beta) {
        ScatteringSpectrum mie;
        for (unsigned int i = 0; i < mie.size(); ++i) {
            double lambda = mie.GetSample(i).to(1000.0 * nm);
            mie[i] = angstrom_beta * pow(lambda, -angstrom_alpha) / MieScaleHeight;
        }
        return mie;
    }

    ScatteringSpectrum NewMieScattering(double angstrom_alpha,
        double angstrom_beta) {
        const double kSingleScatteringAlbedo = 0.8;
        return
            NewMieExtinction(angstrom_alpha, angstrom_beta) * kSingleScatteringAlbedo;
    }

    DimensionlessSpectrum NewGroundAlbedo() {
        // Grass spectral albedo from Uwe Feister and Rolf Grewe, "Spectral albedo
        // measurements in the UV and visible region over different types of
        // surfaces", Photochemistry and Photobiology, 62, 736-744, 1995.
        static const float kGrassAlbedo[45] = {
          0.018, 0.019, 0.019, 0.020, 0.022, 0.024, 0.027, 0.029, 0.030, 0.031, 0.032,
          0.032, 0.032, 0.033, 0.035, 0.040, 0.055, 0.073, 0.084, 0.089, 0.089, 0.079,
          0.069, 0.063, 0.061, 0.057, 0.052, 0.051, 0.048, 0.042, 0.039, 0.035, 0.035,
          0.043, 0.087, 0.156, 0.234, 0.334, 0.437, 0.513, 0.553, 0.571, 0.579, 0.581,
          0.587
        };
        std::vector<Number> grass_albedo_samples;
        for (int i = 0; i < 45; ++i) {
            grass_albedo_samples.push_back(kGrassAlbedo[i]);
        }
        return DimensionlessSpectrum(360.0 * nm, 800.0 * nm, grass_albedo_samples);
    }

    const IrradianceSpectrum solar_spectrum = NewSolarSpectrum();
    const ScatteringSpectrum rayleigh_scattering = NewRayleighScattering();
    const ScatteringSpectrum mie_extinction =
        NewMieExtinction(MieAngstromAlpha, MieAngstromBeta);
    const ScatteringSpectrum mie_scattering =
        NewMieScattering(MieAngstromAlpha, MieAngstromBeta);
    const DimensionlessSpectrum ground_albedo = NewGroundAlbedo();

}  // anonymous namespace

const IrradianceSpectrum& SolarSpectrum() { return solar_spectrum; }
const ScatteringSpectrum& RayleighScattering() { return rayleigh_scattering; }
const ScatteringSpectrum& MieExtinction() { return mie_extinction; }
const ScatteringSpectrum& MieScattering() { return mie_scattering; }

ScatteringSpectrum MieExtinction(double angstrom_alpha, double angstrom_beta) {
    return NewMieExtinction(angstrom_alpha, angstrom_beta);
}

ScatteringSpectrum MieScattering(double angstrom_alpha, double angstrom_beta) {
    return NewMieScattering(angstrom_alpha, angstrom_beta);
}

InverseSolidAngle RayleighPhaseFunction(Angle scattering_angle) {
    return RayleighPhaseFunction(cos(scattering_angle));
}

InverseSolidAngle RayleighPhaseFunction(Number scattering_angle_cosine) {
    constexpr InverseSolidAngle kRayleigh =
        3.0 / (16.0 * PI) * InverseSolidAngle::Unit();
    return kRayleigh * (1.0 + scattering_angle_cosine * scattering_angle_cosine);
}

InverseSolidAngle MiePhaseFunction(Angle scattering_angle) {
    return MiePhaseFunction(cos(scattering_angle));
}

InverseSolidAngle MiePhaseFunction(Number scattering_angle_cosine) {
    constexpr double g = MiePhaseFunctionG;
    constexpr InverseSolidAngle kMie = 3.0 / (8.0 * PI) *
        (1.0 - g * g) / (2.0 + g * g) * InverseSolidAngle::Unit();
    return kMie * (1.0 + scattering_angle_cosine * scattering_angle_cosine) /
        pow(1.0 + g * g - 2.0 * g * scattering_angle_cosine, 1.5);
}

InverseSolidAngle MiePhaseFunction(double g, Number scattering_angle_cosine) {
    const InverseSolidAngle kMie = 3.0 / (8.0 * PI) *
        (1.0 - g * g) / (2.0 + g * g) * InverseSolidAngle::Unit();
    return kMie * (1.0 + scattering_angle_cosine * scattering_angle_cosine) /
        pow(1.0 + g * g - 2.0 * g * scattering_angle_cosine, 1.5);
}

const DimensionlessSpectrum& GroundAlbedo() { return ground_albedo; }

int Atmosphere::GetOriginalNumberOfWavelengths() const {
    return DimensionlessSpectrum::SIZE;
}


Angle Atmosphere::GetViewSunAngle(Angle sun_zenith, Angle view_zenith,
    Angle view_sun_azimuth) {
    return acos(cos(view_sun_azimuth) * sin(view_zenith) * sin(sun_zenith) +
        cos(view_zenith) * cos(sun_zenith));
}
