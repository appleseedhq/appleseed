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
#ifndef PHYSICS_UNITS_H_
#define PHYSICS_UNITS_H_

#include "b_scalar_function.h"

// Defines a unit system for radiometric and photometric quantities, using the
// following five base units: length, wavelength, solid angle, power and lumen
// (wavelength is also a length, but we distinguish the two for increased
// clarity). From this we derive the irradiance, radiance, spectral irradiance,
// spectral radiance, candela, etc.

// typedef dimensional::Scalar<1, 0, 0, 0, 0> Length;
// typedef dimensional::Scalar<0, 1, 0, 0, 0> Wavelength;
// typedef dimensional::Scalar<0, 0, 1, 0, 0> SolidAngle;
// typedef dimensional::Scalar<0, 0, 0, 1, 0> Power;
// typedef dimensional::Scalar<0, 0, 0, 0, 1> LuminousPower;

// typedef dimensional::Number Number;
// typedef dimensional::Scalar<2, 0, 0, 0, 0> Area;
// typedef dimensional::Scalar<3, 0, 0, 0, 0> Volume;
// typedef dimensional::Scalar<-2, 0, 0, 1, 0> Irradiance;
// typedef dimensional::Scalar<-2, 0, -1, 1, 0> Radiance;
// typedef dimensional::Scalar<0, -1, 0, 1, 0> SpectralPower;
// typedef dimensional::Scalar<-2, -1, 0, 1, 0> SpectralIrradiance;
// typedef dimensional::Scalar<-2, -1, -1, 1, 0> SpectralRadiance;
// typedef dimensional::Scalar<-1, 0, 0, 0, 0> ScatteringCoefficient;
// typedef dimensional::Scalar<0, 0, -1, 0, 0> InverseSolidAngle;
// typedef dimensional::Scalar<-3, 0, 0, 0, 0> NumberDensity;
// typedef dimensional::Scalar<0, 0, -1, 0, 1> LuminousIntensity;
// typedef dimensional::Scalar<-2, 0, -1, 0, 1> Luminance;

// A function from wavelength to values with some physical dimensions.
using bSpectrum31f = dimensional::ScalarFunction<31, 400, 700>;

// A function from wavelength to dimensionless values.
// typedef WavelengthFunction DimensionlessSpectrum;

// A function from wavelength to spectral power values.
// typedef WavelengthFunction PowerSpectrum;

// A function from wavelength to spectral irradiance values.
// typedef WavelengthFunction IrradianceSpectrum;

// A function from wavelength to spectral radiance values.
// typedef WavelengthFunction RadianceSpectrum;

// A function from wavelength to scattering coefficient values.
// typedef WavelengthFunction ScatteringSpectrum;

// A function from wavelength to phase function values.
// typedef WavelengthFunction PhaseFunctionSpectrum;

// Provides aliases for the five base units, with shorter names than
// Length::Unit(), Wavelength::Unit(), etc.

// constexpr Length m = Length::Unit();
// constexpr Wavelength nm = Wavelength::Unit();
// constexpr SolidAngle sr = SolidAngle::Unit();
// constexpr Power watt = Power::Unit();
// constexpr LuminousPower lm = LuminousPower::Unit();

// Provides aliases for some derived units.

// constexpr Length km = 1000.0 * m;
// constexpr Area m2 = Area::Unit();
// constexpr Volume m3 = Volume::Unit();
// constexpr Irradiance watt_per_square_meter = watt / m2;
// constexpr Radiance watt_per_square_meter_per_sr = watt / (m2 * sr);
// constexpr SpectralIrradiance watt_per_square_meter_per_nm = watt / (m2 * nm);
// constexpr SpectralRadiance watt_per_square_meter_per_sr_per_nm = watt / (m2 * sr * nm);
// constexpr LuminousIntensity cd = lm / sr;
// constexpr LuminousIntensity kcd = 1000.0 * cd;
// constexpr Luminance cd_per_square_meter = cd / m2;
// constexpr Luminance kcd_per_square_meter = kcd / m2;

#endif  // PHYSICS_UNITS_H_
