
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

// appleseed.renderer headers.
#include "renderer/modeling/environmentedf/ienvironmentedffactory.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/compiler.h"

// appleseed.main headers.
#include "main/dllsymbol.h"


// BEGIN-COPY-PASTA -->
#include "b_binary_function.h"
#include "b_units.h"
// <-- END-COPY-PASTA

// Forward declarations.
namespace foundation { class Dictionary; }
namespace foundation { class DictionaryArray; }
namespace renderer { class EnvironmentEDF; }
namespace renderer { class ParamArray; }

constexpr Length EarthRadius = 6360.0 * km;
constexpr Length AtmosphereRadius = 6420.0 * km;

// The height of the atmosphere if its density was uniform.
constexpr Length RayleighScaleHeight = 8000.0 * m;

// The height of the Mie particle layer if its density was uniform.
constexpr Length MieScaleHeight = 1200.0 * m;

// The Angstrom alpha coefficient for the Mie optical depth.
constexpr double MieAngstromAlpha = 0.8;

// The Angstrom beta coefficient for the Mie optical depth.
constexpr double MieAngstromBeta = 0.04;

// The g parameter of the Cornette-Shanks phase function used for Mie particles.
constexpr double MiePhaseFunctionG = 0.7;

// The Linke turbidity value, for the Preetham and Hosek models.
constexpr double Turbidity = 2.53;

// Returns the spectral irradiance of the Sun at the top of the atmosphere.
const IrradianceSpectrum& SolarSpectrum();

// Returns the Rayleigh scattering coefficient at sea level.
const ScatteringSpectrum& RayleighScattering();

// Returns the Mie extinction coefficient at sea level.
const ScatteringSpectrum& MieExtinction();

// Returns the Mie scattering coefficient at sea level.
const ScatteringSpectrum& MieScattering();

// Returns the Mie extinction and scattering coefficients at sea level for
// the given Angstrom parameters.
ScatteringSpectrum MieExtinction(double angstrom_alpha, double angstrom_beta);
ScatteringSpectrum MieScattering(double angstrom_alpha, double angstrom_beta);

// Computes the Rayleigh phase function for the given scattering angle.
// The integral of this function over all solid angles is 1.
InverseSolidAngle RayleighPhaseFunction(double scattering_angle);
InverseSolidAngle RayleighPhaseFunction(Number scattering_angle_cosine);

// Computes the Mie phase function for the given scattering angle (using
// the Cornette-Shanks approximation).
// The integral of this function over all solid angles is 1.
InverseSolidAngle MiePhaseFunction(double scattering_angle);
InverseSolidAngle MiePhaseFunction(Number scattering_angle_cosine);
InverseSolidAngle MiePhaseFunction(double g, Number scattering_angle_cosine);

const DimensionlessSpectrum& GroundAlbedo();


namespace renderer
{

    //
    // Nishita93 environment EDF factory.
    //

    class APPLESEED_DLLSYMBOL Nishita93EnvironmentEDFFactory
        : public IEnvironmentEDFFactory
    {
    public:
        // Delete this instance.
        void release() override;

        // Return a string identifying this environment EDF model.
        const char* get_model() const override;

        // Return metadata for this environment EDF model.
        foundation::Dictionary get_model_metadata() const override;

        // Return metadata for the inputs of this environment EDF model.
        foundation::DictionaryArray get_input_metadata() const override;

        // Create a new environment EDF instance.
        foundation::auto_release_ptr<EnvironmentEDF> create(
            const char*         name,
            const ParamArray&   params) const override;

    };

};   // namespace renderer
