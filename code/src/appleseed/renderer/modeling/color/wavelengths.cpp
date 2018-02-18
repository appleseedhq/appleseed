
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

// Interface header.
#include "wavelengths.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/math/scalar.h"

// Standard headers.
#include <cassert>
#include <vector>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// Range of wavelengths used throughout the light simulation.
//

RegularSpectrum31f g_light_wavelengths_nm;
RegularSpectrum31f g_light_wavelengths_um;

namespace
{
    struct InitializeLightWavelengths
    {
        InitializeLightWavelengths()
        {
            generate_wavelengths(
                LowWavelength,
                HighWavelength,
                g_light_wavelengths_nm.Samples,
                &g_light_wavelengths_nm[0]);

            g_light_wavelengths_um = g_light_wavelengths_nm / 1000.0f;
        }
    };

    InitializeLightWavelengths initialize_light_wavelengths;
}


//
// Utility functions implementation.
//

void generate_wavelengths(
    const float             low_wavelength,
    const float             high_wavelength,
    const size_t            count,
    float                   wavelengths[])
{
    if (count == 1)
        wavelengths[0] = 0.5f * (low_wavelength + high_wavelength);
    else
    {
        for (size_t i = 0; i < count; ++i)
        {
            wavelengths[i] =
                fit(
                    static_cast<float>(i),
                    0.0f,
                    static_cast<float>(count - 1),
                    low_wavelength,
                    high_wavelength);
        }
    }
}

void spectral_values_to_spectrum(
    const float             low_wavelength,
    const float             high_wavelength,
    const size_t            input_spectrum_count,
    const float             input_spectrum[],
    RegularSpectrum31f&     output_spectrum)
{
    assert(low_wavelength < high_wavelength);

    // Generate the wavelengths for which this spectrum is defined.
    vector<float> wavelengths(input_spectrum_count);
    generate_wavelengths(
        low_wavelength,
        high_wavelength,
        input_spectrum_count,
        &wavelengths[0]);

    // Resample the spectrum to the internal wavelength range.
    spectrum_to_spectrum(
        input_spectrum_count,
        &wavelengths[0],
        input_spectrum,
        output_spectrum.Samples,
        &g_light_wavelengths_nm[0],
        &output_spectrum[0]);
}

}   // namespace renderer
