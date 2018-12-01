
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

// appleseed.foundation headers.
#include "foundation/image/regularspectrum.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

namespace renderer
{

//
// Wavelengths used throughout the spectral light simulation.
//

const float LowWavelength = 400.0f;                             // low wavelength, in nm
const float HighWavelength = 700.0f;                            // high wavelength, in nm

extern foundation::RegularSpectrum31f g_light_wavelengths_nm;   // wavelengths, in nm
extern foundation::RegularSpectrum31f g_light_wavelengths_um;   // wavelengths, in um


//
// Utility functions.
//

// Generate a set of regularly spaced wavelengths.
APPLESEED_DLLSYMBOL void generate_wavelengths(
    const float                         low_wavelength,
    const float                         high_wavelength,
    const size_t                        count,
    float                               wavelengths[]);

// Convert a set of regularly spaced spectral values to the internal spectrum format.
APPLESEED_DLLSYMBOL void spectral_values_to_spectrum(
    const float                         low_wavelength,
    const float                         high_wavelength,
    const size_t                        input_spectrum_count,
    const float                         input_spectrum[],
    foundation::RegularSpectrum31f&     output_spectrum);

}   // namespace renderer
