
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "colorsource.h"

// appleseed.renderer headers.
#include "renderer/modeling/color/colorentity.h"
#include "renderer/modeling/color/wavelengths.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"

// Standard headers.
#include <vector>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// ColorSource class implementation.
//

namespace
{

    // Generate a set of regularly spaced wavelengths.
    void generate_wavelengths(
        const Vector2f&         range,
        const size_t            count,
        vector<float>&          wavelengths)
    {
        if (count == 1)
            wavelengths.push_back(0.5f * (range[0] + range[1]));
        else
        {
            for (size_t i = 0; i < count; ++i)
            {
                wavelengths.push_back(
                    fit(
                        static_cast<float>(i),
                        0.0f,
                        static_cast<float>(count - 1),
                        range[0],
                        range[1]));
            }
        }
    }

    // Convert a set of regularly spaced spectral values to the internal spectrum format.
    Spectrum spectral_values_to_spectrum(
        const Vector2f&         wavelength_range,
        const ColorValueArray&  values)
    {
        assert(wavelength_range[0] <= wavelength_range[1]);
        assert(!values.empty());

        // Generate the wavelengths for which this spectrum is defined.
        vector<float> wavelengths;
        generate_wavelengths(
            wavelength_range,
            values.size(),
            wavelengths);

        // Resample the spectrum to the internal wavelength range.
        Spectrum spectrum;
        spectrum_to_spectrum(
            values.size(),
            &wavelengths[0],
            &values[0],
            Spectrum::Samples,
            g_light_wavelengths,
            &spectrum[0]);

        return spectrum;
    }

}   // anonymous namespace

// Constructor.
ColorSource::ColorSource(const ColorEntity& color_entity)
  : Source(true)
  , m_lighting_conditions(          // todo: this should be user-settable
        IlluminantCIED65,
        XYZCMFCIE196410Deg)
{
    const ColorSpace color_space = color_entity.get_color_space();
    const ColorValueArray& values = color_entity.get_values();
    const size_t value_count = values.size();

    if (color_space == ColorSpaceSpectral)
    {
        if (value_count > 0)
        {
            m_scalar = static_cast<double>(values[0]);
            m_spectrum =
                spectral_values_to_spectrum(
                    color_entity.get_wavelength_range(),
                    values);
        }
        else
        {
            m_scalar = 0.0;
            m_spectrum.set(0.0f);
        }
    }
    else
    {
        Color3f color;

        if (value_count == 1)
            color = Color3f(values[0]);
        else if (value_count == 3)
            color = Color3f(values[0], values[1], values[2]);
        else color = Color3f(0.0f);

        m_scalar = static_cast<double>(color[0]);

        switch (color_space)
        {
          case ColorSpaceLinearRGB:
            linear_rgb_to_spectrum(
                m_lighting_conditions,
                color,
                m_spectrum);
            break;

          case ColorSpaceSRGB:
            linear_rgb_to_spectrum(
                m_lighting_conditions,
                srgb_to_linear_rgb(color),
                m_spectrum);
            break;

          case ColorSpaceCIEXYZ:
            linear_rgb_to_spectrum(
                m_lighting_conditions,
                ciexyz_to_linear_rgb(color),
                m_spectrum);
            break;

          default:
            assert(!"Invalid color space.");
            break;
        }
    }

    // Apply multiplier.
    const float multiplier = color_entity.get_multiplier();
    m_scalar *= multiplier;
    m_spectrum *= multiplier;
}

}   // namespace renderer
