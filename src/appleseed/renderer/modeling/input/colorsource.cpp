
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

// Interface header.
#include "colorsource.h"

// appleseed.renderer headers.
#include "renderer/modeling/color/colorentity.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/color/wavelengths.h"
#include "renderer/modeling/entity/entity.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/image/regularspectrum.h"
#include "foundation/utility/api/specializedapiarrays.h"

// Standard headers.
#include <cassert>

using namespace foundation;

namespace renderer
{

//
// ColorSource class implementation.
//

ColorSource::ColorSource(const ColorEntity& color_entity)
  : Source(true)
  , m_color_entity(color_entity)
{
    // Retrieve the color values.
    if (color_entity.get_color_space() == ColorSpaceSpectral)
        initialize_from_spectrum(color_entity);
    else initialize_from_color3(color_entity);

    // Apply the multiplier to the color values.
    const float multiplier = color_entity.get_multiplier();
    m_scalar *= multiplier;
    m_linear_rgb *= multiplier;
    m_spectrum *= multiplier;

    // Store the alpha values.
    // In the case of multiple alpha values, we keep the first one.
    const ColorValueArray& alpha = color_entity.get_alpha();
    m_alpha[0] = alpha.size() >= 1 ? alpha[0] : 0.0f;
}

std::uint64_t ColorSource::compute_signature() const
{
    return m_color_entity.compute_signature();
}

ColorSource::Hints ColorSource::get_hints() const
{
    Hints hints;
    hints.m_width = 1;
    hints.m_height = 1;
    return hints;
}

void ColorSource::initialize_from_spectrum(const ColorEntity& color_entity)
{
    const ColorValueArray& values = color_entity.get_values();

    if (values.empty())
    {
        m_scalar = 0.0f;
        m_linear_rgb.set(0.0f);
        m_spectrum.set(0.0f);
        return;
    }

    m_scalar = values[0];

    RegularSpectrum31f s;
    spectral_values_to_spectrum(
        color_entity.get_wavelength_range()[0],
        color_entity.get_wavelength_range()[1],
        values.size(),
        &values[0],
        s);

    m_linear_rgb =
        ciexyz_to_linear_rgb(
            spectrum_to_ciexyz<float>(g_std_lighting_conditions, s));

    m_spectrum.set(s, g_std_lighting_conditions, Spectrum::Reflectance);
}

void ColorSource::initialize_from_color3(const ColorEntity& color_entity)
{
    Color3f color;

    const ColorValueArray& values = color_entity.get_values();
    if (values.size() == 1)
        color.set(values[0]);
    else if (values.size() == 3)
        color = Color3f(values[0], values[1], values[2]);
    else
    {
        m_scalar = 0.0f;
        m_linear_rgb.set(0.0f);
        m_spectrum.set(0.0f);
        return;
    }

    m_scalar = color[0];

    switch (color_entity.get_color_space())
    {
      case ColorSpaceLinearRGB:
        m_linear_rgb = color;
        break;

      case ColorSpaceSRGB:
        m_linear_rgb = fast_srgb_to_linear_rgb(color);
        break;

      case ColorSpaceCIEXYZ:
        m_linear_rgb = ciexyz_to_linear_rgb(color);
        break;

      default:
        assert(!"Invalid color space.");
        break;
    }

    m_spectrum.set(m_linear_rgb, g_std_lighting_conditions, Spectrum::Reflectance);
}

}   // namespace renderer
