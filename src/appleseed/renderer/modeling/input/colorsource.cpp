
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/entity/entity.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/utility/containers/specializedarrays.h"

// Standard headers.
#include <cassert>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// ColorSource class implementation.
//

ColorSource::ColorSource(
    const ColorEntity&      color_entity,
    const InputFormat       input_format)
  : Source(true)
  , m_color_entity(color_entity)
{
    // Retrieve the color values.
    if (color_entity.get_color_space() == ColorSpaceSpectral)
        initialize_from_spectrum(color_entity);
    else initialize_from_3d_color(color_entity, input_format);

    // Apply the multiplier to the color values.
    const float multiplier = color_entity.get_multiplier();
    m_scalar *= multiplier;
    m_linear_rgb *= multiplier;
    m_spectrum *= multiplier;

    // Store the alpha values.
    const ColorValueArray& alpha = color_entity.get_alpha();
    m_alpha[0] = alpha.size() == 1 ? alpha[0] : 0.0f;
}

uint64 ColorSource::compute_signature() const
{
    return m_color_entity.compute_signature();
}

void ColorSource::initialize_from_spectrum(
    const ColorEntity&      color_entity)
{
    const ColorValueArray& values = color_entity.get_values();

    if (values.size() > 0)
    {
        // todo: this should be user-settable.
        const LightingConditions lighting_conditions(
            IlluminantCIED65,
            XYZCMFCIE196410Deg);

        m_scalar = static_cast<double>(values[0]);

        spectral_values_to_spectrum(
            color_entity.get_wavelength_range()[0],
            color_entity.get_wavelength_range()[1],
            values.size(),
            &values[0],
            &m_spectrum[0]);

        m_linear_rgb =
            ciexyz_to_linear_rgb(
                spectrum_to_ciexyz<float>(lighting_conditions, m_spectrum));
    }
    else
    {
        m_scalar = 0.0;
        m_linear_rgb.set(0.0f);
        m_spectrum.set(0.0f);
    }
}

namespace
{
    void linear_rgb_to_spectrum(
        const InputFormat   input_format,
        const Color3f&      linear_rgb,
        Spectrum&           spectrum)
    {
        if (input_format == InputFormatSpectralReflectance)
        {
            linear_rgb_reflectance_to_spectrum(
                linear_rgb,
                spectrum);
        }
        else
        {
            assert(input_format == InputFormatSpectralIlluminance);
            linear_rgb_illuminance_to_spectrum(
                linear_rgb,
                spectrum);
        }
    }
}

void ColorSource::initialize_from_3d_color(
    const ColorEntity&      color_entity,
    const InputFormat       input_format)
{
    const ColorValueArray& values = color_entity.get_values();

    if (values.size() == 1)
        m_linear_rgb.set(values[0]);
    else if (values.size() == 3)
        m_linear_rgb = Color3f(values[0], values[1], values[2]);
    else m_linear_rgb.set(0.0f);

    m_scalar = static_cast<double>(m_linear_rgb[0]);

    if (input_format == InputFormatSpectralIlluminance || input_format == InputFormatSpectralReflectance)
    {
        switch (color_entity.get_color_space())
        {
          case ColorSpaceLinearRGB:
            linear_rgb_to_spectrum(
                input_format,
                m_linear_rgb,
                m_spectrum);
            break;

          case ColorSpaceSRGB:
            linear_rgb_to_spectrum(
                input_format,
                srgb_to_linear_rgb(m_linear_rgb),
                m_spectrum);
            break;

          case ColorSpaceCIEXYZ:
            linear_rgb_to_spectrum(
                input_format,
                ciexyz_to_linear_rgb(m_linear_rgb),
                m_spectrum);
            break;

          default:
            assert(!"Invalid color space.");
            break;
        }
    }
    else m_spectrum.set(0.0f);
}

}   // namespace renderer
