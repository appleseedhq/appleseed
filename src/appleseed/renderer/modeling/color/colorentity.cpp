
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
#include "colorentity.h"

// appleseed.renderer headers.
#include "renderer/modeling/color/wavelengths.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// ColorValueArray class implementation.
//

DEFINE_ARRAY(ColorValueArray);


//
// ColorEntity class implementation.
//

namespace
{
    // Return the name of a color space.
    const char* color_space_name(const ColorSpace color_space)
    {
        switch (color_space)
        {
          case ColorSpaceLinearRGB:     return "linear_rgb";
          case ColorSpaceSRGB:          return "srgb";
          case ColorSpaceCIEXYZ:        return "ciexyz";
          case ColorSpaceSpectral:      return "spectral";
          default:                      return "";
        }
    }
}

struct ColorEntity::Impl
{
    string                  m_name;
    ColorValueArray         m_values;
    ColorSpace              m_color_space;
    Vector2f                m_wavelength_range;
    float                   m_multiplier;
};

// Constructor.
ColorEntity::ColorEntity(
    const char*             name,
    const ParamArray&       params,
    const ColorValueArray&  values)
  : Entity(params)
  , impl(new Impl())
{
    assert(name);

    impl->m_name = name;
    impl->m_values = values;

    // Retrieve the color space.
    const string color_space = m_params.get_required<string>("color_space", "linear_rgb");
    if (color_space == "linear_rgb")
        impl->m_color_space = ColorSpaceLinearRGB;
    else if (color_space == "srgb")
        impl->m_color_space = ColorSpaceSRGB;
    else if (color_space == "ciexyz")
        impl->m_color_space = ColorSpaceCIEXYZ;
    else if (color_space == "spectral")
        impl->m_color_space = ColorSpaceSpectral;
    else
    {
        RENDERER_LOG_ERROR(
            "invalid value \"%s\" for parameter \"color_space\", "
            "using default value \"linear_rgb\"",
            color_space.c_str());
        impl->m_color_space = ColorSpaceLinearRGB;
    }

    // For the spectral color space, retrieve the wavelength range.
    if (impl->m_color_space == ColorSpaceSpectral)
    {
        const Vector2f DefaultWavelengthRange(LowWavelength, HighWavelength);
        impl->m_wavelength_range =
            m_params.get_required<Vector2f>(
                "wavelength_range",
                DefaultWavelengthRange);

        if (impl->m_wavelength_range[0] < 0.0 ||
            impl->m_wavelength_range[1] < 0.0 ||
            impl->m_wavelength_range[0] > impl->m_wavelength_range[1])
        {
            RENDERER_LOG_ERROR(
                "invalid value \"%f %f\" for parameter \"%s\", "
                "using default value \"%f %f\"",
                impl->m_wavelength_range[0],
                impl->m_wavelength_range[1],
                "wavelength_range",
                DefaultWavelengthRange[0],
                DefaultWavelengthRange[1]);

            impl->m_wavelength_range = DefaultWavelengthRange;
        }
    }
    else
    {
        impl->m_wavelength_range[0] =
        impl->m_wavelength_range[1] = 0.0f;
    }

    // Retrieve multiplier.
    impl->m_multiplier = m_params.get_optional<float>("multiplier", 1.0f);

    // Check the number of color values.
    if (impl->m_color_space == ColorSpaceSpectral)
    {
        if (values.empty())
        {
            RENDERER_LOG_ERROR("1 or more values required for \"spectral\" color space, got 0");
        }
    }
    else
    {
        if (values.size() != 1 && values.size() != 3)
        {
            RENDERER_LOG_ERROR(
                "1 or 3 values required for \"%s\" color space, got " FMT_SIZE_T,
                color_space_name(impl->m_color_space),
                values.size());
        }
    }
}

// Destructor.
ColorEntity::~ColorEntity()
{
    delete impl;
}

// Delete this instance.
void ColorEntity::release()
{
    delete this;
}

// Return the name of this color.
const char* ColorEntity::get_name() const
{
    return impl->m_name.c_str();
}

// Return the color values.
const ColorValueArray& ColorEntity::get_values() const
{
    return impl->m_values;
}

// Return the color space of this color.
ColorSpace ColorEntity::get_color_space() const
{
    return impl->m_color_space;
}

// Return the range of wavelength of this color (spectral color space only).
const Vector2f& ColorEntity::get_wavelength_range() const
{
    assert(impl->m_color_space == ColorSpaceSpectral);
    return impl->m_wavelength_range;
}

// Retrieve the multiplier value.
float ColorEntity::get_multiplier() const
{
    return impl->m_multiplier;
}


//
// ColorEntityFactory class implementation.
//

// Create a new color entity.
auto_release_ptr<ColorEntity> ColorEntityFactory::create(
    const char*             name,
    const ParamArray&       params,
    const ColorValueArray&  values)
{
    return
        auto_release_ptr<ColorEntity>(
            new ColorEntity(name, params, values));
}

}   // namespace renderer
