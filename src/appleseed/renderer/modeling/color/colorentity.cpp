
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
#include "colorentity.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/color/wavelengths.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/platform/types.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/iostreamop.h"

// Standard headers.
#include <cassert>
#include <string>

using namespace foundation;

namespace renderer
{

//
// ColorEntity class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

UniqueID ColorEntity::get_class_uid()
{
    return g_class_uid;
}

struct ColorEntity::Impl
{
    ColorValueArray         m_values;
    ColorValueArray         m_alpha;
    ColorSpace              m_color_space;
    Vector2f                m_wavelength_range;
    float                   m_multiplier;
};

ColorEntity::ColorEntity(
    const char*             name,
    const ParamArray&       params)
  : Entity(g_class_uid, params)
  , impl(new Impl())
{
    set_name(name);

    extract_parameters();
    extract_values();
    remove_color_alpha_parameters();

    check_validity();
}

ColorEntity::ColorEntity(
    const char*             name,
    const ParamArray&       params,
    const ColorValueArray&  values)
  : Entity(g_class_uid, params)
  , impl(new Impl())
{
    set_name(name);

    extract_parameters();
    remove_color_alpha_parameters();

    impl->m_values = values;
    impl->m_alpha.push_back(1.0f);

    check_validity();
}

ColorEntity::ColorEntity(
    const char*             name,
    const ParamArray&       params,
    const ColorValueArray&  values,
    const ColorValueArray&  alpha)
  : Entity(g_class_uid, params)
  , impl(new Impl())
{
    set_name(name);

    extract_parameters();
    remove_color_alpha_parameters();

    impl->m_values = values;
    impl->m_alpha = alpha;

    check_validity();
}

ColorEntity::~ColorEntity()
{
    delete impl;
}

void ColorEntity::extract_parameters()
{
    // Retrieve the color space.
    const ColorSpace DefaultColorSpace = ColorSpaceSRGB;
    const char* DefaultColorSpaceName = color_space_name(DefaultColorSpace);
    const std::string color_space = m_params.get_required<std::string>("color_space", DefaultColorSpaceName);
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
            "using default value \"%s\".",
            color_space.c_str(),
            DefaultColorSpaceName);
        impl->m_color_space = DefaultColorSpace;
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
            impl->m_wavelength_range[0] >= impl->m_wavelength_range[1])
        {
            RENDERER_LOG_ERROR(
                "invalid value \"%f %f\" for parameter \"%s\", "
                "using default value \"%f %f\".",
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
}

void ColorEntity::extract_values()
{
    ColorValueArray black;
    black.push_back(0.0f);

    ColorValueArray opaque;
    opaque.push_back(1.0f);

    impl->m_values = m_params.get_required("color", black);
    impl->m_alpha = m_params.get_optional("alpha", opaque);
}

void ColorEntity::remove_color_alpha_parameters()
{
    m_params.strings().remove("color");
    m_params.strings().remove("alpha");
}

void ColorEntity::check_validity()
{
    // Check the number of color values.
    if (impl->m_color_space == ColorSpaceSpectral)
    {
        if (impl->m_values.empty())
        {
            RENDERER_LOG_ERROR("1 or more values required for \"spectral\" color space, got 0.");
        }
    }
    else
    {
        if (impl->m_values.size() != 1 && impl->m_values.size() != 3)
        {
            RENDERER_LOG_ERROR(
                "1 or 3 values required for \"%s\" color space, got " FMT_SIZE_T ".",
                color_space_name(impl->m_color_space),
                impl->m_values.size());
        }
    }
}

void ColorEntity::release()
{
    delete this;
}

const ColorValueArray& ColorEntity::get_values() const
{
    return impl->m_values;
}

const ColorValueArray& ColorEntity::get_alpha() const
{
    return impl->m_alpha;
}

ColorSpace ColorEntity::get_color_space() const
{
    return impl->m_color_space;
}

const Vector2f& ColorEntity::get_wavelength_range() const
{
    assert(impl->m_color_space == ColorSpaceSpectral);
    return impl->m_wavelength_range;
}

float ColorEntity::get_multiplier() const
{
    return impl->m_multiplier;
}


//
// ColorEntityFactory class implementation.
//

DictionaryArray ColorEntityFactory::get_input_metadata()
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "color_space")
            .insert("label", "Color Space")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("Linear RGB", "linear_rgb")
                    .insert("sRGB", "srgb")
                    .insert("CIE XYZ", "ciexyz")
                    .insert("Spectral", "spectral"))
            .insert("default", "srgb")
            .insert("use", "required")
            .insert("on_change", "rebuild_form"));

    metadata.push_back(
        Dictionary()
            .insert("name", "wavelength_range")
            .insert("label", "Wavelength Range")
            .insert("type", "text")
            .insert("default", "400.0 700.0")
            .insert("use", "optional")
            .insert("visible_if",
                Dictionary()
                    .insert("color_space", "spectral")));

    metadata.push_back(
        Dictionary()
            .insert("name", "color")
            .insert("label", "Color")
            .insert("type", "color")
            .insert("wavelength_range_widget", "wavelength_range")
            .insert("default", "0.0 0.0 0.0")
            .insert("use", "required"));

    metadata.push_back(
        Dictionary()
            .insert("name", "alpha")
            .insert("label", "Alpha")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "1.0")
                    .insert("type", "hard"))
            .insert("default", "1.0")
            .insert("use", "optional"));

    metadata.push_back(
        Dictionary()
            .insert("name", "multiplier")
            .insert("label", "Multiplier")
            .insert("type", "numeric")
            .insert("min",
                Dictionary()
                    .insert("value", "0.0")
                    .insert("type", "hard"))
            .insert("max",
                Dictionary()
                    .insert("value", "10.0")
                    .insert("type", "soft"))
            .insert("default", "1.0")
            .insert("use", "optional"));

    return metadata;
}

auto_release_ptr<ColorEntity> ColorEntityFactory::create(
    const char*             name,
    const ParamArray&       params)
{
    return
        auto_release_ptr<ColorEntity>(
            new ColorEntity(name, params));
}

auto_release_ptr<ColorEntity> ColorEntityFactory::create(
    const char*             name,
    const ParamArray&       params,
    const ColorValueArray&  values)
{
    return
        auto_release_ptr<ColorEntity>(
            new ColorEntity(name, params, values));
}

auto_release_ptr<ColorEntity> ColorEntityFactory::create(
    const char*             name,
    const ParamArray&       params,
    const ColorValueArray&  values,
    const ColorValueArray&  alpha)
{
    return
        auto_release_ptr<ColorEntity>(
            new ColorEntity(name, params, values, alpha));
}

}   // namespace renderer
