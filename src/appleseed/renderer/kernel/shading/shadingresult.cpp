
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
#include "shadingresult.h"

// appleseed.foundation headers.
#include "foundation/utility/otherwise.h"

// Standard headers.
#include <cassert>
#include <cstddef>

using namespace foundation;

namespace renderer
{

//
// ShadingResult class implementation.
//

namespace
{
    inline const Color3f& spectrum_as_color3f(const Spectrum& s)
    {
        return reinterpret_cast<const Color3f&>(s[0]);
    }

    template <typename T>
    inline bool is_valid_scalar(const T x);

    template <>
    inline bool is_valid_scalar(const float x)
    {
        const uint32 ix = binary_cast<uint32>(x);
        const uint32 sign = (ix & 0x80000000L) >> 31;
        const uint32 exponent = (ix >> 23) & 255;
        const uint32 mantissa = ix & 0x007FFFFFL;
        const bool is_neg = sign == 1 && ix != 0x80000000L;
        const bool is_nan = exponent == 255 && mantissa != 0;
        const bool is_inf = (ix & 0x7FFFFFFFL) == 0x7F800000UL;
        return !is_neg && !is_nan && !is_inf;
    }

    template <typename T, size_t N>
    inline bool is_valid_color(const Color<T, N>& c)
    {
        for (size_t i = 0; i < N; ++i)
        {
            if (!is_valid_scalar(c[i]))
                return false;
        }

        return true;
    }
}

bool ShadingResult::is_valid_linear_rgb() const
{
    assert(m_color_space == ColorSpaceLinearRGB);

    if (!is_valid_color(spectrum_as_color3f(m_main.m_color)))
        return false;

    if (!is_valid_color(m_main.m_alpha))
        return false;

    for (size_t i = 0, e = m_aovs.size(); i < e; ++i)
    {
        const ShadingFragment& aov = m_aovs[i];

        if (!is_valid_color(spectrum_as_color3f(aov.m_color)))
            return false;

        if (!is_valid_color(aov.m_alpha))
            return false;
    }

    return true;
}

void ShadingResult::set_aovs_to_transparent_black_linear_rgba()
{
    for (size_t i = 0, e = m_aovs.size(); i < e; ++i)
    {
        ShadingFragment& aov = m_aovs[i];
        aov.m_color[0] = 0.0f;
        aov.m_color[1] = 0.0f;
        aov.m_color[2] = 0.0f;
        aov.m_alpha.set(0.0f);
    }
}

namespace
{
    inline void transform_spectrum_to_linear_rgb(const LightingConditions& lighting, Spectrum& s)
    {
        s = ciexyz_to_linear_rgb(spectrum_to_ciexyz<float>(lighting, s));
    }

    inline void transform_spectrum_to_linear_rgb(const LightingConditions& lighting, const Matrix3f& xyz_to_rgb_matrix, Spectrum& s)
    {
        s = ciexyz_to_linear_rgb(spectrum_to_ciexyz<float>(lighting, s), xyz_to_rgb_matrix);
    }
}

void ShadingResult::transform_to_linear_rgb(const LightingConditions& lighting)
{
    if (m_color_space == ColorSpaceLinearRGB)
        return;

    assert(m_color_space == ColorSpaceSpectral);

    if (m_main.m_color.is_spectral())
        transform_spectrum_to_linear_rgb(lighting, m_main.m_color);
    for (size_t i = 0, e = m_aovs.size(); i < e; ++i)
    {
        ShadingFragment& aov = m_aovs[i];
        if (aov.m_color.is_spectral())
            transform_spectrum_to_linear_rgb(lighting, aov.m_color);
    }

    m_color_space = ColorSpaceLinearRGB;
}

void ShadingResult::transform_to_linear_rgb(
    const LightingConditions&   lighting,
    const Matrix3f&             xyz_to_rgb_matrix)
{
    if (m_color_space == ColorSpaceLinearRGB)
        return;

    assert(m_color_space == ColorSpaceSpectral);

    if (m_main.m_color.is_spectral())
        transform_spectrum_to_linear_rgb(lighting, xyz_to_rgb_matrix, m_main.m_color);
    for (size_t i = 0, e = m_aovs.size(); i < e; ++i)
    {
        ShadingFragment& aov = m_aovs[i];
        if (aov.m_color.is_spectral())
            transform_spectrum_to_linear_rgb(lighting, xyz_to_rgb_matrix, aov.m_color);
    }

    m_color_space = ColorSpaceLinearRGB;
}

void ShadingResult::composite_over_linear_rgb(const ShadingResult& background)
{
    //
    // Shading results use premultiplied alpha.
    //
    // References and interesting resources on alpha compositing:
    //
    //   http://keithp.com/~keithp/porterduff/p253-porter.pdf
    //   http://en.wikipedia.org/wiki/Alpha_compositing
    //   http://dvd-hq.info/alpha_matting.php
    //   http://my.opera.com/emoller/blog/2012/08/28/alpha-blending
    //

    assert(m_color_space == ColorSpaceLinearRGB);
    assert(background.m_color_space == ColorSpaceLinearRGB);

    const Alpha contrib = Alpha(1.0f) - m_main.m_alpha;
    m_main.m_color[0] += contrib[0] * background.m_main.m_color[0];
    m_main.m_color[1] += contrib[0] * background.m_main.m_color[1];
    m_main.m_color[2] += contrib[0] * background.m_main.m_color[2];
    m_main.m_alpha += contrib * background.m_main.m_alpha;

    for (size_t i = 0, e = m_aovs.size(); i < e; ++i)
    {
        const ShadingFragment& background_aov = background.m_aovs[i];
        ShadingFragment& aov = m_aovs[i];

        const Alpha contrib = Alpha(1.0f) - aov.m_alpha;
        aov.m_color[0] += contrib[0] * background_aov.m_color[0];
        aov.m_color[1] += contrib[0] * background_aov.m_color[1];
        aov.m_color[2] += contrib[0] * background_aov.m_color[2];
        aov.m_alpha += contrib * background_aov.m_alpha;
    }
}

void ShadingResult::apply_alpha_premult_linear_rgb()
{
    assert(m_color_space == ColorSpaceLinearRGB);

    m_main.m_color[0] *= m_main.m_alpha[0];
    m_main.m_color[1] *= m_main.m_alpha[0];
    m_main.m_color[2] *= m_main.m_alpha[0];

    for (size_t i = 0, e = m_aovs.size(); i < e; ++i)
    {
        ShadingFragment& aov = m_aovs[i];
        aov.m_color[0] *= aov.m_alpha[0];
        aov.m_color[1] *= aov.m_alpha[0];
        aov.m_color[2] *= aov.m_alpha[0];
    }
}

}   // namespace renderer
