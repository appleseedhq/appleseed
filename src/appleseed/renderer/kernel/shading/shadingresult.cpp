
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
    inline void transform_srgb_to_linear_rgb(Spectrum& s)
    {
        Color3f& linear_rgb = *reinterpret_cast<Color3f*>(&s[0]);
        linear_rgb = srgb_to_linear_rgb(Color3f(s[0], s[1], s[2]));
    }

    inline void transform_ciexyz_to_linear_rgb(Spectrum& s)
    {
        Color3f& linear_rgb = *reinterpret_cast<Color3f*>(&s[0]);
        linear_rgb = ciexyz_to_linear_rgb(Color3f(s[0], s[1], s[2]));
    }

    inline void transform_spectrum_to_linear_rgb(const LightingConditions& lighting, Spectrum& s)
    {
        Color3f& linear_rgb = *reinterpret_cast<Color3f*>(&s[0]);
        linear_rgb = ciexyz_to_linear_rgb(spectrum_to_ciexyz<float>(lighting, s));
    }
}

void ShadingResult::transform_to_linear_rgb(const LightingConditions& lighting)
{
    const size_t aov_count = m_aovs.size();

    switch (m_color_space)
    {
      case ColorSpaceLinearRGB:
        break;

      case ColorSpaceSRGB:
        transform_srgb_to_linear_rgb(m_color);
        for (size_t i = 0; i < aov_count; ++i)
            transform_srgb_to_linear_rgb(m_aovs[i]);
        break;

      case ColorSpaceCIEXYZ:
        transform_ciexyz_to_linear_rgb(m_color);
        for (size_t i = 0; i < aov_count; ++i)
            transform_ciexyz_to_linear_rgb(m_aovs[i]);
        break;

      case ColorSpaceSpectral:
        transform_spectrum_to_linear_rgb(lighting, m_color);
        for (size_t i = 0; i < aov_count; ++i)
            transform_spectrum_to_linear_rgb(lighting, m_aovs[i]);
        break;

      assert_otherwise;
    }

    m_color_space = ColorSpaceLinearRGB;
}

namespace
{
    inline void transform_linear_rgb_to_spectrum(const LightingConditions& lighting, Spectrum& s)
    {
        linear_rgb_to_spectrum(lighting, Color3f(s[0], s[1], s[2]), s);
    }

    inline void transform_srgb_to_spectrum(const LightingConditions& lighting, Spectrum& s)
    {
        linear_rgb_to_spectrum(lighting, srgb_to_linear_rgb(Color3f(s[0], s[1], s[2])), s);
    }

    inline void transform_ciexyz_to_spectrum(const LightingConditions& lighting, Spectrum& s)
    {
        ciexyz_to_spectrum(lighting, Color3f(s[0], s[1], s[2]), s);
    }
}

void ShadingResult::transform_to_spectrum(const LightingConditions& lighting)
{
    const size_t aov_count = m_aovs.size();

    switch (m_color_space)
    {
      case ColorSpaceLinearRGB:
        transform_linear_rgb_to_spectrum(lighting, m_color);
        for (size_t i = 0; i < aov_count; ++i)
            transform_linear_rgb_to_spectrum(lighting, m_aovs[i]);
        break;

      case ColorSpaceSRGB:
        transform_srgb_to_spectrum(lighting, m_color);
        for (size_t i = 0; i < aov_count; ++i)
            transform_srgb_to_spectrum(lighting, m_aovs[i]);
        break;

      case ColorSpaceCIEXYZ:
        transform_ciexyz_to_spectrum(lighting, m_color);
        for (size_t i = 0; i < aov_count; ++i)
            transform_ciexyz_to_spectrum(lighting, m_aovs[i]);
        break;

      case ColorSpaceSpectral:
        break;

      assert_otherwise;
    }

    m_color_space = ColorSpaceSpectral;
}

void ShadingResult::composite_over(const ShadingResult& background)
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

    const Alpha contrib = Alpha(1.0f) - m_alpha;

    m_color[0] += contrib[0] * background.m_color[0];
    m_color[1] += contrib[0] * background.m_color[1];
    m_color[2] += contrib[0] * background.m_color[2];

    const size_t aov_count = m_aovs.size();

    for (size_t i = 0; i < aov_count; ++i)
    {
        const Spectrum& background_aov_color = background.m_aovs[i];
        Spectrum& aov_color = m_aovs[i];

        aov_color[0] += contrib[0] * background_aov_color[0];
        aov_color[1] += contrib[0] * background_aov_color[1];
        aov_color[2] += contrib[0] * background_aov_color[2];
    }

    m_alpha += contrib * background.m_alpha;
}

}   // namespace renderer
