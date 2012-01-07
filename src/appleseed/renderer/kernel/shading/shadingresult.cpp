
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

using namespace foundation;

namespace renderer
{

//
// ShadingResult class implementation.
//

void ShadingResult::transform_to_color_space(
    const ColorSpace            target,
    const LightingConditions&   lighting)
{
    const ColorSpace source = m_color_space;
    m_color_space = target;

    Color3f& rgb_out = *reinterpret_cast<Color3f*>(&m_color[0]);

    switch (source)
    {
      case ColorSpaceLinearRGB:
        switch (target)
        {
          case ColorSpaceLinearRGB:
            return;

          case ColorSpaceSRGB:
            rgb_out =
                fast_linear_rgb_to_srgb(
                    Color3f(m_color[0], m_color[1], m_color[2]));
            return;

          case ColorSpaceCIEXYZ:
            rgb_out =
                linear_rgb_to_ciexyz(
                    Color3f(m_color[0], m_color[1], m_color[2]));
            return;

          case ColorSpaceSpectral:
            linear_rgb_to_spectrum(
                lighting,
                Color3f(m_color[0], m_color[1], m_color[2]),
                m_color);
            return;

          assert_otherwise;
        }

      case ColorSpaceSRGB:
        switch (target)
        {
          case ColorSpaceLinearRGB:
            rgb_out =
                srgb_to_linear_rgb(
                    Color3f(m_color[0], m_color[1], m_color[2]));
            return;

          case ColorSpaceSRGB:
            return;

          case ColorSpaceCIEXYZ:
            rgb_out =
                linear_rgb_to_ciexyz(
                    srgb_to_linear_rgb(
                        Color3f(m_color[0], m_color[1], m_color[2])));
            return;

          case ColorSpaceSpectral:
            linear_rgb_to_spectrum(
                lighting,
                srgb_to_linear_rgb(
                    Color3f(m_color[0], m_color[1], m_color[2])),
                m_color);
            return;

          assert_otherwise;
        }

      case ColorSpaceCIEXYZ:
        switch (target)
        {
          case ColorSpaceLinearRGB:
            rgb_out =
                ciexyz_to_linear_rgb(
                    Color3f(m_color[0], m_color[1], m_color[2]));
            return;

          case ColorSpaceSRGB:
            rgb_out =
                fast_linear_rgb_to_srgb(
                    ciexyz_to_linear_rgb(
                        Color3f(m_color[0], m_color[1], m_color[2])));
            return;

          case ColorSpaceCIEXYZ:
            return;

          case ColorSpaceSpectral:
            ciexyz_to_spectrum(
                lighting,
                Color3f(m_color[0], m_color[1], m_color[2]),
                m_color);
            return;

          assert_otherwise;
        }

      case ColorSpaceSpectral:
        switch (target)
        {
          case ColorSpaceLinearRGB:
            rgb_out =
                ciexyz_to_linear_rgb(
                    spectrum_to_ciexyz<float>(lighting, m_color));
            return;

          case ColorSpaceSRGB:
            rgb_out =
                fast_linear_rgb_to_srgb(
                    ciexyz_to_linear_rgb(
                        spectrum_to_ciexyz<float>(lighting, m_color)));
            return;

          case ColorSpaceCIEXYZ:
            rgb_out = spectrum_to_ciexyz<float>(lighting, m_color);
            return;

          case ColorSpaceSpectral:
            return;

          assert_otherwise;
        }

      assert_otherwise;
    }
}

void ShadingResult::transform_to_linear_rgb(
    const LightingConditions&   lighting)
{
    const ColorSpace source = m_color_space;
    m_color_space = ColorSpaceLinearRGB;

    Color3f& rgb_out = *reinterpret_cast<Color3f*>(&m_color[0]);

    switch (source)
    {
      case ColorSpaceLinearRGB:
        return;

      case ColorSpaceSRGB:
        rgb_out =
            srgb_to_linear_rgb(
                Color3f(m_color[0], m_color[1], m_color[2]));
        for (size_t i = 0; i < m_aovs.m_aov_count; ++i)
        {
            const Spectrum& aov_in = m_aovs.m_aovs[i].m_spectrum;
            Color3f& aov_rgb_out = *reinterpret_cast<Color3f*>(&m_aovs.m_aovs[i].m_spectrum[0]);
            aov_rgb_out =
                srgb_to_linear_rgb(
                    Color3f(aov_in[0], aov_in[1], aov_in[2]));
        }
        return;

      case ColorSpaceCIEXYZ:
        rgb_out =
            ciexyz_to_linear_rgb(
                Color3f(m_color[0], m_color[1], m_color[2]));
        for (size_t i = 0; i < m_aovs.m_aov_count; ++i)
        {
            const Spectrum& aov_in = m_aovs.m_aovs[i].m_spectrum;
            Color3f& aov_rgb_out = *reinterpret_cast<Color3f*>(&m_aovs.m_aovs[i].m_spectrum[0]);
            aov_rgb_out =
                ciexyz_to_linear_rgb(
                    Color3f(aov_in[0], aov_in[1], aov_in[2]));
        }
        return;

      case ColorSpaceSpectral:
        rgb_out =
            ciexyz_to_linear_rgb(
                spectrum_to_ciexyz<float>(lighting, m_color));
        for (size_t i = 0; i < m_aovs.m_aov_count; ++i)
        {
            const Spectrum& aov_in = m_aovs.m_aovs[i].m_spectrum;
            Color3f& aov_rgb_out = *reinterpret_cast<Color3f*>(&m_aovs.m_aovs[i].m_spectrum[0]);
            aov_rgb_out =
                ciexyz_to_linear_rgb(
                    spectrum_to_ciexyz<float>(lighting, aov_in));
        }
        return;

      assert_otherwise;
    }
}

void ShadingResult::transform_to_spectrum(
    const LightingConditions&   lighting)
{
    const ColorSpace source = m_color_space;
    m_color_space = ColorSpaceSpectral;

    switch (source)
    {
      case ColorSpaceLinearRGB:
        linear_rgb_to_spectrum(
            lighting,
            Color3f(m_color[0], m_color[1], m_color[2]),
            m_color);
        return;

      case ColorSpaceSRGB:
        linear_rgb_to_spectrum(
            lighting,
            srgb_to_linear_rgb(
                Color3f(m_color[0], m_color[1], m_color[2])),
            m_color);
        return;

      case ColorSpaceCIEXYZ:
        ciexyz_to_spectrum(
            lighting,
            Color3f(m_color[0], m_color[1], m_color[2]),
            m_color);
        return;

      case ColorSpaceSpectral:
        return;

      assert_otherwise;
    }
}

void ShadingResult::composite_over(const ShadingResult& other)
{
    assert(m_color_space == ColorSpaceLinearRGB);
    assert(other.m_color_space == ColorSpaceLinearRGB);

    const Alpha contrib = Alpha(1.0) - m_alpha;
    const Alpha color_contrib = contrib * other.m_alpha;

    m_color[0] += color_contrib[0] * other.m_color[0];
    m_color[1] += color_contrib[0] * other.m_color[1];
    m_color[2] += color_contrib[0] * other.m_color[2];

    for (size_t i = 0; i < m_aovs.m_aov_count; ++i)
    {
        const Spectrum& other_aov_color = other.m_aovs.m_aovs[i].m_spectrum;
        Spectrum& aov_color = m_aovs.m_aovs[i].m_spectrum;

        aov_color[0] += color_contrib[0] * other_aov_color[0];
        aov_color[1] += color_contrib[0] * other_aov_color[1];
        aov_color[2] += color_contrib[0] * other_aov_color[2];
    }

    m_alpha += contrib * other.m_alpha;
}

}   // namespace renderer
