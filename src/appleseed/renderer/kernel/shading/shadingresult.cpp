
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

// Transform the shading result to a given color space.
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

// Transform the shading result to the linear RGB color space.
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
        return;

      case ColorSpaceCIEXYZ:
        rgb_out =
            ciexyz_to_linear_rgb(
                Color3f(m_color[0], m_color[1], m_color[2]));
        return;

      case ColorSpaceSpectral:
        rgb_out =
            ciexyz_to_linear_rgb(
                spectrum_to_ciexyz<float>(lighting, m_color));
        return;

      assert_otherwise;
    }
}

// Transform the shading result to the spectral color space.
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

}   // namespace renderer
