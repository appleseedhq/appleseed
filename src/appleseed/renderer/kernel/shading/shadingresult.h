
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_RENDERER_KERNEL_SHADING_SHADINGRESULT_H
#define APPLESEED_RENDERER_KERNEL_SHADING_SHADINGRESULT_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/aov/spectrumstack.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"

namespace renderer
{

//
// Shading result.
//

class ShadingResult
  : public foundation::NonCopyable
{
  public:
    // Public members.
    foundation::ColorSpace  m_color_space;
    Spectrum                m_color;
    Alpha                   m_alpha;
    SpectrumStack           m_aovs;

    // Set the shading result to transparent black in linear RGB.
    void clear();

    // Set the shading result to a given linear RGBA value.
    void set_to_linear_rgba(const foundation::Color4f& linear_rgba);

    // Set the shading result to a given fully opaque linear RGB value.
    void set_to_linear_rgb(const foundation::Color3f& linear_rgb);

    // Transform the shading result to the linear RGB color space.
    void transform_to_linear_rgb(
        const foundation::LightingConditions&   lighting);

    // Transform the shading result to the spectral color space.
    void transform_to_spectrum(
        const foundation::LightingConditions&   lighting);

    // Set the shading result to solid pink (used for debugging).
    void set_to_solid_pink();

    // Composite this shading result over 'background'.
    void composite_over(const ShadingResult& background);
};


//
// ShadingResult class implementation.
//

inline void ShadingResult::clear()
{
    m_color_space = foundation::ColorSpaceLinearRGB;

    m_color[0] =
    m_color[1] =
    m_color[2] = 0.0f;

    m_alpha.set(0.0f);

    m_aovs.set(0.0f);
}

inline void ShadingResult::set_to_linear_rgba(const foundation::Color4f& linear_rgba)
{
    m_color_space = foundation::ColorSpaceLinearRGB;

    m_color[0] = linear_rgba[0];
    m_color[1] = linear_rgba[1];
    m_color[2] = linear_rgba[2];

    m_alpha.set(linear_rgba[3]);
}

inline void ShadingResult::set_to_linear_rgb(const foundation::Color3f& linear_rgb)
{
    m_color_space = foundation::ColorSpaceLinearRGB;

    m_color[0] = linear_rgb[0];
    m_color[1] = linear_rgb[1];
    m_color[2] = linear_rgb[2];

    m_alpha.set(1.0f);
}

inline void ShadingResult::set_to_solid_pink()
{
    set_to_linear_rgb(foundation::Color3f(1.0f, 0.0f, 1.0f));
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_SHADING_SHADINGRESULT_H
