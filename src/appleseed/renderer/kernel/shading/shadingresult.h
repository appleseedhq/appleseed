
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

    // Return true if this shading result contains valid linear RGB values;
    // false if the color, alpha or any AOV contains NaN or negative values.
    bool is_valid_linear_rgb() const;

    // Set the main color and all AOVs to transparent black in linear RGBA.
    void set_to_transparent_black();

    // Set the main color to solid pink in linear RGBA (used for debugging).
    // All AOVs are set to black.
    void set_to_solid_pink();

    // Set the main color to a given fully opaque linear RGB value.
    // All AOVs are set to black.
    void set_to_linear_rgb(const foundation::Color3f& linear_rgb);

    // Set the main color to a given linear RGBA value.
    // All AOVs are set to black.
    void set_to_linear_rgba(const foundation::Color4f& linear_rgba);

    // Transform the shading result to the linear RGB color space.
    void transform_to_linear_rgb(
        const foundation::LightingConditions&   lighting);

    // Transform the shading result to the spectral color space.
    void transform_to_spectrum(
        const foundation::LightingConditions&   lighting);

    // Composite this shading result over 'background'.
    // Both shading results must be expressed in linear RGB.
    void composite_over_linear_rgb(const ShadingResult& background);
};


//
// ShadingResult class implementation.
//

inline void ShadingResult::set_to_transparent_black()
{
    set_to_linear_rgba(foundation::Color4f(0.0f));
}

inline void ShadingResult::set_to_solid_pink()
{
    set_to_linear_rgba(foundation::Color4f(1.0f, 0.0f, 1.0f, 1.0f));
}

inline void ShadingResult::set_to_linear_rgb(const foundation::Color3f& linear_rgb)
{
    set_to_linear_rgba(foundation::Color4f(linear_rgb[0], linear_rgb[1], linear_rgb[2], 1.0f));
}

inline void ShadingResult::set_to_linear_rgba(const foundation::Color4f& linear_rgba)
{
    m_color_space = foundation::ColorSpaceLinearRGB;

    m_color[0] = linear_rgba[0];
    m_color[1] = linear_rgba[1];
    m_color[2] = linear_rgba[2];

    m_alpha.set(linear_rgba[3]);

    const size_t aov_count = m_aovs.size();

    for (size_t i = 0; i < aov_count; ++i)
    {
        Spectrum& aov = m_aovs[i];
        aov[0] = aov[1] = aov[2] = 0.0f;
    }
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_SHADING_SHADINGRESULT_H
