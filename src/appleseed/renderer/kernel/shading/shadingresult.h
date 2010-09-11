
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

#ifndef APPLESEED_RENDERER_KERNEL_SHADING_SHADINGRESULT_H
#define APPLESEED_RENDERER_KERNEL_SHADING_SHADINGRESULT_H

// appleseed.renderer headers.
#include "renderer/global/global.h"

// appleseed.foundation headers.
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

    // Set the shading result to transparent black.
    void clear();

    // Set the shading result to solid pink (used for debugging).
    void set_to_solid_pink();

    // Transform the shading result to a given color space.
    void transform_to_color_space(
        const foundation::ColorSpace            target,
        const foundation::LightingConditions&   lighting);

    // Transform the shading result to the linear RGB color space.
    void transform_to_linear_rgb(
        const foundation::LightingConditions&   lighting);

    // Transform the shading result to the spectral color space.
    void transform_to_spectrum(
        const foundation::LightingConditions&   lighting);
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
}

inline void ShadingResult::set_to_solid_pink()
{
    m_color_space = foundation::ColorSpaceLinearRGB;

    m_color[0] = 1.0f;
    m_color[1] = 0.0f;
    m_color[2] = 1.0f;

    m_alpha.set(1.0f);
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_SHADING_SHADINGRESULT_H
