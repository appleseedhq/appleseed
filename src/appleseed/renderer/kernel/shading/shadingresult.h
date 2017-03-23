
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

#ifndef APPLESEED_RENDERER_KERNEL_SHADING_SHADINGRESULT_H
#define APPLESEED_RENDERER_KERNEL_SHADING_SHADINGRESULT_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/aov/shadingfragmentstack.h"
#include "renderer/kernel/shading/shadingfragment.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/utility/poison.h"

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
    foundation::ColorSpace      m_color_space;
    ShadingFragment             m_main;
    ShadingFragmentStack        m_aovs;

    // Constructor.
    // AOVs are cleared to transparent black but the main output is left uninitialized.
    explicit ShadingResult(const size_t aov_count = 0);

    // Return true if this shading result contains valid linear RGB values;
    // false if the color, alpha or any AOV contain NaN or negative values.
    bool is_valid_linear_rgb() const;

    // Set the main color to a given linear RGB value. Leaves the alpha channel intact.
    void set_main_to_linear_rgb(const foundation::Color3f& linear_rgb);

    // Set the main color and alpha channel to a given linear RGBA value.
    void set_main_to_linear_rgba(const foundation::Color4f& linear_rgba);

    // Set the main color and alpha channel to transparent black in linear RGB.
    void set_main_to_transparent_black_linear_rgba();

    // Set the main color and alpha channel to opaque pink in linear RGB (useful for debugging).
    void set_main_to_opaque_pink_linear_rgba();

    // Set all AOV colors and alpha channels to transparent black in linear RGB.
    void set_aovs_to_transparent_black_linear_rgba();

    // Transform main and AOV colors to the linear RGB color space.
    void transform_to_linear_rgb(const foundation::LightingConditions& lighting);

    // Composite this shading result over 'background'.
    // Both shading results must be expressed in linear RGB.
    void composite_over_linear_rgb(const ShadingResult& background);

    // Multiply main and AOV colors by their respective alpha channels.
    void apply_alpha_premult_linear_rgb();
};


//
// ShadingResult class implementation.
//

inline ShadingResult::ShadingResult(const size_t aov_count)
  : m_aovs(aov_count)
{
#ifdef DEBUG
    poison(*this);
#endif

    set_aovs_to_transparent_black_linear_rgba();
}

inline void ShadingResult::set_main_to_linear_rgb(const foundation::Color3f& linear_rgb)
{
    m_color_space = foundation::ColorSpaceLinearRGB;
    m_main.m_color[0] = linear_rgb[0];
    m_main.m_color[1] = linear_rgb[1];
    m_main.m_color[2] = linear_rgb[2];
}

inline void ShadingResult::set_main_to_linear_rgba(const foundation::Color4f& linear_rgba)
{
    set_main_to_linear_rgb(linear_rgba.rgb());
    m_main.m_alpha.set(linear_rgba[3]);
}

inline void ShadingResult::set_main_to_transparent_black_linear_rgba()
{
    set_main_to_linear_rgba(foundation::Color4f(0.0f));
}

inline void ShadingResult::set_main_to_opaque_pink_linear_rgba()
{
    set_main_to_linear_rgba(foundation::Color4f(1.0f, 0.0f, 1.0f, 1.0f));
}

}       // namespace renderer

namespace foundation
{
    template <>
    class PoisonImpl<renderer::ShadingResult>
    {
      public:
        static void do_poison(renderer::ShadingResult& result)
        {
            poison(result.m_color_space);

            poison(result.m_main.m_color);
            poison(result.m_main.m_alpha);

            for (size_t i = 0, e = result.m_aovs.size(); i < e; ++i)
            {
                poison(result.m_aovs[i].m_color);
                poison(result.m_aovs[i].m_alpha);
            }
        }
    };
}

#endif  // !APPLESEED_RENDERER_KERNEL_SHADING_SHADINGRESULT_H
