
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
#include "foundation/platform/types.h"
#include "foundation/utility/casts.h"

using namespace foundation;

namespace renderer
{

//
// ShadingResult class implementation.
//

namespace
{
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

    inline bool is_valid_color(const Color4f& c)
    {
        return
            is_valid_scalar(c[0]) &&
            is_valid_scalar(c[1]) &&
            is_valid_scalar(c[2]) &&
            is_valid_scalar(c[3]);
    }
}

bool ShadingResult::is_valid() const
{
    if (!is_valid_color(m_main))
        return false;

    for (size_t i = 0, e = m_aov_count; i < e; ++i)
    {
        if (!is_valid_color(m_aovs[i]))
            return false;
    }

    return true;
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

    m_main += (1.0f - m_main.a) * background.m_main;

    for (size_t i = 0, e = m_aov_count; i < e; ++i)
    {
        Color4f& aov = m_aovs[i];
        aov += (1.0f - aov.a) * background.m_aovs[i];
    }
}

void ShadingResult::apply_alpha_premult()
{
    m_main.rgb() *= m_main.a;

    for (size_t i = 0, e = m_aov_count; i < e; ++i)
    {
        Color4f& aov = m_aovs[i];
        aov.rgb() *= aov.a;
    }
}

void ShadingResult::set_main_to_opaque_pink()
{
    m_main = Color4f(1.0f, 0.0f, 1.0f, 1.0f);
}

}   // namespace renderer
