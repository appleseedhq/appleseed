
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

#pragma once

// appleseed.renderer headers.
#include "renderer/kernel/aov/aovsettings.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/image/color.h"

// Standard headers.
#include <cassert>
#include <cstddef>

namespace renderer
{

//
// The result of shading an image sample.
//
// All colors are expressed in linear RGB and use premultiplied alpha.
//

class ShadingResult
  : public foundation::NonCopyable
{
  public:
    // Public members.
    foundation::Color4f m_main;
    foundation::Color4f m_aovs[MaxAOVCount];
    size_t              m_aov_count;

    // Constructor.
    // The main output and AOVs are cleared to transparent black.
    explicit ShadingResult(const size_t aov_count = 0);

    // Return true if the main output is finite (not NaN, not infinite) and non-negative.
    bool is_main_valid() const;

    // Return true if the main output and all AOVs are finite (not NaN, not infinite) and non-negative.
    bool is_valid() const;

    // Composite this shading result over `background`.
    void composite_over(const ShadingResult& background);

    // Apply alpha premultiplication to the main output and the AOVs.
    void apply_alpha_premult();

    // Set the main output to opaque pink.
    void set_main_to_opaque_pink();
};


//
// ShadingResult class implementation.
//

inline ShadingResult::ShadingResult(const size_t aov_count)
  : m_aov_count(aov_count)
{
    assert(aov_count <= MaxAOVCount);

    m_main.set(0.0f);

    for (size_t i = 0, e = m_aov_count; i < e; ++i)
        m_aovs[i].set(0.0f);
}

inline void ShadingResult::composite_over(const ShadingResult& background)
{
    m_main += (1.0f - m_main.a) * background.m_main;

    for (size_t i = 0, e = m_aov_count; i < e; ++i)
    {
        foundation::Color4f& aov = m_aovs[i];
        aov += (1.0f - aov.a) * background.m_aovs[i];
    }
}

inline void ShadingResult::apply_alpha_premult()
{
    m_main.premultiply_in_place();

    for (size_t i = 0, e = m_aov_count; i < e; ++i)
        m_aovs[i].premultiply_in_place();
}

inline void ShadingResult::set_main_to_opaque_pink()
{
    m_main = foundation::Color4f(1.0f, 0.0f, 1.0f, 1.0f);
}

}   // namespace renderer
