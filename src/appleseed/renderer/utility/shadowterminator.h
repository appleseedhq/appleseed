
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Francois Beaune, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/math/fastmath.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>

namespace renderer
{

//
// Of interest (although unrelated to this solution):
//
//   Ray Tracing Shadows: The Shadow Line Artifact
//   https://computergraphics.stackexchange.com/questions/4986
//

inline float shift_cos_in(const float cos_in, const float correction)
{
    assert(std::abs(cos_in) <= 1.0f);
    assert(correction >= 0.0f && correction < 1.0f);

    if (correction == 0.0f)
        return cos_in;

    const float k = 1.0f / (1.0f - correction);
    const float angle = foundation::fast_acos(cos_in);
    return std::max(std::cos(angle * k), 0.0f);
}

// The `k` parameter should be set `1.0f / (1.0f - correction)` where `correction` is a correction factor in [0,1).
// 0 means no correction while higher values pushes the shadow boundary further away from the geometric boundary.
inline float shift_cos_in_fast(const float cos_in, const float k)
{
    assert(std::abs(cos_in) <= 1.0f);
    assert(k >= 1.0f);

    if (k == 1.0f)
        return cos_in;

    const float angle = foundation::fast_acos(cos_in);
    return std::max(std::cos(angle * k), 0.0f);
}

}   // namespace renderer
