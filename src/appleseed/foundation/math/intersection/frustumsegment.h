
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2018 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/frustum.h"
#include "foundation/math/intersection/planesegment.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>

namespace foundation
{

//
// 3D frustum-segment clipping function.
//

// Clip a line segment against the frustum.
// Returns false if the line segment was entirely clipped away.
template <typename T, size_t N>
bool clip(
    const Frustum<T, N>&    frustum,
    Vector<T, 3>&           a,
    Vector<T, 3>&           b);


//
// Implementation.
//

template <typename T, size_t N>
inline bool clip(
    const Frustum<T, N>&    frustum,
    Vector<T, 3>&           a,
    Vector<T, 3>&           b)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (!clip(frustum.get_plane(i), a, b))
            return false;
    }

    return true;
}

}   // namespace foundation
