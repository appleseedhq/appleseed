
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
#include "foundation/math/aabb.h"
#include "foundation/math/frustum.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>

namespace foundation
{

//
// 3D frustum-AABB intersection function.
//

// Check if a frustum and a bounding box intersect.
template <typename T, size_t N>
bool intersect(
    const Frustum<T, N>&    frustum,
    const AABB<T, 3>&       aabb);


//
// Implementation.
//

template <typename T, size_t N>
bool intersect(
    const Frustum<T, N>&    frustum,
    const AABB<T, 3>&       aabb)
{
    for (size_t i = 0; i < N; ++i)
    {
        const Vector<T, 4>& plane = frustum.get_plane(i);

        // No intersection if all eight corners of the box are in the positive half space of this plane.
        size_t outside = 0;
        outside += plane[0] * aabb.min[0] + plane[1] * aabb.min[1] + plane[2] * aabb.min[2] + plane[3] > T(0.0) ? 1 : 0;
        outside += plane[0] * aabb.min[0] + plane[1] * aabb.min[1] + plane[2] * aabb.max[2] + plane[3] > T(0.0) ? 1 : 0;
        outside += plane[0] * aabb.min[0] + plane[1] * aabb.max[1] + plane[2] * aabb.min[2] + plane[3] > T(0.0) ? 1 : 0;
        outside += plane[0] * aabb.min[0] + plane[1] * aabb.max[1] + plane[2] * aabb.max[2] + plane[3] > T(0.0) ? 1 : 0;
        outside += plane[0] * aabb.max[0] + plane[1] * aabb.min[1] + plane[2] * aabb.min[2] + plane[3] > T(0.0) ? 1 : 0;
        outside += plane[0] * aabb.max[0] + plane[1] * aabb.min[1] + plane[2] * aabb.max[2] + plane[3] > T(0.0) ? 1 : 0;
        outside += plane[0] * aabb.max[0] + plane[1] * aabb.max[1] + plane[2] * aabb.min[2] + plane[3] > T(0.0) ? 1 : 0;
        outside += plane[0] * aabb.max[0] + plane[1] * aabb.max[1] + plane[2] * aabb.max[2] + plane[3] > T(0.0) ? 1 : 0;
        if (outside == 8)
            return false;
    }

    // todo: if the frustum had vertices and if we somehow had them, we could now check, for any
    // of the six planes of the bounding box, if all vertices of the frustum are on the outside.
    // See http://www.iquilezles.org/www/articles/frustumcorrect/frustumcorrect.htm for details.

    return true;
}

}   // namespace foundation
