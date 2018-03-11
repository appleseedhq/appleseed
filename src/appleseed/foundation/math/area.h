
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

#ifndef APPLESEED_FOUNDATION_MATH_AREA_H
#define APPLESEED_FOUNDATION_MATH_AREA_H

// appleseed.foundation headers.
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>

namespace foundation
{

//
// Area functions.
//

// Compute the square of the area of a triangle defined by three points.
template <typename T, size_t N>
T square_area(
    const Vector<T, N>& v0,
    const Vector<T, N>& v1,
    const Vector<T, N>& v2);


//
// Area functions implementation.
//

template <typename T, size_t N>
inline T square_area(
    const Vector<T, N>& v0,
    const Vector<T, N>& v1,
    const Vector<T, N>& v2)
{
    const Vector<T, N> e0 = v1 - v0;
    const Vector<T, N> e1 = v2 - v0;
    return T(0.25) * square_norm(cross(e0, e1));
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_AREA_H
