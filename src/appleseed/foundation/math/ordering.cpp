
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "ordering.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionnotimplemented.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <algorithm>
#include <cassert>

using namespace foundation;
using namespace std;

namespace foundation
{

//
// 2D ordering generators implementation.
//

void linear_ordering(
    vector<size_t>&     ordering,
    const size_t        size)
{
    for (size_t i = 0; i < size; ++i)
        ordering.push_back(i);
}

void spiral_ordering(
    vector<size_t>&     ordering,
    const size_t        size_x,
    const size_t        size_y)
{
    throw ExceptionNotImplemented();
}

namespace
{
    void hilbert(
        vector<size_t>& ordering,
        const int       size_x,
        const int       size_y,
        Vector2i        point,
        int             size,
        const Vector2i& dx,
        const Vector2i& dy)
    {
        if (size > 1)
        {
            // Recursively visit all four sub quads.
            size >>= 1;
            hilbert(ordering, size_x, size_y, point, size, dy, dx);
            point += dy * size;
            hilbert(ordering, size_x, size_y, point, size, dx, dy);
            point += dx * size;
            hilbert(ordering, size_x, size_y, point, size, dx, dy);
            point += dx * (size - 1) - dy;
            hilbert(ordering, size_x, size_y, point, size, -dy, -dx);
        }
        else
        {
            // Insert this tile into the tile array if it's inside the boundaries of the frame.
            assert(size == 1);
            assert(point.x >= 0);
            assert(point.y >= 0);
            if (point.x < size_x && point.y < size_y)
                ordering.push_back(size_t(point.y * size_x + point.x));
        }
    }
}

void hilbert_ordering(
    vector<size_t>&     ordering,
    const size_t        size_x,
    const size_t        size_y)
{
    // This Hilbert curve generator only works on a square grid whose size is a power of two.
    const size_t root_size = next_pow2(max(size_x, size_y));

    hilbert(
        ordering,
        static_cast<int>(size_x),
        static_cast<int>(size_y),
        Vector2i(0, 0),
        static_cast<int>(root_size),
        Vector2i(1, 0),
        Vector2i(0, 1));
}

}   // namespace foundation
