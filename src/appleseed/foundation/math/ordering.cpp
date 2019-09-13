
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

// Interface header.
#include "ordering.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <algorithm>

using namespace foundation;

namespace foundation
{

//
// 2D ordering generators implementation.
//

void linear_ordering(
    std::vector<size_t>&     ordering,
    const size_t             size)
{
    assert(ordering.empty());
    assert(size > 0);

    ordering.resize(size);

    identity_permutation(size, &ordering[0]);
}

void spiral_ordering(
    std::vector<size_t>&     ordering,
    const size_t             size_x,
    const size_t             size_y)
{
    assert(ordering.empty());

    ordering.reserve(size_x * size_y);

    const int size = static_cast<int>(size_x * size_y);
    const int tw = static_cast<int>(size_x);
    const int th = static_cast<int>(size_y);
    const int center = (std::min(tw, th) - 1) / 2;

    for (int i = 0; i < size; ++i)
    {
        int tx = tw;
        int ty = th;

        while (i < (tx * ty))
        {
            tx--;
            ty--;
        }

        const int mintxty = std::min(tx, ty);
        const int txty = tx * ty;
        int x = center;
        int y = center;

        if (mintxty % 2)
        {
            if (i <= (txty + ty))
            {
                // Down-right side.
                x += tx - mintxty / 2;
                y += -mintxty / 2 + i - txty;
            }
            else
            {
                // Back across bottom.
                x += tx - mintxty / 2 - (i - (txty + ty));
                y += ty - mintxty / 2;
            }
        }
        else
        {
            if (i <= (txty + ty))
            {
                // Up-left side.
                x += -mintxty / 2;
                y += ty - mintxty / 2 - (i - txty);
            }
            else
            {
                // Across top.
                x += -mintxty / 2 + (i - (txty + ty));
                y += -mintxty / 2;
            }
        }

        assert(x >= 0);
        assert(y >= 0);

        ordering.push_back(static_cast<size_t>(y * tw + x));
    }
}

namespace
{
    void hilbert(
        std::vector<size_t>& ordering,
        const int            size_x,
        const int            size_y,
        Vector2i             point,
        int                  size,
        const Vector2i&      dx,
        const Vector2i&      dy)
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
                ordering.push_back(static_cast<size_t>(point.y * size_x + point.x));
        }
    }
}

void hilbert_ordering(
    std::vector<size_t>&     ordering,
    const size_t             size_x,
    const size_t             size_y)
{
    assert(ordering.empty());

    ordering.reserve(size_x * size_y);

    // This Hilbert curve generator only works on a square grid whose size is a power of two.
    const size_t root_size = next_pow2(std::max(size_x, size_y));

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
