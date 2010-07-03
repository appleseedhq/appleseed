
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

// Interface header.
#include "accumulationframebuffer.h"

// appleseed.foundation headers.
#include "foundation/image/pixel.h"

// Standard headers.
#include <cstring>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// AccumulationFrameBuffer class implementation.
//

// Constructor.
AccumulationFrameBuffer::AccumulationFrameBuffer(
    const size_t    width,
    const size_t    height)
  : m_width(width)
  , m_height(height)
  , m_pixel_count(width * height)
  , m_rcp_pixel_count(1.0 / m_pixel_count)
{
    // todo: change to static_assert<>.
    assert(sizeof(AccumulationPixel) == 5 * sizeof(float));

    m_tile.reset(
        new Tile(
            m_width,
            m_height,
            4 + 1,
            PixelFormatFloat));

    clear();
}

void AccumulationFrameBuffer::copy(
    const AccumulationFrameBuffer&  source,
    AccumulationFrameBuffer&        destination)
{
    assert(destination.m_width == source.m_width);
    assert(destination.m_height == source.m_height);

    memcpy(
        destination.m_tile->pixel(0),
        source.m_tile->pixel(0),
        source.m_tile->get_size());

    destination.m_coverage = source.m_coverage;
}

void AccumulationFrameBuffer::clear()
{
    AccumulationPixel* pixel =
        reinterpret_cast<AccumulationPixel*>(m_tile->pixel(0));

    for (size_t i = 0; i < m_pixel_count; ++i)
    {
        pixel[i].m_color.set(0.0f);
        pixel[i].m_count = 0;
    }

    m_coverage = 0;
}

}   // namespace renderer
