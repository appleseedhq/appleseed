
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_PROGRESSIVE_ACCUMULATIONFRAMEBUFFER_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_PROGRESSIVE_ACCUMULATIONFRAMEBUFFER_H

// appleseed.renderer headers.
#include "renderer/global/global.h"

// appleseed.foundation headers.
#include "foundation/image/tile.h"
#include "foundation/math/scalar.h"

// Standard headers.
#include <algorithm>

namespace renderer
{

class AccumulationFrameBuffer
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    AccumulationFrameBuffer(
        const size_t                width,
        const size_t                height);

    static void copy(
        const AccumulationFrameBuffer&  source,
        AccumulationFrameBuffer&        destination);

    size_t get_width() const;
    size_t get_height() const;
    size_t get_pixel_count() const;

    bool is_complete() const;
    size_t get_coverage_count() const;
    double get_coverage_factor() const;

    void clear();

    void clear_pixel(
        const size_t                x,
        const size_t                y);

    void add_pixel(
        const size_t                x,
        const size_t                y,
        const foundation::Color4f&  color);

    void set_pixel(
        const size_t                x,
        const size_t                y,
        const foundation::Color4f&  color);

    bool is_set(
        const size_t                x,
        const size_t                y) const;

    foundation::Color4f get_pixel(
        const size_t                x,
        const size_t                y) const;

    size_t get_pixel(
        const size_t                x,
        const size_t                y,
        foundation::Color4f&        color) const;

    foundation::Color4f get_pixel_bilinear(
        const float                 x,
        const float                 y) const;

  private:
    struct AccumulationPixel
    {
        foundation::Color4f         m_color;
        foundation::uint32          m_count;
    };

    const size_t                    m_width;
    const size_t                    m_height;
    const size_t                    m_pixel_count;
    const double                    m_rcp_pixel_count;

    std::auto_ptr<foundation::Tile> m_tile;

    size_t                          m_coverage;
};


//
// AccumulationFrameBuffer class implementation.
//

inline size_t AccumulationFrameBuffer::get_width() const
{
    return m_width;
}

inline size_t AccumulationFrameBuffer::get_height() const
{
    return m_height;
}

inline size_t AccumulationFrameBuffer::get_pixel_count() const
{
    return m_pixel_count;
}

inline bool AccumulationFrameBuffer::is_complete() const
{
    return m_coverage == m_pixel_count;
}

inline size_t AccumulationFrameBuffer::get_coverage_count() const
{
    return m_coverage;
}

inline double AccumulationFrameBuffer::get_coverage_factor() const
{
    return static_cast<double>(m_coverage) * m_rcp_pixel_count;
}

inline void AccumulationFrameBuffer::clear_pixel(
    const size_t                x,
    const size_t                y)
{
    AccumulationPixel* pixel =
        reinterpret_cast<AccumulationPixel*>(m_tile->pixel(x, y));

    m_coverage -= pixel->m_count > 0 ? 1 : 0;

    pixel->m_count = 0;
}

inline void AccumulationFrameBuffer::add_pixel(
    const size_t                x,
    const size_t                y,
    const foundation::Color4f&  color)
{
    AccumulationPixel* pixel =
        reinterpret_cast<AccumulationPixel*>(m_tile->pixel(x, y));

    m_coverage += pixel->m_count == 0 ? 1 : 0;

    pixel->m_color += color;
    pixel->m_count += 1;
}

inline void AccumulationFrameBuffer::set_pixel(
    const size_t                x,
    const size_t                y,
    const foundation::Color4f&  color)
{
    AccumulationPixel* pixel =
        reinterpret_cast<AccumulationPixel*>(m_tile->pixel(x, y));

    m_coverage += pixel->m_count == 0 ? 1 : 0;

    pixel->m_color = color;
    pixel->m_count = 1;
}

inline bool AccumulationFrameBuffer::is_set(
    const size_t                x,
    const size_t                y) const
{
    const AccumulationPixel* pixel =
        reinterpret_cast<const AccumulationPixel*>(m_tile->pixel(x, y));

    return pixel->m_count > 0;
}

inline foundation::Color4f AccumulationFrameBuffer::get_pixel(
    const size_t                x,
    const size_t                y) const
{
    const AccumulationPixel* pixel =
        reinterpret_cast<const AccumulationPixel*>(m_tile->pixel(x, y));

    return
        pixel->m_count > 0
            ? pixel->m_color / static_cast<float>(pixel->m_count)
            : foundation::Color4f(0.0f);
}

inline size_t AccumulationFrameBuffer::get_pixel(
    const size_t                x,
    const size_t                y,
    foundation::Color4f&        color) const
{
    const AccumulationPixel* pixel =
        reinterpret_cast<const AccumulationPixel*>(m_tile->pixel(x, y));

    if (pixel->m_count > 0)
        color = pixel->m_color;
    else color.set(0.0f);

    return pixel->m_count;
}

inline foundation::Color4f AccumulationFrameBuffer::get_pixel_bilinear(
    const float                 x,
    const float                 y) const
{
    assert(x >= 0.0f);
    assert(x <= 1.0f);
    assert(y >= 0.0f);
    assert(y <= 1.0f);

    const float fx = x * m_width - 0.5f;
    const float fy = y * m_height - 0.5f;

    const int ix = foundation::truncate<int>(fx);
    const int iy = foundation::truncate<int>(fy);

    const size_t sx0 = static_cast<size_t>(std::max<int>(ix, 0));
    const size_t sy0 = static_cast<size_t>(std::max<int>(iy, 0));
    const size_t sx1 = static_cast<size_t>(std::min<int>(ix + 1, m_width - 1));
    const size_t sy1 = static_cast<size_t>(std::min<int>(iy + 1, m_height - 1));

    foundation::Color4f c00 = get_pixel(sx0, sy0);
    foundation::Color4f c01 = get_pixel(sx0, sy1);
    foundation::Color4f c10 = get_pixel(sx1, sy0);
    foundation::Color4f c11 = get_pixel(sx1, sy1);

    const float wx1 = fx - ix;
    const float wy1 = fy - iy;
    const float wx0 = 1.0f - wx1;
    const float wy0 = 1.0f - wy1;

    c00 *= wx0 * wy0;
    c01 *= wx0 * wy1;
    c10 *= wx1 * wy0;
    c11 *= wx1 * wy1;

    return c00 + c01 + c10 + c11;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_PROGRESSIVE_ACCUMULATIONFRAMEBUFFER_H
