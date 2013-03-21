
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_FILTEREDFRAMEBUFFER_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_FILTEREDFRAMEBUFFER_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/aabb.h"
#include "foundation/math/filter.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <vector>

namespace renderer
{

class FilteredFrameBuffer
  : public foundation::NonCopyable
{
  public:
    FilteredFrameBuffer(
        const size_t                width,
        const size_t                height,
        const size_t                channel_count,
        const foundation::Filter2d& filter);

    FilteredFrameBuffer(
        const size_t                width,
        const size_t                height,
        const size_t                channel_count,
        const foundation::AABB2u&   crop_window,
        const foundation::Filter2d& filter);

    size_t get_width() const;
    size_t get_height() const;
    size_t get_pixel_count() const;
    size_t get_channel_count() const;
    const foundation::AABB2u& get_crop_window() const;
    const foundation::Filter2d& get_filter() const;

    void clear();

    // The point (x, y) is expressed in continuous image space (http://appleseedhq.net/conventions).
    void add(
        const double                x,
        const double                y,
        const float*                values);

    void get(
        const size_t                x,
        const size_t                y,
        float*                      values) const;

    float* pixel(
        const size_t                x,
        const size_t                y);

    const float* pixel(
        const size_t                x,
        const size_t                y) const;

  protected:
    const size_t                    m_width;
    const size_t                    m_height;
    const size_t                    m_channel_count;
    const foundation::AABB2u        m_crop_window;
    const foundation::Filter2d&     m_filter;
    const size_t                    m_buffer_size;
    std::vector<float>              m_buffer;
};


//
// FilteredFrameBuffer class implementation.
//

inline size_t FilteredFrameBuffer::get_width() const
{
    return m_width;
}

inline size_t FilteredFrameBuffer::get_height() const
{
    return m_height;
}

inline size_t FilteredFrameBuffer::get_pixel_count() const
{
    return m_width * m_height;
}

inline size_t FilteredFrameBuffer::get_channel_count() const
{
    return m_channel_count;
}

inline const foundation::AABB2u& FilteredFrameBuffer::get_crop_window() const
{
    return m_crop_window;
}

inline const foundation::Filter2d& FilteredFrameBuffer::get_filter() const
{
    return m_filter;
}

inline void FilteredFrameBuffer::get(
    const size_t                    x,
    const size_t                    y,
    float*                          values) const
{
    const float* RESTRICT ptr = pixel(x, y);

    const float weight = *ptr++;
    const float rcp_weight = weight == 0.0f ? 0.0f : 1.0f / weight;

    for (size_t i = 0; i < m_channel_count; ++i)
        values[i] = ptr[i] * rcp_weight;
}

inline float* FilteredFrameBuffer::pixel(
    const size_t                    x,
    const size_t                    y)
{
    assert(x < m_width);
    assert(y < m_height);

    return &m_buffer[(y * m_width + x) * (m_channel_count + 1)];
}

inline const float* FilteredFrameBuffer::pixel(
    const size_t                    x,
    const size_t                    y) const
{
    return const_cast<FilteredFrameBuffer*>(this)->pixel(x, y);
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_FILTEREDFRAMEBUFFER_H
