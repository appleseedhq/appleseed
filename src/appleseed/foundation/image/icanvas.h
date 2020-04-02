
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

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/tile.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdint>

namespace foundation
{

//
// Canvas interface.
//

class APPLESEED_DLLSYMBOL ICanvas
  : public NonCopyable
{
  public:
    // Destructor.
    virtual ~ICanvas() = default;

    // Access canvas properties.
    virtual const CanvasProperties& properties() const = 0;

    // Direct access to a given tile.
    virtual Tile& tile(
        const size_t            tile_x,
        const size_t            tile_y) = 0;
    virtual const Tile& tile(
        const size_t            tile_x,
        const size_t            tile_y) const = 0;

    // Direct access to a given pixel.
    std::uint8_t* pixel(
        const size_t            x,
        const size_t            y);
    const std::uint8_t* pixel(
        const size_t            x,
        const size_t            y) const;

    // Structured write access to a given pixel, with automatic pixel format conversion.
    // It is legal to set only a subset of the image's channels.
    template <typename T>
    void set_pixel(
        const size_t            x,
        const size_t            y,
        const T                 components[],
        const size_t            component_count);
    template <typename T, size_t N>
    void set_pixel(
        const size_t            x,
        const size_t            y,
        const Color<T, N>&      color);

    // Structured read access to a given pixel, with automatic pixel format conversion.
    // It is legal to get only a subset of the image's channels.
    template <typename T>
    void get_pixel(
        const size_t            x,
        const size_t            y,
        T                       components[],
        const size_t            component_count) const;
    template <typename T, size_t N>
    void get_pixel(
        const size_t            x,
        const size_t            y,
        Color<T, N>&            color) const;

    // Set all pixels to a given color.
    // This causes all tiles to be accessed, and created if necessary.
    template <typename T, size_t N>
    void clear(const Color<T, N>& color);
};


//
// ICanvas class implementation.
//

inline std::uint8_t* ICanvas::pixel(
    const size_t        x,
    const size_t        y)
{
    const CanvasProperties& props = properties();
    assert(x < props.m_canvas_width);
    assert(y < props.m_canvas_height);

    const size_t tile_x = x / props.m_tile_width;
    const size_t pixel_x = x % props.m_tile_width;
    assert(tile_x < props.m_tile_count_x);

    const size_t tile_y = y / props.m_tile_height;
    const size_t pixel_y = y % props.m_tile_height;
    assert(tile_y < props.m_tile_count_y);

    const Tile& t = tile(tile_x, tile_y);
    assert(pixel_x < t.get_width());
    assert(pixel_y < t.get_height());

    return t.pixel(pixel_x, pixel_y);
}

inline const std::uint8_t* ICanvas::pixel(
    const size_t        x,
    const size_t        y) const
{
    const CanvasProperties& props = properties();
    assert(x < props.m_canvas_width);
    assert(y < props.m_canvas_height);

    const size_t tile_x = x / props.m_tile_width;
    const size_t pixel_x = x % props.m_tile_width;
    assert(tile_x < props.m_tile_count_x);

    const size_t tile_y = y / props.m_tile_height;
    const size_t pixel_y = y % props.m_tile_height;
    assert(tile_y < props.m_tile_count_y);

    const Tile& t = tile(tile_x, tile_y);
    assert(pixel_x < t.get_width());
    assert(pixel_y < t.get_height());

    return t.pixel(pixel_x, pixel_y);
}

template <typename T>
inline void ICanvas::set_pixel(
    const size_t        x,
    const size_t        y,
    const T             components[],
    const size_t        component_count)
{
    const CanvasProperties& props = properties();
    assert(x < props.m_canvas_width);
    assert(y < props.m_canvas_height);
    assert(component_count <= props.m_channel_count);

    const size_t tile_x = x / props.m_tile_width;
    const size_t pixel_x = x % props.m_tile_width;
    assert(tile_x < props.m_tile_count_x);

    const size_t tile_y = y / props.m_tile_height;
    const size_t pixel_y = y % props.m_tile_height;
    assert(tile_y < props.m_tile_count_y);

    Tile& t = tile(tile_x, tile_y);
    assert(pixel_x < t.get_width());
    assert(pixel_y < t.get_height());

    t.set_pixel(pixel_x, pixel_y, components, component_count);
}

template <typename T, size_t N>
inline void ICanvas::set_pixel(
    const size_t        x,
    const size_t        y,
    const Color<T, N>&  color)
{
    const CanvasProperties& props = properties();
    assert(x < props.m_canvas_width);
    assert(y < props.m_canvas_height);
    assert(N <= props.m_channel_count);

    const size_t tile_x = x / props.m_tile_width;
    const size_t pixel_x = x % props.m_tile_width;
    assert(tile_x < props.m_tile_count_x);

    const size_t tile_y = y / props.m_tile_height;
    const size_t pixel_y = y % props.m_tile_height;
    assert(tile_y < props.m_tile_count_y);

    Tile& t = tile(tile_x, tile_y);
    assert(pixel_x < t.get_width());
    assert(pixel_y < t.get_height());

    t.set_pixel(pixel_x, pixel_y, color);
}

template <typename T>
inline void ICanvas::get_pixel(
    const size_t        x,
    const size_t        y,
    T                   components[],
    const size_t        component_count) const
{
    const CanvasProperties& props = properties();
    assert(x < props.m_canvas_width);
    assert(y < props.m_canvas_height);
    assert(component_count <= props.m_channel_count);

    const size_t tile_x = x / props.m_tile_width;
    const size_t pixel_x = x % props.m_tile_width;
    assert(tile_x < props.m_tile_count_x);

    const size_t tile_y = y / props.m_tile_height;
    const size_t pixel_y = y % props.m_tile_height;
    assert(tile_y < props.m_tile_count_y);

    const Tile& t = tile(tile_x, tile_y);
    assert(pixel_x < t.get_width());
    assert(pixel_y < t.get_height());

    t.get_pixel(pixel_x, pixel_y, components, component_count);
}

template <typename T, size_t N>
inline void ICanvas::get_pixel(
    const size_t        x,
    const size_t        y,
    Color<T, N>&        color) const
{
    const CanvasProperties& props = properties();
    assert(x < props.m_canvas_width);
    assert(y < props.m_canvas_height);
    assert(N <= props.m_channel_count);

    const size_t tile_x = x / props.m_tile_width;
    const size_t pixel_x = x % props.m_tile_width;
    assert(tile_x < props.m_tile_count_x);

    const size_t tile_y = y / props.m_tile_height;
    const size_t pixel_y = y % props.m_tile_height;
    assert(tile_y < props.m_tile_count_y);

    const Tile& t = tile(tile_x, tile_y);
    assert(pixel_x < t.get_width());
    assert(pixel_y < t.get_height());

    t.get_pixel(pixel_x, pixel_y, color);
}

template <typename T, size_t N>
inline void ICanvas::clear(const Color<T, N>& color)
{
    const CanvasProperties& props = properties();
    assert(N == props.m_channel_count);

    for (size_t ty = 0; ty < props.m_tile_count_y; ++ty)
    {
        for (size_t tx = 0; tx < props.m_tile_count_x; ++tx)
            tile(tx, ty).clear(color);
    }
}

}   // namespace foundation
