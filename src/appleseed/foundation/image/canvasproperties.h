
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
#include "foundation/image/pixel.h"
#include "foundation/math/scalar.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <algorithm>
#include <cmath>
#include <cstddef>

namespace foundation
{

//
// Canvas properties.
//

class APPLESEED_DLLSYMBOL CanvasProperties
{
  public:
    // Properties.
    size_t                  m_canvas_width;             // canvas width, in pixels
    size_t                  m_canvas_height;            // canvas height, in pixels
    size_t                  m_tile_width;               // tile width, in pixels
    size_t                  m_tile_height;              // tile height, in pixels
    size_t                  m_channel_count;            // number of channels per pixel
    PixelFormat             m_pixel_format;             // pixel format

    // Precomputed values.
    double                  m_rcp_canvas_width;         // canvas width reciprocal
    double                  m_rcp_canvas_height;        // canvas height reciprocal
    double                  m_rcp_tile_width;           // tile width reciprocal
    double                  m_rcp_tile_height;          // tile height reciprocal
    size_t                  m_tile_count_x;             // number of tiles of a row in the canvas
    size_t                  m_tile_count_y;             // number of tiles of a column in the canvas
    size_t                  m_tile_count;               // total number of tiles in the canvas
    size_t                  m_pixel_count;              // total number of pixels in the canvas
    size_t                  m_channel_size;             // size in bytes of one channel
    size_t                  m_pixel_size;               // size in bytes of one pixel

    // Constructors.
    CanvasProperties() = default;
    CanvasProperties(
        const size_t        canvas_width,
        const size_t        canvas_height,
        const size_t        tile_width,
        const size_t        tile_height,
        const size_t        channel_count,
        const PixelFormat   pixel_format);

    // Compute the width and height in pixels of a given tile.
    size_t get_tile_width(const size_t tile_x) const;
    size_t get_tile_height(const size_t tile_y) const;
};


//
// CanvasProperties class implementation.
//

inline CanvasProperties::CanvasProperties(
    const size_t            canvas_width,
    const size_t            canvas_height,
    const size_t            tile_width,
    const size_t            tile_height,
    const size_t            channel_count,
    const PixelFormat       pixel_format)
  : m_canvas_width(canvas_width)
  , m_canvas_height(canvas_height)
  , m_tile_width(tile_width)
  , m_tile_height(tile_height)
  , m_channel_count(channel_count)
  , m_pixel_format(pixel_format)
{
    m_rcp_canvas_width = 1.0 / m_canvas_width;
    m_rcp_canvas_height = 1.0 / m_canvas_height;

    m_rcp_tile_width = 1.0 / m_tile_width;
    m_rcp_tile_height = 1.0 / m_tile_height;

    const double nx = std::ceil(static_cast<double>(m_canvas_width) / m_tile_width);
    const double ny = std::ceil(static_cast<double>(m_canvas_height) / m_tile_height);

    m_tile_count_x = truncate<size_t>(nx);
    m_tile_count_y = truncate<size_t>(ny);
    m_tile_count = m_tile_count_x * m_tile_count_y;

    m_pixel_count = m_canvas_width * m_canvas_height;
    m_channel_size = Pixel::size(m_pixel_format);
    m_pixel_size = m_channel_count * m_channel_size;
}

inline size_t CanvasProperties::get_tile_width(const size_t tile_x) const
{
    return
        std::min(
            m_canvas_width - tile_x * m_tile_width,
            m_tile_width);
}

inline size_t CanvasProperties::get_tile_height(const size_t tile_y) const
{
    return
        std::min(
            m_canvas_height - tile_y * m_tile_height,
            m_tile_height);
}

}   // namespace foundation
