
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
#include "nativedrawing.h"

// appleseed.foundation headers.
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"

// Standard headers.
#include <cassert>
#include <cstring>

namespace foundation
{

//
// NativeDrawing class implementation.
//

void NativeDrawing::clear(
    std::uint8_t*           dest,
    const size_t            dest_width,
    const size_t            dest_height,
    const size_t            dest_stride,
    const std::uint8_t*     pixel,
    const size_t            pixel_size)
{
    assert(dest);
    assert(pixel);
    assert(pixel_size > 0);

    // Clear the first row.
    for (size_t i = 0; i < dest_width; ++i)
        memcpy(&dest[i * pixel_size], pixel, pixel_size);

    // Clear the remaining rows.
    for (size_t i = 1; i < dest_height; ++i)
        memcpy(&dest[i * dest_stride], dest, pixel_size * dest_width);
}

void NativeDrawing::draw_hline(
    std::uint8_t*           dest,
    const int               span,
    const std::uint8_t*     pixel,
    const size_t            pixel_size)
{
    assert(dest);
    assert(pixel);
    assert(pixel_size > 0);

    const std::uint8_t* end = dest + span * pixel_size;
    const int step = span < 0 ? -static_cast<int>(pixel_size) : static_cast<int>(pixel_size);

    while (dest != end)
    {
        memcpy(dest, pixel, pixel_size);
        dest += step;
    }
}

void NativeDrawing::draw_vline(
    std::uint8_t*           dest,
    const size_t            dest_stride,
    const int               span,
    const std::uint8_t*     pixel,
    const size_t            pixel_size)
{
    assert(dest);
    assert(pixel);
    assert(pixel_size > 0);

    const std::uint8_t* end = dest + span * dest_stride;
    const int step = span < 0 ? -static_cast<int>(dest_stride) : static_cast<int>(dest_stride);

    while (dest != end)
    {
        memcpy(dest, pixel, pixel_size);
        dest += step;
    }
}

void NativeDrawing::blit(
    std::uint8_t*           dest,
    const size_t            dest_stride,
    const Tile&             tile)
{
    assert(dest);

    // Retrieve tile size.
    const size_t tile_width = tile.get_width();
    const size_t tile_height = tile.get_height();

    // Compute the size of one row of the tile.
    const size_t pixel_size =
        tile.get_channel_count() * Pixel::size(tile.get_pixel_format());
    const size_t src_row_size = tile_width * pixel_size;

    // Retrieve pointer to the pixel data of the tile.
    const std::uint8_t* src = tile.pixel(0, 0);

    // Copy the tile into the image.
    for (size_t i = 0; i < tile_height; ++i)
    {
        memcpy(
            &dest[i * dest_stride],
            &src[i * src_row_size],
            src_row_size);
    }
}

}   // namespace foundation
