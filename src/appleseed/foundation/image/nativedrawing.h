
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

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>
#include <cstdint>

// Forward declarations.
namespace foundation    { class Tile; }

namespace foundation
{

//
// A collection of optimized low-level drawing routines.
//
// Supported:
//   - Any pixel format
//   - Any number of channels
//
// Not supported:
//   - Clipping
//   - Pixel format conversions (image and pixel formats must match)
//   - Color space conversions (image and pixel color spaces must match)
//

class APPLESEED_DLLSYMBOL NativeDrawing
{
  public:
    // Clear an image buffer with a given pixel color.
    // dest must point to the first destination pixel.
    static void clear(
        std::uint8_t*           dest,
        const size_t            dest_width,
        const size_t            dest_height,
        const size_t            dest_stride,
        const std::uint8_t*     pixel,
        const size_t            pixel_size);

    // Draw an horizontal line segment into an image buffer.
    // dest must point to the first destination pixel.
    static void draw_hline(
        std::uint8_t*           dest,
        const int               span,
        const std::uint8_t*     pixel,
        const size_t            pixel_size);

    // Draw a vertical line segment into an image buffer.
    // dest must point to the first destination pixel.
    static void draw_vline(
        std::uint8_t*           dest,
        const size_t            dest_stride,
        const int               span,
        const std::uint8_t*     pixel,
        const size_t            pixel_size);

    // Blit a tile to an image buffer.
    // dest must point to the first destination pixel.
    static void blit(
        std::uint8_t*           dest,
        const size_t            dest_stride,
        const Tile&             tile);
};

}   // namespace foundation
