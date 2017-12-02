
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

#ifndef APPLESEED_FOUNDATION_IMAGE_NATIVEDRAWING_H
#define APPLESEED_FOUNDATION_IMAGE_NATIVEDRAWING_H

// appleseed.foundation headers.
#include "foundation/platform/types.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class Tile; }

namespace foundation
{

//
// A collection of functions to perform low-level image drawing.
//
// Warning: none of those methods handle pixel format conversion or clipping.
//

class APPLESEED_DLLSYMBOL NativeDrawing
{
  public:
    // Clear an image buffer with a given pixel color.
    // dest must point to the first destination pixel.
    static void clear(
        uint8*          dest,
        const size_t    dest_width,
        const size_t    dest_height,
        const size_t    dest_stride,
        const uint8*    pixel,
        const size_t    pixel_size);

    // Draw an horizontal line segment into an image buffer.
    // dest must point to the first destination pixel.
    static void draw_hline(
        uint8*          dest,
        const int       span,
        const uint8*    pixel,
        const size_t    pixel_size);

    // Draw a vertical line segment into an image buffer.
    // dest must point to the first destination pixel.
    static void draw_vline(
        uint8*          dest,
        const size_t    dest_stride,
        const int       span,
        const uint8*    pixel,
        const size_t    pixel_size);

    // Blit a tile to an image buffer.
    // dest must point to the first destination pixel.
    static void blit(
        uint8*          dest,
        const size_t    dest_stride,
        const Tile&     tile);
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_IMAGE_NATIVEDRAWING_H
