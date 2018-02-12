
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

#ifndef APPLESEED_FOUNDATION_IMAGE_DRAWING_H
#define APPLESEED_FOUNDATION_IMAGE_DRAWING_H

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/pixel.h"
#include "foundation/math/vector.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation { class Image; }

namespace foundation
{

//
// A collection of simple, unoptimized drawing routines.
//
// Supported:
//   - RGBA images
//   - Any pixel format
//   - Pixel format conversions
//   - Any number of channels
//   - Clipping
//
// Not supported:
//   - Non-RGBA images
//   - Color space conversions (image and pixel color spaces must match)
//

class Drawing
{
  public:
    // Draw a filled rectangle without anti-aliasing.
    static void draw_filled_rect(
        Image&              image,
        const Vector2i&     from,       // inclusive
        const Vector2i&     to,         // inclusive
        const Color4f&      color);

    // Draw an antialiased 4x4 pixel dot.
    static void draw_dot(
        Image&              image,
        const Vector2d&     position,
        const Color4f&      color);

    // Blit an RGBA bitmap with any pixel format.
    static void blit_bitmap(
        Image&              image,
        const Vector2i&     position,   // position of the top-left corner
        const uint8*        bitmap,
        const size_t        bitmap_width,
        const size_t        bitmap_height,
        const PixelFormat   bitmap_pixel_format,
        const Color4f&      multiplier);
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_IMAGE_DRAWING_H
