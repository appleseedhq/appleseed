
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_IMAGE_TEXT_TEXTRENDERER_H
#define APPLESEED_FOUNDATION_IMAGE_TEXT_TEXTRENDERER_H

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"

// Forward declarations.
namespace foundation { class Image; }
struct stbtt_fontinfo;

namespace foundation
{

//
// High quality, unoptimized text rendering routines.
//
// Supported:
//   - Paragraphs
//   - Subpixel positioning
//   - Kerning
//   - Linear space alpha blending
//   - sRGB target
//
// Not supported:
//   - Glyph caching (thus very slow)
//   - Text rotation
//

class TextRenderer
{
  public:
    // Built-in fonts.
    enum class Font
    {
        UbuntuL,
        UbuntuM
    };

    static float compute_string_width(
        const Font              font,
        const float             font_height,
        const char*             string);

    static float compute_string_width(
        const stbtt_fontinfo&   font_info,
        const float             font_height,
        const char*             string);

    static float compute_string_height(
        const float             font_height,
        const char*             string);

    static float compute_string_height(
        const float             font_height,
        const float             line_spacing,
        const char*             string);

    static void draw_string(
        Image&                  image,
        const ColorSpace        image_color_space,
        const Font              font,
        const float             font_height,
        const float             line_spacing,
        const Color4f&          color_srgb,
        const float             origin_x,
        const float             origin_y,
        const char*             string);

    static void draw_string(
        Image&                  image,
        const ColorSpace        image_color_space,
        const Font              font,
        const float             font_height,
        const Color4f&          color_srgb,
        const float             origin_x,
        const float             origin_y,
        const char*             string);

    static void draw_string(
        Image&                  image,
        const ColorSpace        image_color_space,
        const stbtt_fontinfo&   font_info,
        const float             font_height,
        const Color4f&          color_srgb,
        const float             origin_x,
        const float             origin_y,
        const char*             string);

    static void draw_string(
        Image&                  image,
        const ColorSpace        image_color_space,
        const stbtt_fontinfo&   font_info,
        const float             font_height,
        const float             line_spacing,
        const Color4f&          color_srgb,
        const float             origin_x,
        const float             origin_y,
        const char*             string);
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_IMAGE_TEXT_TEXTRENDERER_H
