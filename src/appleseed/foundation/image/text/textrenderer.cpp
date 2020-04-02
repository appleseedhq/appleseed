
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

// Interface header.
#include "textrenderer.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/image/text/stb_truetype.h"
#include "foundation/math/scalar.h"
#include "foundation/resources/fonts/Ubuntu-L.ttf.h"
#include "foundation/resources/fonts/Ubuntu-M.ttf.h"

// Standard headers.
#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <vector>

namespace foundation
{

//
// TextRenderer class implementation.
//

namespace
{
    // Default spacing between lines as a factor of the font size.
    const float DefaultLineSpacing = 1.1f;

    struct BuiltinFonts
    {
        stbtt_fontinfo m_ubuntu_l_font;
        stbtt_fontinfo m_ubuntu_m_font;

        BuiltinFonts()
        {
            stbtt_InitFont(&m_ubuntu_l_font, Ubuntu_L_ttf, 0);
            stbtt_InitFont(&m_ubuntu_m_font, Ubuntu_M_ttf, 0);
        }
    };

    BuiltinFonts g_builtin_fonts;
}

float TextRenderer::compute_string_width(
    const Font              font,
    const float             font_height,
    const char*             string)
{
    const stbtt_fontinfo& font_info =
        font == Font::UbuntuL
            ? g_builtin_fonts.m_ubuntu_l_font
            : g_builtin_fonts.m_ubuntu_m_font;

    return compute_string_width(font_info, font_height, string);
}

float TextRenderer::compute_string_width(
    const stbtt_fontinfo&   font_info,
    const float             font_height,
    const char*             string)
{
    float width = 0.0f, max_width = 0.0f;

    // Compute font scaling factor for the desired pixel height.
    const float scale = stbtt_ScaleForPixelHeight(&font_info, font_height);

    for (const char* s = string; *s; ++s)
    {
        // Codepoint to render.
        const char cp = s[0];

        // Handle carriage returns.
        if (cp == '\n')
        {
            if (max_width < width)
                max_width = width;
            width = 0.0f;
            continue;
        }

        // Advance cursor.
        int advance_width;
        stbtt_GetCodepointHMetrics(&font_info, cp, &advance_width, nullptr);
        width += advance_width * scale;

        // Apply kerning.
        const char next_cp = s[1];
        if (next_cp != 0)
        {
            const int kern_advance =
                stbtt_GetCodepointKernAdvance(&font_info, cp, next_cp);
            width += kern_advance * scale;
        }
    }

    return std::max(width, max_width);
}
    
float TextRenderer::compute_string_height(
    const float             font_height,
    const char*             string)
{
    return
        compute_string_height(
            font_height,
            DefaultLineSpacing,
            string);
}

float TextRenderer::compute_string_height(
    const float             font_height,
    const float             line_spacing,
    const char*             string)
{
    size_t lines = 1;

    for (const char* s = string; *s; ++s)
    {
        if (*s == '\n')
            ++lines;
    }

    return font_height * (1 + (lines - 1) * line_spacing);
}

void TextRenderer::draw_string(
    Image&                  image,
    const Font              font,
    const float             font_height,
    const Color4f&          color,
    const float             origin_x,
    const float             origin_y,
    const char*             string)
{
    const stbtt_fontinfo& font_info =
        font == Font::UbuntuL
            ? g_builtin_fonts.m_ubuntu_l_font
            : g_builtin_fonts.m_ubuntu_m_font;

    draw_string(
        image,
        font_info,
        font_height,
        color,
        origin_x,
        origin_y,
        string);
}

void TextRenderer::draw_string(
    Image&                  image,
    const Font              font,
    const float             font_height,
    const float             line_spacing,
    const Color4f&          color,
    const float             origin_x,
    const float             origin_y,
    const char*             string)
{
    const stbtt_fontinfo& font_info =
        font == Font::UbuntuL
            ? g_builtin_fonts.m_ubuntu_l_font
            : g_builtin_fonts.m_ubuntu_m_font;

    draw_string(
        image,
        font_info,
        font_height,
        line_spacing,
        color,
        origin_x,
        origin_y,
        string);
}

void TextRenderer::draw_string(
    Image&                  image,
    const stbtt_fontinfo&   font_info,
    const float             font_height,
    const Color4f&          color,
    const float             origin_x,
    const float             origin_y,
    const char*             string)
{
    draw_string(
        image,
        font_info,
        font_height,
        DefaultLineSpacing,
        color,
        origin_x,
        origin_y,
        string);
}

void TextRenderer::draw_string(
    Image&                  image,
    const stbtt_fontinfo&   font_info,
    const float             font_height,
    const float             line_spacing,
    const Color4f&          color,
    const float             origin_x,
    const float             origin_y,
    const char*             string)
{
    const CanvasProperties& props = image.properties();
    const int canvas_width = static_cast<int>(props.m_canvas_width);
    const int canvas_height = static_cast<int>(props.m_canvas_height);

    // Compute font scaling factor for the desired pixel height.
    const float scale = stbtt_ScaleForPixelHeight(&font_info, font_height);

    // Compute baseline.
    int ascent;
    stbtt_GetFontVMetrics(&font_info, &ascent, nullptr, nullptr);
    const float baseline = ascent * scale;

    // Bitmap inside which glyphs will be rendered.
    std::vector<std::uint8_t> glyph_bitmap(32 * 32, 0);

    float x = origin_x;
    float y = origin_y;

    for (const char* s = string; *s; ++s)
    {
        // Codepoint to render.
        const char cp = s[0];

        // Handle carriage returns.
        if (cp == '\n')
        {
            x = origin_x;
            y += font_height * line_spacing;
            continue;
        }

        // Compute subpixel shifts.
        const float shift_x = x - std::floor(x);
        const float shift_y = y - std::floor(y);

        // Compute glyph box.
        int glyph_x0, glyph_y0, glyph_x1, glyph_y1;
        stbtt_GetCodepointBitmapBoxSubpixel(
            &font_info,
            cp,
            scale, scale,
            shift_x, shift_y,
            &glyph_x0, &glyph_y0,
            &glyph_x1, &glyph_y1);
        const int glyph_w = glyph_x1 - glyph_x0;
        const int glyph_h = glyph_y1 - glyph_y0;

        // Resize glyph bitmap if necessary.
        const size_t glyph_size = static_cast<size_t>(glyph_w * glyph_h);
        if (glyph_bitmap.size() < glyph_size)
            glyph_bitmap.resize(glyph_size);

        // Render glyph into bitmap.
        stbtt_MakeCodepointBitmapSubpixel(
            &font_info,
            &glyph_bitmap[0],
            glyph_w, glyph_h,
            glyph_w,
            scale, scale,
            shift_x, shift_y,
            cp);

        // Blit glyph bitmap to image.
        for (int j = 0; j < glyph_h; ++j)
        {
            const int iy = truncate<int>(y + j + glyph_y0 + baseline);

            // Vertical clipping.
            if (iy < 0 || iy >= canvas_height)
                continue;

            for (int i = 0; i < glyph_w; ++i)
            {
                const int ix = truncate<int>(x + i + glyph_x0);

                // Horizontal clipping.
                if (ix < 0 || ix >= canvas_width)
                    continue;

                // Compute text opacity.
                const std::uint8_t alpha_uint8 = glyph_bitmap[j * glyph_w + i];
                if (alpha_uint8 == 0)
                    continue;
                const float alpha = alpha_uint8 * (1.0f / 255.0f);

                // Retrieve background color.
                Color4f background_premult;
                image.get_pixel(ix, iy, background_premult);

                // Compute premultiplied text color.
                Color4f color_premult = color;
                color_premult.a *= alpha;
                color_premult.premultiply_in_place();

                // Composite text over background.
                color_premult += background_premult * (1.0f - color_premult.a);

                // Write final color to image.
                image.set_pixel(ix, iy, color_premult);
            }
        }

        // Advance cursor.
        int advance_width;
        stbtt_GetCodepointHMetrics(&font_info, cp, &advance_width, nullptr);
        x += advance_width * scale;

        // Apply kerning.
        const char next_cp = s[1];
        if (next_cp != 0)
        {
            const int kern_advance =
                stbtt_GetCodepointKernAdvance(&font_info, cp, next_cp);
            x += kern_advance * scale;
        }
    }
}

}   // namespace foundation
