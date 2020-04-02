
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Francois Beaune, The appleseedhq Organization
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
#include "conversion.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/image.h"
#include "foundation/string/string.h"

// Standard headers.
#include <cassert>
#include <cstddef>

namespace bf = boost::filesystem;

namespace foundation
{

bool is_linear_image_file_format(const bf::path& path)
{
    const std::string extension = lower_case(path.extension().string());

    return
        extension == ".exr"  ||
        extension == ".tiff" ||
        extension == ".tif"  ||
        extension == ".hdr";
}

void convert_srgb_to_linear_rgb(Tile& tile)
{
    assert(tile.get_channel_count() == 3 || tile.get_channel_count() == 4);

    if (tile.get_channel_count() == 3)
    {
        for (size_t i = 0, e = tile.get_pixel_count(); i < e; ++i)
        {
            Color3f color;
            tile.get_pixel(i, color);

            color = fast_srgb_to_linear_rgb(color);
            color = saturate(color);

            tile.set_pixel(i, color);
        }
    }
    else if (tile.get_channel_count() == 4)
    {
        for (size_t i = 0, e = tile.get_pixel_count(); i < e; ++i)
        {
            Color4f color;
            tile.get_pixel(i, color);

            color.unpremultiply_in_place();
            color.rgb() = fast_srgb_to_linear_rgb(color.rgb());
            color = saturate(color);
            color.premultiply_in_place();

            tile.set_pixel(i, color);
        }
    }
}

void convert_srgb_to_linear_rgb(Image& image)
{
    const CanvasProperties& props = image.properties();

    for (size_t y = 0; y < props.m_tile_count_y; ++y)
    {
        for (size_t x = 0; x < props.m_tile_count_x; ++x)
            convert_srgb_to_linear_rgb(image.tile(x, y));
    }
}

void convert_linear_rgb_to_srgb(Tile& tile)
{
    assert(tile.get_channel_count() == 3 || tile.get_channel_count() == 4);

    if (tile.get_channel_count() == 3)
    {
        for (size_t i = 0, e = tile.get_pixel_count(); i < e; ++i)
        {
            Color3f color;
            tile.get_pixel(i, color);

            color = fast_linear_rgb_to_srgb(color);
            color = saturate(color);

            tile.set_pixel(i, color);
        }
    }
    else if (tile.get_channel_count() == 4)
    {
        for (size_t i = 0, e = tile.get_pixel_count(); i < e; ++i)
        {
            Color4f color;
            tile.get_pixel(i, color);

            color.unpremultiply_in_place();
            color.rgb() = fast_linear_rgb_to_srgb(color.rgb());
            color = saturate(color);
            color.premultiply_in_place();

            tile.set_pixel(i, color);
        }
    }
}

void convert_linear_rgb_to_srgb(Image& image)
{
    const CanvasProperties& props = image.properties();

    for (size_t y = 0; y < props.m_tile_count_y; ++y)
    {
        for (size_t x = 0; x < props.m_tile_count_x; ++x)
            convert_linear_rgb_to_srgb(image.tile(x, y));
    }
}

}   // namespace foundation
