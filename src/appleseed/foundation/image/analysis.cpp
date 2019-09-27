
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
#include "analysis.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/math/scalar.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>

namespace foundation
{

namespace
{
    void accumulate_luminance(
        const Tile&     tile,
        double&         accumulated_luminance,
        size_t&         relevant_pixel_count)
    {
        assert(tile.get_channel_count() == 4);

        const size_t tile_width = tile.get_width();
        const size_t tile_height = tile.get_height();

        for (size_t y = 0; y < tile_height; ++y)
        {
            for (size_t x = 0; x < tile_width; ++x)
            {
                // Fetch the pixel color; assume linear RGBA.
                Color4f linear_rgba;
                tile.get_pixel(x, y, linear_rgba);

                // Extract the RGB part (ignore the alpha channel).
                const Color3f linear_rgb = linear_rgba.rgb();

                // Skip pixels containing NaN values.
                if (has_nan(linear_rgb))
                    continue;

                // Compute the Rec. 709 relative luminance of this pixel.
                const float lum = luminance(clamp_low(linear_rgb, 0.0f));

                // It should no longer be possible to have NaN at this point.
                assert(lum == lum);

                accumulated_luminance += static_cast<double>(lum);
                ++relevant_pixel_count;
            }
        }
    }

    void accumulate_luminance(
        const Image&    image,
        double&         accumulated_luminance,
        size_t&         relevant_pixel_count)
    {
        accumulated_luminance = 0.0;
        relevant_pixel_count = 0;

        const CanvasProperties& props = image.properties();

        for (size_t ty = 0; ty < props.m_tile_count_y; ++ty)
        {
            for (size_t tx = 0; tx < props.m_tile_count_x; ++tx)
            {
                const Tile& tile = image.tile(tx, ty);
                accumulate_luminance(tile, accumulated_luminance, relevant_pixel_count);
            }
        }
    }
}

double compute_average_luminance(const Image& image)
{
    double accumulated_luminance;
    size_t relevant_pixel_count;

    accumulate_luminance(image, accumulated_luminance, relevant_pixel_count);

    return relevant_pixel_count > 0
        ? accumulated_luminance / relevant_pixel_count
        : 0.0;
}

bool are_images_compatible(const Image& image1, const Image& image2)
{
    const CanvasProperties& props1 = image1.properties();
    const CanvasProperties& props2 = image2.properties();

    return
        props1.m_canvas_width == props2.m_canvas_width &&
        props1.m_canvas_height == props2.m_canvas_height;
}

double compute_rms_deviation(const Image& image1, const Image& image2)
{
    if (!are_images_compatible(image1, image2))
        throw ExceptionIncompatibleImages();

    const CanvasProperties& props = image1.properties();
    double mse = 0.0;   // mean square error

    for (size_t y = 0; y < props.m_canvas_height; ++y)
    {
        for (size_t x = 0; x < props.m_canvas_width; ++x)
        {
            Color3f color1;
            image1.get_pixel(x, y, color1);

            Color3f color2;
            image2.get_pixel(x, y, color2);

            mse += square_distance(color1, color2);
        }
    }

    mse /= props.m_pixel_count * 3.0;

    return std::sqrt(mse);
}

}   // namespace foundation
