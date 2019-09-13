
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
#include "testutils.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/drawing.h"
#include "foundation/image/genericimagefilewriter.h"
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"
#include "foundation/math/aabb.h"
#include "foundation/math/scalar.h"

// Standard headers.
#include <cassert>
#include <fstream>
#include <sstream>

namespace foundation
{

bool load_text_file(const std::string& filename, std::string& contents)
{
    std::ifstream file(filename.c_str());

    if (!file.is_open())
        return false;

    std::stringstream sstr;
    sstr << file.rdbuf();
    contents = sstr.str();

    return true;
}

bool compare_text_files(const std::string& filename1, const std::string& filename2)
{
    std::string contents1;

    if (!load_text_file(filename1, contents1))
        return false;

    std::string contents2;

    if (!load_text_file(filename2, contents2))
        return false;

    return contents1 == contents2;
}

bool are_images_feq(
    const Image&                image1,
    const Image&                image2,
    const float                 eps)
{
    const CanvasProperties& image1_props = image1.properties();

    const size_t width = image1_props.m_canvas_width;
    const size_t height = image1_props.m_canvas_height;
    const size_t channel_count = image1_props.m_channel_count;

    if (image2.properties().m_canvas_width != width ||
        image2.properties().m_canvas_height != height ||
        image2.properties().m_channel_count != channel_count)
        return false;

    assert(channel_count <= 4);

    for (size_t y = 0; y < height; ++y)
    {
        for (size_t x = 0; x < width; ++x)
        {
            float color1[4];
            image1.get_pixel(x, y, color1, channel_count);

            float color2[4];
            image2.get_pixel(x, y, color2, channel_count);

            for (size_t c = 0; c < channel_count; ++c)
            {
                if (!feq(color1[c], color2[c], eps))
                    return false;
            }
        }
    }

    return true;
}

void fit_point_cloud_to_image(
    std::vector<Vector2d>&           points)
{
    AABB2d aabb;
    aabb.invalidate();

    for (const Vector2d& p : points)
        aabb.insert(p);

    const Vector2d aabb_extent = aabb.extent();
    const double scale = 0.8 / max_value(aabb_extent);

    for (Vector2d& p : points)
    {
        p -= aabb.min;
        p *= scale;
        p += (Vector2d(1.0) - aabb_extent * scale) * 0.5;
    }
}

void write_point_cloud_image(
    const std::string&               image_path,
    const size_t                     image_width,
    const size_t                     image_height,
    const std::vector<Vector2d>&     points)
{
    Image image(
        image_width,
        image_height,
        32,
        32,
        4,
        PixelFormatFloat);

    image.clear(Color4f(0.0f, 0.0f, 0.0f, 1.0f));

    for (size_t i = 0; i < points.size(); ++i)
        Drawing::draw_dot(image, points[i], Color4f(1.0f));

    GenericImageFileWriter writer(image_path.c_str());
    writer.append_image(&image);
    writer.write();
}

void write_point_cloud_image(
    const std::string&               image_path,
    const std::vector<Vector2d>&     points)
{
    write_point_cloud_image(
        image_path,
        512,
        512,
        points);
}

}   // namespace foundation
