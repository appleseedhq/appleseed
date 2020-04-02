
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

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/genericimagefilereader.h"
#include "foundation/image/genericimagefilewriter.h"
#include "foundation/image/image.h"
#include "foundation/image/imageattributes.h"
#include "foundation/image/pixel.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/math/scalar.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>
#include <memory>

using namespace foundation;

TEST_SUITE(Foundation_Image_GenericImageFileWriter)
{
    TEST_CASE(Write_CorrectlyWritesImagePixels)
    {
        const char* ImageFilePath = "unit tests/outputs/test_genericimagefilewriter_pixels.exr";

        {
            Image image(2, 2, 2, 2, 4, PixelFormatFloat);
            image.clear(Color4b(50, 100, 150, 42));

            GenericImageFileWriter writer(ImageFilePath);
            writer.append_image(&image);
            writer.write();
        }

        {
            GenericImageFileReader reader;
            std::unique_ptr<Image> image(reader.read(ImageFilePath));

            for (size_t y = 0; y < 2; ++y)
            {
                for (size_t x = 0; x < 2; ++x)
                {
                    Color4b c;
                    image->get_pixel(x, y, c);
                    EXPECT_EQ(Color4b(50, 100, 150, 42), c);
                }
            }
        }
    }

    TEST_CASE(Write_CorrectlyWritesImageAttributes)
    {
        const char* ImageFilePath = "unit tests/outputs/test_genericimagefilewriter_attributes.exr";

        {
            Image image(2, 2, 2, 2, 4, PixelFormatFloat);
            image.clear(Color4b(0));

            ImageAttributes attrs;
            attrs.insert("appleseed:test:StringAttr", "something");
            attrs.insert("appleseed:test:StringButIntAttr", "47");
            attrs.insert("appleseed:test:StringButFloatAttr", "47.5");
            attrs.insert("appleseed:test:FloatAttr", 32.0f);
            attrs.insert("appleseed:test:DoubleAttr", 32.0);
            attrs.insert("appleseed:test:IntAttr", 32);
            attrs.insert("appleseed:test:UnsignedIntAttr", static_cast<size_t>(32));

            GenericImageFileWriter writer(ImageFilePath);
            writer.append_image(&image);
            writer.set_image_attributes(attrs);
            writer.write();
        }

        {
            ImageAttributes attrs;

            GenericImageFileReader reader;
            std::unique_ptr<Image> image(reader.read(ImageFilePath, &attrs));

            EXPECT_EQ(std::string("something"), attrs.get<std::string>("appleseed:test:StringAttr"));
            EXPECT_EQ(47, attrs.get<int>("appleseed:test:StringButIntAttr"));
            EXPECT_EQ(47.5f, attrs.get<float>("appleseed:test:StringButFloatAttr"));
            EXPECT_EQ(32.0f, attrs.get<float>("appleseed:test:FloatAttr"));
            EXPECT_EQ(32.0, attrs.get<double>("appleseed:test:DoubleAttr"));
            EXPECT_EQ(32, attrs.get<int>("appleseed:test:IntAttr"));
            EXPECT_EQ(static_cast<size_t>(32), attrs.get<size_t>("appleseed:test:UnsignedIntAttr"));
        }
    }

    void draw_radial_gradient_prone_to_banding(Image& image)
    {
        const CanvasProperties& props = image.properties();

        // The image must be square.
        assert(props.m_canvas_width == props.m_canvas_height);
        const size_t image_size = props.m_canvas_width;

        const float half_image_size = static_cast<float>(image_size) / 2.0f;
        const float half_diagonal_length = half_image_size * std::sqrt(2.0f);

        MersenneTwister rng;

        for (size_t y = 0; y < image_size; ++y)
        {
            for (size_t x = 0; x < image_size; ++x)
            {
                const float dx = std::abs(static_cast<float>(x) - half_image_size);
                const float dy = std::abs(static_cast<float>(y) - half_image_size);
                const float d = std::sqrt(dx * dx + dy * dy);
                const float nd = saturate(d / half_diagonal_length);
                const float c = fit(nd, 0.0f, 1.0f, 0.6f, 0.5f);
                image.set_pixel(x, y, Color3f(c));
            }
        }
    }

    TEST_CASE(Write_DitherAttributeIsNotSet_PNGFileExhibitsBanding)
    {
        const size_t ImageSize = 512;
        Image image(ImageSize, ImageSize, ImageSize, ImageSize, 3, PixelFormatFloat);

        draw_radial_gradient_prone_to_banding(image);

        GenericImageFileWriter writer("unit tests/outputs/test_genericimagefilewriter_dithering_off.png");
        writer.append_image(&image);
        writer.write();
    }

    TEST_CASE(Write_DitherAttributeIsSet_PNGFileDoesNotExhibitBanding)
    {
        const size_t ImageSize = 512;
        Image image(ImageSize, ImageSize, ImageSize, ImageSize, 3, PixelFormatFloat);

        draw_radial_gradient_prone_to_banding(image);

        ImageAttributes attrs;
        attrs.insert("dither", 42); // the value of the dither attribute is a hash seed

        GenericImageFileWriter writer("unit tests/outputs/test_genericimagefilewriter_dithering_on.png");
        writer.append_image(&image);
        writer.set_image_attributes(attrs);
        writer.write();
    }
}
