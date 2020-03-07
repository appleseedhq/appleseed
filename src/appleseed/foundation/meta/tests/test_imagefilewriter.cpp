//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019-2020 Chin-Ying Li, The appleseedhq Organization
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
#include "foundation/image/colorspace.h"
#include "foundation/image/genericimagefilereader.h"
#include "foundation/image/genericimagefilewriter.h"
#include "foundation/image/image.h"
#include "foundation/image/imageattributes.h"
#include "foundation/image/pixel.h"
#include "foundation/math/scalar.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>
#include <cstring>
#include <exception>
#include <memory>

using namespace foundation;

TEST_SUITE(Foundation_Image_ImageFileWriter)
{
    template <typename Color>
    void write_image(const Color fill_color, const PixelFormat pixel_format, const std::size_t image_size, const std::string image_file_path)
    {
        std::unique_ptr<Image> image(new Image(image_size, image_size, image_size, image_size, fill_color.Components, pixel_format));
        image->clear(fill_color);
        
        GenericImageFileWriter writer(image_file_path.c_str());
        writer.append_image(image.get());
        writer.write();
    }
    
    TEST_CASE(WriteBMP_CorrectlyWritesImagePixels)
    {
        const std::string image_file_path = "unit tests/outputs/test_imagefilewriter.bmp";
        const std::size_t image_size = 16;
        const Color4b fill_color(50, 100, 150, 42);
        
        write_image<Color4b>(fill_color, PixelFormatUInt8, image_size, image_file_path);

        GenericImageFileReader reader;
        std::unique_ptr<Image> image(reader.read(image_file_path.c_str()));
            
        for (std::size_t y = 0; y < image_size; ++y)
        {
            for (std::size_t x = 0; x < image_size; ++x)
            {
                Color4b c;
                image->get_pixel(x, y, c);
                EXPECT_EQ(fill_color, c);
            }
        }
    }
    
    TEST_CASE(WriteEXR_CorrectlyWritesImagePixels)
    {
        const std::string image_file_path = "unit tests/outputs/test_imagefilewriter.exr";
        const std::size_t image_size = 16;
        const Color4b fill_color(50, 100, 150, 42);
                        
        write_image<Color4b>(fill_color, PixelFormatUInt8, image_size, image_file_path);

        GenericImageFileReader reader;
        std::unique_ptr<Image> image(reader.read(image_file_path.c_str()));
            
        for (std::size_t y = 0; y < image_size; ++y)
        {
            for (std::size_t x = 0; x < image_size; ++x)
            {
                Color4b c;
                image->get_pixel(x, y, c);
                EXPECT_EQ(fill_color, c);
            }
        }
    }
    
    TEST_CASE(WriteHDR_CorrectlyWritesImagePixels)
    {
        const std::string image_file_path = "unit tests/outputs/test_imagefilewriter.hdr";
        const std::size_t image_size = 16;
        const Color3b fill_color(50, 100, 150);
                
        write_image<Color3b>(fill_color, PixelFormatUInt8, image_size, image_file_path);

        GenericImageFileReader reader;
        std::unique_ptr<Image> image(reader.read(image_file_path.c_str()));
            
        for (std::size_t y = 0; y < image_size; ++y)
        {
            for (std::size_t x = 0; x < image_size; ++x)
            {
                Color3b c;
                image->get_pixel(x, y, c);
                EXPECT_EQ(fill_color, c);
            }
        }
    }
    
    TEST_CASE(WriteJPG_ApproximatelyWritesImagePixels)
    {
        const std::string image_file_path = "unit tests/outputs/test_imagefilewriter.jpg";
        const std::size_t image_size = 16;
        const Color3b fill_color(50, 100, 150);
                
        write_image<Color3b>(fill_color, PixelFormatUInt8, image_size, image_file_path);

        GenericImageFileReader reader;
        std::unique_ptr<Image> image(reader.read(image_file_path.c_str()));
            
        for (std::size_t y = 0; y < image_size; ++y)
        {
            for (std::size_t x = 0; x < image_size; ++x)
            {
                Color3b c;
                image->get_pixel(x, y, c);
                
                // the actual written and expected pixel value may be off by 1 in JPG format
                for (std::size_t i = 0; i < 3; ++i)
                {
                    int difference = std::min(c[i]-fill_color[i], fill_color[i] - c[i]);
                    EXPECT_LT(difference, 2);
                }
            }
        }
    }
    
    TEST_CASE(WritePNG_CorrectlyWritesImagePixels)
    {
        const std::string image_file_path = "unit tests/outputs/test_imagefilewriter.png";
        const std::size_t image_size = 16;
        const Color3b fill_color(50, 100, 150);
                
        write_image<Color3b>(fill_color, PixelFormatUInt8, image_size, image_file_path);

        GenericImageFileReader reader;
        std::unique_ptr<Image> image(reader.read(image_file_path.c_str()));
            
        for (std::size_t y = 0; y < image_size; ++y)
        {
            for (std::size_t x = 0; x < image_size; ++x)
            {
                Color3b c;
                image->get_pixel(x, y, c);
                EXPECT_EQ(fill_color, c);
            }
        }
    }
    
    TEST_CASE(WriteTIFF_CorrectlyWritesImagePixels)
    {
        const std::string image_file_path = "unit tests/outputs/test_imagefilewriter.tif";
        const std::size_t image_size = 16;
        const Color4b fill_color(50, 100, 150, 42);
                        
        write_image<Color4b>(fill_color, PixelFormatUInt8, image_size, image_file_path);

        GenericImageFileReader reader;
        std::unique_ptr<Image> image(reader.read(image_file_path.c_str()));
            
        for (std::size_t y = 0; y < image_size; ++y)
        {
            for (std::size_t x = 0; x < image_size; ++x)
            {
                Color4b c;
                image->get_pixel(x, y, c);
                EXPECT_EQ(fill_color, c);
            }
        }
    }
}
