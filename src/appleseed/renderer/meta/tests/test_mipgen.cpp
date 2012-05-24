
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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

// appleseed.renderer headers.
#include "renderer/kernel/texturing/mipgen.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/genericimagefilereader.h"
#include "foundation/image/genericimagefilewriter.h"
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/string.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <algorithm>
#include <cstddef>
#include <memory>
#include <string>

using namespace foundation;
using namespace std;

TEST_SUITE(MipmapGenerationExploration)
{
    auto_ptr<Image> copy_image(
        const Image&        source,
        const size_t        new_tile_width,
        const size_t        new_tile_height,
        const PixelFormat   new_pixel_format)
    {
        const CanvasProperties& props = source.properties();

        auto_ptr<Image> result(
            new Image(
                props.m_canvas_width,
                props.m_canvas_height,
                new_tile_width,
                new_tile_height,
                props.m_channel_count,
                new_pixel_format));

        for (size_t y = 0; y < props.m_canvas_height; ++y)
        {
            for (size_t x = 0; x < props.m_canvas_width; ++x)
            {
                Pixel::convert(
                    props.m_pixel_format,
                    source.pixel(x, y),
                    source.pixel(x, y) + props.m_pixel_size,
                    1,
                    new_pixel_format,
                    result->pixel(x, y),
                    1);
            }
        }

        return result;
    }

    auto_ptr<Image> create_image(
        const size_t        input_width,
        const size_t        input_height,
        const size_t        channel_count)
    {
        return
            auto_ptr<Image>(
                new Image(
                    input_width,
                    input_height,
                    input_width,
                    input_height,
                    3,
                    PixelFormatFloat));
    }

    auto_ptr<Image> read_image(const string& filepath)
    {
        GenericImageFileReader reader;
        auto_ptr<Image> image(reader.read(filepath.c_str()));

        const CanvasProperties& props = image->properties();

        return
            copy_image(
                *image.get(),
                props.m_canvas_width,
                props.m_canvas_height,
                PixelFormatFloat);
    }

    void write_image(
        const Image&        image,
        const string&       filepath)
    {
        GenericImageFileWriter writer;
        writer.write(filepath.c_str(), image);
    }

    auto_ptr<Image> generate_mipmap_level(
        const Image&        source,
        const size_t        level,
        const float         filter_radius = 2.0f,
        const float         filter_sharpness = 0.5f)
    {
        const CanvasProperties& source_props = source.properties();

        const size_t output_width = max(1u, source_props.m_canvas_width >> level);
        const size_t output_height = max(1u, source_props.m_canvas_height >> level);

        auto_ptr<Image> result(
            new Image(
                output_width,
                output_height,
                output_width,
                output_height,
                source_props.m_channel_count,
                PixelFormatFloat));

        ::generate_mipmap_level(
            reinterpret_cast<float*>(result->tile(0, 0).get_storage()),
            reinterpret_cast<const float*>(source.tile(0, 0).get_storage()),
            static_cast<int>(source_props.m_canvas_width),
            static_cast<int>(source_props.m_canvas_height),
            static_cast<int>(source_props.m_channel_count),
            static_cast<int>(level));

        return result;
    }

    #define EXPECT_IMAGE_EQ(expected, actual)                                                   \
        EXPECT_SEQUENCE_EQ(                                                                     \
            (expected).properties().m_pixel_count * (expected).properties().m_channel_count,    \
            reinterpret_cast<const float*>((expected).tile(0, 0).get_storage()),                \
            reinterpret_cast<const float*>((actual).tile(0, 0).get_storage()))

    TEST_CASE(ExpectImageEq_GivenIdenticalImages_ReturnsTrue)
    {
        auto_ptr<Image> input(create_image(4, 4, 3));
        input->clear(Color3f(0.5f));

        EXPECT_IMAGE_EQ(*input.get(), *input.get());
    }

    TEST_CASE(GenerateMipmapLevel_Given1x1Input_LevelIs1_Generates1x1OutputWithIdenticalContent)
    {
        auto_ptr<Image> input(create_image(1, 1, 3));
        input->clear(Color3f(0.5f));

        auto_ptr<Image> output(generate_mipmap_level(*input.get(), 1));

        EXPECT_IMAGE_EQ(*input.get(), *output.get());
    }

    #undef EXPECT_IMAGE_EQ

    TEST_CASE(GenerateMipmapLevel_Given8x8BlackInput_LevelIs2_Generates2x2BlackOutput)
    {
        auto_ptr<Image> input(create_image(8, 8, 3));
        input->clear(Color3f(0.0f));

        auto_ptr<Image> output(generate_mipmap_level(*input.get(), 2));

        Color3f c;
        output->get_pixel(0, 0, c); EXPECT_EQ(Color3f(0.0), c);
        output->get_pixel(1, 0, c); EXPECT_EQ(Color3f(0.0), c);
        output->get_pixel(0, 1, c); EXPECT_EQ(Color3f(0.0), c);
        output->get_pixel(1, 1, c); EXPECT_EQ(Color3f(0.0), c);
    }

    TEST_CASE(GenerateMipmapLevel_WritesMipMapPyramidToDisk)
    {
        auto_ptr<Image> input(read_image("unit tests/inputs/test_mipmapgenerationexploration.exr"));

        for (size_t i = 1; i < 10; ++i)
        {
            auto_ptr<Image> output(generate_mipmap_level(*input.get(), i));

            const string filepath =
                "unit tests/outputs/test_mipmapgenerationexploration_" + to_string(i) + ".png";

            write_image(*output.get(), filepath);
        }
    }
}
