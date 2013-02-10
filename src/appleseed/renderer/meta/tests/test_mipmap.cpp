
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include "renderer/kernel/atomkraft/mipmap.h"
#include "renderer/kernel/atomkraft/textureobject.h"

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
#include "foundation/utility/testutils.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <memory>
#include <string>

using namespace foundation;
using namespace renderer;
using namespace std;

TEST_SUITE(AtomKraft_MipMap)
{
    auto_ptr<Image> create_image(
        const size_t                image_width,
        const size_t                image_height,
        const size_t                tile_width,
        const size_t                tile_height,
        const size_t                channel_count)
    {
        return
            auto_ptr<Image>(
                new Image(
                    image_width,
                    image_height,
                    tile_width,
                    tile_height,
                    channel_count,
                    PixelFormatFloat));
    }

    auto_ptr<Image> read_image(const string& filepath)
    {
        GenericImageFileReader reader;
        auto_ptr<Image> image(reader.read(filepath.c_str()));

        return
            auto_ptr<Image>(
                new Image(
                    *image.get(),
                    image->properties().m_tile_width,
                    image->properties().m_tile_height,
                    PixelFormatFloat));
    }

    void write_image(
        const Image&                image,
        const string&               filepath)
    {
        GenericImageFileWriter writer;
        writer.write(filepath.c_str(), image);
    }

    auto_ptr<Image> generate_mipmap_level(
        Image&                      input,
        const size_t                output_tile_width,
        const size_t                output_tile_height,
        const size_t                level,
        const size_t                filter_radius = 2,
        const float                 filter_sharpness = 0.5f)
    {
        const CanvasProperties& input_props = input.properties();

        assert(input_props.m_channel_count == 3 || input_props.m_channel_count == 4);

        auto_ptr<Image> output(
            new Image(
                max<size_t>(input_props.m_canvas_width >> level, 1),
                max<size_t>(input_props.m_canvas_height >> level, 1),
                output_tile_width,
                output_tile_height,
                input_props.m_channel_count,
                input_props.m_pixel_format));

        TiledTextureObject input_texture(input);
        TiledTextureObject output_texture(*output.get());

        if (input_props.m_channel_count == 3)
        {
            for (int ty = 0; ty < output_texture.tile_count_y(); ++ty)
            {
                for (int tx = 0; tx < output_texture.tile_count_x(); ++tx)
                {
                    ak::generate_mipmap_level<3, TiledTextureObject>(
                        output_texture,
                        input_texture,
                        static_cast<int>(level),
                        tx,
                        ty,
                        static_cast<int>(filter_radius),
                        filter_sharpness);
                }
            }
        }
        else
        {
            for (int ty = 0; ty < output_texture.tile_count_y(); ++ty)
            {
                for (int tx = 0; tx < output_texture.tile_count_x(); ++tx)
                {
                    ak::generate_mipmap_level<4, TiledTextureObject>(
                        output_texture,
                        input_texture,
                        static_cast<int>(level),
                        tx,
                        ty,
                        static_cast<int>(filter_radius),
                        filter_sharpness);
                }
            }
        }

        return output;
    }

    auto_ptr<Image> generate_mipmap_level_float_clamp_linear_rgba(
        Image&                      input,
        const size_t                output_tile_width,
        const size_t                output_tile_height,
        const size_t                level,
        const size_t                filter_radius = 2,
        const float                 filter_sharpness = 0.5f)
    {
        const CanvasProperties& input_props = input.properties();

        assert(input_props.m_channel_count == 4);
        assert(input_props.m_pixel_format == PixelFormatFloat);

        auto_ptr<Image> output(
            new Image(
                max<size_t>(1, input_props.m_canvas_width >> level),
                max<size_t>(1, input_props.m_canvas_height >> level),
                output_tile_width,
                output_tile_height,
                4,
                PixelFormatFloat));

        TiledTextureObject input_texture(input);
        TiledTextureObject output_texture(*output.get());

        for (int ty = 0; ty < output_texture.tile_count_y(); ++ty)
        {
            for (int tx = 0; tx < output_texture.tile_count_x(); ++tx)
            {
                ak::generate_mipmap_level_float_clamp_linear_rgba(
                    output_texture,
                    input_texture,
                    static_cast<int>(level),
                    tx,
                    ty,
                    static_cast<int>(filter_radius),
                    filter_sharpness);
            }
        }

        return output;
    }

#define EXPECT_IMAGE_EQ(expected, actual)                                                   \
    EXPECT_SEQUENCE_EQ(                                                                     \
        (expected).properties().m_pixel_count * (expected).properties().m_channel_count,    \
        reinterpret_cast<const float*>((expected).tile(0, 0).get_storage()),                \
        reinterpret_cast<const float*>((actual).tile(0, 0).get_storage()))

    TEST_CASE(ExpectImageEq_GivenIdenticalImages_ReturnsTrue)
    {
        auto_ptr<Image> input(create_image(4, 4, 32, 32, 3));
        input->clear(Color3f(0.5f));

        EXPECT_IMAGE_EQ(*input.get(), *input.get());
    }

    TEST_CASE(GenerateMipmapLevel_Given1x1Input_LevelIs1_Generates1x1OutputWithIdenticalContent)
    {
        auto_ptr<Image> input(create_image(1, 1, 32, 32, 3));
        input->clear(Color3f(0.5f));

        auto_ptr<Image> output(generate_mipmap_level(*input.get(), 32, 32, 1));

        EXPECT_IMAGE_EQ(*input.get(), *output.get());
    }

    TEST_CASE(GenerateMipmapLevelFloatClampLinearRGBA_Given1x1Input_LevelIs1_Generates1x1OutputWithIdenticalContent)
    {
        auto_ptr<Image> input(create_image(1, 1, 32, 32, 4));
        input->clear(Color4f(0.5f));

        auto_ptr<Image> output(generate_mipmap_level_float_clamp_linear_rgba(*input.get(), 32, 32, 1));

        EXPECT_IMAGE_EQ(*input.get(), *output.get());
    }

#undef EXPECT_IMAGE_EQ

    TEST_CASE(GenerateMipmapLevel_Given8x8BlackInput_LevelIs2_Generates2x2BlackOutput)
    {
        auto_ptr<Image> input(create_image(8, 8, 32, 32, 3));
        input->clear(Color3f(0.0f));

        auto_ptr<Image> output(generate_mipmap_level(*input.get(), 32, 32, 2));

        Color3f c;
        output->get_pixel(0, 0, c); EXPECT_EQ(Color3f(0.0), c);
        output->get_pixel(1, 0, c); EXPECT_EQ(Color3f(0.0), c);
        output->get_pixel(0, 1, c); EXPECT_EQ(Color3f(0.0), c);
        output->get_pixel(1, 1, c); EXPECT_EQ(Color3f(0.0), c);
    }

    TEST_CASE(GenerateMipmapLevelFloatClampLinearRGBA_Given8x8BlackInput_LevelIs2_Generates2x2BlackOutput)
    {
        auto_ptr<Image> input(create_image(8, 8, 32, 32, 4));
        input->clear(Color4f(0.0f));

        auto_ptr<Image> output(generate_mipmap_level_float_clamp_linear_rgba(*input.get(), 32, 32, 2));

        Color4f c;
        output->get_pixel(0, 0, c); EXPECT_EQ(Color4f(0.0), c);
        output->get_pixel(1, 0, c); EXPECT_EQ(Color4f(0.0), c);
        output->get_pixel(0, 1, c); EXPECT_EQ(Color4f(0.0), c);
        output->get_pixel(1, 1, c); EXPECT_EQ(Color4f(0.0), c);
    }

    TEST_CASE(GenerateMipmapLevel_InputAndOutputTilesDimensionsAreDifferent)
    {
        auto_ptr<Image> input(read_image("unit tests/inputs/test_mipmap_rgb.exr"));
        auto_ptr<Image> expected_level1(read_image("unit tests/inputs/test_mipmap_rgb_expected_level1.exr"));

        auto_ptr<Image> output(generate_mipmap_level(*input.get(), 17, 17, 1, 8));

        EXPECT_TRUE(are_images_feq(*expected_level1.get(), *output.get(), 1.0e-3f));
    }

    TEST_CASE(GenerateMipmapLevel_InputIsRectangularWithPrimeDimensions)
    {
        auto_ptr<Image> input(read_image("unit tests/inputs/test_mipmap_rgb_prime_rectangle.exr"));
        auto_ptr<Image> expected_level1(read_image("unit tests/inputs/test_mipmap_rgb_prime_rectangle_expected_level1.exr"));

        auto_ptr<Image> output(generate_mipmap_level(*input.get(), 32, 32, 1, 8));

        EXPECT_TRUE(are_images_feq(*expected_level1.get(), *output.get(), 1.0e-3f));
    }

    TEST_CASE(GenerateMipmapLevel_WriteMipMapPyramidToDisk)
    {
        auto_ptr<Image> input(read_image("unit tests/inputs/test_mipmap_rgb.exr"));
        auto_ptr<Image> expected_level1(read_image("unit tests/inputs/test_mipmap_rgb_expected_level1.exr"));

        for (size_t i = 1; i < 10; ++i)
        {
            auto_ptr<Image> output(generate_mipmap_level(*input.get(), 32, 32, i, 8));

            if (i == 1)
                EXPECT_TRUE(are_images_feq(*expected_level1.get(), *output.get(), 1.0e-3f));

            write_image(*output.get(), "unit tests/outputs/test_mipmap_rgb_" + to_string(i) + ".png");
        }
    }

    TEST_CASE(GenerateMipmapLevelFloatClampLinearRGBA_WriteMipMapPyramidToDisk)
    {
        auto_ptr<Image> input(read_image("unit tests/inputs/test_mipmap_rgba.exr"));
        auto_ptr<Image> expected_level1(read_image("unit tests/inputs/test_mipmap_rgba_expected_level1.exr"));

        for (size_t i = 1; i < 10; ++i)
        {
            auto_ptr<Image> output(generate_mipmap_level_float_clamp_linear_rgba(*input.get(), 32, 32, i, 8));

            if (i == 1)
                EXPECT_TRUE(are_images_feq(*expected_level1.get(), *output.get(), 1.0e-3f));

            write_image(*output.get(), "unit tests/outputs/test_mipmap_rgba_" + to_string(i) + ".png");
        }
    }
}
