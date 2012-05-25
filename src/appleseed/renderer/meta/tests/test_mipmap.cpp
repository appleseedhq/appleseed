
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

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <memory>
#include <string>

using namespace foundation;
using namespace std;

TEST_SUITE(AtomKraft_MipMap)
{
    auto_ptr<Image> create_image(
        const size_t                input_width,
        const size_t                input_height,
        const size_t                channel_count)
    {
        return
            auto_ptr<Image>(
                new Image(
                    input_width,
                    input_height,
                    input_width,
                    input_height,
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
                    image->properties().m_canvas_width,
                    image->properties().m_canvas_height,
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
        const size_t                level,
        const size_t                filter_radius = 2,
        const float                 filter_sharpness = 0.5f)
    {
        const CanvasProperties& input_props = input.properties();

        assert(input_props.m_channel_count == 3 || input_props.m_channel_count == 4);

        const size_t output_width = max(1u, input_props.m_canvas_width >> level);
        const size_t output_height = max(1u, input_props.m_canvas_height >> level);

        auto_ptr<Image> result(
            new Image(
                output_width,
                output_height,
                output_width,
                output_height,
                input_props.m_channel_count,
                input_props.m_pixel_format));

        renderer::TextureObject input_texture(input);
        renderer::TextureObject output_texture(*result.get());

        if (input_props.m_channel_count == 3)
        {
            ak::generate_mipmap_level<3, renderer::TextureObject>(
                output_texture,
                input_texture,
                static_cast<int>(level),
                static_cast<int>(filter_radius),
                filter_sharpness);
        }
        else
        {
            ak::generate_mipmap_level<4, renderer::TextureObject>(
                output_texture,
                input_texture,
                static_cast<int>(level),
                static_cast<int>(filter_radius),
                filter_sharpness);
        }

        return result;
    }

    auto_ptr<Image> generate_mipmap_level_float_clamp_linear_rgba(
        Image&                      input,
        const size_t                level,
        const size_t                filter_radius = 2,
        const float                 filter_sharpness = 0.5f)
    {
        const CanvasProperties& input_props = input.properties();

        assert(input_props.m_channel_count == 4);
        assert(input_props.m_pixel_format == PixelFormatFloat);

        const size_t output_width = max(1u, input_props.m_canvas_width >> level);
        const size_t output_height = max(1u, input_props.m_canvas_height >> level);

        auto_ptr<Image> result(
            new Image(
                output_width,
                output_height,
                output_width,
                output_height,
                4,
                PixelFormatFloat));

        ak::generate_mipmap_level_float_clamp_linear_rgba(
            reinterpret_cast<float*>(result->tile(0, 0).get_storage()),
            reinterpret_cast<const float*>(input.tile(0, 0).get_storage()),
            static_cast<int>(input_props.m_canvas_width),
            static_cast<int>(input_props.m_canvas_height),
            static_cast<int>(level),
            static_cast<int>(filter_radius),
            filter_sharpness);

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

    TEST_CASE(GenerateMipmapLevelFloatClampLinearRGBA_Given1x1Input_LevelIs1_Generates1x1OutputWithIdenticalContent)
    {
        auto_ptr<Image> input(create_image(1, 1, 4));
        input->clear(Color4f(0.5f));

        auto_ptr<Image> output(generate_mipmap_level_float_clamp_linear_rgba(*input.get(), 1));

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

    TEST_CASE(GenerateMipmapLevelFloatClampLinearRGBA_Given8x8BlackInput_LevelIs2_Generates2x2BlackOutput)
    {
        auto_ptr<Image> input(create_image(8, 8, 4));
        input->clear(Color4f(0.0f));

        auto_ptr<Image> output(generate_mipmap_level_float_clamp_linear_rgba(*input.get(), 2));

        Color4f c;
        output->get_pixel(0, 0, c); EXPECT_EQ(Color4f(0.0), c);
        output->get_pixel(1, 0, c); EXPECT_EQ(Color4f(0.0), c);
        output->get_pixel(0, 1, c); EXPECT_EQ(Color4f(0.0), c);
        output->get_pixel(1, 1, c); EXPECT_EQ(Color4f(0.0), c);
    }

    TEST_CASE(GenerateMipmapLevel_WriteMipMapPyramidToDisk)
    {
        auto_ptr<Image> input(read_image("unit tests/inputs/test_mipmap_rgb.exr"));

        for (size_t i = 1; i < 10; ++i)
        {
            auto_ptr<Image> output(generate_mipmap_level(*input.get(), i, 4));

            const string filepath =
                "unit tests/outputs/test_mipmap_rgb_" + to_string(i) + ".png";

            write_image(*output.get(), filepath);
        }
    }

    TEST_CASE(GenerateMipmapLevelFloatClampLinearRGBA_WriteMipMapPyramidToDisk)
    {
        auto_ptr<Image> input(read_image("unit tests/inputs/test_mipmap_rgba.exr"));

        for (size_t i = 1; i < 10; ++i)
        {
            auto_ptr<Image> output(generate_mipmap_level_float_clamp_linear_rgba(*input.get(), i, 4));

            const string filepath =
                "unit tests/outputs/test_mipmap_rgba_" + to_string(i) + ".png";

            write_image(*output.get(), filepath);
        }
    }
}
