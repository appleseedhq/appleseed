
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2018 Francois Beaune, The appleseedhq Organization
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
#include "renderer/kernel/rendering/localsampleaccumulationbuffer.h"

// appleseed.foundation headers.
#include "foundation/image/accumulatortile.h"
#include "foundation/image/color.h"
#include "foundation/image/tile.h"
#include "foundation/math/aabb.h"
#include "foundation/math/rng/distribution.h"
#include "foundation/math/rng/mersennetwister.h"
#include "foundation/math/vector.h"
#include "foundation/utility/job.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <cstdint>

using namespace foundation;
using namespace renderer;

TEST_SUITE(Renderer_Kernel_Rendering_LocalSampleAccumulationBuffer)
{
    bool honors_crop_window(const AABB2u& crop_window)
    {
        // A full low resolution framebuffer.
        AccumulatorTile level(64, 64, 4);
        level.clear();

        for (size_t y = 0; y < level.get_height(); ++y)
        {
            for (size_t x = 0; x < level.get_width(); ++x)
            {
                static const float values[4] = { 1.0f, 1.0f, 1.0f, 1.0f };
                level.add(Vector2u(x, y), values);
            }
        }

        // A tile of the high resolution framebuffer.
        Tile color_tile(32, 32, 4, PixelFormatFloat);

        color_tile.clear(Color4f(0.0f));

        const AABB2u tile_rect(
            Vector2u(0, 0),
            Vector2u(color_tile.get_width() - 1, color_tile.get_height() - 1));

        const AABB2u rect = AABB2u::intersect(tile_rect, crop_window);

        // Develop the low resolution framebuffer to one tile of the high resolution framebuffer.
        LocalSampleAccumulationBuffer::develop_to_tile(
            color_tile,
            256, 256,
            level,
            0, 0,
            rect);

        // Check the contents of the high resolution framebuffer tile.
        for (size_t y = 0; y < color_tile.get_height(); ++y)
        {
            for (size_t x = 0; x < color_tile.get_width(); ++x)
            {
                Color4f color;
                color_tile.get_pixel(x, y, color);

                const float expected = rect.contains(Vector2u(x, y)) ? 1.0f : 0.0f;

                if (color[0] != expected ||
                    color[1] != expected ||
                    color[2] != expected ||
                    color[3] != expected)
                    return false;
            }
        }

        return true;
    }

    TEST_CASE(DevelopToTile_CropWindowIsFullFrame_HonorsCropWindow)
    {
        EXPECT_TRUE(honors_crop_window(AABB2u(Vector2u(0, 0), Vector2u(255, 255))));
    }

    TEST_CASE(DevelopToTile_CropWindowIsSmall_HonorsCropWindow)
    {
        EXPECT_TRUE(honors_crop_window(AABB2u(Vector2u(5, 3), Vector2u(6, 3))));
    }

    TEST_CASE(DevelopToTile_StressTest)
    {
        MersenneTwister rng;

        for (size_t i = 0; i < 100; ++i)
        {
            const std::int32_t min_x = rand_int1(rng, 0, 31);
            const std::int32_t min_y = rand_int1(rng, 0, 31);
            const std::int32_t max_x = min_x + rand_int1(rng, 0, 31 - min_x);
            const std::int32_t max_y = min_y + rand_int1(rng, 0, 31 - min_y);

            const AABB2u crop_window(
                Vector2u(
                    static_cast<size_t>(min_x),
                    static_cast<size_t>(min_y)),
                Vector2u(
                    static_cast<size_t>(max_x),
                    static_cast<size_t>(max_y)));

            EXPECT_TRUE(honors_crop_window(crop_window));
        }
    }
}
