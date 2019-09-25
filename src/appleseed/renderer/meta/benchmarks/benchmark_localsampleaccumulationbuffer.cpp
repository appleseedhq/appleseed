
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2018 Francois Beaune, The appleseedhq Organization
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
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"
#include "foundation/math/aabb.h"
#include "foundation/math/vector.h"
#include "foundation/utility/benchmark.h"

using namespace foundation;
using namespace renderer;

BENCHMARK_SUITE(Renderer_Kernel_Rendering_LocalSampleAccumulationBuffer)
{
    struct Fixture
    {
        Tile                m_color_tile;
        AccumulatorTile     m_level;
        AABB2u              m_rect;

        Fixture()
          : m_color_tile(64, 64, 4, PixelFormatHalf)
          , m_level(256, 256, 4)
          , m_rect(Vector2u(0, 0), Vector2u(63, 63))
        {
            m_level.clear();
        }
    };

    BENCHMARK_CASE_F(DevelopToTile_FastPath, Fixture)
    {
        LocalSampleAccumulationBuffer::develop_to_tile(
            m_color_tile,
            1024, 1024,
            m_level,
            0, 0,
            m_rect);
    }

    BENCHMARK_CASE_F(DevelopToTile_SlowPath, Fixture)
    {
        LocalSampleAccumulationBuffer::develop_to_tile(
            m_color_tile,
            1025, 1025,
            m_level,
            0, 0,
            m_rect);
    }
}
