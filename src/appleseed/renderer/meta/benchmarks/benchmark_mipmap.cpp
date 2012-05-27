
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune
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
#include "foundation/image/genericimagefilereader.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/utility/benchmark.h"

// Standard headers.
#include <cassert>
#include <memory>

using namespace foundation;
using namespace renderer;
using namespace std;

BENCHMARK_SUITE(AtomKraft_MipMap)
{
    struct FixtureBase
    {
        auto_ptr<Image> m_level0;
        auto_ptr<Image> m_level1;
        auto_ptr<Image> m_level6;
        auto_ptr<Image> m_level7;

        explicit FixtureBase(const char* filepath)
        {
            GenericImageFileReader reader;
            auto_ptr<Image> source(reader.read(filepath));

            const size_t width = source->properties().m_canvas_width;
            const size_t height = source->properties().m_canvas_height;
            const size_t channel_count = source->properties().m_channel_count;

            m_level0.reset(
                new Image(
                    *source.get(),
                    width,
                    height,
                    PixelFormatFloat));

            m_level1.reset(
                new Image(
                    width  / 2,
                    height / 2,
                    width  / 2,
                    height / 2,
                    channel_count,
                    PixelFormatFloat));

            m_level6.reset(
                new Image(
                    width  / 64,
                    height / 64,
                    width  / 64,
                    height / 64,
                    channel_count,
                    PixelFormatFloat));

            m_level7.reset(
                new Image(
                    width  / 128,
                    height / 128,
                    width  / 128,
                    height / 128,
                    channel_count,
                    PixelFormatFloat));
        }
    };

    struct FixtureRGB
      : public FixtureBase
    {
        FixtureRGB()
          : FixtureBase("unit benchmarks/inputs/test_mipmap_rgb.exr")
        {
            assert(m_level0->properties().m_channel_count == 3);

            TextureObject level0_texture(*m_level0.get());
            TextureObject level6_texture(*m_level6.get());

            ak::generate_mipmap_level<3, TextureObject>(
                level6_texture,
                level0_texture,
                6,
                4);
        }
    };

    BENCHMARK_CASE_F(GenerateMipmapLevel_Level0ToLevel1, FixtureRGB)
    {
        TextureObject level0_texture(*m_level0.get());
        TextureObject level1_texture(*m_level1.get());

        ak::generate_mipmap_level<3, TextureObject>(
            level1_texture,
            level0_texture,
            1,
            4);
    }

    BENCHMARK_CASE_F(GenerateMipmapLevel_Level6ToLevel7, FixtureRGB)
    {
        TextureObject level6_texture(*m_level6.get());
        TextureObject level7_texture(*m_level7.get());

        ak::generate_mipmap_level<3, TextureObject>(
            level7_texture,
            level6_texture,
            1,
            4);
    }

    struct FixtureRGBA
      : public FixtureBase
    {
        FixtureRGBA()
          : FixtureBase("unit benchmarks/inputs/test_mipmap_rgba.exr")
        {
            assert(m_level0->properties().m_channel_count == 4);

            ak::generate_mipmap_level_float_clamp_linear_rgba(
                reinterpret_cast<float*>(m_level6->tile(0, 0).get_storage()),
                reinterpret_cast<const float*>(m_level0->tile(0, 0).get_storage()),
                static_cast<int>(m_level0->properties().m_canvas_width),
                static_cast<int>(m_level0->properties().m_canvas_height),
                6,
                4);
        }
    };

    BENCHMARK_CASE_F(GenerateMipmapLevelFloatClampLinearRGBA_Level0ToLevel1, FixtureRGBA)
    {
        const CanvasProperties& level0_props = m_level0->properties();

        ak::generate_mipmap_level_float_clamp_linear_rgba(
            reinterpret_cast<float*>(m_level1->tile(0, 0).get_storage()),
            reinterpret_cast<const float*>(m_level0->tile(0, 0).get_storage()),
            static_cast<int>(level0_props.m_canvas_width),
            static_cast<int>(level0_props.m_canvas_height),
            1,
            4);
    }

    BENCHMARK_CASE_F(GenerateMipmapLevelFloatClampLinearRGBA_Level6ToLevel7, FixtureRGBA)
    {
        const CanvasProperties& level6_props = m_level6->properties();

        ak::generate_mipmap_level_float_clamp_linear_rgba(
            reinterpret_cast<float*>(m_level7->tile(0, 0).get_storage()),
            reinterpret_cast<const float*>(m_level6->tile(0, 0).get_storage()),
            static_cast<int>(level6_props.m_canvas_width),
            static_cast<int>(level6_props.m_canvas_height),
            1,
            4);
    }
}
