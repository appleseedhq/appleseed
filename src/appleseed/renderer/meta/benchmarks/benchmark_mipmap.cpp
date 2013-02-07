
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
#include "foundation/image/genericimagefilereader.h"
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"
#include "foundation/utility/benchmark.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <memory>

using namespace foundation;
using namespace renderer;
using namespace std;

BENCHMARK_SUITE(AtomKraft_MipMap)
{
    struct FixtureBase
    {
        auto_ptr<Image>                 m_level0;
        auto_ptr<Image>                 m_level1;
        auto_ptr<Image>                 m_level6;
        auto_ptr<Image>                 m_level7;

        auto_ptr<TiledTextureObject>    m_level0_texture;
        auto_ptr<TiledTextureObject>    m_level1_texture;
        auto_ptr<TiledTextureObject>    m_level6_texture;
        auto_ptr<TiledTextureObject>    m_level7_texture;

        explicit FixtureBase(const char* filepath)
        {
            GenericImageFileReader reader;
            auto_ptr<Image> source(reader.read(filepath));

            const CanvasProperties& props = source->properties();
            const size_t image_width = props.m_canvas_width;
            const size_t image_height = props.m_canvas_height;
            const size_t tile_width = props.m_tile_width;
            const size_t tile_height = props.m_tile_height;
            const size_t channel_count = props.m_channel_count;

            m_level0.reset(
                new Image(
                    *source.get(),
                    tile_width,
                    tile_height,
                    PixelFormatFloat));

            m_level1.reset(
                new Image(
                    image_width >> 1,
                    image_height >> 1,
                    tile_width,
                    tile_height,
                    channel_count,
                    PixelFormatFloat));

            m_level6.reset(
                new Image(
                    image_width >> 6,
                    image_height >> 6,
                    tile_width,
                    tile_height,
                    channel_count,
                    PixelFormatFloat));

            m_level7.reset(
                new Image(
                    image_width >> 7,
                    image_height >> 7,
                    tile_width,
                    tile_height,
                    channel_count,
                    PixelFormatFloat));

            m_level0_texture.reset(new TiledTextureObject(*m_level0.get()));
            m_level1_texture.reset(new TiledTextureObject(*m_level1.get()));
            m_level6_texture.reset(new TiledTextureObject(*m_level6.get()));
            m_level7_texture.reset(new TiledTextureObject(*m_level7.get()));
        }
    };

    class FixtureRGB
      : public FixtureBase
    {
      public:
        FixtureRGB()
          : FixtureBase("unit benchmarks/inputs/test_mipmap_rgb.exr")
        {
            assert(m_level0->properties().m_channel_count == 3);

            initialize_level6();
        }

      private:
        void initialize_level6()
        {
            for (int ty = 0; ty < m_level6_texture->tile_count_y(); ++ty)
            {
                for (int tx = 0; tx < m_level6_texture->tile_count_x(); ++tx)
                {
                    ak::generate_mipmap_level<3, TiledTextureObject>(
                        *m_level6_texture.get(),    // output
                        *m_level0_texture.get(),    // input
                        6,
                        tx,
                        ty,
                        4);
                }
            }
        }
    };

    BENCHMARK_CASE_F(GenerateMipmapLevel_Level0ToLevel1, FixtureRGB)
    {
        const int tile_count_x = m_level1_texture->tile_count_x();
        const int tile_count_y = m_level1_texture->tile_count_y();

        for (int ty = 0; ty < tile_count_y; ++ty)
        {
            for (int tx = 0; tx < tile_count_x; ++tx)
            {
                ak::generate_mipmap_level<3, TiledTextureObject>(
                    *m_level1_texture.get(),    // output
                    *m_level0_texture.get(),    // input
                    1,
                    tx,
                    ty,
                    4);
            }
        }
    }

    BENCHMARK_CASE_F(GenerateMipmapLevel_Level6ToLevel7, FixtureRGB)
    {
        const int tile_count_x = m_level7_texture->tile_count_x();
        const int tile_count_y = m_level7_texture->tile_count_y();

        for (int ty = 0; ty < tile_count_y; ++ty)
        {
            for (int tx = 0; tx < tile_count_x; ++tx)
            {
                ak::generate_mipmap_level<3, TiledTextureObject>(
                    *m_level7_texture.get(),    // output
                    *m_level6_texture.get(),    // input
                    1,
                    tx,
                    ty,
                    4);
            }
        }
    }

    class FixtureRGBA
      : public FixtureBase
    {
      public:
        FixtureRGBA()
          : FixtureBase("unit benchmarks/inputs/test_mipmap_rgba.exr")
        {
            assert(m_level0->properties().m_channel_count == 4);

            initialize_level6();
        }

      private:
        void initialize_level6()
        {
            for (int ty = 0; ty < m_level6_texture->tile_count_y(); ++ty)
            {
                for (int tx = 0; tx < m_level6_texture->tile_count_x(); ++tx)
                {
                    ak::generate_mipmap_level_float_clamp_linear_rgba<TiledTextureObject>(
                        *m_level6_texture.get(),    // output
                        *m_level0_texture.get(),    // input
                        6,
                        tx,
                        ty,
                        4);
                }
            }
        }
    };

    BENCHMARK_CASE_F(GenerateMipmapLevelFloatClampLinearRGBA_Level0ToLevel1, FixtureRGBA)
    {
        const int tile_count_x = m_level1_texture->tile_count_x();
        const int tile_count_y = m_level1_texture->tile_count_y();

        for (int ty = 0; ty < tile_count_y; ++ty)
        {
            for (int tx = 0; tx < tile_count_x; ++tx)
            {
                ak::generate_mipmap_level_float_clamp_linear_rgba<TiledTextureObject>(
                    *m_level1_texture.get(),    // output
                    *m_level0_texture.get(),    // input
                    1,
                    tx,
                    ty,
                    4);
            }
        }
    }

    BENCHMARK_CASE_F(GenerateMipmapLevelFloatClampLinearRGBA_Level6ToLevel7, FixtureRGBA)
    {
        const int tile_count_x = m_level7_texture->tile_count_x();
        const int tile_count_y = m_level7_texture->tile_count_y();

        for (int ty = 0; ty < tile_count_y; ++ty)
        {
            for (int tx = 0; tx < tile_count_x; ++tx)
            {
                ak::generate_mipmap_level_float_clamp_linear_rgba<TiledTextureObject>(
                    *m_level7_texture.get(),    // output
                    *m_level6_texture.get(),    // input
                    1,
                    tx,
                    ty,
                    4);
            }
        }
    }
}
