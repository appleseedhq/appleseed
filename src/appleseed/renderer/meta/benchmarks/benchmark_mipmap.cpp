
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/genericimagefilereader.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/platform/types.h"
#include "foundation/utility/benchmark.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <memory>
#include <vector>

using namespace foundation;
using namespace std;

BENCHMARK_SUITE(AtomKraft_MipMap)
{
    struct FixtureBase
    {
        auto_ptr<Image> m_input;
        vector<uint8>   m_dummy;

        explicit FixtureBase(const char* filepath)
        {
            GenericImageFileReader reader;
            auto_ptr<Image> source(reader.read(filepath));

            m_input.reset(
                new Image(
                    *source.get(),
                    source->properties().m_canvas_width,
                    source->properties().m_canvas_height,
                    PixelFormatFloat));

            m_dummy.resize(m_input->properties().m_pixel_count * m_input->properties().m_pixel_size);
        }
    };

    struct FixtureRGB
      : public FixtureBase
    {
        FixtureRGB()
          : FixtureBase("unit benchmarks/inputs/test_mipmap_rgb.exr")
        {
            assert(m_input->properties().m_channel_count == 3);
        }
    };

    struct FixtureRGBA
      : public FixtureBase
    {
        FixtureRGBA()
          : FixtureBase("unit benchmarks/inputs/test_mipmap_rgba.exr")
        {
            assert(m_input->properties().m_channel_count == 4);
        }
    };

    BENCHMARK_CASE_F(GenerateMipmapLevelFloatClampLinearRGBA, FixtureRGBA)
    {
        const CanvasProperties& input_props = m_input->properties();

        for (size_t i = 1; i <= 10; ++i)
        {
            ak::generate_mipmap_level_float_clamp_linear_rgba(
                reinterpret_cast<float*>(&m_dummy[0]),
                reinterpret_cast<const float*>(m_input->tile(0, 0).get_storage()),
                static_cast<int>(input_props.m_canvas_width),
                static_cast<int>(input_props.m_canvas_height),
                static_cast<int>(i),
                4);
        }
    }
}
