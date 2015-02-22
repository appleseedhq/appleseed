
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015 Francois Beaune, The appleseedhq Organization
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
#include "renderer/kernel/aov/imagestack.h"
#include "renderer/kernel/rendering/localsampleaccumulationbuffer.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/benchmark.h"

using namespace foundation;
using namespace renderer;

BENCHMARK_SUITE(Renderer_Kernel_Rendering_LocalSampleAccumulationBuffer)
{
    template <bool PremultipliedAlpha>
    struct Fixture
    {
        auto_release_ptr<Frame>         m_frame;
        LocalSampleAccumulationBuffer   m_buffer;

        Fixture()
          : m_frame(
                FrameFactory::create(
                    "frame",
                    ParamArray()
                        .insert("resolution", "1920 1080")
                        .insert("tile_size", "64 64")
                        .insert("pixel_format", "float")
                        .insert("filter", "box")
                        .insert("filter_size", "2.0")
                        .insert("color_space", "linear_rgb")
                        .insert("premultiplied_alpha", PremultipliedAlpha)))
          , m_buffer(
                m_frame->image().properties().m_canvas_width,
                m_frame->image().properties().m_canvas_height,
                m_frame->get_filter())
        {
            m_frame->aov_images().append("depth", ImageStack::ContributionType, 4, PixelFormatFloat);

            m_buffer.clear();

            // Make sure tiles are allocated in the frame.
            m_buffer.develop_to_frame(m_frame.ref());
        }
    };

    BENCHMARK_CASE_F(DevelopToFrame_PremultipledAlpha, Fixture<true>)
    {
        m_buffer.develop_to_frame(m_frame.ref());
    }

    BENCHMARK_CASE_F(DevelopToFrame_StraightAlpha, Fixture<false>)
    {
        m_buffer.develop_to_frame(m_frame.ref());
    }
}
