
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/frame/frame.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/pixel.h"
#include "foundation/image/tile.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/benchmark.h"

// Standard headers.
#include <memory>

using namespace foundation;
using namespace renderer;
using namespace std;

BENCHMARK_SUITE(Renderer_Modeling_Frame)
{
    template <PixelFormat pf>
    struct Fixture
    {
        auto_release_ptr<Frame> m_frame;
        unique_ptr<Tile>        m_tile;

        Fixture()
          : m_frame(
                FrameFactory::create("frame",
                    ParamArray()
                        .insert("resolution", "1280 720")
                        .insert("tile_size", "32 32")
                        .insert("pixel_format", "float")
                        .insert("color_space", "srgb")))
          , m_tile(new Tile(32, 32, 4, pf))
        {
            m_tile->clear(Color4f(0.8f, -0.3f, 0.6f, 0.5f));
        }
    };
}
