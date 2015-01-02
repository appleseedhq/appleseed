
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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

// Interface header.
#include "pixelrendererbase.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"

// appleseed.foundation headers.
#include "foundation/utility/string.h"

// Standard headers.
#include <string>

using namespace foundation;

namespace renderer
{

//
// PixelRendererBase class implementation.
//

PixelRendererBase::PixelRendererBase()
  : m_invalid_sample_count(0)
{
}

PixelRendererBase::~PixelRendererBase()
{
    if (m_invalid_sample_count > 0)
    {
        RENDERER_LOG_WARNING(
            "found %s pixel sample%s with NaN or negative values.",
            pretty_uint(m_invalid_sample_count).c_str(),
            m_invalid_sample_count > 1 ? "s" : "");
    }
}

void PixelRendererBase::on_tile_begin(
    const Frame&    frame,
    Tile&           tile,
    TileStack&      aov_tiles)
{
}

void PixelRendererBase::on_tile_end(
    const Frame&    frame,
    Tile&           tile,
    TileStack&      aov_tiles)
{
}

void PixelRendererBase::signal_invalid_sample()
{
    // todo: mark pixel as faulty in the diagnostic map.
    if (m_invalid_sample_count++ == 0)
        RENDERER_LOG_WARNING("found at least one pixel sample with NaN or negative values.");
}

}   // namespace renderer
