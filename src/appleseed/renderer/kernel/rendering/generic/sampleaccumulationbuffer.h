
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_GENERIC_SAMPLEACCUMULATIONBUFFER_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_GENERIC_SAMPLEACCUMULATIONBUFFER_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// Standard headers.
#include <cstddef>
#include <vector>

// Forward declarations.
namespace foundation    { class Tile; }
namespace renderer      { class ShadingResult; }
namespace renderer      { class TileStack; }

namespace renderer
{

class SampleAccumulationBuffer
  : public foundation::NonCopyable
{
  public:
    SampleAccumulationBuffer(
        const size_t                    width,
        const size_t                    height,
        const size_t                    aov_count);

    void clear();

    void add(
        const size_t                    x,
        const size_t                    y,
        const ShadingResult&            sample,
        const float                     weight);

    void add(
        const SampleAccumulationBuffer& source,
        const size_t                    source_x,
        const size_t                    source_y,
        const size_t                    dest_x,
        const size_t                    dest_y,
        const float                     weight);

    void develop_to_tile_premult_alpha(
        foundation::Tile&               tile,
        TileStack&                      aov_tiles) const;

    void develop_to_tile_straight_alpha(
        foundation::Tile&               tile,
        TileStack&                      aov_tiles) const;

  private:
    const size_t        m_width;
    const size_t        m_height;
    const size_t        m_aov_count;
    const size_t        m_channel_count;
    const size_t        m_buffer_size;
    std::vector<float>  m_buffer;

    float* pixel(
        const size_t                    x,
        const size_t                    y);

    const float* pixel(
        const size_t                    x,
        const size_t                    y) const;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_GENERIC_SAMPLEACCUMULATIONBUFFER_H
