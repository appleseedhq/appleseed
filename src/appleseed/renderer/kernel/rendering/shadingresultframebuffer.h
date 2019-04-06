
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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

#pragma once

// appleseed.foundation headers.
#include "foundation/image/accumulatortile.h"
#include "foundation/math/aabb.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>
#include <vector>

// Forward declarations.
namespace foundation    { class Tile; }
namespace renderer      { class ShadingResult; }
namespace renderer      { class TileStack; }

namespace renderer
{

class ShadingResultFrameBuffer
  : public foundation::AccumulatorTile
{
  public:
    ShadingResultFrameBuffer(
        const size_t                    width,
        const size_t                    height,
        const size_t                    aov_count);

    ShadingResultFrameBuffer(
        const size_t                    width,
        const size_t                    height,
        const size_t                    aov_count,
        const foundation::AABB2u&       crop_window);

    static size_t get_total_channel_count(const size_t aov_count);

    void add(
        const foundation::Vector2u&     pi,
        const ShadingResult&            sample);

    void merge(
        const size_t                    dest_x,
        const size_t                    dest_y,
        const ShadingResultFrameBuffer& source,
        const size_t                    source_x,
        const size_t                    source_y,
        const float                     scaling);

    void develop_to_tile(
        foundation::Tile&               tile,
        TileStack&                      aov_tiles) const;

  private:
    const size_t                        m_aov_count;
    std::vector<float>                  m_scratch;
};

inline size_t ShadingResultFrameBuffer::get_total_channel_count(const size_t aov_count)
{
    // The main image plus a number of AOVs, all RGBA.
    return (1 + aov_count) * 4;
}

}   // namespace renderer
