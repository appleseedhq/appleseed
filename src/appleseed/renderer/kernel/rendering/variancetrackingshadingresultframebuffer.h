
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Stephen Agyemang, The appleseedhq Organization
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

// appleseed.renderer headers.
#include "renderer/kernel/rendering/shadingresultframebuffer.h"

// appleseed.foundation headers.
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/math/aabb.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class Tile; }
namespace renderer      { class ShadingResult; }
namespace renderer      { class TileStack; }

namespace renderer
{

class VarianceTrackingShadingResultFrameBuffer
  : public ShadingResultFrameBuffer
{
  public:
    VarianceTrackingShadingResultFrameBuffer(
        const size_t                    width,
        const size_t                    height,
        const size_t                    aov_count);

    VarianceTrackingShadingResultFrameBuffer(
        const size_t                    width,
        const size_t                    height,
        const size_t                    aov_count,
        const foundation::AABB2u&       crop_window);

    ~VarianceTrackingShadingResultFrameBuffer() override {}

    static size_t get_total_channel_count(
        const size_t                    aov_count);

    void add(
        const foundation::Vector2u&     pi,
        const ShadingResult&            sample) override;

    virtual void develop_to_tile(
        foundation::Tile&               tile,
        TileStack&                      aov_tiles) const override;

    // Return estimate of the mean pixel variance.
    float estimator_variance() const;

    // Transfer pixel variance estimates to tile and return estimate of the mean pixel variance.
    float estimator_variance_to_tile(
        foundation::Tile&               tile) const;
  
  private:
    const size_t                        m_aov_count;
};

inline size_t VarianceTrackingShadingResultFrameBuffer::get_total_channel_count(const size_t aov_count)
{
    // The squared sample sum plus all channels of the ShadingResultFramebuffer.
    return 4 + ShadingResultFrameBuffer::get_total_channel_count(aov_count);
}

}   // namespace renderer
