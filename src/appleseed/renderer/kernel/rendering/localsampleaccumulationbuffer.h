
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

// appleseed.renderer headers.
#include "renderer/kernel/rendering/sampleaccumulationbuffer.h"

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/filter.h"
#include "foundation/platform/atomic.h"
#include "foundation/platform/thread.h"

// Standard headers.
#include <cstddef>
#include <cstdint>
#include <vector>

// Forward declarations.
namespace foundation    { class AccumulatorTile; }
namespace foundation    { class IAbortSwitch; }
namespace foundation    { class Tile; }
namespace renderer      { class Frame; }
namespace renderer      { class Sample; }

namespace renderer
{

class LocalSampleAccumulationBuffer
  : public SampleAccumulationBuffer
{
  public:
    // Constructor.
    LocalSampleAccumulationBuffer(
        const size_t                            width,
        const size_t                            height);

    // Destructor.
    ~LocalSampleAccumulationBuffer() override;

    // Reset the buffer to its initial state. Thread-safe.
    void clear() override;

    // Store a set of samples into the buffer. Thread-safe.
    void store_samples(
        const size_t                            sample_count,
        const Sample                            samples[],
        foundation::IAbortSwitch&               abort_switch) override;

    // Develop the buffer to a frame. Thread-safe.
    void develop_to_frame(
        Frame&                                  frame,
        foundation::IAbortSwitch&               abort_switch) override;

    // Exposed for tests and benchmarks.
    static void develop_to_tile(
        foundation::Tile&                       color_tile,
        const size_t                            image_width,
        const size_t                            image_height,
        const foundation::AccumulatorTile&      level,
        const size_t                            origin_x,
        const size_t                            origin_y,
        const foundation::AABB2u&               rect);

  private:
    typedef foundation::ReadWriteLock<
        foundation::SleepWaitPolicy<5>
    > LockType;

    LockType                                    m_lock;
    std::vector<foundation::AccumulatorTile*>   m_levels;
    std::vector<foundation::Vector2f>           m_level_scales;
    boost::atomic<std::int32_t>*                m_remaining_pixels;
    boost::atomic<std::uint32_t>                m_active_level;
};

}   // namespace renderer
