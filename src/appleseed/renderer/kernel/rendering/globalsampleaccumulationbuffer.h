
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
#include "foundation/image/accumulatortile.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/thread.h"

// Standard headers.
#include <cstddef>
#include <cstdint>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace foundation    { class Tile; }
namespace renderer      { class Frame; }
namespace renderer      { class Sample; }

namespace renderer
{

class GlobalSampleAccumulationBuffer
  : public SampleAccumulationBuffer
{
  public:
    // Constructor.
    GlobalSampleAccumulationBuffer(
        const size_t                width,
        const size_t                height);

    // Reset the buffer to its initial state. Thread-safe.
    void clear() override;

    // Store a set of samples into the buffer. Thread-safe.
    void store_samples(
        const size_t                sample_count,
        const Sample                samples[],
        foundation::IAbortSwitch&   abort_switch) override;

    // Develop the buffer to a frame. Thread-safe.
    void develop_to_frame(
        Frame&                      frame,
        foundation::IAbortSwitch&   abort_switch) override;

    // Increment the number of samples used for pixel values renormalization. Thread-safe.
    void increment_sample_count(const std::uint64_t delta_sample_count);

  private:
    boost::shared_mutex             m_mutex;
    foundation::AccumulatorTile     m_fb;

    void develop_to_tile(
        foundation::Tile&           tile,
        const size_t                origin_x,
        const size_t                origin_y,
        const size_t                tile_x,
        const size_t                tile_y,
        const float                 scale) const;
};

}   // namespace renderer
