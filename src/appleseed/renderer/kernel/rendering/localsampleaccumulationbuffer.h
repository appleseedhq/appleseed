
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_LOCALSAMPLEACCUMULATIONBUFFER_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_LOCALSAMPLEACCUMULATIONBUFFER_H

// appleseed.renderer headers.
#include "renderer/kernel/rendering/sampleaccumulationbuffer.h"

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/filter.h"
#include "foundation/platform/atomic.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/thread.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cstddef>
#include <vector>

// Forward declarations.
namespace foundation    { class FilteredTile; }
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
        const size_t                        width,
        const size_t                        height,
        const foundation::Filter2f&         filter);

    // Destructor.
    ~LocalSampleAccumulationBuffer();

    // Reset the buffer to its initial state. Thread-safe.
    virtual void clear() APPLESEED_OVERRIDE;

    // Store a set of samples into the buffer. Thread-safe.
    virtual void store_samples(
        const size_t                        sample_count,
        const Sample                        samples[],
        foundation::IAbortSwitch&           abort_switch) APPLESEED_OVERRIDE;

    // Develop the buffer to a frame. Thread-safe.
    virtual void develop_to_frame(
        Frame&                              frame,
        foundation::IAbortSwitch&           abort_switch) APPLESEED_OVERRIDE;

    // Exposed for tests and benchmarks.
    static void develop_to_tile_undo_premult_alpha(
        foundation::Tile&                   color_tile,
        foundation::Tile&                   depth_tile,
        const size_t                        image_width,
        const size_t                        image_height,
        const foundation::FilteredTile&     level,
        const size_t                        origin_x,
        const size_t                        origin_y,
        const foundation::AABB2u&           rect);
    static void develop_to_tile(
        foundation::Tile&                   color_tile,
        foundation::Tile&                   depth_tile,
        const size_t                        image_width,
        const size_t                        image_height,
        const foundation::FilteredTile&     level,
        const size_t                        origin_x,
        const size_t                        origin_y,
        const foundation::AABB2u&           rect);

  private:
    typedef foundation::ReadWriteLock<
        foundation::SleepWaitPolicy<5>
    > LockType;

    LockType                                m_lock;
    std::vector<foundation::FilteredTile*>  m_levels;
    boost::atomic<foundation::int32>*       m_remaining_pixels;
    boost::atomic<foundation::uint32>       m_active_level;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_LOCALSAMPLEACCUMULATIONBUFFER_H
