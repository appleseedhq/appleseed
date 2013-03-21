
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_LOCALACCUMULATIONFRAMEBUFFER_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_LOCALACCUMULATIONFRAMEBUFFER_H

// appleseed.renderer headers.
#include "renderer/kernel/rendering/accumulationframebuffer.h"

// appleseed.foundation headers.
#include "foundation/math/filter.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cstddef>
#include <vector>

// Forward declarations.
namespace renderer  { class FilteredFrameBuffer; }
namespace renderer  { class Frame; }
namespace renderer  { class Sample; }

namespace renderer
{

class LocalAccumulationFramebuffer
  : public AccumulationFramebuffer
{
  public:
    // Constructor.
    LocalAccumulationFramebuffer(
        const size_t                    width,
        const size_t                    height,
        const foundation::Filter2d&     filter);

    // Destructor.
    ~LocalAccumulationFramebuffer();

    // Reset the framebuffer to its initial state. Thread-safe.
    virtual void clear() OVERRIDE;

    // Store @samples into the framebuffer. Thread-safe.
    virtual void store_samples(
        const size_t                    sample_count,
        const Sample                    samples[]) OVERRIDE;

    // Develop the framebuffer to a frame. Thread-safe.
    virtual void develop_to_frame(Frame& frame) OVERRIDE;

  private:
    std::vector<FilteredFrameBuffer*>   m_levels;
    std::vector<size_t>                 m_remaining_pixels;
    size_t                              m_active_level;

    // Find the first (the highest resolution) level that has all its pixels set.
    const FilteredFrameBuffer& find_display_level() const;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_LOCALACCUMULATIONFRAMEBUFFER_H
