
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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
#include "foundation/image/color.h"
#include "foundation/image/tile.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cstddef>
#include <vector>

// Forward declarations.
namespace renderer      { class Frame; }
namespace renderer      { class Sample; }

namespace renderer
{

class LocalAccumulationFramebuffer
  : public AccumulationFramebuffer
{
  public:
    // Constructor.
    LocalAccumulationFramebuffer(
        const size_t    width,
        const size_t    height);

    // Destructor.
    ~LocalAccumulationFramebuffer();

    // Reset the framebuffer to its initial state. Thread-safe.
    virtual void clear();

    // Store @samples into the framebuffer. Thread-safe.
    virtual void store_samples(
        const size_t    sample_count,
        const Sample    samples[]);

  private:
    struct AccumulationPixel
    {
        foundation::Color4f             m_color;
        foundation::uint32              m_count;
    };

    std::vector<foundation::Tile*>      m_levels;
    std::vector<size_t>                 m_set_pixels;
    size_t                              m_active_level;

    virtual void develop_to_frame(Frame& frame) const;

    void develop_to_tile(
        foundation::Tile&               tile,
        const size_t                    origin_x,
        const size_t                    origin_y,
        const size_t                    tile_x,
        const size_t                    tile_y) const;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_LOCALACCUMULATIONFRAMEBUFFER_H
