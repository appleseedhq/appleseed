
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_GLOBALACCUMULATIONFRAMEBUFFER_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_GLOBALACCUMULATIONFRAMEBUFFER_H

// appleseed.renderer headers.
#include "renderer/kernel/rendering/accumulationframebuffer.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/tile.h"

// Standard headers.
#include <cstddef>
#include <memory>

// Forward declarations.
namespace renderer      { class Frame; }
namespace renderer      { class Sample; }

namespace renderer
{

class GlobalAccumulationFramebuffer
  : public AccumulationFramebuffer
{
  public:
    // Constructor.
    GlobalAccumulationFramebuffer(
        const size_t    width,
        const size_t    height);

    // Reset the framebuffer to its initial state. Thread-safe.
    virtual void clear();

    // Store @samples into the framebuffer. Thread-safe.
    virtual void store_samples(
        const size_t    sample_count,
        const Sample    samples[]);

  private:
    std::auto_ptr<foundation::Tile>     m_tile;

    void add_pixel(
        const size_t                    x,
        const size_t                    y,
        const foundation::Color3f&      color);

    foundation::Color3f get_pixel(
        const size_t                    x,
        const size_t                    y) const;

    virtual void develop_to_frame(Frame& frame) const;

    void develop_to_tile(
        foundation::Tile&               tile,
        const size_t                    origin_x,
        const size_t                    origin_y,
        const size_t                    tile_x,
        const size_t                    tile_y,
        const float                     scale) const;
};


//
// GlobalAccumulationFramebuffer class implementation.
//

inline void GlobalAccumulationFramebuffer::add_pixel(
    const size_t                        x,
    const size_t                        y,
    const foundation::Color3f&          color)
{
    foundation::Color3f* pixel =
        reinterpret_cast<foundation::Color3f*>(m_tile->pixel(x, y));

    *pixel += color;
}

inline foundation::Color3f GlobalAccumulationFramebuffer::get_pixel(
    const size_t                        x,
    const size_t                        y) const
{
    const foundation::Color3f* pixel =
        reinterpret_cast<const foundation::Color3f*>(m_tile->pixel(x, y));

    return *pixel;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_GLOBALACCUMULATIONFRAMEBUFFER_H
