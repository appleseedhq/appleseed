
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_IPIXELRENDERER_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_IPIXELRENDERER_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/iunknown.h"
#include "foundation/math/aabb.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class StatisticsVector; }
namespace foundation    { class Tile; }
namespace renderer      { class Frame; }
namespace renderer      { class ShadingResultFrameBuffer; }
namespace renderer      { class TileStack; }

namespace renderer
{

//
// Pixel renderer interface.
//

class IPixelRenderer
  : public foundation::IUnknown
{
  public:
    // This method is called before a tile gets rendered.
    virtual void on_tile_begin(
        const Frame&                frame,
        foundation::Tile&           tile,
        TileStack&                  aov_tiles) = 0;

    // This method is called after a tile has been rendered.
    virtual void on_tile_end(
        const Frame&                frame,
        foundation::Tile&           tile,
        TileStack&                  aov_tiles) = 0;

    // Render a pixel.
    virtual void render_pixel(
        const Frame&                frame,
        foundation::Tile&           tile,
        TileStack&                  aov_tiles,
        const foundation::AABB2i&   tile_bbox,
        const size_t                pass_hash,
        const foundation::Vector2i& pi,             // image-space pixel coordinates
        const foundation::Vector2i& pt,             // tile-space pixel coordinates
        SamplingContext::RNGType&   rng,
        ShadingResultFrameBuffer&   framebuffer) = 0;

    // Retrieve performance statistics.
    virtual foundation::StatisticsVector get_statistics() const = 0;
};


//
// Interface of a IPixelRenderer factory.
//

class IPixelRendererFactory
  : public foundation::IUnknown
{
  public:
    // Return a new pixel renderer instance.
    virtual IPixelRenderer* create(const size_t thread_index) = 0;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_IPIXELRENDERER_H
