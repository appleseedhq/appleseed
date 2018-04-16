
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_PIXELRENDERERBASE_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_PIXELRENDERERBASE_H

// appleseed.renderer headers.
#include "renderer/kernel/rendering/ipixelrenderer.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cstddef>
#include <memory>

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace foundation    { class Tile; }
namespace renderer      { class AOVAccumulatorContainer; }
namespace renderer      { class Frame; }
namespace renderer      { class TileStack; }

namespace renderer
{

//
// A convenient base class for pixel renderers.
//

class PixelRendererBase
  : public IPixelRenderer
{
  public:
    // Constructor.
    PixelRendererBase(
        const Frame&        frame,
        const size_t        thread_index,
        const ParamArray&   params);

    bool are_diagnostics_enabled() const;

    // This method is called before a tile gets rendered.
    void on_tile_begin(
        const Frame&                frame,
        foundation::Tile&           tile,
        TileStack&                  aov_tiles) override;

    // This method is called after a tile has been rendered.
    void on_tile_end(
        const Frame&                frame,
        foundation::Tile&           tile,
        TileStack&                  aov_tiles) override;

  protected:
    void on_pixel_begin(
        const foundation::Vector2i&         pi,
        const foundation::Vector2i&         pt,
        const foundation::AABB2i&           tile_bbox,
        AOVAccumulatorContainer&            aov_accumulators);

    void on_pixel_end(
        const foundation::Vector2i&         pi,
        const foundation::Vector2i&         pt,
        const foundation::AABB2i&           tile_bbox,
        AOVAccumulatorContainer&            aov_accumulators);

    void signal_invalid_sample();

  private:
    struct Parameters
    {
        const bool m_diagnostics;

        explicit Parameters(const ParamArray& params)
            : m_diagnostics(params.get_optional<bool>("enable_diagnostics", false))
        {
        }
    };

    size_t                                  m_invalid_sample_count;
    size_t                                  m_invalid_pixel_count;
    size_t                                  m_invalid_sample_aov_index;
    std::unique_ptr<foundation::Tile>       m_invalid_sample_diagnostic;
    const Parameters                        m_params;
};


//
// Pixel renderer base factory.
//

class PixelRendererBaseFactory
  : public IPixelRendererFactory
{
  public:
    static foundation::Dictionary get_params_metadata();
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_PIXELRENDERERBASE_H
