
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_POSTPROCESSINGSTAGE_POSTPROCESSINGSTAGE_H
#define APPLESEED_RENDERER_MODELING_POSTPROCESSINGSTAGE_POSTPROCESSINGSTAGE_H

// appleseed.renderer headers.
#include "renderer/modeling/entity/connectableentity.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/color.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer  { class ParamArray; }

namespace renderer
{

class APPLESEED_DLLSYMBOL PostProcessingStage
  : public ConnectableEntity
{
  public:
    // Return the unique ID of this class of entities.
    static foundation::UniqueID get_class_uid();

    // Constructor.
    PostProcessingStage(
        const char*             name,
        const ParamArray&       params);

    // Return a string identifying the model of this entity.
    virtual const char* get_model() const = 0;

    // Return the order number of this stage. Stages are executed in increasing order.
    int get_order() const;

    // Execute this post-processing stage on a given frame.
    virtual void execute(
        Frame&                  frame) const = 0;

  protected:
    template <typename Func>
    static void for_each_pixel(
        const Frame&            frame,
        const Func&             func);

    static void find_min_max(
        const Frame&            frame,
        foundation::Color4f&    min,
        foundation::Color4f&    max);

  private:
    int m_order;
};


//
// PostProcessingStage class implementation.
//

inline int PostProcessingStage::get_order() const
{
    return m_order;
}

template <typename Func>
void PostProcessingStage::for_each_pixel(const Frame& frame, const Func& func)
{
    foundation::Image& image = frame.image();
    const foundation::CanvasProperties& frame_props = image.properties();

    for (size_t ty = 0; ty < frame_props.m_tile_count_y; ++ty)
    {
        for (size_t tx = 0; tx < frame_props.m_tile_count_x; ++tx)
        {
            foundation::Tile& tile = image.tile(tx, ty);
            for (size_t y = 0, th = tile.get_height(); y < th; ++y)
            {
                for (size_t x = 0, tw = tile.get_width(); x < tw; ++x)
                {
                    foundation::Color4f color;
                    tile.get_pixel(x, y, color);
                    func(color);
                    tile.set_pixel(x, y, color);
                }
            }
        }
    }
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_POSTPROCESSINGSTAGE_POSTPROCESSINGSTAGE_H
