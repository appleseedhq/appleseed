
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Esteban Tovagliari, The appleseedhq Organization
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

// Interface header.
#include "aovaccumulator.h"

// appleseed.renderer headers.
#include "renderer/kernel/shading/shadingcomponents.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingresult.h"
#include "renderer/modeling/aov/aov.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"

// Standard headers.
#include <cassert>
#include <cstdlib>
#include <cstring>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// AOVAccumulator class implementation.
//

AOVAccumulator::~AOVAccumulator()
{
}

void AOVAccumulator::release()
{
    delete this;
}

void AOVAccumulator::on_tile_begin(
    const Frame& frame,
    const size_t tile_x,
    const size_t tile_y,
    const size_t max_spp)
{
}

void AOVAccumulator::on_tile_end(
    const Frame& frame,
    const size_t tile_x,
    const size_t tile_y)
{
}

void AOVAccumulator::on_sample_begin()
{
}

void AOVAccumulator::on_sample_end()
{
}

void AOVAccumulator::write(
    const PixelContext&         pixel_context,
    const ShadingPoint&         shading_point,
    const ShadingComponents&    shading_components,
    ShadingResult&              shading_result)
{
}

namespace
{
    //
    // BeautyAOVAccumulator class.
    //

    class BeautyAOVAccumulator
      : public AOVAccumulator
    {
      public:
        void write(
            const PixelContext&         pixel_context,
            const ShadingPoint&         shading_point,
            const ShadingComponents&    shading_components,
            ShadingResult&              shading_result) override
        {
            shading_result.m_main.rgb() =
                shading_components.m_beauty.to_rgb(g_std_lighting_conditions);
        }
    };
}


//
// UnfilteredAOVAccumulator class implementation.
//

UnfilteredAOVAccumulator::UnfilteredAOVAccumulator(Image& image)
  : m_image(image)
{
}

void UnfilteredAOVAccumulator::on_tile_begin(
    const Frame&                frame,
    const size_t                tile_x,
    const size_t                tile_y,
    const size_t                max_spp)
{
    // Fetch the destination tile.
    const CanvasProperties& props = frame.image().properties();
    m_tile = &m_image.tile(tile_x, tile_y);

    // Fetch the tile bounds (inclusive).
    m_tile_origin_x = static_cast<int>(tile_x * props.m_tile_width);
    m_tile_origin_y = static_cast<int>(tile_y * props.m_tile_height);
    m_tile_end_x = static_cast<int>(m_tile_origin_x + m_tile->get_width() - 1);
    m_tile_end_y = static_cast<int>(m_tile_origin_y + m_tile->get_height() - 1);
}


//
// AOVAccumulatorContainer class implementation.
//

AOVAccumulatorContainer::AOVAccumulatorContainer()
{
    init();
}

AOVAccumulatorContainer::AOVAccumulatorContainer(const AOVContainer& aovs)
{
    init();

    // Create the remaining accumulators.
    for (size_t i = 0, e = aovs.size(); i < e; ++i)
    {
        const AOV* aov = aovs.get_by_index(i);
        insert(aov->create_accumulator(i));
    }
}

void AOVAccumulatorContainer::init()
{
    m_size = 0;
    memset(m_accumulators, 0, MaxAovAccumulators * sizeof(AOVAccumulator*));

    // Create beauty accumulator.
    insert(auto_release_ptr<AOVAccumulator>(new BeautyAOVAccumulator()));
}

AOVAccumulatorContainer::~AOVAccumulatorContainer()
{
    for (size_t i = 0, e = m_size; i < e; ++i)
        delete m_accumulators[i];
}

void AOVAccumulatorContainer::on_tile_begin(
    const Frame&                frame,
    const size_t                tile_x,
    const size_t                tile_y,
    const size_t                max_spp)
{
    for (size_t i = 0, e = m_size; i < e; ++i)
        m_accumulators[i]->on_tile_begin(frame, tile_x, tile_y, max_spp);
}

void AOVAccumulatorContainer::on_tile_end(
    const Frame&                frame,
    const size_t                tile_x,
    const size_t                tile_y)
{
    for (size_t i = 0, e = m_size; i < e; ++i)
        m_accumulators[i]->on_tile_end(frame, tile_x, tile_y);
}

void AOVAccumulatorContainer::on_sample_begin()
{
    for (size_t i = 0, e = m_size; i < e; ++i)
        m_accumulators[i]->on_sample_begin();
}

void AOVAccumulatorContainer::on_sample_end()
{
    for (size_t i = 0, e = m_size; i < e; ++i)
        m_accumulators[i]->on_sample_end();
}

void AOVAccumulatorContainer::write(
    const PixelContext&         pixel_context,
    const ShadingPoint&         shading_point,
    const ShadingComponents&    shading_components,
    ShadingResult&              shading_result)
{
    for (size_t i = 0, e = m_size; i < e; ++i)
    {
        m_accumulators[i]->write(
            pixel_context,
            shading_point,
            shading_components,
            shading_result);
    }
}

bool AOVAccumulatorContainer::insert(auto_release_ptr<AOVAccumulator> aov_accum)
{
    assert(aov_accum.get());

    if (m_size == MaxAovAccumulators - 1)
        return false;

    m_accumulators[m_size++] = aov_accum.release();
    return true;
}

}   // namespace renderer
