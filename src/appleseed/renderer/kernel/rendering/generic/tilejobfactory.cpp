
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

// Interface header.
#include "tilejobfactory.h"

// appleseed.renderer headers.
#include "renderer/modeling/frame/frame.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/math/ordering.h"
#include "foundation/utility/otherwise.h"

// Standard headers.
#include <cassert>

using namespace foundation;

namespace renderer
{

//
// TileJobFactory class implementation.
//

void TileJobFactory::create(
    const Frame&                        frame,
    const TileOrdering                  tile_ordering,
    const TileJob::TileRendererVector&  tile_renderers,
    const TileJob::TileCallbackVector&  tile_callbacks,
    const size_t                        thread_count,
    const std::uint32_t                 pass_hash,
    const Spectrum::Mode                spectrum_mode,
    TileJobVector&                      tile_jobs,
    IAbortSwitch&                       abort_switch)
{
    // Retrieve frame properties.
    const CanvasProperties& props = frame.image().properties();

    // Generate tiles ordering.
    std::vector<size_t> tiles;
    generate_tile_ordering(props, tile_ordering, tiles);

    // Make sure the right number of tiles was created.
    assert(tiles.size() == props.m_tile_count);

    // Create tile jobs, one per tile.
    for (size_t i = 0; i < props.m_tile_count; ++i)
    {
        // Compute coordinates of the tile in the frame.
        const size_t tile_index = tiles[i];
        const size_t tile_x = tile_index % props.m_tile_count_x;
        const size_t tile_y = tile_index / props.m_tile_count_x;
        assert(tile_x < props.m_tile_count_x);
        assert(tile_y < props.m_tile_count_y);

        // Create the tile job.
        tile_jobs.push_back(
            new TileJob(
                tile_renderers,
                tile_callbacks,
                frame,
                tile_x,
                tile_y,
                thread_count,
                pass_hash,
                spectrum_mode,
                abort_switch));
    }
}

void TileJobFactory::generate_tile_ordering(
    const CanvasProperties&             frame_properties,
    const TileOrdering                  tile_ordering,
    std::vector<size_t>&                tiles)
{
    switch (tile_ordering)
    {
      case LinearOrdering:
        linear_ordering(
            tiles,
            frame_properties.m_tile_count);
        break;

      case SpiralOrdering:
        spiral_ordering(
            tiles,
            frame_properties.m_tile_count_x,
            frame_properties.m_tile_count_y);
        break;

      case HilbertOrdering:
        hilbert_ordering(
            tiles,
            frame_properties.m_tile_count_x,
            frame_properties.m_tile_count_y);
        break;

      case RandomOrdering:
        random_ordering(
            tiles,
            frame_properties.m_tile_count,
            m_rng);
        break;

      assert_otherwise;
    }
}

}   // namespace renderer
