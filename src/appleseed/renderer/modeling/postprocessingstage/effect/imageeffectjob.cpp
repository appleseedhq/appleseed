
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Tiago Chaves, The appleseedhq Organization
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
#include "imageeffectjob.h"

// appleseed.foundation headers.
#include "foundation/image/canvasproperties.h"
#include "foundation/image/image.h"
#include "foundation/image/tile.h"
#include "foundation/math/ordering.h"

// Standard headers.
#include <cassert>

using namespace foundation;

namespace renderer
{

//
// ImageEffectJob class implementation.
//

ImageEffectJob::ImageEffectJob(
    const ImageEffectApplier&   effect_applier,
    Image&                      image,
    const std::size_t           tile_x,
    const std::size_t           tile_y)
  : m_effect_applier(effect_applier)
  , m_image(image)
  , m_tile_x(tile_x)
  , m_tile_y(tile_y)
{
}

void ImageEffectJob::execute(const std::size_t thread_index)
{
    // Apply the image effect to a single tile.
    m_effect_applier.apply(
        m_image,
        m_tile_x,
        m_tile_y);
}


//
// ImageEffectJobFactory class implementation.
//

ImageEffectJobFactory::EffectJobVector ImageEffectJobFactory::create(
    Image&                      image,
    const ImageEffectApplier&   effect_applier) const
{
    // Retrieve image properties.
    const CanvasProperties& props = image.properties();

    // Generate tiles.
    std::vector<std::size_t> tiles;
    hilbert_ordering(tiles, props.m_tile_count_x, props.m_tile_count_y);

    // Make sure the right number of tiles was created.
    assert(tiles.size() == props.m_tile_count);

    // Create effect jobs, one per tile.
    EffectJobVector effect_jobs;
    effect_jobs.reserve(props.m_tile_count);
    for (std::size_t i = 0; i < props.m_tile_count; ++i)
    {
        // Compute coordinates of the tile in the image.
        const std::size_t tile_index = tiles[i];
        const std::size_t tile_x = tile_index % props.m_tile_count_x;
        const std::size_t tile_y = tile_index / props.m_tile_count_x;
        assert(tile_x < props.m_tile_count_x);
        assert(tile_y < props.m_tile_count_y);

        // Create the tile job.
        effect_jobs.push_back(
            new ImageEffectJob(
                effect_applier,
                image,
                tile_x,
                tile_y));
    }

    return effect_jobs;
}

}   // namespace renderer
