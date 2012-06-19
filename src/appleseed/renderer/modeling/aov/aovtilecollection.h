
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_RENDERER_MODELING_AOV_AOVTILECOLLECTION_H
#define APPLESEED_RENDERER_MODELING_AOV_AOVTILECOLLECTION_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/aov/aovcollection.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/tile.h"

// Standard headers.
#include <cassert>
#include <cstddef>

namespace renderer
{

//
// A small array of image tiles.
//

class AOVTileCollection
{
  public:
    static const size_t MaxSize = AOVCollection::MaxSize;

    AOVTileCollection();
    AOVTileCollection(const AOVTileCollection& rhs);

    void append(foundation::Tile* tile);

    void set_pixel(
       const size_t             x,
       const size_t             y,
       const AOVCollection&     aovs) const;

  private:
    foundation::Tile*   m_tiles[MaxSize];
    size_t              m_size;
};


//
// AOVTileCollection class implementation.
//

inline AOVTileCollection::AOVTileCollection()
  : m_size(0)
{
}

inline AOVTileCollection::AOVTileCollection(const AOVTileCollection& rhs)
  : m_size(rhs.m_size)
{
    for (size_t i = 0; i < m_size; ++i)
        m_tiles[i] = rhs.m_tiles[i];
}

inline void AOVTileCollection::append(foundation::Tile* tile)
{
    assert(m_size < MaxSize);
    m_tiles[m_size++] = tile;
}

inline void AOVTileCollection::set_pixel(
    const size_t                x,
    const size_t                y,
    const AOVCollection&        aovs) const
{
    for (size_t i = 0; i < m_size; ++i)
    {
        const Spectrum& spectrum = aovs[i];
        const foundation::Color4f linear_rgb(spectrum[0], spectrum[1], spectrum[2], 1.0f);

        m_tiles[i]->set_pixel(x, y, linear_rgb);
    }
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_AOV_AOVTILECOLLECTION_H
