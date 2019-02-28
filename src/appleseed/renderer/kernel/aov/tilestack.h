
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

#pragma once

// appleseed.renderer headers.
#include "renderer/kernel/aov/aovsettings.h"

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

class TileStack
{
  public:
    TileStack();
    TileStack(const TileStack& rhs);

    void append(foundation::Tile* tile);

    void set_pixel(
       const size_t                 x,
       const size_t                 y,
       const size_t                 i,
       const foundation::Color4f&   color) const;

    foundation::Tile& get_tile(
       const size_t                 index);

    const foundation::Tile& get_tile(
       const size_t                 index) const;

  private:
    foundation::Tile*   m_tiles[MaxAOVCount];
    size_t              m_size;
};


//
// TileStack class implementation.
//

inline TileStack::TileStack()
  : m_size(0)
{
}

inline TileStack::TileStack(const TileStack& rhs)
  : m_size(rhs.m_size)
{
    for (size_t i = 0; i < m_size; ++i)
        m_tiles[i] = rhs.m_tiles[i];
}

inline void TileStack::append(foundation::Tile* tile)
{
    assert(m_size < MaxAOVCount);
    m_tiles[m_size++] = tile;
}

inline void TileStack::set_pixel(
    const size_t                x,
    const size_t                y,
    const size_t                i,
    const foundation::Color4f&  color) const
{
    m_tiles[i]->set_pixel(x, y, color);
}

inline foundation::Tile& TileStack::get_tile(
    const size_t                index)
{
    assert(index < m_size);
    return *m_tiles[index];
}

inline const foundation::Tile& TileStack::get_tile(
    const size_t                index) const
{
    assert(index < m_size);
    return *m_tiles[index];
}

}   // namespace renderer
