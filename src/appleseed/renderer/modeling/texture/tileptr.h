
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Francois Beaune, The appleseedhq Organization
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

// Standard headers.
#include <cassert>
#include <cstdint>

// Forward declarations.
namespace foundation    { class Tile; }

namespace renderer
{

//
// A pointer to a tile that includes a bit indicating whether or not the pointer
// has ownership of the tile.
//

class TilePtr
{
  public:
    static TilePtr make_nullptr();
    static TilePtr make_owning(foundation::Tile* tile);
    static TilePtr make_non_owning(foundation::Tile* tile);

    foundation::Tile* get_tile() const;
    bool has_ownership() const;

  private:
    static constexpr std::uintptr_t BitMask = 1;

    foundation::Tile* m_tile;   // bit 0 indicates whether the tile is owned (1) or not (0)
};

static_assert(sizeof(TilePtr) == sizeof(foundation::Tile*), "renderer::TilePtr should not be larger than a pointer");


//
// TilePtr class implementation.
//

inline TilePtr TilePtr::make_nullptr()
{
    TilePtr ptr;
    ptr.m_tile = nullptr;       // non-owning
    return ptr;
}

inline TilePtr TilePtr::make_owning(foundation::Tile* tile)
{
    assert((reinterpret_cast<std::uintptr_t>(tile) & BitMask) == 0);

    TilePtr ptr;
    ptr.m_tile =
        reinterpret_cast<foundation::Tile*>(
            reinterpret_cast<std::uintptr_t>(tile) | BitMask);
    return ptr;
}

inline TilePtr TilePtr::make_non_owning(foundation::Tile* tile)
{
    assert((reinterpret_cast<std::uintptr_t>(tile) & BitMask) == 0);

    TilePtr ptr;
    ptr.m_tile = tile;
    return ptr;
}

inline foundation::Tile* TilePtr::get_tile() const
{
    return
        reinterpret_cast<foundation::Tile*>(
            reinterpret_cast<std::uintptr_t>(m_tile) & ~BitMask);
}

inline bool TilePtr::has_ownership() const
{
    return (reinterpret_cast<std::uintptr_t>(m_tile) & BitMask) == BitMask;
}

}   // namespace renderer
