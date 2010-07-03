
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "image.h"

// appleseed.foundation headers.
#include "foundation/image/tile.h"

// Standard headers.
#include <cassert>
#include <cstring>

using namespace std;

namespace foundation
{

//
// Image class implementation.
//

// Constructor.
Image::Image(
    const size_t        image_width,
    const size_t        image_height,
    const size_t        tile_width,
    const size_t        tile_height,
    const size_t        channel_count,
    const PixelFormat   pixel_format)
  : m_props(
        image_width,
        image_height,
        tile_width,
        tile_height,
        channel_count,
        pixel_format)
{
    assert(image_width > 0);
    assert(image_height > 0);
    assert(tile_width > 0);
    assert(tile_height > 0);
    assert(channel_count > 0);

    m_tiles = new Tile*[m_props.m_tile_count];

    for (size_t i = 0; i < m_props.m_tile_count; ++i)
        m_tiles[i] = 0;
}

// Copy constructor.
Image::Image(const Image& rhs)
  : m_props(rhs.m_props)
{
    m_tiles = new Tile*[m_props.m_tile_count];

    for (size_t ty = 0; ty < m_props.m_tile_count_y; ++ty)
    {
        for (size_t tx = 0; tx < m_props.m_tile_count_x; ++tx)
        {
            const size_t tile_index = ty * m_props.m_tile_count_x + tx;

            m_tiles[tile_index] = new Tile(rhs.tile(tx, ty));
        }
    }
}

// Destructor.
Image::~Image()
{
    for (size_t i = 0; i < m_props.m_tile_count; ++i)
        delete m_tiles[i];

    delete [] m_tiles;
}

// Direct access to a given tile.
Tile& Image::tile(
    const size_t        tile_x,
    const size_t        tile_y)
{
    return
        const_cast<Tile&>(
            const_cast<const Image*>(this)->tile(tile_x, tile_y));
}
const Tile& Image::tile(
    const size_t        tile_x,
    const size_t        tile_y) const
{
    const size_t tile_index = tile_y * m_props.m_tile_count_x + tile_x;

    if (m_tiles[tile_index] == 0)
    {
        Tile* tile =
            new Tile(
                m_props.get_tile_width(tile_x),
                m_props.get_tile_height(tile_y),
                m_props.m_channel_count,
                m_props.m_pixel_format);

        memset(tile->pixel(0, 0), 0, tile->get_size());

        m_tiles[tile_index] = tile;
    }

    return *m_tiles[tile_index];
}

}   // namespace foundation
