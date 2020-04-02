
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
#include "image.h"

// appleseed.foundation headers.
#include "foundation/image/tile.h"

// Standard headers.
#include <cassert>
#include <cstdint>
#include <cstring>

namespace foundation
{

//
// Image class implementation.
//

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
        m_tiles[i] = nullptr;
}

Image::Image(const CanvasProperties& props)
  : m_props(props)
{
    m_tiles = new Tile*[m_props.m_tile_count];

    for (size_t i = 0; i < m_props.m_tile_count; ++i)
        m_tiles[i] = nullptr;
}

Image::Image(const Image& rhs)
  : m_props(rhs.m_props)
{
    m_tiles = new Tile*[m_props.m_tile_count];

    for (size_t ty = 0; ty < m_props.m_tile_count_y; ++ty)
    {
        for (size_t tx = 0; tx < m_props.m_tile_count_x; ++tx)
            m_tiles[ty * m_props.m_tile_count_x + tx] = new Tile(rhs.tile(tx, ty));
    }
}

Image::Image(
    const Image&        source,
    const size_t        tile_width,
    const size_t        tile_height,
    const PixelFormat   pixel_format)
  : m_props(
        source.properties().m_canvas_width,
        source.properties().m_canvas_height,
        tile_width,
        tile_height,
        source.properties().m_channel_count,
        pixel_format)
{
    assert(tile_width > 0);
    assert(tile_height > 0);

    const CanvasProperties& source_props = source.properties();

    m_tiles = new Tile*[m_props.m_tile_count];

    for (size_t ty = 0; ty < m_props.m_tile_count_y; ++ty)
    {
        for (size_t tx = 0; tx < m_props.m_tile_count_x; ++tx)
        {
            const size_t tw = m_props.get_tile_width(tx);
            const size_t th = m_props.get_tile_height(ty);

            Tile* tile =
                new Tile(
                    tw,
                    th,
                    m_props.m_channel_count,
                    m_props.m_pixel_format);

            m_tiles[ty * m_props.m_tile_count_x + tx] = tile;

            for (size_t py = 0; py < th; ++py)
            {
                for (size_t px = 0; px < tw; ++px)
                {
                    const size_t ix = tx * m_props.m_tile_width + px;
                    const size_t iy = ty * m_props.m_tile_height + py;
                    const std::uint8_t* source_pixel = source.pixel(ix, iy);

                    Pixel::convert(
                        source_props.m_pixel_format,
                        source_pixel,
                        source_pixel + source_props.m_pixel_size,
                        1,
                        m_props.m_pixel_format,
                        tile->pixel(px, py),
                        1);
                }
            }
        }
    }
}

Image::Image(
    const Image&        source,
    const PixelFormat   pixel_format,
    const size_t*       shuffle_table)
  : m_props(
        source.properties().m_canvas_width,
        source.properties().m_canvas_height,
        source.properties().m_tile_width,
        source.properties().m_tile_height,
        Pixel::get_dest_channel_count(source.properties().m_channel_count, shuffle_table),
        pixel_format
  )
{
    m_tiles = new Tile*[m_props.m_tile_count];

    for (size_t ty = 0; ty < m_props.m_tile_count_y; ++ty)
    {
        for (size_t tx = 0; tx < m_props.m_tile_count_x; ++tx)
        {
            Tile* tile =
                new Tile(
                    source.tile(tx, ty),
                    source.properties().m_pixel_format,
                    shuffle_table);

            m_tiles[ty * m_props.m_tile_count_x + tx] = tile;
        }
    }
}

Image::~Image()
{
    for (size_t i = 0; i < m_props.m_tile_count; ++i)
        delete m_tiles[i];

    delete[] m_tiles;
}

void Image::release()
{
    delete this;
}

Tile& Image::tile(
    const size_t        tile_x,
    const size_t        tile_y)
{
    const size_t tile_index = tile_y * m_props.m_tile_count_x + tile_x;

    if (m_tiles[tile_index] == nullptr)
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

const Tile& Image::tile(
    const size_t        tile_x,
    const size_t        tile_y) const
{
    return const_cast<Image*>(this)->tile(tile_x, tile_y);
}

void Image::set_tile(
    const size_t        tile_x,
    const size_t        tile_y,
    Tile*               tile)
{
    const size_t tile_index = tile_y * m_props.m_tile_count_x + tile_x;
    delete m_tiles[tile_index];
    m_tiles[tile_index] = tile;
}

void Image::copy_from(const Image& source)
{
    assert(m_props.m_canvas_width == source.m_props.m_canvas_width);
    assert(m_props.m_canvas_height == source.m_props.m_canvas_height);
    assert(m_props.m_tile_width == source.m_props.m_tile_width);
    assert(m_props.m_tile_height == source.m_props.m_tile_height);
    assert(m_props.m_channel_count == source.m_props.m_channel_count);

    for (size_t ty = 0; ty < m_props.m_tile_count_y; ++ty)
    {
        for (size_t tx = 0; tx < m_props.m_tile_count_x; ++tx)
            tile(tx, ty).copy_from(source.tile(tx, ty));
    }
}

}   // namespace foundation
