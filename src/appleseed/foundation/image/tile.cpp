
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
#include "tile.h"

namespace foundation
{

//
// Tile class implementation.
//

Tile::Tile(
    const size_t        width,
    const size_t        height,
    const size_t        channel_count,
    const PixelFormat   pixel_format,
    std::uint8_t*       storage)
  : m_width(width)
  , m_height(height)
  , m_channel_count(channel_count)
  , m_pixel_format(pixel_format)
  , m_pixel_count(width * height)
  , m_channel_size(Pixel::size(pixel_format))
  , m_pixel_size(m_channel_size * channel_count)
  , m_array_size(m_pixel_size * m_pixel_count)
{
    assert(m_width > 0);
    assert(m_height > 0);
    assert(m_channel_count > 0);

    if (storage)
    {
         m_pixel_array = storage;
         m_own_storage = false;
    }
    else
    {
        m_pixel_array = new std::uint8_t[m_array_size];
        m_own_storage = true;
    }
}

Tile::Tile(
    const Tile&         tile,
    const PixelFormat   pixel_format,
    std::uint8_t*       storage)
  : m_width(tile.m_width)
  , m_height(tile.m_height)
  , m_channel_count(tile.m_channel_count)
  , m_pixel_format(pixel_format)
  , m_pixel_count(tile.m_pixel_count)
  , m_channel_size(Pixel::size(pixel_format))
  , m_pixel_size(m_channel_size * m_channel_count)
  , m_array_size(m_pixel_size * m_pixel_count)
{
    if (storage)
    {
         m_pixel_array = storage;
         m_own_storage = false;
    }
    else
    {
        m_pixel_array = new std::uint8_t[m_array_size];
        m_own_storage = true;
    }

    copy_from(tile);
}

Tile::Tile(
    const Tile&         tile,
    const PixelFormat   pixel_format,
    const size_t*       shuffle_table,
    std::uint8_t*       storage)
  : m_width(tile.m_width)
  , m_height(tile.m_height)
  , m_channel_count(Pixel::get_dest_channel_count(tile.m_channel_count, shuffle_table))
  , m_pixel_format(pixel_format)
  , m_pixel_count(tile.m_pixel_count)
  , m_channel_size(Pixel::size(pixel_format))
  , m_pixel_size(m_channel_size * m_channel_count)
  , m_array_size(m_pixel_size * m_pixel_count)
{
    if (storage)
    {
         m_pixel_array = storage;
         m_own_storage = false;
    }
    else
    {
        m_pixel_array = new std::uint8_t[m_array_size];
        m_own_storage = true;
    }

    Pixel::convert_and_shuffle(
        tile.m_pixel_format,                            // source format
        tile.m_channel_count,                           // source channels
        tile.m_pixel_array,                             // source begin
        tile.m_pixel_array + tile.m_array_size,         // source end
        m_pixel_format,                                 // destination format
        m_channel_count,                                // destination channels
        m_pixel_array,                                  // destination
        shuffle_table);                                 // channel shuffling table
}

Tile::Tile(const Tile& rhs)
  : m_width(rhs.m_width)
  , m_height(rhs.m_height)
  , m_channel_count(rhs.m_channel_count)
  , m_pixel_format(rhs.m_pixel_format)
  , m_pixel_count(rhs.m_pixel_count)
  , m_channel_size(rhs.m_channel_size)
  , m_pixel_size(rhs.m_pixel_size)
  , m_array_size(rhs.m_array_size)
  , m_pixel_array(new std::uint8_t[rhs.m_array_size])
  , m_own_storage(true)
{
    memcpy(
        m_pixel_array,                                  // destination
        rhs.m_pixel_array,                              // source
        rhs.m_array_size);                              // bytes to copy
}

Tile::~Tile()
{
    if (m_own_storage)
        delete[] m_pixel_array;
}

void Tile::release()
{
    delete this;
}

size_t Tile::get_memory_size() const
{
    return sizeof(*this) + get_size();
}

void Tile::copy_from(const Tile& source)
{
    assert(m_width == source.m_width);
    assert(m_height == source.m_height);
    assert(m_channel_count == source.m_channel_count);

    if (m_pixel_format == source.m_pixel_format)
    {
        memcpy(
            m_pixel_array,                              // destination
            source.m_pixel_array,                       // source
            source.m_array_size);                       // bytes to copy
    }
    else
    {
        Pixel::convert(
            source.m_pixel_format,                      // source format
            source.m_pixel_array,                       // source begin
            source.m_pixel_array + source.m_array_size, // source end
            1,                                          // source stride
            m_pixel_format,                             // destination format
            m_pixel_array,                              // destination
            1);                                         // destination stride
    }
}

}   // namespace foundation
