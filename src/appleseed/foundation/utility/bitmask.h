
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

// appleseed.foundation headers.
#include "foundation/platform/types.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstring>

namespace foundation
{

//
// A compact 2D array of bits.
//

class BitMask2
{
  public:
    // The constructor does not initialize the content of the bit mask.
    BitMask2(
        const size_t    width,
        const size_t    height);

    ~BitMask2();

    size_t get_width() const;
    size_t get_height() const;

    void clear();

    void clear(
        const size_t    x,
        const size_t    y);

    void set(
        const size_t    x,
        const size_t    y);

    void set(
        const size_t    x,
        const size_t    y,
        const bool      bit);

    bool get(
        const size_t    x,
        const size_t    y) const;

    bool is_clear(
        const size_t    x,
        const size_t    y) const;

    bool is_set(
        const size_t    x,
        const size_t    y) const;

    size_t get_memory_size() const;

  private:
    size_t  m_width;
    size_t  m_height;
    size_t  m_block_width;
    size_t  m_size;
    uint8*  m_bits;
};


//
// BitMask2 class implementation.
//

inline BitMask2::BitMask2(
    const size_t        width,
    const size_t        height)
  : m_width(width)
  , m_height(height)
  , m_block_width((width + 7) / 8)
  , m_size(m_block_width * m_height)
  , m_bits(new uint8[m_size])
{
}

inline BitMask2::~BitMask2()
{
    delete[] m_bits;
}

inline size_t BitMask2::get_width() const
{
    return m_width;
}

inline size_t BitMask2::get_height() const
{
    return m_height;
}

inline void BitMask2::clear()
{
    std::memset(m_bits, 0, m_size);
}

inline void BitMask2::clear(
    const size_t        x,
    const size_t        y)
{
    assert(x < m_width);
    assert(y < m_height);

    m_bits[y * m_block_width + x / 8] &= ~(1UL << (x & 7));
}

inline void BitMask2::set(
    const size_t        x,
    const size_t        y)
{
    assert(x < m_width);
    assert(y < m_height);

    m_bits[y * m_block_width + x / 8] |= 1UL << (x & 7);
}

inline void BitMask2::set(
    const size_t        x,
    const size_t        y,
    const bool          bit)
{
    bit ? set(x, y) : clear(x, y);
}

inline bool BitMask2::get(
    const size_t        x,
    const size_t        y) const
{
    assert(x < m_width);
    assert(y < m_height);

    return (m_bits[y * m_block_width + x / 8] & (1UL << (x & 7))) != 0;
}

inline bool BitMask2::is_clear(
    const size_t        x,
    const size_t        y) const
{
    return get(x, y) == false;
}

inline bool BitMask2::is_set(
    const size_t        x,
    const size_t        y) const
{
    return get(x, y) == true;
}

inline size_t BitMask2::get_memory_size() const
{
    return sizeof(*this) + m_size;
}

}   // namespace foundation
