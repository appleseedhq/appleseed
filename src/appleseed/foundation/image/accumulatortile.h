
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
#include "foundation/image/tile.h"
#include "foundation/math/aabb.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cassert>
#include <cstddef>

namespace foundation
{

//
// A 2D tile that supports accumulation of values.
//

class AccumulatorTile
  : public Tile
{
  public:
    AccumulatorTile(
        const size_t        width,
        const size_t        height,
        const size_t        channel_count);

    AccumulatorTile(
        const size_t        width,
        const size_t        height,
        const size_t        channel_count,
        const AABB2u&       crop_window);

    // Tile properties.
    size_t get_channel_count() const;   // number of channels in one pixel, excluding the weight channel
    const AABB2u& get_crop_window() const;

    // Direct access to a given pixel.
    float* pixel(
        const size_t        i) const;
    float* pixel(
        const size_t        x,
        const size_t        y) const;

    // Structured write access to a given pixel is not available.
    template <typename T>
    void set_pixel(
        const size_t        i,
        const T             components[]);
    template <typename T>
    void set_pixel(
        const size_t        x,
        const size_t        y,
        const T             components[]);
    template <typename Color>
    void set_pixel(
        const size_t        i,
        const Color&        color);
    template <typename Color>
    void set_pixel(
        const size_t        x,
        const size_t        y,
        const Color&        color);

    // Structured read access to a given pixel.
    template <typename T>
    void get_pixel(
        const size_t        i,
        T                   components[]) const;
    template <typename T>
    void get_pixel(
        const size_t        x,
        const size_t        y,
        T                   components[]) const;
    template <typename Color>
    void get_pixel(
        const size_t        i,
        Color&              color) const;
    template <typename Color>
    void get_pixel(
        const size_t        x,
        const size_t        y,
        Color&              color) const;

    // Set all pixels to black and all weights to zero.
    void clear();

    void add(
        const Vector2u&     pi,
        const float*        values);

    // Thread-safe variant of add().
    void atomic_add(
        const Vector2u&     pi,
        const float*        values);

  protected:
    const AABB2u m_crop_window;
};


//
// AccumulatorTile class implementation.
//

inline size_t AccumulatorTile::get_channel_count() const
{
    return m_channel_count - 1;
}

inline const AABB2u& AccumulatorTile::get_crop_window() const
{
    return m_crop_window;
}

inline float* AccumulatorTile::pixel(
    const size_t            i) const
{
    return reinterpret_cast<float*>(Tile::pixel(i));
}

inline float* AccumulatorTile::pixel(
    const size_t            x,
    const size_t            y) const
{
    return reinterpret_cast<float*>(Tile::pixel(x, y));
}

template <typename T>
inline void AccumulatorTile::get_pixel(
    const size_t            i,
    T                       components[]) const
{
    const float* APPLESEED_RESTRICT ptr = pixel(i);

    const float weight = *ptr++;
    const float rcp_weight = weight == 0.0f ? 0.0f : 1.0f / weight;

    for (size_t i = 0; i < m_channel_count - 1; ++i)
        components[i] = ptr[i] * rcp_weight;
}

template <typename T>
inline void AccumulatorTile::get_pixel(
    const size_t            x,
    const size_t            y,
    T                       components[]) const
{
    assert(x < m_width);
    assert(y < m_height);

    get_pixel<T>(y * m_width + x, components);
}

template <typename Color>
inline void AccumulatorTile::get_pixel(
    const size_t            i,
    Color&                  color) const
{
    assert(sizeof(Color) == (m_channel_count - 1) * sizeof(color[0]));

    get_pixel(i, &color[0]);
}

template <typename Color>
inline void AccumulatorTile::get_pixel(
    const size_t            x,
    const size_t            y,
    Color&                  color) const
{
    assert(sizeof(Color) == (m_channel_count - 1) * sizeof(color[0]));

    get_pixel(x, y, &color[0]);
}

}   // namespace foundation
