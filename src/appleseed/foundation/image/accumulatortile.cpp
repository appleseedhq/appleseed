
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
#include "accumulatortile.h"

// appleseed.foundation headers.
#include "foundation/image/pixel.h"
#include "foundation/math/scalar.h"
#include "foundation/platform/atomic.h"

namespace foundation
{

AccumulatorTile::AccumulatorTile(
    const size_t            width,
    const size_t            height,
    const size_t            channel_count)
  : Tile(width, height, channel_count + 1, PixelFormatFloat)
  , m_crop_window(Vector2u(0, 0), Vector2u(width - 1, height - 1))
{
}

AccumulatorTile::AccumulatorTile(
    const size_t            width,
    const size_t            height,
    const size_t            channel_count,
    const AABB2u&           crop_window)
  : Tile(width, height, channel_count + 1, PixelFormatFloat)
  , m_crop_window(crop_window)
{
}

void AccumulatorTile::clear()
{
    float* ptr = reinterpret_cast<float*>(pixel(0));

    for (size_t i = 0, e = m_pixel_count * m_channel_count; i < e; ++i)
        ptr[i] = 0.0f;
}

void AccumulatorTile::add(
    const Vector2u&         pi,
    const float*            values)
{
    // Ignore samples outside the crop window.
    if (!m_crop_window.contains(pi))
        return;

    float* APPLESEED_RESTRICT ptr = reinterpret_cast<float*>(pixel(pi.x, pi.y));
    *ptr++ += 1.0f;

    for (size_t i = 0, e = m_channel_count - 1; i < e; ++i)
        *ptr++ += values[i];
}

void AccumulatorTile::atomic_add(
    const Vector2u&         pi,
    const float*            values)
{
    // Ignore samples outside the crop window.
    if (!m_crop_window.contains(pi))
        return;

    float* APPLESEED_RESTRICT ptr = reinterpret_cast<float*>(pixel(pi.x, pi.y));
    foundation::atomic_add(ptr++, 1.0f);

    for (size_t i = 0, e = m_channel_count - 1; i < e; ++i)
        foundation::atomic_add(ptr++, values[i]);
}

}   // namespace foundation
