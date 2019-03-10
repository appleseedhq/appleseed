
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/math/cdf.h"
#include "foundation/math/filter.h"
#include "foundation/math/scalar.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <memory>

namespace foundation
{

//
// Filter Sampling Table.
//

class FilterSamplingTable
{
  public:
    // Build a sampling table from a filter that can be analytically sampled.
    explicit FilterSamplingTable(const BoxFilter1<float>& filter);
    explicit FilterSamplingTable(const TriangleFilter1<float>& filter);

    // Build a sampling table inverting a CDF.
    template <typename Filter1Df>
    explicit FilterSamplingTable(const Filter1Df& filter);

    float operator[](const size_t i) const;

    float sample(const float s) const;

  private:
    enum {TableSize = 511};
    float m_table[TableSize + 1];

    float do_sample(const float s) const;
};


//
// FilterTable class implementation.
//

template <typename Filter1Df>
FilterSamplingTable::FilterSamplingTable(const Filter1Df& filter)
{
    CDF<float, float> cdf;
    cdf.reserve(TableSize);

    const float filter_radius = filter.get_radius();

    for (size_t i = 0; i < TableSize; ++i)
    {
        const float t = i / static_cast<float>(TableSize);
        const float d = lerp(0.0f, filter_radius, t);
        cdf.insert(d, filter.evaluate(d));
    }

    cdf.prepare();

    for (size_t i = 0; i < TableSize; ++i)
    {
        const float s = i / static_cast<float>(TableSize);
        const float d = cdf.sample(s).first;
        m_table[i] = d;
    }

    m_table[TableSize] = filter_radius;
}

inline float FilterSamplingTable::operator[](const size_t i) const
{
    assert(i < TableSize);
    return m_table[i];
}

inline float FilterSamplingTable::sample(const float s) const
{
    return s < 0.5f
        ? -do_sample(2.0f * s)
        :  do_sample(2.0f * (s - 0.5f));
}

inline float FilterSamplingTable::do_sample(const float s) const
{
    assert(s >= 0.0f);
    assert(s < 1.0f);

    size_t i;
    const float t = floor_frac(s * TableSize, i);
    return lerp(m_table[i], m_table[i + 1], t);
}

}   // namespace foundation
