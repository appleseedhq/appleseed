
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

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdint>

namespace renderer
{

template <std::size_t N>
class SampleCountHistory
{
  public:
    SampleCountHistory()
    {
        clear();
    }

    void clear()
    {
        m_size = 0;
        m_first = 0;
        m_index = 0;
    }

    void insert(const double time, const std::uint64_t value)
    {
        m_index = m_first;

        m_records[m_index].m_time = time;
        m_records[m_index].m_value = value;

        m_first = (m_first + 1) % N;

        if (m_size < N)
            ++m_size;
    }

    double get_samples_per_second() const
    {
        if (m_size == 0)
            return 0.0;

        const Record* first = &m_records[m_size < N ? 0 : m_first];
        const Record* last = &m_records[m_index];

        assert(last->m_value >= first->m_value);
        assert(last->m_time >= first->m_time);

        const std::uint64_t delta_value = last->m_value - first->m_value;
        const double delta_time = last->m_time - first->m_time;

        return delta_time > 0.0 ? delta_value / delta_time : 0.0;
    }

  private:
    struct Record
    {
        double          m_time;
        std::uint64_t   m_value;
    };

    Record      m_records[N];
    std::size_t m_size;
    std::size_t m_first;
    std::size_t m_index;
};

}   // namespace renderer
