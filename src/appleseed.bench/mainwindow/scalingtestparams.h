
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Francois Beaune, The appleseedhq Organization
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
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <iterator>
#include <vector>

namespace appleseed {
namespace bench {

class ScalingTestParams
{
  public:
    double m_single_thread_timing;

    enum class Mode
    {
        Quick,
        Exhaustive
    };

    ScalingTestParams(
        const std::size_t   enabled_core_count,
        const std::size_t   max_thread_count)
      : m_enabled_core_count(enabled_core_count)
      , m_max_thread_count(max_thread_count)
    {
    }

    void set_mode(const Mode mode)
    {
        m_mode = mode;
    }

    void reset()
    {
        m_thread_counts.clear();

        if (m_mode == Mode::Quick)
        {
            for (std::size_t t = 1; t < m_max_thread_count; t *= 2)
                m_thread_counts.push_back(t);

            m_thread_counts.push_back(m_enabled_core_count);
            m_thread_counts.push_back(m_max_thread_count);

            std::sort(std::begin(m_thread_counts), std::end(m_thread_counts));

            m_thread_counts.erase(
                std::unique(std::begin(m_thread_counts), std::end(m_thread_counts)),
                std::end(m_thread_counts));
        }
        else
        {
            for (std::size_t t = 1; t <= m_max_thread_count; ++t)
                m_thread_counts.push_back(t);
        }

        m_index = 0;
    }

    std::size_t get_thread_count() const
    {
        assert(m_index < m_thread_counts.size());
        return m_thread_counts[m_index];
    }

    void next()
    {
        assert(m_index < m_thread_counts.size());
        ++m_index;
    }

    bool is_complete() const
    {
        return m_index == m_thread_counts.size();
    }

  private:
    const std::size_t           m_enabled_core_count;
    const std::size_t           m_max_thread_count;
    Mode                        m_mode = Mode::Quick;
    std::vector<std::size_t>    m_thread_counts;
    std::size_t                 m_index = 0;
};

}   // namespace bench
}   // namespace appleseed
