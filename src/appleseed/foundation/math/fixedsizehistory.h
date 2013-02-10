
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_FOUNDATION_MATH_FIXEDSIZEHISTORY_H
#define APPLESEED_FOUNDATION_MATH_FIXEDSIZEHISTORY_H

// Standard headers.
#include <cstddef>

namespace foundation
{

template <typename T, size_t N>
class FixedSizeHistory
{
  public:
    FixedSizeHistory()
      : m_size(0)
      , m_index(0)
    {
    }

    size_t size() const
    {
        return m_size;
    }

    void insert(const T val)
    {
        m_history[m_index++] = val;

        if (m_index == N)
            m_index = 0;

        if (m_size < N)
            ++m_size;
    }

    T compute_average() const
    {
        T sum = T(0);

        for (size_t i = 0; i < m_size; ++i)
            sum += m_history[i];

        return m_size > 0 ? sum / m_size : T(0);
    }

  private:
    T       m_history[N];
    size_t  m_size;
    size_t  m_index;
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_FIXEDSIZEHISTORY_H
