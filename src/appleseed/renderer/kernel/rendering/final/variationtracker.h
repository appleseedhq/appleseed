
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_RENDERING_FINAL_VARIATIONTRACKER_H
#define APPLESEED_RENDERER_KERNEL_RENDERING_FINAL_VARIATIONTRACKER_H

// Standard headers.
#include <algorithm>
#include <cmath>
#include <cstddef>

namespace renderer
{

class VariationTracker
{
  public:
    VariationTracker()
      : m_size(0)
      , m_mean(0.0f)
    {
        reset_variation();
    }

    void reset_variation()
    {
        m_min = +1.0e38f;
        m_max = -1.0e38f;
    }

    void insert(const float value)
    {
        ++m_size;

        m_mean += (value - m_mean) / m_size;

        m_min = std::min(m_min, m_mean);
        m_max = std::max(m_max, m_mean);
    }

    size_t get_size() const
    {
        return m_size;
    }

    float get_mean() const
    {
        return m_mean;
    }

    float get_variation() const
    {
        if (m_size == 0)
            return 0.0;

        const float spread = m_max - m_min;
        return std::abs(m_mean != 0.0f ? spread / m_mean : spread);
    }

  private:
    size_t  m_size;
    float   m_mean;
    float   m_min;
    float   m_max;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_RENDERING_FINAL_VARIATIONTRACKER_H
