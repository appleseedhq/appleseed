
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "transformsequence.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <vector>

using namespace foundation;
using namespace std;

namespace renderer
{

TransformSequence::TransformSequence()
  : m_capacity(0)
  , m_size(0)
  , m_keys(0)
  , m_interpolators(0)
{
}

TransformSequence::~TransformSequence()
{
    delete [] m_interpolators;
    delete [] m_keys;
}

void TransformSequence::set_transform(
    const double        time,
    const Transformd&   transform)
{
    assert(m_size <= m_capacity);
    assert(m_interpolators == 0);

    for (size_t i = 0; i < m_size; ++i)
    {
        if (m_keys[i].m_time == time)
        {
            m_keys[i].m_transform = transform;
            return;
        }
    }

    if (m_size == m_capacity)
    {
        m_capacity += m_capacity > 0 ? m_capacity : 1;

        TransformKey* new_keys = new TransformKey[m_capacity];

        for (size_t i = 0; i < m_size; ++i)
            new (&new_keys[i]) TransformKey(m_keys[i]);

        delete [] m_keys;
        m_keys = new_keys;
    }

    m_keys[m_size].m_time = time;
    m_keys[m_size].m_transform = transform;

    ++m_size;
}

void TransformSequence::get_transform(
    const size_t        index,
    double&             time,
    Transformd&         transform) const
{
    assert(m_size > 0);
    assert(index < m_size);

    time = m_keys[index].m_time;
    transform = m_keys[index].m_transform;
}

bool TransformSequence::empty() const
{
    return m_size == 0;
}

size_t TransformSequence::size() const
{
    return m_size;
}

void TransformSequence::prepare()
{
    assert(m_size > 0);
    assert(m_interpolators == 0);

    if (m_size > 1)
    {
        sort(m_keys, m_keys + m_size);

        m_interpolators = new TransformInterpolatord[m_size - 1];

        for (size_t i = 0; i < m_size - 1; ++i)
        {
            m_interpolators[i].set_transforms(
                m_keys[i].m_transform,
                m_keys[i + 1].m_transform);
        }
    }
}

Transformd TransformSequence::evaluate(const double time) const
{
    assert(m_size > 0);
    assert(m_size == 1 || m_interpolators != 0);

    const TransformKey* first = m_keys;

    if (m_size == 1)
        return first->m_transform;

    if (time <= first->m_time)
        return first->m_transform;

    const TransformKey* last = m_keys + m_size - 1;

    if (time >= last->m_time)
        return last->m_transform;

    size_t begin = 0;
    size_t end = m_size;

    while (end - begin > 1)
    {
        const size_t mid = (begin + end) / 2;
        if (time < m_keys[mid].m_time)
            end = mid;
        else begin = mid;
    }

    const double begin_time = m_keys[begin].m_time;
    const double end_time = m_keys[end].m_time;
    const double span_time = end_time - begin_time;

    return m_interpolators[begin].evaluate((time - begin_time) / span_time);
}

}   // namespace renderer
