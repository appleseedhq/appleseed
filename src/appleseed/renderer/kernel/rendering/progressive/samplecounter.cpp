
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "samplecounter.h"

// Standard headers.
#include <algorithm>
#include <cassert>

using namespace boost;
using namespace foundation;
using namespace std;

namespace renderer
{

SampleCounter::SampleCounter(const uint64 max_sample_count)
  : m_max_sample_count(max_sample_count)
  , m_sample_count(0)
{
}

void SampleCounter::clear()
{
    Spinlock::ScopedLock lock(m_spinlock);

    m_sample_count = 0;
}

uint64 SampleCounter::read() const
{
    Spinlock::ScopedLock lock(m_spinlock);

    return m_sample_count;
}

size_t SampleCounter::reserve(const size_t sample_count)
{
    Spinlock::ScopedLock lock(m_spinlock);

    assert(m_sample_count <= m_max_sample_count);

    const uint64 reserved_sample_count =
        min(
            static_cast<uint64>(sample_count),
            m_max_sample_count - m_sample_count);

    m_sample_count += reserved_sample_count;

    return static_cast<size_t>(reserved_sample_count);
}

}   // namespace renderer
