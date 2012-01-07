
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
#include "aovcollection.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"

// Standard headers.
#include <cassert>
#include <cstddef>

using namespace foundation;
using namespace std;

namespace renderer
{

AOVCollection::AOVCollection()
  : m_aov_count(0)
{
}

void AOVCollection::copy_declarations_from(const AOVCollection& source)
{
    m_aov_count = source.m_aov_count;

    for (size_t i = 0; i < m_aov_count; ++i)
        m_aovs[i].m_uid = source.m_aovs[i].m_uid;
}

void AOVCollection::declare(const UniqueID uid)
{
    if (m_aov_count == MaxAOVCount)
        throw new Exception("too many AOVs");

    m_aovs[m_aov_count].m_uid = uid;

    ++m_aov_count;
}

void AOVCollection::set(const float val)
{
    for (size_t i = 0; i < m_aov_count; ++i)
        m_aovs[i].m_spectrum.set(val);

    m_trash.set(0.0f);
}

Spectrum& AOVCollection::operator[](const UniqueID uid)
{
    for (size_t i = 0; i < m_aov_count; ++i)
    {
        if (m_aovs[i].m_uid == uid)
            return m_aovs[i].m_spectrum;
    }

    return m_trash;
}

const Spectrum& AOVCollection::operator[](const UniqueID uid) const
{
    return const_cast<AOVCollection&>(*this)[uid];
}

AOVCollection& AOVCollection::operator+=(const AOVCollection& rhs)
{
    assert(m_aov_count == rhs.m_aov_count);

    for (size_t i = 0; i < m_aov_count; ++i)
    {
        assert(m_aovs[i].m_uid == rhs.m_aovs[i].m_uid);
        m_aovs[i].m_spectrum += rhs.m_aovs[i].m_spectrum;
    }

    return *this;
}

AOVCollection& AOVCollection::operator*=(const Spectrum& rhs)
{
    for (size_t i = 0; i < m_aov_count; ++i)
        m_aovs[i].m_spectrum *= rhs;

    return *this;
}

AOVCollection& AOVCollection::operator*=(const float rhs)
{
    for (size_t i = 0; i < m_aov_count; ++i)
        m_aovs[i].m_spectrum *= rhs;

    return *this;
}

AOVCollection& AOVCollection::operator/=(const float rhs)
{
    for (size_t i = 0; i < m_aov_count; ++i)
        m_aovs[i].m_spectrum /= rhs;

    return *this;
}

}   // namespace renderer
