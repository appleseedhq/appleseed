
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

// Interface header.
#include "sppmphoton.h"

// appleseed.foundation headers.
#include "foundation/utility/memory.h"

using namespace foundation;

namespace renderer
{

//
// SPPMPhotonVector class implementation.
//

bool SPPMPhotonVector::empty() const
{
    assert(m_positions.empty() == m_data.empty());
    return m_positions.empty();
}

size_t SPPMPhotonVector::size() const
{
    assert(m_positions.size() == m_data.size());
    return m_positions.size();
}

size_t SPPMPhotonVector::get_memory_size() const
{
    return
        m_positions.capacity() * sizeof(Vector3f) +
        m_data.capacity() * sizeof(SPPMPhotonData);
}

void SPPMPhotonVector::swap(SPPMPhotonVector& rhs)
{
    m_positions.swap(rhs.m_positions);
    m_data.swap(rhs.m_data);
}

void SPPMPhotonVector::clear_keep_memory()
{
    foundation::clear_keep_memory(m_positions);
    foundation::clear_keep_memory(m_data);
}

void SPPMPhotonVector::reserve(const size_t capacity)
{
    m_positions.reserve(capacity);
    m_data.reserve(capacity);
}

void SPPMPhotonVector::push_back(const SPPMPhoton& photon)
{
    m_positions.push_back(photon.m_position);
    m_data.push_back(photon.m_data);
}

}   // namespace renderer
