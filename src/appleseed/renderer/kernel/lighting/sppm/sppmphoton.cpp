
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
#include "sppmphoton.h"

// appleseed.foundation headers.
#include "foundation/memory/memory.h"

using namespace foundation;

namespace renderer
{

//
// SPPMPhotonVector class implementation.
//

bool SPPMPhotonVector::empty() const
{
    return m_positions.empty();
}

size_t SPPMPhotonVector::size() const
{
    return m_positions.size();
}

size_t SPPMPhotonVector::get_memory_size() const
{
    return
        m_positions.capacity() * sizeof(Vector3f) +
        m_mono_photons.capacity() * sizeof(SPPMMonoPhoton) +
        m_poly_photons.capacity() * sizeof(SPPMPolyPhoton);
}

void SPPMPhotonVector::swap(SPPMPhotonVector& rhs)
{
    m_positions.swap(rhs.m_positions);
    m_mono_photons.swap(rhs.m_mono_photons);
    m_poly_photons.swap(rhs.m_poly_photons);
}

void SPPMPhotonVector::clear_release_memory()
{
    foundation::clear_release_memory(m_positions);
    foundation::clear_release_memory(m_mono_photons);
    foundation::clear_release_memory(m_poly_photons);
}

void SPPMPhotonVector::clear_keep_memory()
{
    foundation::clear_keep_memory(m_positions);
    foundation::clear_keep_memory(m_mono_photons);
    foundation::clear_keep_memory(m_poly_photons);
}

void SPPMPhotonVector::reserve_mono_photons(const size_t capacity)
{
    m_positions.reserve(capacity);
    m_mono_photons.reserve(capacity);
}

void SPPMPhotonVector::reserve_poly_photons(const size_t capacity)
{
    m_positions.reserve(capacity);
    m_poly_photons.reserve(capacity);
}

void SPPMPhotonVector::push_back(
    const Vector3f&         position,
    const SPPMMonoPhoton&   photon)
{
    m_positions.push_back(position);
    m_mono_photons.push_back(photon);
}

void SPPMPhotonVector::push_back(
    const Vector3f&         position,
    const SPPMPolyPhoton&   photon)
{
    m_positions.push_back(position);
    m_poly_photons.push_back(photon);
}

void SPPMPhotonVector::append(const SPPMPhotonVector& rhs)
{
    boost::mutex::scoped_lock lock(m_mutex);
    m_positions.insert(m_positions.end(), rhs.m_positions.begin(), rhs.m_positions.end());
    m_mono_photons.insert(m_mono_photons.end(), rhs.m_mono_photons.begin(), rhs.m_mono_photons.end());
    m_poly_photons.insert(m_poly_photons.end(), rhs.m_poly_photons.begin(), rhs.m_poly_photons.end());
}

}   // namespace renderer
