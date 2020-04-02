
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

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/platform/thread.h"

// Standard headers.
#include <cstddef>
#include <cstdint>
#include <vector>

namespace renderer
{

//
// A monochromatic photon.
//

struct SpectrumLine
{
    std::uint32_t           m_wavelength;
    float                   m_amplitude;
};

class SPPMMonoPhoton
{
  public:
    foundation::Vector3f    m_incoming;             // incoming direction, world space, unit length
    foundation::Vector3f    m_geometric_normal;     // geometric normal at the photon location, world space, unit length
    SpectrumLine            m_flux;                 // flux carried by this photon (in W)
};


//
// A polychromatic photon.
//

class SPPMPolyPhoton
{
  public:
    foundation::Vector3f    m_incoming;             // incoming direction, world space, unit length
    foundation::Vector3f    m_geometric_normal;     // geometric normal at the photon location, world space, unit length
    Spectrum                m_flux;                 // flux carried by this photon (in W)
};


//
// A vector of photons.
//

class SPPMPhotonVector
{
  public:
    std::vector<foundation::Vector3f>   m_positions;
    std::vector<SPPMMonoPhoton>         m_mono_photons;
    std::vector<SPPMPolyPhoton>         m_poly_photons;
    boost::mutex                        m_mutex;

    bool empty() const;
    size_t size() const;

    // Return the size (in bytes) of this object in memory.
    size_t get_memory_size() const;

    void swap(SPPMPhotonVector& rhs);

    void clear_release_memory();
    void clear_keep_memory();

    void reserve_mono_photons(const size_t capacity);
    void reserve_poly_photons(const size_t capacity);

    void push_back(
        const foundation::Vector3f&     position,
        const SPPMMonoPhoton&           photon);
    void push_back(
        const foundation::Vector3f&     position,
        const SPPMPolyPhoton&           photon);

    // The only thread-safe method of this class.
    void append(const SPPMPhotonVector& rhs);
};

}   // namespace renderer
