
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_SPPM_SPPMPHOTONMAP_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_SPPM_SPPMPHOTONMAP_H

// appleseed.renderer headers.
#include "renderer/kernel/lighting/sppm/sppmphoton.h"

// appleseed.foundation headers.
#include "foundation/math/knn.h"

// Standard headers.
#include <cstddef>

namespace renderer
{

class SPPMPhotonMap
  : public foundation::knn::Tree3f
{
  public:
    // Constructor, moves the photons into the photon map.
    explicit SPPMPhotonMap(PhotonVector& photons);

    // Return the size (in bytes) of this object in memory.
    size_t get_memory_size() const;

    // Return the payload of the index'th photon in the photon map.
    const SPPMPhotonPayload& get_photon_payload(const size_t index) const;

  private:
    PhotonVector m_photons;
};


//
// SPPMPhotonMap class implementation.
//

inline const SPPMPhotonPayload& SPPMPhotonMap::get_photon_payload(const size_t index) const
{
    return m_photons.m_payloads[index];
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_SPPM_SPPMPHOTONMAP_H
