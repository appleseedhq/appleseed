
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_SPPM_SPPMGATHERPOINT_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_SPPM_SPPMGATHERPOINT_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/vector.h"
#include "foundation/platform/thread.h"

// Standard headers.
#include <cstddef>
#include <vector>

// Forward declarations.
namespace renderer  { class BSDF; }

namespace renderer
{

//
// A gather point.
//

class SPPMGatherPoint
{
  public:
    foundation::Vector3f    m_position;             // location of the gather point, in world space
    foundation::Vector2d    m_uv;                   // UV coordinates at the gather point
    foundation::Vector3d    m_geometric_normal;     // geometric normal at the gather point, in world space, unit length
    foundation::Basis3d     m_shading_basis;        // shading basis, in world space
    foundation::Vector3d    m_incoming;             // incoming direction, in world space, unit length
    const BSDF*             m_bsdf;                 // BSDF at the gather point
    Spectrum                m_throughput;           // total throughput from this gather point to the camera
    size_t                  m_pixel_index;          // index of the pixel (y * width + x)
};


//
// A vector of gather points.
//

class SPPMGatherPointVector
  : public std::vector<SPPMGatherPoint>
{
  public:
    // Thread-safe.
    void append(const SPPMGatherPointVector& rhs);

  private:
    boost::mutex m_mutex;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_SPPM_SPPMGATHERPOINT_H
