
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

#ifndef APPLESEED_RENDERER_KERNEL_SHADING_AMBIENTOCCLUSION_H
#define APPLESEED_RENDERER_KERNEL_SHADING_AMBIENTOCCLUSION_H

// appleseed.renderer headers.
#include "renderer/global/global.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"

// Forward declarations.
namespace renderer      { class Intersector; }
namespace renderer      { class ShadingPoint; }

namespace renderer
{

//
// Compute classic ambient occlusion at a given point in space.
//
// todo: implement optional computation of the mean unoccluded direction.
//

double compute_ambient_occlusion(
    const SamplingContext&          sampling_context,
    const Intersector&              intersector,
    const foundation::Vector3d&     point,              // world space point
    const foundation::Vector3d&     geometic_normal,    // world space geometric normal, unit-length
    const foundation::Basis3d&      shading_basis,      // world space orthonormal basis around shading normal
    const double                    max_distance,
    const size_t                    sample_count,
    const ShadingPoint*             parent_shading_point = 0);

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_SHADING_AMBIENTOCCLUSION_H
