
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Bishev Artem, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_VOLUME_DISTANCESAMPLE_H
#define APPLESEED_RENDERER_MODELING_VOLUME_DISTANCESAMPLE_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/directshadingcomponents.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/bsdf/bsdf.h"

namespace renderer { class BSDF; }
namespace renderer { class ShadingPoint; }
namespace renderer { class ShadingRay; }

// Forward declarations.
namespace renderer
{

//
// The DistanceSample class represents the result of sampling a scattering distance in Volume.
// It is also used to pass arguments to the Volume::sample_distance() method.
//

class DistanceSample
{
  public:
    // Inputs.
    const ShadingPoint*             m_outgoing_point;
    const ShadingRay*               m_volume_ray;           // ray that is being traced through the volume
    const foundation::Vector3d*	    m_pivot;                // pivot that can be used for better importance sampling 

    // Outputs.
    ShadingPoint*                   m_incoming_point;
    bool                            m_transmitted;          // if ray reached the next surface
    double                          m_distance;             // sampled distance
    float                           m_probability;          // PDF value, defined only if m_transmitted is false
    Spectrum                        m_value;                // transmission value
    const BSDF*                     m_bsdf;                 // BSDF representing phase function at the sampled point
    void*                           m_bsdf_data;            // BSDF data evaluated at the sample point
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_BSDF_BSDFSAMPLE_H
