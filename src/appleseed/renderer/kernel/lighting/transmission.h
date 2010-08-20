
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_TRANSMISSION_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_TRANSMISSION_H

// appleseed.renderer headers.
#include "renderer/global/global.h"

// Forward declarations.
namespace renderer      { class ShadingContext; }
namespace renderer      { class ShadingPoint; }

namespace renderer
{

//
// Compute the transmission factor along a given direction.
//

double compute_transmission(
    SamplingContext&                sampling_context,
    const ShadingContext&           shading_context,
    const foundation::Vector3d&     origin,
    const foundation::Vector3d&     direction,
    const double                    max_distance,
    const ShadingPoint*             parent_shading_point = 0);

double compute_transmission(
    SamplingContext&                sampling_context,
    const ShadingContext&           shading_context,
    const foundation::Vector3d&     origin,
    const foundation::Vector3d&     direction,
    const ShadingPoint*             parent_shading_point = 0);


//
// Compute the transmission factor between two points in space.
//

double compute_transmission_between(
    SamplingContext&                sampling_context,
    const ShadingContext&           shading_context,
    const foundation::Vector3d&     origin,
    const foundation::Vector3d&     target,
    const ShadingPoint*             parent_shading_point = 0);


//
// Implementation.
//

inline double compute_transmission(
    SamplingContext&                sampling_context,
    const ShadingContext&           shading_context,
    const foundation::Vector3d&     origin,
    const foundation::Vector3d&     direction,
    const ShadingPoint*             parent_shading_point)
{
    return
        compute_transmission(
            sampling_context,
            shading_context,
            origin,
            direction,
            std::numeric_limits<double>::max(),
            parent_shading_point);
}

inline double compute_transmission_between(
    SamplingContext&                sampling_context,
    const ShadingContext&           shading_context,
    const foundation::Vector3d&     origin,
    const foundation::Vector3d&     target,
    const ShadingPoint*             parent_shading_point)
{
    return
        compute_transmission(
            sampling_context,
            shading_context,
            origin,
            target - origin,
            1.0,
            parent_shading_point);
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_TRANSMISSION_H
