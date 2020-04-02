
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
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/scene/visibilityflags.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cassert>
#include <cstddef>

namespace renderer
{

//
// Compute ambient occlusion at a given point in space.
//
// todo: implement optional computation of the mean unoccluded direction.
//

template <typename SamplingFunction>
double compute_ambient_occlusion(
    const SamplingContext&  sampling_context,
    SamplingFunction&       sampling_function,
    const Intersector&      intersector,
    const ShadingPoint&     shading_point,
    const double            max_distance,
    const size_t            sample_count)
{
    const foundation::Vector3d& geometric_normal = shading_point.get_geometric_normal();
    const foundation::Basis3d& shading_basis = shading_point.get_shading_basis();

    // Create a sampling context.
    SamplingContext child_sampling_context = sampling_context.split(2, sample_count);

    // Construct the ambient occlusion ray.
    ShadingRay ray;
    ray.m_tmin = 0.0;
    ray.m_tmax = max_distance;
    ray.m_time = shading_point.get_time();
    ray.m_flags = VisibilityFlags::ProbeRay;
    ray.m_depth = shading_point.get_ray().m_depth + 1;

    size_t computed_samples = 0;
    size_t occluded_samples = 0;

    for (size_t i = 0; i < sample_count; ++i)
    {
        // Generate a direction over the unit hemisphere.
        ray.m_dir = sampling_function(child_sampling_context.next2<foundation::Vector2d>());

        // Transform the direction to world space.
        ray.m_dir = shading_basis.transform_to_parent(ray.m_dir);

        // Don't cast rays on or below the geometric surface.
        if (foundation::dot(ray.m_dir, geometric_normal) <= 0.0)
            continue;

        // Compute the ray origin.
        ray.m_org = shading_point.get_point();

        // Count the number of computed samples.
        ++computed_samples;

        // Trace the ambient occlusion ray and count the number of occluded samples.
        if (intersector.trace_probe(ray, &shading_point))
            ++occluded_samples;
    }

    // Compute occlusion as a scalar between 0.0 and 1.0.
    double occlusion = static_cast<double>(occluded_samples);
    if (computed_samples > 1)
        occlusion /= computed_samples;
    assert(occlusion >= 0.0);
    assert(occlusion <= 1.0);

    return occlusion;
}

}   // namespace renderer
