
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
#include "ambientocclusion.h"

// appleseed.renderer headers.
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/shading/shadingray.h"

// appleseed.foundation headers.
#include "foundation/math/sampling.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// Compute classic ambient occlusion at a given point in space.
//

double compute_ambient_occlusion(
    const SamplingContext&  sampling_context,
    const Intersector&      intersector,
    const Vector3d&         point,
    const Vector3d&         geometic_normal,
    const Basis3d&          shading_basis,
    const double            max_distance,
    const size_t            sample_count,
    const ShadingPoint*     parent_shading_point)
{
    // Create a sampling context.
    SamplingContext child_context = sampling_context.split(2, sample_count);

    // Construct an ambient occlusion ray.
    ShadingRay ao_ray;
    ao_ray.m_org = point;
    ao_ray.m_tmin = 0.0;
    ao_ray.m_tmax = max_distance;
    ao_ray.m_time = 0.0f;
    ao_ray.m_flags = ~0;

    size_t computed_samples = 0;
    size_t occluded_samples = 0;

    for (size_t i = 0; i < sample_count; ++i)
    {
        // Generate a cosine-weighted direction over the unit hemisphere.
        const Vector2d s = child_context.next_vector2<2>();
        ao_ray.m_dir = sample_hemisphere_cosine(s);

        // Transform the direction to world space.
        ao_ray.m_dir = shading_basis.transform_to_parent(ao_ray.m_dir);

        // Don't cast rays on or below the geometric surface.
        if (dot(ao_ray.m_dir, geometic_normal) <= 0.0)
            continue;

        // Count the number of computed samples.
        ++computed_samples;

        // Trace the ambient occlusion ray and count the number of occluded samples.
        if (intersector.trace_probe(ao_ray, parent_shading_point))
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
