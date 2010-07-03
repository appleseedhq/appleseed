
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

// Interface header.
#include "transmission.h"

// appleseed.renderer headers.
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/shading/shadingcontext.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"

using namespace foundation;

namespace renderer
{

//
// Compute the transmission factor along a given direction.
//

double compute_transmission(
    const ShadingContext&   shading_context,
    const Vector3d&         point,
    const Vector3d&         direction,
    const double            max_distance,
    const ShadingPoint*     parent_shading_point)
{
    // Initialize the transmission factor.
    double transmission = 1.0;

    // Retrieve the intersector.
    const Intersector& intersector = shading_context.get_intersector();

    ShadingPoint shading_points[2];
    size_t shading_point_index = 0;
    const ShadingPoint* shading_point_ptr = parent_shading_point;

    while (transmission > 0.0)
    {
        // Construct the visibility ray.
        // todo: get rid of this epsilon.
        const double Eps = 1.0e-6;
        const ShadingRay visibility_ray(
            point,
            direction,
            0.0,                    // ray tmin
            max_distance - Eps,     // ray tmax
            0.0f,                   // ray time
            ~0);                    // ray flags

        // Trace the ray.
        shading_points[shading_point_index].clear();
        intersector.trace(
            visibility_ray,
            shading_points[shading_point_index],
            shading_point_ptr);

        // Update the pointers to the shading points.
        shading_point_ptr = &shading_points[shading_point_index];
        shading_point_index = 1 - shading_point_index;

        // Stop if the target was reached.
        if (!shading_point_ptr->hit())
            return transmission;

        // Retrieve the material at the shading point.
        const Material* material = shading_point_ptr->get_material();

        // Return full occlusion if the surface has no material.
        if (material == 0)
            return 0.0;

        // Retrieve the surface shader.
        const SurfaceShader& surface_shader = material->get_surface_shader();

        // Evaluate the alpha mask at the shading point.
        Alpha alpha_mask;
        surface_shader.evaluate_alpha_mask(
            shading_context,
            *shading_point_ptr,
            alpha_mask);

        // Update the transmission factor.
        transmission *= 1.0 - static_cast<double>(alpha_mask[0]);
    }
    
    return transmission;
}

}   // namespace renderer
