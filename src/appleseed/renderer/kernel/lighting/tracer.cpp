
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "tracer.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#ifdef WITH_OSL
#include "renderer/kernel/shading/oslshadergroupexec.h"
#include "renderer/modeling/shadergroup/shadergroup.h"
#endif
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"
#include "foundation/utility/string.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <set>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    bool uses_alpha_mapping(const MaterialArray& materials)
    {
        for (size_t i = 0; i < materials.size(); ++i)
        {
            if (materials[i])
            {
                if (materials[i]->has_alpha_map())
                    return true;
#ifdef WITH_OSL
                if (const ShaderGroup* sg = materials[i]->get_osl_surface())
                {
                    if (sg->has_transparency())
                        return true;
                }
#endif
            }
        }

        return false;
    }

    bool uses_alpha_mapping(const ObjectInstance& object_instance)
    {
        return
            uses_alpha_mapping(object_instance.get_back_materials()) ||
            uses_alpha_mapping(object_instance.get_front_materials());
    }

    bool uses_alpha_mapping(const Assembly& assembly, set<UniqueID>& visited_assemblies)
    {
        if (visited_assemblies.find(assembly.get_uid()) == visited_assemblies.end())
        {
            visited_assemblies.insert(assembly.get_uid());

            for (const_each<ObjectInstanceContainer> i = assembly.object_instances(); i; ++i)
            {
                if (uses_alpha_mapping(*i))
                    return true;
            }

            for (const_each<AssemblyContainer> i = assembly.assemblies(); i; ++i)
            {
                if (uses_alpha_mapping(*i, visited_assemblies))
                    return true;
            }
        }

        return false;
    }

    bool uses_alpha_mapping(const Scene& scene)
    {
        set<UniqueID> visited_assemblies;

        for (const_each<AssemblyContainer> i = scene.assemblies(); i; ++i)
        {
            if (uses_alpha_mapping(*i, visited_assemblies))
                return true;
        }

        return false;
    }
}

Tracer::Tracer(
    const Scene&                scene,
    const Intersector&          intersector,
    TextureCache&               texture_cache,
#ifdef WITH_OSL
    OSLShaderGroupExec*         shadergroup_exec,
#endif
    const float                 transparency_threshold,
    const size_t                max_iterations,
    const bool                  print_details)
  : m_intersector(intersector)
  , m_texture_cache(texture_cache)
#ifdef WITH_OSL
  , m_shadergroup_exec(shadergroup_exec)
#endif
  , m_assume_no_alpha_mapping(!uses_alpha_mapping(scene))
  , m_transmission_threshold(static_cast<double>(transparency_threshold))
  , m_max_iterations(max_iterations)
{
    if (print_details)
    {
        if (m_assume_no_alpha_mapping)
            RENDERER_LOG_DEBUG("the scene does not rely on alpha mapping; using probe tracing.");
        else RENDERER_LOG_DEBUG("the scene uses alpha mapping; using standard tracing.");
    }
}

const ShadingPoint& Tracer::do_trace(
    const Vector3d&             origin,
    const Vector3d&             direction,
    const double                time,
    const ShadingRay::Type      ray_type,
    const ShadingRay::DepthType ray_depth,
    double&                     transmission,
    const ShadingPoint*         parent_shading_point)
{
    transmission = 1.0;

    const ShadingPoint* shading_point_ptr = parent_shading_point;
    size_t shading_point_index = 0;
    Vector3d point = origin;
    size_t iterations = 0;

    while (true)
    {
        // Put a hard limit on the number of iterations.
        if (++iterations >= m_max_iterations)
        {
            RENDERER_LOG_WARNING(
                "reached hard iteration limit (%s), breaking trace loop.",
                pretty_int(m_max_iterations).c_str());
            break;
        }

        // Construct the visibility ray.
        const ShadingRay ray(
            point,
            direction,
            time,
            ray_type,
            ray_depth);         // ray depth does not increase when passing through an alpha-mapped surface

        // Trace the ray.
        m_shading_points[shading_point_index].clear();
        m_intersector.trace(
            ray,
            m_shading_points[shading_point_index],
            shading_point_ptr);

        // Update the pointers to the shading points.
        shading_point_ptr = &m_shading_points[shading_point_index];
        shading_point_index = 1 - shading_point_index;

        // Stop if the ray escaped the scene.
        if (!shading_point_ptr->hit())
            break;

        // Retrieve the material at the shading point.
        const Material* material = shading_point_ptr->get_material();
        if (material == 0)
            break;

        // Retrieve the alpha map at the shading point.
        const Source* alpha_map = material->get_alpha_map();
        if (alpha_map == 0)
            break;

        // Evaluate the alpha map at the shading point.
        Alpha alpha;
        alpha_map->evaluate(
            m_texture_cache,
            shading_point_ptr->get_uv(0),
            alpha);

        // Stop at the first fully opaque occluder.
        if (alpha[0] >= 1.0f)
            break;

        // Update the transmission factor.
        transmission *= 1.0 - static_cast<double>(alpha[0]);

        // Stop once we hit full opacity.
        if (transmission < m_transmission_threshold)
            break;

        // Move past this partial occluder.
        point = shading_point_ptr->get_point();
    }

    return *shading_point_ptr;
}

const ShadingPoint& Tracer::do_trace_between(
    const Vector3d&             origin,
    const Vector3d&             target,
    const double                time,
    const ShadingRay::Type      ray_type,
    const ShadingRay::DepthType ray_depth,
    double&                     transmission,
    const ShadingPoint*         parent_shading_point)
{
    transmission = 1.0;

    const ShadingPoint* shading_point_ptr = parent_shading_point;
    size_t shading_point_index = 0;
    Vector3d point = origin;
    size_t iterations = 0;

    while (true)
    {
        // Put a hard limit on the number of iterations.
        if (++iterations >= m_max_iterations)
        {
            RENDERER_LOG_WARNING(
                "reached hard iteration limit (%s), breaking trace loop.",
                pretty_int(m_max_iterations).c_str());
            break;
        }

        // Construct the visibility ray.
        const ShadingRay ray(
            point,
            target - point,
            0.0,                // ray tmin
            1.0 - 1.0e-6,       // ray tmax
            time,
            ray_type,
            ray_depth);         // ray depth does not increase when passing through an alpha-mapped surface

        // Trace the ray.
        m_shading_points[shading_point_index].clear();
        m_intersector.trace(
            ray,
            m_shading_points[shading_point_index],
            shading_point_ptr);

        // Update the pointers to the shading points.
        shading_point_ptr = &m_shading_points[shading_point_index];
        shading_point_index = 1 - shading_point_index;

        // Stop if the ray reached the target point.
        if (!shading_point_ptr->hit())
            break;

        // Retrieve the material at the shading point.
        const Material* material = shading_point_ptr->get_material();
        if (material == 0)
            break;

        // Retrieve the alpha map at the shading point.
        const Source* alpha_map = material->get_alpha_map();
        if (alpha_map == 0)
            break;

        // Evaluate the alpha map at the shading point.
        Alpha alpha;
        alpha_map->evaluate(
            m_texture_cache,
            shading_point_ptr->get_uv(0),
            alpha);

        // Stop at the first fully opaque occluder.
        if (alpha[0] >= 1.0f)
            break;

        // Update the transmission factor.
        transmission *= 1.0 - static_cast<double>(alpha[0]);

        // Stop once we hit full opacity.
        if (transmission < m_transmission_threshold)
            break;

        // Move past this partial occluder.
        point = shading_point_ptr->get_point();
    }

    return *shading_point_ptr;
}

}   // namespace renderer
