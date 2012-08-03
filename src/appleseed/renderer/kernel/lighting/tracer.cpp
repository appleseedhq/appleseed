
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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
#include "renderer/kernel/intersection/intersector.h"
#include "renderer/kernel/shading/shadingray.h"
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
#include <vector>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    bool use_alpha_mapping(const MaterialArray& materials)
    {
        for (size_t i = 0; i < materials.size(); ++i)
        {
            if (materials[i] && materials[i]->has_alpha_map())
                return true;
        }

        return false;
    }

    bool use_alpha_mapping(const ObjectInstance& object_instance)
    {
        return
            use_alpha_mapping(object_instance.get_back_materials()) ||
            use_alpha_mapping(object_instance.get_front_materials());
    }

    bool use_alpha_mapping(const Assembly& assembly)
    {
        for (const_each<ObjectInstanceContainer> i = assembly.object_instances(); i; ++i)
        {
            if (use_alpha_mapping(*i))
                return true;
        }

        return false;
    }

    void collect_assemblies(const Scene& scene, vector<UniqueID>& assemblies)
    {
        assert(assemblies.empty());

        assemblies.reserve(scene.assembly_instances().size());

        for (const_each<AssemblyInstanceContainer> i = scene.assembly_instances(); i; ++i)
            assemblies.push_back(i->get_assembly_uid());

        sort(assemblies.begin(), assemblies.end());

        assemblies.erase(
            unique(assemblies.begin(), assemblies.end()),
            assemblies.end());
    }

    bool use_alpha_mapping(const Scene& scene)
    {
        vector<UniqueID> assemblies;
        collect_assemblies(scene, assemblies);

        for (size_t i = 0; i < assemblies.size(); ++i)
        {
            if (use_alpha_mapping(*scene.assemblies().get_by_uid(assemblies[i])))
                return true;
        }

        return false;
    }
}

Tracer::Tracer(
    const Scene&            scene,
    const Intersector&      intersector,
    TextureCache&           texture_cache,
    const float             transparency_threshold,
    const size_t            max_iterations)
  : m_intersector(intersector)
  , m_texture_cache(texture_cache)
  , m_assume_no_alpha_mapping(!use_alpha_mapping(scene))
  , m_transmission_threshold(static_cast<double>(transparency_threshold))
  , m_max_iterations(max_iterations)
{
    if (m_assume_no_alpha_mapping)
        RENDERER_LOG_DEBUG("the scene does not rely on alpha mapping; using probe tracing.");
    else RENDERER_LOG_DEBUG("the scene uses alpha mapping; using standard tracing.");
}

const ShadingPoint& Tracer::trace(
    const Vector3d&         origin,
    const Vector3d&         direction,
    const double            time,
    double&                 transmission,
    const ShadingPoint*     parent_shading_point)
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
            ~0);            // ray flags

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

double Tracer::trace(
    const Vector3d&         origin,
    const Vector3d&         direction,
    const double            time,
    const ShadingPoint*     parent_shading_point)
{
    if (m_assume_no_alpha_mapping)
    {
        const ShadingRay ray(
            origin,
            direction,
            time,
            ~0);            // ray flags

        return m_intersector.trace_probe(ray, parent_shading_point) ? 0.0 : 1.0;
    }
    else
    {
        double transmission;
        const ShadingPoint& shading_point =
            trace(
                origin,
                direction,
                time,
                transmission,
                parent_shading_point);

        return transmission;
    }
}

namespace
{
    // todo: get rid of this epsilon.
    const double SafeMaxDistance = 1.0 - 1.0e-6;
}

const ShadingPoint& Tracer::trace_between(
    const Vector3d&         origin,
    const Vector3d&         target,
    const double            time,
    double&                 transmission,
    const ShadingPoint*     parent_shading_point)
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
                "reached hard iteration limit (%s), breaking trace_between loop.",
                pretty_int(m_max_iterations).c_str());
            break;
        }

        // Construct the visibility ray.
        const ShadingRay ray(
            point,
            target - point,
            0.0,                    // ray tmin
            SafeMaxDistance,        // ray tmax
            time,
            ~0);                    // ray flags

        // Trace the ray.
        m_shading_points[shading_point_index].clear();
        m_intersector.trace(
            ray,
            m_shading_points[shading_point_index],
            shading_point_ptr);

        // Update the pointers to the shading points.
        shading_point_ptr = &m_shading_points[shading_point_index];
        shading_point_index = 1 - shading_point_index;

        // Stop if the target point was reached.
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

double Tracer::trace_between(
    const Vector3d&         origin,
    const Vector3d&         target,
    const double            time,
    const ShadingPoint*     parent_shading_point)
{
    if (m_assume_no_alpha_mapping)
    {
        const ShadingRay ray(
            origin,
            target - origin,
            0.0,                // ray tmin
            SafeMaxDistance,    // ray tmax
            time,
            ~0);                // ray flags

        return m_intersector.trace_probe(ray, parent_shading_point) ? 0.0 : 1.0;
    }
    else
    {
        double transmission;
        const ShadingPoint& shading_point =
            trace_between(
                origin,
                target,
                time,
                transmission,
                parent_shading_point);

        return transmission;
    }
}

}   // namespace renderer
