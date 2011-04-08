
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
#include "intersector.h"

// appleseed.renderer headers.
#include "renderer/kernel/intersection/assemblytree.h"
#include "renderer/kernel/intersection/tracecontext.h"
#include "renderer/kernel/shading/shadingpoint.h"

// appleseed.foundation headers.
#include "foundation/math/intersection.h"
#include "foundation/utility/casts.h"
#include "foundation/utility/string.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// Intersector class implementation.
//

namespace
{
    // Return true if two shading points reference the same triangle.
    inline bool same_triangle(
        const ShadingPoint&     lhs,
        const ShadingPoint&     rhs)
    {
        assert(lhs.hit());
        assert(rhs.hit());

        return
            lhs.get_triangle_index() == rhs.get_triangle_index() &&
            lhs.get_region_index() == rhs.get_region_index() &&
            lhs.get_object_instance_index() == rhs.get_object_instance_index() &&
            lhs.get_assembly_instance_uid() == rhs.get_assembly_instance_uid();
    }

    // Print a message if a self-intersection situation is detected.
    void report_self_intersection(
        const ShadingPoint&     shading_point,
        const ShadingPoint*     parent_shading_point)
    {
        if (shading_point.hit() &&
            parent_shading_point &&
            same_triangle(*parent_shading_point, shading_point))
        {
            RENDERER_LOG_WARNING(
                "self-intersection detected, distance %e",
                shading_point.get_distance());
        }
    }
}

Intersector::Intersector(
    const TraceContext& trace_context,
    const bool          print_statistics,
    const bool          report_self_intersections)
  : m_trace_context(trace_context)
  , m_print_statistics(print_statistics)
  , m_report_self_intersections(report_self_intersections)
  , m_assembly_tree_aabb(m_trace_context.get_assembly_tree().get_bbox())
  , m_ray_count(0)
  , m_probe_ray_count(0)
{
    if (m_print_statistics)
    {
        RENDERER_LOG_DEBUG(
            "data structures size:\n"
            "  bsp::NodeType    %s\n"
            "  ShadingRay       %s\n"
            "  ShadingPoint     %s\n",
            pretty_size(sizeof(TriangleTree::NodeType)).c_str(),
            pretty_size(sizeof(ShadingRay)).c_str(),
            pretty_size(sizeof(ShadingPoint)).c_str());
    }
}

Intersector::~Intersector()
{
    if (m_print_statistics)
    {
        const uint64 total_ray_count = m_ray_count + m_probe_ray_count;
        RENDERER_LOG_DEBUG(
            "general intersection statistics:\n"
            "  total rays       %s\n"
            "  probe rays       %s (%s)",
            pretty_int(total_ray_count).c_str(),
            pretty_int(m_probe_ray_count).c_str(),
            pretty_percent(m_probe_ray_count, total_ray_count).c_str());

#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        RENDERER_LOG_DEBUG("assembly bvh intersection statistics:");
        m_assembly_bvh_traversal_stats.print(global_logger());
#endif

#ifdef FOUNDATION_BSP_ENABLE_TRAVERSAL_STATS
        RENDERER_LOG_DEBUG("triangle bsp trees intersection statistics:");
        m_triangle_bsp_traversal_stats.print(global_logger());
#endif

        RENDERER_LOG_DEBUG(
            "access caches efficiency:\n"
            "  region trees     %s\n"
            "  triangle trees   %s\n"
            "  region kits      %s\n"
            "  tessellations    %s",
            format_cache_stats(m_region_tree_cache).c_str(),
            format_cache_stats(m_triangle_tree_cache).c_str(),
            format_cache_stats(m_region_kit_cache).c_str(),
            format_cache_stats(m_tess_cache).c_str());
    }
}

Vector3d Intersector::offset(const Vector3d& p, Vector3d n)
{
    const double Threshold = 1.0e-25;
    const int EpsLut[2] = { 8, -8 };

    // Check whether any of the components of p is close to zero, in absolute value.
    const bool small_x = abs(p.x) < Threshold;
    const bool small_y = abs(p.y) < Threshold;
    const bool small_z = abs(p.z) < Threshold;

    // If it is the case, we need n to be normalized.
    if (small_x || small_y || small_z)
        n = normalize(n);

    Vector3d res;

    // Offset X component.
    if (small_x)
        res.x = p.x + n.x * Threshold;
    else
    {
        uint64 ix = binary_cast<uint64>(p.x);
        ix += EpsLut[(ix ^ binary_cast<uint64>(n.x)) >> 63];
        res.x = binary_cast<double>(ix);
    }

    // Offset Y component.
    if (small_y)
        res.y = p.y + n.y * Threshold;
    else
    {
        uint64 iy = binary_cast<uint64>(p.y);
        iy += EpsLut[(iy ^ binary_cast<uint64>(n.y)) >> 63];
        res.y = binary_cast<double>(iy);
    }

    // Offset Z component.
    if (small_z)
        res.z = p.z + n.z * Threshold;
    else
    {
        uint64 iz = binary_cast<uint64>(p.z);
        iz += EpsLut[(iz ^ binary_cast<uint64>(n.z)) >> 63];
        res.z = binary_cast<double>(iz);
    }

    return res;
}

bool Intersector::trace(
    const ShadingRay&   ray,
    ShadingPoint&       shading_point,
    const ShadingPoint* parent_shading_point) const
{
    assert(shading_point.m_scene == 0);
    assert(shading_point.hit() == false);
    assert(parent_shading_point == 0 || parent_shading_point != &shading_point);
    assert(parent_shading_point == 0 || parent_shading_point->hit());

    // Update ray casting statistics.
    ++m_ray_count;

    // Initialize the shading point.
    shading_point.m_region_kit_cache = &m_region_kit_cache;
    shading_point.m_tess_cache = &m_tess_cache;
    shading_point.m_scene = &m_trace_context.get_scene();
    shading_point.m_ray = ray;

    // Compute ray info once for the entire traversal.
    const ShadingRay::RayInfoType ray_info(shading_point.m_ray);

    // Refine and offset the previous intersection point.
    if (parent_shading_point &&
        parent_shading_point->hit() &&
        !(parent_shading_point->m_members & ShadingPoint::HasRefinedPoints))
        parent_shading_point->refine_and_offset();

    // Retrieve assembly tree.
    const AssemblyTree& assembly_tree = m_trace_context.get_assembly_tree();

    // Check the intersection between the ray and the assembly tree.
    AssemblyLeafVisitor visitor(
        shading_point,
        assembly_tree,
        m_region_tree_cache,
        m_triangle_tree_cache,
        parent_shading_point
#ifdef FOUNDATION_BSP_ENABLE_TRAVERSAL_STATS
        , m_triangle_bsp_traversal_stats
#endif
        );
    AssemblyLeafIntersector intersector;
    intersector.intersect(
        assembly_tree,
        shading_point.m_ray,
        ray_info,
        visitor
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , m_assembly_bvh_traversal_stats
#endif
        );

    // Detect and report self-intersections.
    if (m_report_self_intersections)
        report_self_intersection(shading_point, parent_shading_point);

    return shading_point.hit();
}

bool Intersector::trace_probe(
    const ShadingRay&   ray,
    const ShadingPoint* parent_shading_point) const
{
    assert(parent_shading_point == 0 || parent_shading_point->hit());

    // Update ray casting statistics.
    ++m_probe_ray_count;

    // Compute ray info once for the entire traversal.
    const ShadingRay::RayInfoType ray_info(ray);

    // Refine and offset the previous intersection point.
    if (parent_shading_point &&
        parent_shading_point->hit() &&
        !(parent_shading_point->m_members & ShadingPoint::HasRefinedPoints))
        parent_shading_point->refine_and_offset();

    // Retrieve assembly tree.
    const AssemblyTree& assembly_tree = m_trace_context.get_assembly_tree();

    // Check the intersection between the ray and the assembly tree.
    AssemblyLeafProbeVisitor visitor(
        assembly_tree,
        m_region_tree_cache,
        m_triangle_tree_cache,
        parent_shading_point
#ifdef FOUNDATION_BSP_ENABLE_TRAVERSAL_STATS
        , m_triangle_bsp_traversal_stats
#endif
        );
    AssemblyLeafProbeIntersector intersector;
    intersector.intersect(
        assembly_tree,
        ray,
        ray_info,
        visitor
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , m_assembly_bvh_traversal_stats
#endif
        );

    return visitor.hit();
}

}   // namespace renderer
