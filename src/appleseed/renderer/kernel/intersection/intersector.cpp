
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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
#include "renderer/kernel/intersection/regioninfo.h"
#include "renderer/kernel/intersection/tracecontext.h"
#include "renderer/kernel/intersection/trianglekey.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/object/triangle.h"
#include "renderer/utility/cache.h"

// appleseed.foundation headers.
#include "foundation/math/intersection.h"
#include "foundation/platform/system.h"
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
        const ShadingPoint&         lhs,
        const ShadingPoint&         rhs)
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
        const ShadingPoint&         shading_point,
        const ShadingPoint*         parent_shading_point)
    {
        if (shading_point.hit() &&
            parent_shading_point &&
            same_triangle(*parent_shading_point, shading_point))
        {
            RENDERER_LOG_WARNING(
                "self-intersection detected, distance %e.",
                shading_point.get_distance());
        }
    }
}

Intersector::Intersector(
    const TraceContext&             trace_context,
    const bool                      print_statistics,
    const bool                      report_self_intersections)
  : m_trace_context(trace_context)
  , m_print_statistics(print_statistics)
  , m_report_self_intersections(report_self_intersections)
  , m_ray_count(0)
  , m_probe_ray_count(0)
{
    if (m_print_statistics)
    {
        RENDERER_LOG_DEBUG(
            "system information:\n"
            "  L1 data cache    size %s, line size %s\n"
            "  L2 cache         size %s, line size %s\n"
            "  L3 cache         size %s, line size %s\n",
            pretty_size(System::get_l1_data_cache_size()).c_str(),
            pretty_size(System::get_l1_data_cache_line_size()).c_str(),
            pretty_size(System::get_l2_cache_size()).c_str(),
            pretty_size(System::get_l2_cache_line_size()).c_str(),
            pretty_size(System::get_l3_cache_size()).c_str(),
            pretty_size(System::get_l3_cache_line_size()).c_str());

        RENDERER_LOG_DEBUG(
            "data structures size:\n"
            "  bvh::NodeType    %s\n"
            "  GTriangleType    %s\n"
            "  RegionInfo       %s\n"
            "  ShadingPoint     %s\n"
            "  ShadingRay       %s\n"
            "  TriangleKey      %s\n",
            pretty_size(sizeof(TriangleTree::NodeType)).c_str(),
            pretty_size(sizeof(GTriangleType)).c_str(),
            pretty_size(sizeof(RegionInfo)).c_str(),
            pretty_size(sizeof(ShadingPoint)).c_str(),
            pretty_size(sizeof(ShadingRay)).c_str(),
            pretty_size(sizeof(TriangleKey)).c_str());
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
        RENDERER_LOG_DEBUG("assembly tree intersection statistics:");
        m_assembly_tree_traversal_stats.print(global_logger());

        RENDERER_LOG_DEBUG("triangle tree intersection statistics:");
        m_triangle_tree_traversal_stats.print(global_logger());
#endif

        print_dual_stage_cache_stats(m_region_tree_cache, "region tree access cache statistics");
        print_dual_stage_cache_stats(m_triangle_tree_cache, "triangle tree access cache statistics");
        print_dual_stage_cache_stats(m_region_kit_cache, "region kit access cache statistics");
        print_dual_stage_cache_stats(m_tess_cache, "tessellation access cache statistics");
    }
}

Vector3d Intersector::refine(
    const TriangleSupportPlaneType& support_plane,
    const Vector3d&                 point,
    const Vector3d&                 direction)
{
    Vector3d result = point;

    const size_t RefinementSteps = 2;

    for (size_t i = 0; i < RefinementSteps; ++i)
    {
        const double t = support_plane.intersect(result, direction);
        result += t * direction;
    }

    return result;
}

void Intersector::offset(
    const Vector3d&                 p,
    Vector3d                        n,
    Vector3d&                       front,
    Vector3d&                       back)
{
    //
    // Reference:
    //
    //   Quasi-Monte Carlo light transport simulation by efficient ray tracing
    //   http://vts.uni-ulm.de/docs/2008/6265/vts_6265_8393.pdf
    //

    // Offset parameters.
    const double Threshold = 1.0e-25;
    const int EpsMag = 8;
    const int EpsLut[2] = { EpsMag, -EpsMag };

    // Check which components of p are close to the origin.
    const bool is_small[3] =
    {
        abs(p[0]) < Threshold,
        abs(p[1]) < Threshold,
        abs(p[2]) < Threshold
    };

    // If any of the components of p is close to the origin, we need to normalize n.
    if (is_small[0] | is_small[1] | is_small[2])
        n = normalize(n);

    // Compute the offset points.
    for (size_t i = 0; i < 3; ++i)
    {
        if (is_small[i])
        {
            const double shift = n[i] * Threshold;
            front[i] = p[i] + shift;
            back[i] = p[i] - shift;
        }
        else
        {
            const uint64 pi = binary_cast<uint64>(p[i]);
            const int shift = EpsLut[(pi ^ binary_cast<uint64>(n[i])) >> 63];
            front[i] = binary_cast<double>(pi + shift);
            back[i] = binary_cast<double>(pi - shift);
        }
    }
}

namespace
{
    Vector3d offset_point(
        const Vector3d&                 p,
        const Vector3d&                 n,
        const int64                     mag)
    {
        const double Threshold = 1.0e-25;
        const int64 eps_lut[2] = { mag, -mag };

        Vector3d result;

        for (size_t i = 0; i < 3; ++i)
        {
            if (abs(p[i]) < Threshold)
                result[i] = p[i] + n[i] * Threshold;
            else
            {
                const uint64 pi = binary_cast<uint64>(p[i]);
                const int64 shift = eps_lut[(pi ^ binary_cast<uint64>(n[i])) >> 63];
                result[i] = binary_cast<double>(pi + shift);
            }
        }

        return result;
    }

    Vector3d adaptive_offset_point(
        const TriangleSupportPlaneType& support_plane,
        const Vector3d&                 p,
        const Vector3d&                 n,
        const int64                     initial_mag)
    {
        int64 mag = initial_mag;
        Vector3d result = p;

        for (size_t i = 0; i < 64; ++i)
        {
            result = offset_point(result, n, mag);

            if (support_plane.intersect(result, n) < 0.0)
                break;

            mag *= 2;
        }

        return result;
    }
}

void Intersector::adaptive_offset(
    const TriangleSupportPlaneType& support_plane,
    const Vector3d&                 p,
    Vector3d                        n,
    Vector3d&                       front,
    Vector3d&                       back)
{
    const int64 InitialMag = 8;

    n = normalize(n);

    front = adaptive_offset_point(support_plane, p, n, InitialMag);
    back = adaptive_offset_point(support_plane, p, -n, InitialMag);
}

bool Intersector::trace(
    const ShadingRay&               ray,
    ShadingPoint&                   shading_point,
    const ShadingPoint*             parent_shading_point) const
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
    AssemblyTreeIntersector intersector;
    AssemblyLeafVisitor visitor(
        shading_point,
        assembly_tree,
        m_region_tree_cache,
        m_triangle_tree_cache,
        parent_shading_point
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , m_triangle_tree_traversal_stats
#endif
        );
    intersector.intersect(
        assembly_tree,
        shading_point.m_ray,
        ray_info,
        visitor
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , m_assembly_tree_traversal_stats
#endif
        );

    // Detect and report self-intersections.
    if (m_report_self_intersections)
        report_self_intersection(shading_point, parent_shading_point);

    return shading_point.hit();
}

bool Intersector::trace_probe(
    const ShadingRay&               ray,
    const ShadingPoint*             parent_shading_point) const
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
    AssemblyTreeProbeIntersector intersector;
    AssemblyLeafProbeVisitor visitor(
        assembly_tree,
        m_region_tree_cache,
        m_triangle_tree_cache,
        parent_shading_point
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , m_triangle_tree_traversal_stats
#endif
        );
    intersector.intersect(
        assembly_tree,
        ray,
        ray_info,
        visitor
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , m_assembly_tree_traversal_stats
#endif
        );

    return visitor.hit();
}

void Intersector::manufacture_hit(
    ShadingPoint&                   shading_point,
    const ShadingRay&               shading_ray,
    const UniqueID                  assembly_instance_uid,
    const size_t                    object_instance_index,
    const size_t                    region_index,
    const size_t                    triangle_index,
    const TriangleSupportPlaneType& triangle_support_plane) const
{
    shading_point.m_region_kit_cache = &m_region_kit_cache;
    shading_point.m_tess_cache = &m_tess_cache;
    shading_point.m_scene = &m_trace_context.get_scene();
    shading_point.m_ray = shading_ray;
    shading_point.m_hit = true;
    shading_point.m_asm_instance_uid = assembly_instance_uid;
    shading_point.m_object_instance_index = object_instance_index;
    shading_point.m_region_index = region_index;
    shading_point.m_triangle_index = triangle_index;
    shading_point.m_triangle_support_plane = triangle_support_plane;
}

}   // namespace renderer
