
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

#ifndef APPLESEED_RENDERER_KERNEL_INTERSECTION_INTERSECTOR_H
#define APPLESEED_RENDERER_KERNEL_INTERSECTION_INTERSECTOR_H

// appleseed.renderer headers.
#include "renderer/global/global.h"
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/kernel/intersection/regiontree.h"
#include "renderer/kernel/intersection/triangletree.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/kernel/tessellation/statictessellation.h"
#include "renderer/modeling/object/regionkit.h"

// appleseed.foundation headers.
#include "foundation/math/bvh.h"

// Forward declarations.
namespace renderer  { class ShadingPoint; }
namespace renderer  { class TraceContext; }

namespace renderer
{

//
// A thread-local scene intersector.
//

class Intersector
  : public foundation::NonCopyable
{
  public:
    // Constructor, binds the intersector to a given trace context.
    Intersector(
        const TraceContext&             trace_context,
        const bool                      print_statistics = false,
        const bool                      report_self_intersections = false);

    // Destructor.
    ~Intersector();

    // Refine the location of a point on a surface.
    static foundation::Vector3d refine(
        const TriangleSupportPlaneType& support_plane,
        const foundation::Vector3d&     point,
        const foundation::Vector3d&     direction);     // incoming or outgoing direction

    // Offset a point away from a surface represented by its normal.
    static void offset(
        const foundation::Vector3d&     p,
        foundation::Vector3d            n,
        foundation::Vector3d&           front,
        foundation::Vector3d&           back);
    static void adaptive_offset(
        const TriangleSupportPlaneType& support_plane,
        const foundation::Vector3d&     p,
        foundation::Vector3d            n,
        foundation::Vector3d&           front,
        foundation::Vector3d&           back);

    // Trace a world space ray through the scene.
    bool trace(
        const ShadingRay&               ray,
        ShadingPoint&                   shading_point,
        const ShadingPoint*             parent_shading_point = 0) const;

    // Trace a world space probe ray through the scene.
    bool trace_probe(
        const ShadingRay&               ray,
        const ShadingPoint*             parent_shading_point = 0) const;

    // Return true if a given point is occluded from another given point.
    bool occluded(
        const foundation::Vector3d&     origin,
        const foundation::Vector3d&     target,
        const ShadingPoint*             parent_shading_point = 0) const;

    // Manufacture a hit "by hand".
    void manufacture_hit(
        ShadingPoint&                   shading_point,
        const ShadingRay&               shading_ray,
        const foundation::UniqueID      assembly_instance_uid,
        const size_t                    object_instance_index,
        const size_t                    region_index,
        const size_t                    triangle_index,
        const TriangleSupportPlaneType& triangle_support_plane) const;

  private:
    const TraceContext&                             m_trace_context;
    const bool                                      m_print_statistics;
    const bool                                      m_report_self_intersections;
    const foundation::AABB3d                        m_assembly_tree_aabb;

    // Access caches.
    mutable RegionTreeAccessCache                   m_region_tree_cache;
    mutable TriangleTreeAccessCache                 m_triangle_tree_cache;
    mutable RegionKitAccessCache                    m_region_kit_cache;
    mutable StaticTriangleTessAccessCache           m_tess_cache;

    // Intersection statistics.
    mutable foundation::uint64                      m_ray_count;
    mutable foundation::uint64                      m_probe_ray_count;
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    mutable foundation::bvh::TraversalStatistics    m_assembly_tree_traversal_stats;
    mutable foundation::bvh::TraversalStatistics    m_triangle_tree_traversal_stats;
#endif
};


//
// Intersector class implementation.
//

inline bool Intersector::occluded(
    const foundation::Vector3d&     origin,
    const foundation::Vector3d&     target,
    const ShadingPoint*             parent_shading_point) const
{
    // todo: get rid of this epsilon.
    const double Eps = 1.0e-6;

    const ShadingRay visibility_ray(
        origin,
        target - origin,
        0.0,                // ray tmin
        1.0 - Eps,          // ray tmax
        0.0f,               // ray time
        ~0);                // ray flags

    return trace_probe(visibility_ray, parent_shading_point);
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_INTERSECTION_INTERSECTOR_H
