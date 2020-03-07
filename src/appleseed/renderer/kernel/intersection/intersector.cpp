
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

// Interface header.
#include "intersector.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/intersection/assemblytree.h"
#include "renderer/kernel/intersection/tracecontext.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/scene/assemblyinstance.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/string/string.h"
#include "foundation/utility/cache.h"
#include "foundation/utility/casts.h"
#include "foundation/utility/lazy.h"
#include "foundation/utility/poison.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstdint>
#include <memory>
#include <string>

using namespace foundation;

namespace renderer
{

Intersector::Intersector(
    const TraceContext&             trace_context,
    TextureCache&                   texture_cache,
    const bool                      report_self_intersections)
  : m_trace_context(trace_context)
  , m_texture_cache(texture_cache)
  , m_report_self_intersections(report_self_intersections)
  , m_shading_ray_count(0)
  , m_probe_ray_count(0)
{
}

namespace
{
    // Return true if two shading points reference the same primitive.
    inline bool same_primitive(
        const ShadingPoint&         lhs,
        const ShadingPoint&         rhs)
    {
        assert(lhs.hit_surface());
        assert(rhs.hit_surface());

        // todo: this won't work for procedural objects. It can return false positives in such case.
        // Being on the same primitive doesn't mean it's a self-intersection.
        // For triangles you have a different normal for each primitive; this is not the case with
        // procedural objects.
        return
            lhs.get_primitive_type() == rhs.get_primitive_type() &&
            lhs.get_primitive_index() == rhs.get_primitive_index() &&
            lhs.get_object_instance_index() == rhs.get_object_instance_index() &&
            lhs.get_assembly_instance().get_uid() == rhs.get_assembly_instance().get_uid();
    }

    // Print a message if a self-intersection situation is detected.
    void report_self_intersection(
        const ShadingPoint&         shading_point,
        const ShadingPoint*         parent_shading_point)
    {
        constexpr size_t MaxWarningsPerThread = 20;
        static size_t warning_count = 0;

        if (shading_point.hit_surface() &&
            parent_shading_point &&
            same_primitive(*parent_shading_point, shading_point))
        {
            if (warning_count < MaxWarningsPerThread)
            {
                RENDERER_LOG_WARNING(
                    "self-intersection detected, distance %e.",
                    shading_point.get_distance());

                ++warning_count;
            }
            else if (warning_count == MaxWarningsPerThread)
            {
                RENDERER_LOG_WARNING("more self-intersections detected, omitting warning messages for brevity.");

                ++warning_count;
            }
        }
    }
}

bool Intersector::trace(
    const ShadingRay&                   ray,
    ShadingPoint&                       shading_point,
    const ShadingPoint*                 parent_shading_point) const
{
    assert(is_normalized(ray.m_dir));
    assert(shading_point.m_scene == nullptr);
    assert(!shading_point.is_valid());
    assert(parent_shading_point == nullptr || parent_shading_point != &shading_point);
    assert(parent_shading_point == nullptr || parent_shading_point->is_valid());

    // Update ray casting statistics.
    ++m_shading_ray_count;

    // Initialize the shading point.
    shading_point.m_texture_cache = &m_texture_cache;
    shading_point.m_scene = &m_trace_context.get_scene();
    shading_point.m_ray = ray;

    // Compute ray info once for the entire traversal.
    const ShadingRay::RayInfoType ray_info(shading_point.m_ray);

    // Refine and offset the previous intersection point.
    if (parent_shading_point &&
        parent_shading_point->hit_surface() &&
        !(parent_shading_point->m_members & ShadingPoint::HasRefinedPoints))
        parent_shading_point->refine_and_offset();

    // Retrieve assembly tree.
    const AssemblyTree& assembly_tree = m_trace_context.get_assembly_tree();

    // Check the intersection between the ray and the assembly tree.
    AssemblyTreeIntersector intersector;
    AssemblyLeafVisitor visitor(
        shading_point,
        assembly_tree,
        m_triangle_tree_cache,
        m_curve_tree_cache,
#ifdef APPLESEED_WITH_EMBREE
        m_embree_scene_cache,
#endif
        parent_shading_point
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , m_triangle_tree_traversal_stats
#endif
        );
    intersector.intersect_no_motion(
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

    const ShadingRay::Medium* medium = ray.get_current_medium();
    if (!shading_point.hit_surface() && medium != nullptr && medium->get_volume() != nullptr)
        shading_point.m_primitive_type = ShadingPoint::PrimitiveVolume;

    return shading_point.hit_surface();
}

bool Intersector::trace_probe(
    const ShadingRay&                   ray,
    const ShadingPoint*                 parent_shading_point) const
{
    assert(is_normalized(ray.m_dir));
    assert(parent_shading_point == 0 || parent_shading_point->hit_surface());

    // Update ray casting statistics.
    ++m_probe_ray_count;

    // Compute ray info once for the entire traversal.
    const ShadingRay::RayInfoType ray_info(ray);

    // Refine and offset the previous intersection point.
    if (parent_shading_point &&
        parent_shading_point->hit_surface() &&
        !(parent_shading_point->m_members & ShadingPoint::HasRefinedPoints))
        parent_shading_point->refine_and_offset();

    // Retrieve assembly tree.
    const AssemblyTree& assembly_tree = m_trace_context.get_assembly_tree();

    // Check the intersection between the ray and the assembly tree.
    AssemblyTreeProbeIntersector intersector;
    AssemblyLeafProbeVisitor visitor(
        assembly_tree,
        m_triangle_tree_cache,
        m_curve_tree_cache,
#ifdef APPLESEED_WITH_EMBREE
        m_embree_scene_cache,
#endif
        parent_shading_point
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , m_triangle_tree_traversal_stats
#endif
        );
    intersector.intersect_no_motion(
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

void Intersector::make_triangle_shading_point(
    ShadingPoint&                       shading_point,
    const ShadingRay&                   shading_ray,
    const Vector2f&                     bary,
    const AssemblyInstance*             assembly_instance,
    const Transformd&                   assembly_instance_transform,
    const size_t                        object_instance_index,
    const size_t                        primitive_index,
    const TriangleSupportPlaneType&     triangle_support_plane) const
{
    // This helps finding bugs if make_surface_shading_point()
    // is called on a previously used shading point.
    debug_poison(shading_point);

    // Context.
    shading_point.m_texture_cache = &m_texture_cache;
    shading_point.m_scene = &m_trace_context.get_scene();
    shading_point.m_ray = shading_ray;

    // Primary intersection results.
    shading_point.m_primitive_type = ShadingPoint::PrimitiveTriangle;
    shading_point.m_bary = bary;
    shading_point.m_assembly_instance = assembly_instance;
    shading_point.m_assembly_instance_transform = assembly_instance_transform;
    shading_point.m_assembly_instance_transform_seq = &assembly_instance->transform_sequence();
    shading_point.m_object_instance_index = object_instance_index;
    shading_point.m_primitive_index = primitive_index;
    shading_point.m_triangle_support_plane = triangle_support_plane;

    // Available on-demand results: none.
    shading_point.m_members = 0;
}

void Intersector::make_procedural_surface_shading_point(
    ShadingPoint&                       shading_point,
    const ShadingRay&                   shading_ray,
    const Vector2f&                     uv,
    const AssemblyInstance*             assembly_instance,
    const Transformd&                   assembly_instance_transform,
    const size_t                        object_instance_index,
    const size_t                        primitive_index,
    const Vector3d&                     point,
    const Vector3d&                     normal,
    const Vector3d&                     dpdu,
    const Vector3d&                     dpdv) const
{
    // This helps finding bugs if make_surface_shading_point()
    // is called on a previously used shading point.
    debug_poison(shading_point);

    shading_point.m_texture_cache = &m_texture_cache;
    shading_point.m_scene = &m_trace_context.get_scene();

    assert(shading_ray.m_has_differentials == false);
    shading_point.m_ray = shading_ray;

    shading_point.m_primitive_type = ShadingPoint::PrimitiveProceduralSurface;

    shading_point.m_bary = uv;
    shading_point.m_assembly_instance = assembly_instance;
    shading_point.m_assembly_instance_transform = assembly_instance_transform;
    shading_point.m_assembly_instance_transform_seq = &assembly_instance->transform_sequence();
    shading_point.m_object_instance_index = object_instance_index;
    shading_point.m_primitive_index = primitive_index;

    shading_point.m_point = point;
    shading_point.m_members |= ShadingPoint::HasPoint;

    assert(is_normalized(normal));
    shading_point.m_geometric_normal = shading_point.m_original_shading_normal = normal;
    shading_point.m_members |= ShadingPoint::HasGeometricNormal | ShadingPoint::HasOriginalShadingNormal;

    shading_point.m_shading_basis = Basis3d(
        normal,
        normalize(dpdu),
        normalize(dpdv));
    shading_point.m_members |= ShadingPoint::HasShadingBasis;

    shading_point.m_uv = uv;
    shading_point.m_members = ShadingPoint::HasUV0;

    shading_point.m_dpdu = dpdu;
    shading_point.m_dpdu = dpdv;
    shading_point.m_members |= ShadingPoint::HasWorldSpaceDerivatives;

    shading_point.m_dpdx = Vector3d(0.0);
    shading_point.m_dpdy = Vector3d(0.0);
    shading_point.m_duvdx = Vector2f(0.0);
    shading_point.m_duvdy = Vector2f(0.0);
    shading_point.m_members = ShadingPoint::HasScreenSpaceDerivatives;
}

void Intersector::make_volume_shading_point(
    ShadingPoint&                       shading_point,
    const ShadingRay&                   volume_ray,
    const double                        distance) const
{
    // This helps finding bugs if make_volume_shading_point()
    // is called on a previously used shading point.
    debug_poison(shading_point);

    assert(is_normalized(volume_ray.m_dir));
    assert(volume_ray.get_current_medium() != nullptr);

    // Context.
    shading_point.m_texture_cache = &m_texture_cache;
    shading_point.m_scene = &m_trace_context.get_scene();

    // Primary data.
    shading_point.m_ray = volume_ray;
    shading_point.m_ray.m_tmax = distance;
    shading_point.m_primitive_type = ShadingPoint::PrimitiveVolume;

    // Available on-demand results: none.
    shading_point.m_members = 0;
}

namespace
{
    struct RayCountStatisticsEntry
      : public Statistics::Entry
    {
        std::uint64_t   m_ray_count;
        std::uint64_t   m_total_ray_count;

        RayCountStatisticsEntry(
            const std::string&   name,
            const std::uint64_t  ray_count,
            const std::uint64_t  total_ray_count)
          : Entry(name)
          , m_ray_count(ray_count)
          , m_total_ray_count(total_ray_count)
        {
        }

        std::unique_ptr<Entry> clone() const override
        {
            return std::unique_ptr<Entry>(new RayCountStatisticsEntry(*this));
        }

        void merge(const Entry* other) override
        {
            const RayCountStatisticsEntry* typed_other =
                cast<RayCountStatisticsEntry>(other);

            m_ray_count += typed_other->m_ray_count;
            m_total_ray_count += typed_other->m_total_ray_count;
        }

        std::string to_string() const override
        {
            return pretty_uint(m_ray_count) + " (" + pretty_percent(m_ray_count, m_total_ray_count) + ")";
        }
    };
}

StatisticsVector Intersector::get_statistics() const
{
    const std::uint64_t total_ray_count = m_shading_ray_count + m_probe_ray_count;

    Statistics intersection_stats;
    intersection_stats.insert("total rays", total_ray_count);
    intersection_stats.insert(
        std::unique_ptr<RayCountStatisticsEntry>(
            new RayCountStatisticsEntry(
                "shading rays",
                m_shading_ray_count,
                total_ray_count)));
    intersection_stats.insert(
        std::unique_ptr<RayCountStatisticsEntry>(
            new RayCountStatisticsEntry(
                "probe rays",
                m_probe_ray_count,
                total_ray_count)));

    StatisticsVector vec;

    vec.insert("intersection statistics", intersection_stats);

#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    vec.insert(
        "assembly tree intersection statistics",
        m_assembly_tree_traversal_stats.get_statistics());

    vec.insert(
        "triangle trees intersection statistics",
        m_triangle_tree_traversal_stats.get_statistics());
#endif

    vec.insert(
        "triangle tree access cache statistics",
        make_dual_stage_cache_stats(m_triangle_tree_cache));

    return vec;
}

}   // namespace renderer
