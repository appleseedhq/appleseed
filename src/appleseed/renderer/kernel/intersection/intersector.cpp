
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
#include "foundation/utility/cache.h"
#include "foundation/utility/casts.h"
#include "foundation/utility/lazy.h"
#include "foundation/utility/poison.h"
#include "foundation/utility/statistics.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <memory>
#include <string>

using namespace foundation;
using namespace std;

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

void Intersector::fixed_offset(
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
    static const int EpsLut[2] = { EpsMag, -EpsMag };

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
    Vector3d adaptive_offset_point_step(
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
            result = adaptive_offset_point_step(result, n, mag);

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

namespace
{
    // Return true if two shading points reference the same triangle.
    inline bool same_triangle(
        const ShadingPoint&         lhs,
        const ShadingPoint&         rhs)
    {
        assert(lhs.hit_surface());
        assert(rhs.hit_surface());

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
        if (shading_point.hit_surface() &&
            parent_shading_point &&
            same_triangle(*parent_shading_point, shading_point))
        {
            RENDERER_LOG_WARNING(
                "self-intersection detected, distance %e.",
                shading_point.get_distance());
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
        uint64  m_ray_count;
        uint64  m_total_ray_count;

        RayCountStatisticsEntry(
            const string&   name,
            const uint64    ray_count,
            const uint64    total_ray_count)
          : Entry(name)
          , m_ray_count(ray_count)
          , m_total_ray_count(total_ray_count)
        {
        }

        unique_ptr<Entry> clone() const override
        {
            return unique_ptr<Entry>(new RayCountStatisticsEntry(*this));
        }

        void merge(const Entry* other) override
        {
            const RayCountStatisticsEntry* typed_other =
                cast<RayCountStatisticsEntry>(other);

            m_ray_count += typed_other->m_ray_count;
            m_total_ray_count += typed_other->m_total_ray_count;
        }

        string to_string() const override
        {
            return pretty_uint(m_ray_count) + " (" + pretty_percent(m_ray_count, m_total_ray_count) + ")";
        }
    };
}

StatisticsVector Intersector::get_statistics() const
{
    const uint64 total_ray_count = m_shading_ray_count + m_probe_ray_count;

    Statistics intersection_stats;
    intersection_stats.insert("total rays", total_ray_count);
    intersection_stats.insert(
        unique_ptr<RayCountStatisticsEntry>(
            new RayCountStatisticsEntry(
                "shading rays",
                m_shading_ray_count,
                total_ray_count)));
    intersection_stats.insert(
        unique_ptr<RayCountStatisticsEntry>(
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
