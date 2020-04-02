
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
#include "renderer/kernel/intersection/curvetree.h"
#ifdef APPLESEED_WITH_EMBREE
#include "renderer/kernel/intersection/embreescene.h"
#endif
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/kernel/intersection/triangletree.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/tessellation/statictessellation.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/bvh.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>
#include <cstdint>

// Forward declarations.
namespace foundation    { class StatisticsVector; }
namespace renderer      { class AssemblyInstance; }
namespace renderer      { class ShadingRay; }
namespace renderer      { class TextureCache; }
namespace renderer      { class TraceContext; }

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
        const TraceContext&                 trace_context,
        TextureCache&                       texture_cache,
        const bool                          report_self_intersections = false);
    // Trace a world space ray through the scene.
    bool trace(
        const ShadingRay&                   ray,
        ShadingPoint&                       shading_point,
        const ShadingPoint*                 parent_shading_point = nullptr) const;

    // Trace a world space probe ray through the scene.
    bool trace_probe(
        const ShadingRay&                   ray,
        const ShadingPoint*                 parent_shading_point = nullptr) const;

    // Manufacture a triangle hit "by hand".
    // There is no restriction placed on the shading point passed to this method.
    // For instance it may have been previously initialized and used.
    void make_triangle_shading_point(
        ShadingPoint&                       shading_point,
        const ShadingRay&                   shading_ray,
        const foundation::Vector2f&         bary,
        const AssemblyInstance*             assembly_instance,
        const foundation::Transformd&       assembly_instance_transform,
        const size_t                        object_instance_index,
        const size_t                        primitive_index,
        const TriangleSupportPlaneType&     triangle_support_plane) const;

    // Manufacture a procedural surface hit "by hand".
    // There is no restriction placed on the shading point passed to this method.
    // For instance it may have been previously initialized and used.
    void make_procedural_surface_shading_point(
        ShadingPoint&                       shading_point,
        const ShadingRay&                   shading_ray,
        const foundation::Vector2f&         uv,
        const AssemblyInstance*             assembly_instance,
        const foundation::Transformd&       assembly_instance_transform,
        const size_t                        object_instance_index,
        const size_t                        primitive_index,
        const foundation::Vector3d&         point,
        const foundation::Vector3d&         normal,
        const foundation::Vector3d&         dpdu,
        const foundation::Vector3d&         dpdv) const;

    // Manufacture a volume shading point "by hand".
    // There is no restriction placed on the shading point passed to this method.
    // For instance it may have been previously initialized and used.
    void make_volume_shading_point(
        ShadingPoint&                       shading_point,
        const ShadingRay&                   volume_ray,
        const double                        distance) const;

    // Retrieve performance statistics.
    foundation::StatisticsVector get_statistics() const;

  private:
    const TraceContext&                             m_trace_context;
    TextureCache&                                   m_texture_cache;
    const bool                                      m_report_self_intersections;

    // Access caches.
    mutable TriangleTreeAccessCache                 m_triangle_tree_cache;
    mutable CurveTreeAccessCache                    m_curve_tree_cache;
#ifdef APPLESEED_WITH_EMBREE
    mutable EmbreeSceneAccessCache                  m_embree_scene_cache;
#endif
    // Intersection statistics.
    mutable std::uint64_t                           m_shading_ray_count;
    mutable std::uint64_t                           m_probe_ray_count;
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    mutable foundation::bvh::TraversalStatistics    m_assembly_tree_traversal_stats;
    mutable foundation::bvh::TraversalStatistics    m_triangle_tree_traversal_stats;
    mutable foundation::bvh::TraversalStatistics    m_curve_tree_traversal_stats;
#endif
};

}   // namespace renderer
