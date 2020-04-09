
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
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/kernel/intersection/probevisitorbase.h"
#include "renderer/kernel/intersection/trianglekey.h"
#include "renderer/kernel/intersection/trianglevertexinfo.h"
#include "renderer/modeling/scene/visibilityflags.h"

// appleseed.foundation headers.
#include "foundation/containers/alignedvector.h"
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/aabb.h"
#include "foundation/math/bvh.h"
#include "foundation/math/ray.h"
#include "foundation/memory/poolallocator.h"
#include "foundation/utility/lazy.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <cstddef>
#include <cstdint>
#include <map>
#include <memory>
#include <vector>

// Forward declarations.
namespace foundation    { class Statistics; }
namespace renderer      { class Assembly; }
namespace renderer      { class IntersectionFilter; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }
namespace renderer      { class ShadingPoint; }

namespace renderer
{

typedef std::map<std::uint64_t, IntersectionFilter*> IntersectionFilterRepository;


//
// Triangle tree.
//

class TriangleTree
  : public foundation::bvh::Tree<
               foundation::AlignedVector<
                   foundation::bvh::Node<foundation::AABB3d>
               >
           >
{
  public:
    // Construction arguments.
    struct Arguments
    {
        const Project&                          m_project;
        const foundation::UniqueID              m_triangle_tree_uid;
        const GAABB3                            m_bbox;
        const Assembly&                         m_assembly;

        // Constructor.
        Arguments(
            const Project&                      project,
            const foundation::UniqueID          triangle_tree_uid,
            const GAABB3&                       bbox,
            const Assembly&                     assembly);
    };

    // Constructor, builds the tree for a given assembly.
    explicit TriangleTree(const Arguments& arguments);

    // Destructor.
    ~TriangleTree();

    // Update the non-geometry aspects of the tree.
    void update_non_geometry(const bool enable_intersection_filters);

    // Return the number of static and moving triangles.
    size_t get_static_triangle_count() const;
    size_t get_moving_triangle_count() const;

    // Return the size (in bytes) of this object in memory.
    size_t get_memory_size() const;

  private:
    friend class TriangleLeafVisitor;
    friend class TriangleLeafProbeVisitor;

    const Arguments                             m_arguments;

    size_t                                      m_static_triangle_count;
    size_t                                      m_moving_triangle_count;

    std::vector<TriangleKey>                    m_triangle_keys;
    std::vector<std::uint8_t>                   m_leaf_data;

    IntersectionFilterRepository                m_intersection_filters_repository;
    std::vector<const IntersectionFilter*>      m_intersection_filters;

    void build_bvh(
        const ParamArray&                       params,
        const double                            time,
        const bool                              save_memory,
        foundation::Statistics&                 statistics);

    void build_sbvh(
        const ParamArray&                       params,
        const double                            time,
        const bool                              save_memory,
        foundation::Statistics&                 statistics);

    std::vector<GAABB3> compute_motion_bboxes(
        const std::vector<size_t>&              triangle_indices,
        const std::vector<TriangleVertexInfo>&  triangle_vertex_infos,
        const std::vector<GVector3>&            triangle_vertices,
        const size_t                            node_index);

    void store_triangles(
        const std::vector<size_t>&              triangle_indices,
        const std::vector<TriangleVertexInfo>&  triangle_vertex_infos,
        const std::vector<GVector3>&            triangle_vertices,
        const std::vector<TriangleKey>&         triangle_keys,
        foundation::Statistics&                 statistics);

    void update_intersection_filters();
    void delete_intersection_filters();
};


//
// Triangle tree factory.
//

class TriangleTreeFactory
  : public foundation::ILazyFactory<TriangleTree>
{
  public:
    // Constructor.
    explicit TriangleTreeFactory(
        const TriangleTree::Arguments& arguments);

    // Create the triangle tree.
    std::unique_ptr<TriangleTree> create() override;

  private:
    TriangleTree::Arguments m_arguments;
};


//
// Some additional types.
//

// Triangle tree container and iterator types.
typedef std::map<
    foundation::UniqueID,
    foundation::Lazy<TriangleTree>*
> TriangleTreeContainer;
typedef TriangleTreeContainer::iterator TriangleTreeIterator;
typedef TriangleTreeContainer::const_iterator TriangleTreeConstIterator;

// Triangle tree access cache type.
typedef foundation::AccessCacheMap<
    TriangleTreeContainer,
    TriangleTreeAccessCacheLines,
    TriangleTreeAccessCacheWays,
    foundation::PoolAllocator<void, TriangleTreeAccessCacheLines * TriangleTreeAccessCacheWays>
> TriangleTreeAccessCache;


//
// Triangle leaf visitor, used during tree intersection.
//

class TriangleLeafVisitor
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    TriangleLeafVisitor(
        const TriangleTree&                     tree,
        ShadingPoint&                           shading_point);

    // Visit a leaf.
    bool visit(
        const TriangleTree::NodeType&           node,
        const foundation::Ray3d&                ray,
        const foundation::RayInfo3d&            ray_info,
        double&                                 distance
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , foundation::bvh::TraversalStatistics& stats
#endif
        );

    // Read additional data about the triangle that was hit, if any.
    void read_hit_triangle_data() const;

  private:
    const TriangleTree&     m_tree;
    const bool              m_has_intersection_filters;
    ShadingPoint&           m_shading_point;
    GTriangleType           m_interpolated_triangle;
    const GTriangleType*    m_hit_triangle;
    size_t                  m_hit_triangle_index;
};


//
// Triangle leaf visitor for probe rays, only return boolean answers
// (whether an intersection was found or not).
//

class TriangleLeafProbeVisitor
  : public ProbeVisitorBase
{
  public:
    // Constructor.
    TriangleLeafProbeVisitor(
        const TriangleTree&                     tree,
        const double                            ray_time,
        const VisibilityFlags::Type             ray_flags);

    // Visit a leaf.
    bool visit(
        const TriangleTree::NodeType&           node,
        const foundation::Ray3d&                ray,
        const foundation::RayInfo3d&            ray_info,
        double&                                 distance
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , foundation::bvh::TraversalStatistics& stats
#endif
        );

  private:
    const TriangleTree&         m_tree;
    const double                m_ray_time;
    const VisibilityFlags::Type m_ray_flags;
    const bool                  m_has_intersection_filters;
};


//
// Triangle tree intersectors.
//

typedef foundation::bvh::Intersector<
    TriangleTree,
    TriangleLeafVisitor,
    foundation::Ray3d,          // make sure we pick the SSE2-optimized version of foundation::bvh::Intersector
    TriangleTreeStackSize
> TriangleTreeIntersector;

typedef foundation::bvh::Intersector<
    TriangleTree,
    TriangleLeafProbeVisitor,
    foundation::Ray3d,          // make sure we pick the SSE2-optimized version of foundation::bvh::Intersector
    TriangleTreeStackSize
> TriangleTreeProbeIntersector;


//
// TriangleTree class implementation.
//

inline size_t TriangleTree::get_static_triangle_count() const
{
    return m_static_triangle_count;
}

inline size_t TriangleTree::get_moving_triangle_count() const
{
    return m_moving_triangle_count;
}


//
// TriangleLeafVisitor class implementation.
//

inline TriangleLeafVisitor::TriangleLeafVisitor(
    const TriangleTree&         tree,
    ShadingPoint&               shading_point)
  : m_tree(tree)
  , m_has_intersection_filters(!tree.m_intersection_filters.empty())
  , m_shading_point(shading_point)
  , m_hit_triangle(nullptr)
{
}


//
// TriangleLeafProbeVisitor class implementation.
//

inline TriangleLeafProbeVisitor::TriangleLeafProbeVisitor(
    const TriangleTree&         tree,
    const double                ray_time,
    const VisibilityFlags::Type ray_flags)
  : m_tree(tree)
  , m_ray_time(ray_time)
  , m_ray_flags(ray_flags)
  , m_has_intersection_filters(!tree.m_intersection_filters.empty())
{
}

}   // namespace renderer
