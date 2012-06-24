
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

#ifndef APPLESEED_RENDERER_KERNEL_INTERSECTION_TRIANGLETREE_H
#define APPLESEED_RENDERER_KERNEL_INTERSECTION_TRIANGLETREE_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersectionfilter.h"
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/kernel/intersection/probevisitorbase.h"
#include "renderer/kernel/intersection/regioninfo.h"
#include "renderer/kernel/intersection/trianglekey.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/aabb.h"
#include "foundation/math/bvh.h"
#include "foundation/math/intersection.h"
#include "foundation/math/scalar.h"
#include "foundation/utility/alignedvector.h"
#include "foundation/utility/lazy.h"
#include "foundation/utility/poolallocator.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <cstddef>
#include <map>
#include <memory>
#include <vector>

// Forward declarations.
namespace foundation    { class Statistics; }
namespace renderer      { class Assembly; }
namespace renderer      { class Scene; }
namespace renderer      { class TriangleVertexInfo; }

namespace renderer
{

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
        const Scene&                            m_scene;
        const foundation::UniqueID              m_triangle_tree_uid;
        const GAABB3                            m_bbox;
        const Assembly&                         m_assembly;
        const RegionInfoVector                  m_regions;

        // Constructor.
        Arguments(
            const Scene&                        scene,
            const foundation::UniqueID          triangle_tree_uid,
            const GAABB3&                       bbox,
            const Assembly&                     assembly,
            const RegionInfoVector&             regions);
    };

    // Constructor, builds the tree for a given set of regions.
    explicit TriangleTree(const Arguments& arguments);

    // Destructor.
    ~TriangleTree();

    // Return the size (in bytes) of this object in memory.
    size_t get_memory_size() const;

  private:
    friend class TriangleLeafVisitor;
    friend class TriangleLeafProbeVisitor;

    const foundation::UniqueID                  m_triangle_tree_uid;
    std::vector<TriangleKey>                    m_triangle_keys;
    std::vector<foundation::uint8>              m_leaf_data;

    bool                                        m_has_intersection_filters;
    std::vector<const IntersectionFilter*>      m_intersection_filters;

    void build_bvh(
        const Arguments&                        arguments,
        foundation::Statistics&                 statistics);

    void build_sbvh(
        const Arguments&                        arguments,
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

    void create_intersection_filters(const Arguments& arguments);
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
        const TriangleTree::Arguments&  arguments);

    // Create the triangle tree.
    virtual std::auto_ptr<TriangleTree> create();

  private:
    const TriangleTree::Arguments       m_arguments;
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
    TriangleTreeAccessCacheSize,
    1,
    foundation::PoolAllocator<void, TriangleTreeAccessCacheSize>
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
        const ShadingRay&                       ray,
        const ShadingRay::RayInfoType&          ray_info,
        double&                                 distance
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , foundation::bvh::TraversalStatistics& stats
#endif
        );

    // Read additional data about the triangle that was hit, if any.
    void read_hit_triangle_data() const;

  private:
    const TriangleTree&     m_tree;
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
    explicit TriangleLeafProbeVisitor(
        const TriangleTree&                     tree);

    // Visit a leaf.
    bool visit(
        const TriangleTree::NodeType&           node,
        const ShadingRay&                       ray,
        const ShadingRay::RayInfoType&          ray_info,
        double&                                 distance
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , foundation::bvh::TraversalStatistics& stats
#endif
        );

  private:
    const TriangleTree&     m_tree;
};


//
// Triangle tree intersectors.
//

typedef foundation::bvh::Intersector<
    TriangleTree,
    TriangleLeafVisitor,
    ShadingRay,
    TriangleTreeStackSize
> TriangleTreeIntersector;

typedef foundation::bvh::Intersector<
    TriangleTree,
    TriangleLeafProbeVisitor,
    ShadingRay,
    TriangleTreeStackSize
> TriangleTreeProbeIntersector;


//
// Utility class to convert a triangle to the desired precision if necessary,
// but avoid any work (in particular, no copy) if the source triangle already
// has the desired precision and can be used in-place.
//

namespace impl
{
    template <bool CompatibleTypes> struct TriangleReaderImpl;

    // Compatible types: no conversion or copy.
    template <> struct TriangleReaderImpl<true>
    {
        const TriangleType& m_triangle;

        explicit TriangleReaderImpl(const TriangleType& triangle)
          : m_triangle(triangle)
        {
        }
    };

    // Incompatible types: perform a conversion.
    template <> struct TriangleReaderImpl<false>
    {
        const TriangleType m_triangle;

        explicit TriangleReaderImpl(const GTriangleType& triangle)
          : m_triangle(triangle)
        {
        }
    };

    typedef TriangleReaderImpl<
        sizeof(GTriangleType::ValueType) == sizeof(TriangleType::ValueType)
    > TriangleReader;
}


//
// TriangleLeafVisitor class implementation.
//

inline TriangleLeafVisitor::TriangleLeafVisitor(
    const TriangleTree&                     tree,
    ShadingPoint&                           shading_point)
  : m_tree(tree)
  , m_shading_point(shading_point)
  , m_hit_triangle(0)
{
}

inline bool TriangleLeafVisitor::visit(
    const TriangleTree::NodeType&           node,
    const ShadingRay&                       ray,
    const ShadingRay::RayInfoType&          ray_info,
    double&                                 distance
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    , foundation::bvh::TraversalStatistics& stats
#endif
    )
{
    // Retrieve the pointer to the data of this leaf.
    const foundation::uint8* user_data = &node.get_user_data<foundation::uint8>();
    const foundation::uint32 leaf_data_index =
        *reinterpret_cast<const foundation::uint32*>(user_data);
    const foundation::uint8* leaf_data =
        leaf_data_index == ~0
            ? user_data + sizeof(foundation::uint32)    // triangles are stored in the leaf node
            : &m_tree.m_leaf_data[leaf_data_index];     // triangles are stored in the tree

    const size_t triangle_index = node.get_item_index();
    const size_t triangle_count = node.get_item_count();

    // Sequentially intersect all triangles of the leaf.
    for (size_t i = 0; i < triangle_count; ++i)
    {
        // Retrieve the number of motion segments for this triangle.
        const foundation::uint32 motion_segment_count =
            *reinterpret_cast<const foundation::uint32*>(leaf_data);
        leaf_data += sizeof(foundation::uint32);

        if (motion_segment_count == 0)
        {
            // Load the triangle, converting it to the right format if necessary.
            const GTriangleType* triangle_ptr = reinterpret_cast<const GTriangleType*>(leaf_data);
            const impl::TriangleReader reader(*triangle_ptr);
            leaf_data += sizeof(GTriangleType);

            // Intersect the triangle.
            double t, u, v;
            if (reader.m_triangle.intersect(m_shading_point.m_ray, t, u, v))
            {
                // Optionally filter intersections.
                if (m_tree.m_has_intersection_filters)
                {
                    const TriangleKey& triangle_key = m_tree.m_triangle_keys[triangle_index + i];
                    const IntersectionFilter* filter =
                        m_tree.m_intersection_filters[triangle_key.get_object_instance_index()];
                    if (filter && !filter->accept(triangle_key, u, v))
                        continue;
                }

                m_hit_triangle = triangle_ptr;
                m_hit_triangle_index = triangle_index + i;
                m_shading_point.m_ray.m_tmax = t;
                m_shading_point.m_bary[0] = u;
                m_shading_point.m_bary[1] = v;
            }
        }
        else
        {
            // Retrieve the vertices of the triangle at the two keyframes surrounding the ray time.
            const size_t prev_index = foundation::truncate<size_t>(ray.m_time * motion_segment_count);
            const GVector3* prev_vertices = reinterpret_cast<const GVector3*>(leaf_data) + prev_index * 3;
            const GVector3* next_vertices = prev_vertices + 3;
            leaf_data += (motion_segment_count + 1) * 3 * sizeof(GVector3);

            // Interpolate triangle vertices.
            const GScalar k = static_cast<GScalar>(ray.m_time * motion_segment_count - prev_index);
            const GVector3 vert0 = foundation::lerp(prev_vertices[0], next_vertices[0], k);
            const GVector3 vert1 = foundation::lerp(prev_vertices[1], next_vertices[1], k);
            const GVector3 vert2 = foundation::lerp(prev_vertices[2], next_vertices[2], k);

            // Load the triangle, converting it to the right format if necessary.
            const GTriangleType triangle(vert0, vert1, vert2);
            const impl::TriangleReader reader(triangle);

            // Intersect the triangle.
            double t, u, v;
            if (reader.m_triangle.intersect(m_shading_point.m_ray, t, u, v))
            {
                // Optionally filter intersections.
                if (m_tree.m_has_intersection_filters)
                {
                    const TriangleKey& triangle_key = m_tree.m_triangle_keys[triangle_index + i];
                    const IntersectionFilter* filter =
                        m_tree.m_intersection_filters[triangle_key.get_object_instance_index()];
                    if (filter && !filter->accept(triangle_key, u, v))
                        continue;
                }

                m_interpolated_triangle = triangle;
                m_hit_triangle = &m_interpolated_triangle;
                m_hit_triangle_index = triangle_index + i;
                m_shading_point.m_ray.m_tmax = t;
                m_shading_point.m_bary[0] = u;
                m_shading_point.m_bary[1] = v;
            }
        }
    }

    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_intersected_items.insert(triangle_count));

    // Continue traversal.
    distance = m_shading_point.m_ray.m_tmax;
    return true;
}

inline void TriangleLeafVisitor::read_hit_triangle_data() const
{
    if (m_hit_triangle)
    {
        // Record a hit.
        m_shading_point.m_hit = true;

        // Copy the triangle key.
        const TriangleKey& triangle_key = m_tree.m_triangle_keys[m_hit_triangle_index];
        m_shading_point.m_object_instance_index = triangle_key.get_object_instance_index();
        m_shading_point.m_region_index = triangle_key.get_region_index();
        m_shading_point.m_triangle_index = triangle_key.get_triangle_index();

        // Compute and store the support plane of the hit triangle.
        const impl::TriangleReader reader(*m_hit_triangle);
        m_shading_point.m_triangle_support_plane.initialize(reader.m_triangle);
    }
}


//
// TriangleLeafProbeVisitor class implementation.
//

inline TriangleLeafProbeVisitor::TriangleLeafProbeVisitor(
    const TriangleTree&                     tree)
  : m_tree(tree)
{
}

inline bool TriangleLeafProbeVisitor::visit(
    const TriangleTree::NodeType&           node,
    const ShadingRay&                       ray,
    const ShadingRay::RayInfoType&          ray_info,
    double&                                 distance
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    , foundation::bvh::TraversalStatistics& stats
#endif
    )
{
    // Retrieve the pointer to the data of this leaf.
    const foundation::uint8* user_data = &node.get_user_data<foundation::uint8>();
    const foundation::uint32 leaf_data_index =
        *reinterpret_cast<const foundation::uint32*>(user_data);
    const foundation::uint8* leaf_data =
        leaf_data_index == ~0
            ? user_data + sizeof(foundation::uint32)    // triangles are stored in the leaf node
            : &m_tree.m_leaf_data[leaf_data_index];     // triangles are stored in the tree

    const size_t triangle_count = node.get_item_count();

    // Sequentially intersect triangles until a hit is found.
    for (size_t i = 0; i < triangle_count; ++i)
    {
        // Retrieve the number of motion segments for this triangle.
        const foundation::uint32 motion_segment_count =
            *reinterpret_cast<const foundation::uint32*>(leaf_data);
        leaf_data += sizeof(foundation::uint32);

        if (motion_segment_count == 0)
        {
            // Load the triangle, converting it to the right format if necessary.
            const GTriangleType* triangle_ptr = reinterpret_cast<const GTriangleType*>(leaf_data);
            const impl::TriangleReader reader(*triangle_ptr);
            leaf_data += sizeof(GTriangleType);

            // Intersect the triangle.
            if (reader.m_triangle.intersect(ray))
            {
                FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_intersected_items.insert(i + 1));
                m_hit = true;
                return false;
            }
        }
        else
        {
            // Retrieve the vertices of the triangle at the two keyframes surrounding the ray time.
            const size_t prev_index = foundation::truncate<size_t>(ray.m_time * motion_segment_count);
            const GVector3* prev_vertices = reinterpret_cast<const GVector3*>(leaf_data) + prev_index * 3;
            const GVector3* next_vertices = prev_vertices + 3;
            leaf_data += (motion_segment_count + 1) * 3 * sizeof(GVector3);

            // Interpolate triangle vertices.
            const GScalar k = static_cast<GScalar>(ray.m_time * motion_segment_count - prev_index);
            const GVector3 vert0 = foundation::lerp(prev_vertices[0], next_vertices[0], k);
            const GVector3 vert1 = foundation::lerp(prev_vertices[1], next_vertices[1], k);
            const GVector3 vert2 = foundation::lerp(prev_vertices[2], next_vertices[2], k);

            // Load the triangle, converting it to the right format if necessary.
            const GTriangleType triangle(vert0, vert1, vert2);
            const impl::TriangleReader reader(triangle);

            // Intersect the triangle.
            if (reader.m_triangle.intersect(ray))
            {
                FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_intersected_items.insert(i + 1));
                m_hit = true;
                return false;
            }
        }
    }

    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_intersected_items.insert(triangle_count));

    // Continue traversal.
    distance = ray.m_tmax;
    return true;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_INTERSECTION_TRIANGLETREE_H
