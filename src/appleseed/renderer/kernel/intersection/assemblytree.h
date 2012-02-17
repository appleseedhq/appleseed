
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

#ifndef APPLESEED_RENDERER_KERNEL_INTERSECTION_ASSEMBLYTREE_H
#define APPLESEED_RENDERER_KERNEL_INTERSECTION_ASSEMBLYTREE_H

// appleseed.renderer headers.
#include "renderer/global/global.h"
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/kernel/intersection/probevisitorbase.h"
#include "renderer/kernel/intersection/regioninfo.h"
#include "renderer/kernel/intersection/regiontree.h"
#include "renderer/kernel/intersection/triangletree.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/math/bvh.h"
#include "foundation/utility/lazy.h"
#include "foundation/utility/version.h"

// Standard headers.
#include <map>
#include <vector>

// Forward declarations.
namespace renderer  { class Assembly; }
namespace renderer  { class ShadingPoint; }

namespace renderer
{

//
// Assembly tree.
//

class AssemblyTree
  : public foundation::bvh::Tree<double, 3>
{
  public:
    // Constructor, builds the tree for a given scene.
    explicit AssemblyTree(const Scene& scene);

    // Destructor.
    ~AssemblyTree();

    // Update the assembly tree and all the child trees.
    void update();

    // Return the size (in bytes) of this object in memory.
    size_t get_memory_size() const;

  private:
    friend class AssemblyLeafVisitor;
    friend class AssemblyLeafProbeVisitor;
    friend class Intersector;

    typedef std::map<foundation::UniqueID, foundation::VersionID> AssemblyVersionMap;

    const Scene&                        m_scene;
    RegionTreeContainer                 m_region_trees;
    TriangleTreeContainer               m_triangle_trees;
    std::vector<foundation::UniqueID>   m_assembly_instances;
    AssemblyVersionMap                  m_assembly_versions;

    // Create a triangle tree for a given assembly.
    foundation::Lazy<TriangleTree>* create_triangle_tree(const Assembly& assembly) const;

    // Create a region tree for a given assembly.
    foundation::Lazy<RegionTree>* create_region_tree(const Assembly& assembly) const;

    // Clear the assembly tree.
    void clear();

    // Build the assembly tree.
    void build_assembly_tree();

    // Create or update the child trees (one per assembly).
    void update_child_trees();
};


//
// Assembly leaf visitor, used during tree intersection.
//

class AssemblyLeafVisitor
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    AssemblyLeafVisitor(
        ShadingPoint&                               shading_point,
        const AssemblyTree&                         tree,
        RegionTreeAccessCache&                      region_tree_cache,
        TriangleTreeAccessCache&                    triangle_tree_cache,
        const ShadingPoint*                         parent_shading_point
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , foundation::bvh::TraversalStatistics&     triangle_tree_stats
#endif
        );

    // Visit a leaf.
    bool visit(
        const std::vector<foundation::AABB3d>&      bboxes,
        const AssemblyTree::NodeType&               node,
        const ShadingRay::RayType&                  ray,
        const ShadingRay::RayInfoType&              ray_info,
        double&                                     distance
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , foundation::bvh::TraversalStatistics&     stats
#endif
        );

  private:
    ShadingPoint&                                   m_shading_point;
    const AssemblyTree&                             m_tree;
    RegionTreeAccessCache&                          m_region_tree_cache;
    TriangleTreeAccessCache&                        m_triangle_tree_cache;
    const ShadingPoint*                             m_parent_shading_point;
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    foundation::bvh::TraversalStatistics&           m_triangle_tree_stats;
#endif
};


//
// Assembly leaf visitor for probe rays, only return boolean answers
// (whether an intersection was found or not).
//

class AssemblyLeafProbeVisitor
  : public ProbeVisitorBase
{
  public:
    // Constructor.
    AssemblyLeafProbeVisitor(
        const AssemblyTree&                         tree,
        RegionTreeAccessCache&                      region_tree_cache,
        TriangleTreeAccessCache&                    triangle_tree_cache,
        const ShadingPoint*                         parent_shading_point
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , foundation::bvh::TraversalStatistics&     triangle_tree_stats
#endif
        );

    // Visit a leaf.
    bool visit(
        const std::vector<foundation::AABB3d>&      bboxes,
        const AssemblyTree::NodeType&               node,
        const ShadingRay::RayType&                  ray,
        const ShadingRay::RayInfoType&              ray_info,
        double&                                     distance
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , foundation::bvh::TraversalStatistics&     stats
#endif
        );

  private:
    const AssemblyTree&                             m_tree;
    RegionTreeAccessCache&                          m_region_tree_cache;
    TriangleTreeAccessCache&                        m_triangle_tree_cache;
    const ShadingPoint*                             m_parent_shading_point;
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    foundation::bvh::TraversalStatistics&           m_triangle_tree_stats;
#endif
};


//
// Assembly tree intersectors.
//

typedef foundation::bvh::Intersector<
    double,
    AssemblyTree,
    AssemblyLeafVisitor
> AssemblyTreeIntersector;

typedef foundation::bvh::Intersector<
    double,
    AssemblyTree,
    AssemblyLeafProbeVisitor
> AssemblyTreeProbeIntersector;


//
// AssemblyLeafVisitor class implementation.
//

inline AssemblyLeafVisitor::AssemblyLeafVisitor(
    ShadingPoint&                                   shading_point,
    const AssemblyTree&                             tree,
    RegionTreeAccessCache&                          region_tree_cache,
    TriangleTreeAccessCache&                        triangle_tree_cache,
    const ShadingPoint*                             parent_shading_point
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    , foundation::bvh::TraversalStatistics&         triangle_tree_stats
#endif
    )
  : m_shading_point(shading_point)
  , m_tree(tree)
  , m_region_tree_cache(region_tree_cache)
  , m_triangle_tree_cache(triangle_tree_cache)
  , m_parent_shading_point(parent_shading_point)
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
  , m_triangle_tree_stats(triangle_tree_stats)
#endif
{
}


//
// AssemblyLeafProbeVisitor class implementation.
//

inline AssemblyLeafProbeVisitor::AssemblyLeafProbeVisitor(
    const AssemblyTree&                             tree,
    RegionTreeAccessCache&                          region_tree_cache,
    TriangleTreeAccessCache&                        triangle_tree_cache,
    const ShadingPoint*                             parent_shading_point
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    , foundation::bvh::TraversalStatistics&         triangle_tree_stats
#endif
    )
  : m_tree(tree)
  , m_region_tree_cache(region_tree_cache)
  , m_triangle_tree_cache(triangle_tree_cache)
  , m_parent_shading_point(parent_shading_point)
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
  , m_triangle_tree_stats(triangle_tree_stats)
#endif
{
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_INTERSECTION_ASSEMBLYTREE_H
