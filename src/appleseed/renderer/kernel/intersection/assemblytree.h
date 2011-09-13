
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
#include "foundation/math/bsp.h"
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
  : public foundation::bvh::Tree<GScalar, 3, foundation::UniqueID>
{
  public:
    // Constructor, builds the tree for a given scene.
    explicit AssemblyTree(const Scene& scene);

    // Destructor.
    ~AssemblyTree();

    // Update the assembly tree and all the child trees.
    void update();

  private:
    friend class AssemblyLeafVisitor;
    friend class AssemblyLeafProbeVisitor;
    friend class Intersector;

    const Scene&            m_scene;
    RegionTreeContainer     m_region_trees;
    TriangleTreeContainer   m_triangle_trees;

    typedef std::map<foundation::UniqueID, foundation::VersionID> AssemblyVersionMap;

    AssemblyVersionMap      m_assembly_versions;

    // Collect all assemblies of the scene.
    void collect_assemblies(std::vector<foundation::UniqueID>& assemblies) const;

    // Collect all regions of all objects in a given assembly.
    void collect_regions(const Assembly& assembly, RegionInfoVector& regions) const;

    // Create a triangle tree for a given assembly.
    foundation::Lazy<TriangleTree>* create_triangle_tree(const Assembly& assembly) const;

    // Create a region tree for a given assembly.
    foundation::Lazy<RegionTree>* create_region_tree(const Assembly& assembly) const;

    // Build the assembly tree.
    void build_assembly_tree();

    // Create or update the child trees (one per assembly).
    void update_child_trees();
};


//
// Base class for assembly leaf visitors.
//

class AssemblyLeafVisitorBase
  : public foundation::NonCopyable
{
  protected:
    // Transform a ray to the space of an assembly instance.
    void transform_ray_to_assembly_instance_space(
        const AssemblyInstance*                     assembly_instance,
        const ShadingPoint*                         parent_shading_point,
        const ShadingRay::RayType&                  input_ray,
        ShadingRay::RayType&                        output_ray);
};


//
// Assembly leaf visitor, used during tree intersection.
//

class AssemblyLeafVisitor
  : public AssemblyLeafVisitorBase
{
  public:
    // Constructor.
    AssemblyLeafVisitor(
        ShadingPoint&                               shading_point,
        const AssemblyTree&                         tree,
        RegionTreeAccessCache&                      region_tree_cache,
        TriangleTreeAccessCache&                    triangle_tree_cache,
        const ShadingPoint*                         parent_shading_point
#ifdef FOUNDATION_BSP_ENABLE_TRAVERSAL_STATS
        , foundation::bsp::TraversalStatistics&     triangle_bsp_stats
#endif
        );

    // Visit a leaf.
    bool visit(
        const std::vector<foundation::UniqueID>&    items,
        const std::vector<GAABB3>&                  bboxes,
        const size_t                                begin,
        const size_t                                end,
        const ShadingRay::RayType&                  ray,
        const ShadingRay::RayInfoType&              ray_info,
        const double                                tmin,
        const double                                tmax,
        double&                                     distance);

  private:
    ShadingPoint&                                   m_shading_point;
    const AssemblyTree&                             m_tree;
    RegionTreeAccessCache&                          m_region_tree_cache;
    TriangleTreeAccessCache&                        m_triangle_tree_cache;
    const ShadingPoint*                             m_parent_shading_point;
#ifdef FOUNDATION_BSP_ENABLE_TRAVERSAL_STATS
    foundation::bsp::TraversalStatistics&           m_triangle_bsp_stats;
#endif
};


//
// Assembly leaf visitor for probe rays, only return boolean answers
// (whether an intersection was found or not).
//

class AssemblyLeafProbeVisitor
  : public AssemblyLeafVisitorBase
  , public ProbeVisitorBase
{
  public:
    // Constructor.
    AssemblyLeafProbeVisitor(
        const AssemblyTree&                         tree,
        RegionTreeAccessCache&                      region_tree_cache,
        TriangleTreeAccessCache&                    triangle_tree_cache,
        const ShadingPoint*                         parent_shading_point
#ifdef FOUNDATION_BSP_ENABLE_TRAVERSAL_STATS
        , foundation::bsp::TraversalStatistics&     triangle_bsp_stats
#endif
        );

    // Visit a leaf.
    bool visit(
        const std::vector<foundation::UniqueID>&    items,
        const std::vector<GAABB3>&                  bboxes,
        const size_t                                begin,
        const size_t                                end,
        const ShadingRay::RayType&                  ray,
        const ShadingRay::RayInfoType&              ray_info,
        const double                                tmin,
        const double                                tmax,
        double&                                     distance);

  private:
    const AssemblyTree&                             m_tree;
    RegionTreeAccessCache&                          m_region_tree_cache;
    TriangleTreeAccessCache&                        m_triangle_tree_cache;
    const ShadingPoint*                             m_parent_shading_point;
#ifdef FOUNDATION_BSP_ENABLE_TRAVERSAL_STATS
    foundation::bsp::TraversalStatistics&           m_triangle_bsp_stats;
#endif
};


//
// Assembly tree intersectors.
//

typedef foundation::bvh::Intersector<
    double,
    AssemblyTree,
    AssemblyLeafVisitor
> AssemblyLeafIntersector;

typedef foundation::bvh::Intersector<
    double,
    AssemblyTree,
    AssemblyLeafProbeVisitor
> AssemblyLeafProbeIntersector;


//
// AssemblyLeafVisitor class implementation.
//

inline AssemblyLeafVisitor::AssemblyLeafVisitor(
    ShadingPoint&                                   shading_point,
    const AssemblyTree&                             tree,
    RegionTreeAccessCache&                          region_tree_cache,
    TriangleTreeAccessCache&                        triangle_tree_cache,
    const ShadingPoint*                             parent_shading_point
#ifdef FOUNDATION_BSP_ENABLE_TRAVERSAL_STATS
    , foundation::bsp::TraversalStatistics&         triangle_bsp_stats
#endif
    )
  : m_shading_point(shading_point)
  , m_tree(tree)
  , m_region_tree_cache(region_tree_cache)
  , m_triangle_tree_cache(triangle_tree_cache)
  , m_parent_shading_point(parent_shading_point)
#ifdef FOUNDATION_BSP_ENABLE_TRAVERSAL_STATS
  , m_triangle_bsp_stats(triangle_bsp_stats)
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
#ifdef FOUNDATION_BSP_ENABLE_TRAVERSAL_STATS
    , foundation::bsp::TraversalStatistics&         triangle_bsp_stats
#endif
    )
  : m_tree(tree)
  , m_region_tree_cache(region_tree_cache)
  , m_triangle_tree_cache(triangle_tree_cache)
  , m_parent_shading_point(parent_shading_point)
#ifdef FOUNDATION_BSP_ENABLE_TRAVERSAL_STATS
  , m_triangle_bsp_stats(triangle_bsp_stats)
#endif
{
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_INTERSECTION_ASSEMBLYTREE_H
