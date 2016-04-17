
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_INTERSECTION_REGIONTREE_H
#define APPLESEED_RENDERER_KERNEL_INTERSECTION_REGIONTREE_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/kernel/intersection/probevisitorbase.h"
#include "renderer/kernel/intersection/triangletree.h"
#include "renderer/kernel/shading/shadingray.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/bsp.h"
#include "foundation/math/bvh.h"
#include "foundation/utility/lazy.h"
#include "foundation/utility/poolallocator.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <map>
#include <memory>

// Forward declarations.
namespace renderer  { class Assembly; }
namespace renderer  { class RegionTree; }
namespace renderer  { class Scene; }
namespace renderer  { class ShadingPoint; }

namespace renderer
{

//
// Leaf of a region tree.
//

class RegionLeaf
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    RegionLeaf(
        RegionTree&                     tree,
        const foundation::UniqueID      triangle_tree_uid);

    // Return the parent tree.
    RegionTree& get_parent_tree() const;

    // Return the unique ID of the triangle tree for this leaf.
    foundation::UniqueID get_triangle_tree_uid() const;

  private:
    RegionTree&                         m_tree;                 // parent tree
    const foundation::UniqueID          m_triangle_tree_uid;    // contents of the leaf
};


//
// Region tree.
//

class RegionTree
  : public foundation::bsp::Tree<GScalar, 3, RegionLeaf>
{
  public:
    // Construction arguments.
    struct Arguments
    {
        const Scene&                    m_scene;
        const foundation::UniqueID      m_assembly_uid;
        const Assembly&                 m_assembly;

        // Constructor.
        Arguments(
            const Scene&                scene,
            const foundation::UniqueID  assembly_uid,
            const Assembly&             assembly);
    };

    // Constructor, builds the tree for a given assembly.
    explicit RegionTree(const Arguments& arguments);

    // Destructor.
    ~RegionTree();

    // Update the non-geometry aspects of the tree.
    void update_non_geometry(const bool enable_intersection_filters);

  private:
    friend class RegionLeafVisitor;
    friend class RegionLeafProbeVisitor;

    const foundation::UniqueID          m_assembly_uid;
    TriangleTreeContainer               m_triangle_trees;       // contents of the region tree
};


//
// Region tree factory.
//

class RegionTreeFactory
  : public foundation::ILazyFactory<RegionTree>
{
  public:
    // Constructor.
    explicit RegionTreeFactory(
        const RegionTree::Arguments& arguments);

    // Create the region tree.
    virtual std::auto_ptr<RegionTree> create();

  private:
    RegionTree::Arguments m_arguments;
};


//
// Some additional types.
//

// Region tree container and iterator types.
typedef std::map<
    foundation::UniqueID,
    foundation::Lazy<RegionTree>*
> RegionTreeContainer;
typedef RegionTreeContainer::iterator RegionTreeIterator;
typedef RegionTreeContainer::const_iterator RegionTreeConstIterator;

// Region tree access cache type.
typedef foundation::AccessCacheMap<
    RegionTreeContainer,
    RegionTreeAccessCacheLines,
    RegionTreeAccessCacheWays,
    foundation::PoolAllocator<void, RegionTreeAccessCacheLines * RegionTreeAccessCacheWays>
> RegionTreeAccessCache;


//
// Region leaf visitor, used during tree intersection.
//

class RegionLeafVisitor
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    RegionLeafVisitor(
        ShadingPoint&                           shading_point,
        TriangleTreeAccessCache&                triangle_tree_cache
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , foundation::bvh::TraversalStatistics& triangle_tree_stats
#endif
        );

    // Visit a leaf.
    double visit(
        const RegionLeaf*                       leaf,
        const ShadingRay&                       ray,
        const ShadingRay::RayInfoType&          ray_info);

  private:
    ShadingPoint&                               m_shading_point;
    TriangleTreeAccessCache&                    m_triangle_tree_cache;
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    foundation::bvh::TraversalStatistics&       m_triangle_tree_stats;
#endif
};


//
// Region leaf visitor for probe rays, only return boolean answers
// (whether an intersection was found or not).
//

class RegionLeafProbeVisitor
  : public ProbeVisitorBase
{
  public:
    // Constructor.
    RegionLeafProbeVisitor(
        TriangleTreeAccessCache&                triangle_tree_cache
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , foundation::bvh::TraversalStatistics& triangle_tree_stats
#endif
        );

    // Visit a leaf.
    double visit(
        const RegionLeaf*                       leaf,
        const ShadingRay&                       ray,
        const ShadingRay::RayInfoType&          ray_info);

  private:
    TriangleTreeAccessCache&                    m_triangle_tree_cache;
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    foundation::bvh::TraversalStatistics&       m_triangle_tree_stats;
#endif
};


//
// Region tree intersectors.
//

typedef foundation::bsp::Intersector<
    double,
    RegionTree,
    RegionLeafVisitor,
    ShadingRay
> RegionLeafIntersector;

typedef foundation::bsp::Intersector<
    double,
    RegionTree,
    RegionLeafProbeVisitor,
    ShadingRay
> RegionLeafProbeIntersector;


//
// RegionLeaf class implementation.
//

inline RegionLeaf::RegionLeaf(
    RegionTree&                 tree,
    const foundation::UniqueID  triangle_tree_uid)
  : m_tree(tree)
  , m_triangle_tree_uid(triangle_tree_uid)
{
}

inline RegionTree& RegionLeaf::get_parent_tree() const
{
    return m_tree;
}

inline foundation::UniqueID RegionLeaf::get_triangle_tree_uid() const
{
    return m_triangle_tree_uid;
}


//
// RegionLeafVisitor class implementation.
//

inline RegionLeafVisitor::RegionLeafVisitor(
    ShadingPoint&                               shading_point,
    TriangleTreeAccessCache&                    triangle_tree_cache
  #ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    , foundation::bvh::TraversalStatistics&     triangle_tree_stats
#endif
    )
  : m_shading_point(shading_point)
  , m_triangle_tree_cache(triangle_tree_cache)
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
  , m_triangle_tree_stats(triangle_tree_stats)
#endif
{
}


//
// RegionLeafProbeVisitor class implementation.
//

inline RegionLeafProbeVisitor::RegionLeafProbeVisitor(
    TriangleTreeAccessCache&                    triangle_tree_cache
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    , foundation::bvh::TraversalStatistics&     triangle_tree_stats
#endif
    )
  : m_triangle_tree_cache(triangle_tree_cache)
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
  , m_triangle_tree_stats(triangle_tree_stats)
#endif
{
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_INTERSECTION_REGIONTREE_H
