
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
#include "renderer/kernel/intersection/probevisitorbase.h"
#include "renderer/kernel/intersection/treerepository.h"
#include "renderer/kernel/intersection/triangletree.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/containers/alignedvector.h"
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/aabb.h"
#include "foundation/math/bvh.h"
#include "foundation/utility/uid.h"
#include "foundation/utility/version.h"

// Standard headers.
#include <cstddef>
#include <map>
#include <vector>

// Forward declarations.
namespace foundation    { class Statistics; }
namespace renderer      { class AssemblyInstance; }
namespace renderer      { class Project; }
namespace renderer      { class ShadingPoint; }

namespace renderer
{

//
// Assembly tree.
//

class AssemblyTree
  : public foundation::bvh::Tree<
               foundation::AlignedVector<
                   foundation::bvh::Node<foundation::AABB3d>
               >
           >
{
  public:
    // Constructor, builds the tree for a given scene.
    explicit AssemblyTree(const Project& project);

    // Destructor.
    ~AssemblyTree();

    // Update the assembly tree and all the child trees.
    void update();

    // Return the size (in bytes) of this object in memory.
    size_t get_memory_size() const;

#ifdef APPLESEED_WITH_EMBREE

    bool use_embree() const;
    void set_use_embree(const bool value);

#endif

  private:
    friend class AssemblyLeafVisitor;
    friend class AssemblyLeafProbeVisitor;
    friend class Intersector;

    struct Item
    {
        const renderer::Assembly*               m_assembly;
        foundation::UniqueID                    m_assembly_uid;
        const renderer::AssemblyInstance*       m_assembly_instance;
        renderer::TransformSequence             m_transform_sequence;

        Item() {}

        Item(
            const renderer::Assembly*           assembly,
            const renderer::AssemblyInstance*   assembly_instance,
            const renderer::TransformSequence&  transform_sequence)
          : m_assembly(assembly)
          , m_assembly_uid(assembly->get_uid())
          , m_assembly_instance(assembly_instance)
          , m_transform_sequence(transform_sequence)
        {
        }
    };

    typedef std::vector<Item> ItemVector;
    typedef std::vector<foundation::AABB3d> AABBVector;
    typedef std::vector<const Assembly*> AssemblyVector;
    typedef std::map<foundation::UniqueID, foundation::VersionID> AssemblyVersionMap;

    const Project&                  m_project;
    ItemVector                      m_items;
    AssemblyVersionMap              m_assembly_versions;

    TreeRepository<TriangleTree>    m_triangle_tree_repository;
    TriangleTreeContainer           m_triangle_trees;

    TreeRepository<CurveTree>       m_curve_tree_repository;
    CurveTreeContainer              m_curve_trees;

#ifdef APPLESEED_WITH_EMBREE

    TreeRepository<EmbreeScene>     m_embree_scene_repository;
    EmbreeSceneContainer            m_embree_scenes;
    bool                            m_use_embree;
    bool                            m_dirty; // is used to determine triangle tree / embree switch

#endif

    void collect_assembly_instances(
        const AssemblyInstanceContainer&        assembly_instances,
        const TransformSequence&                parent_transform_seq,
        AABBVector&                             assembly_instance_bboxes);

    void rebuild_assembly_tree();
    void store_items_in_leaves(foundation::Statistics& statistics);

    void update_tree_hierarchy();
    void collect_unique_assemblies(AssemblyVector& assemblies) const;
    void delete_unused_child_trees(const AssemblyVector& assemblies);

    void create_child_trees(const Assembly& assembly);
    void create_triangle_tree(const Assembly& assembly);
    void create_curve_tree(const Assembly& assembly);

#ifdef APPLESEED_WITH_EMBREE

    void create_embree_scene(const Assembly& assembly);
    void delete_embree_scene(const foundation::UniqueID assembly_id);

#endif

    void delete_child_trees(const foundation::UniqueID assembly_id);
    void delete_triangle_tree(const foundation::UniqueID assembly_id);
    void delete_curve_tree(const foundation::UniqueID assembly_id);

    void update_triangle_trees();
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
        TriangleTreeAccessCache&                    triangle_tree_cache,
        CurveTreeAccessCache&                       curve_tree_cache,
#ifdef APPLESEED_WITH_EMBREE
        EmbreeSceneAccessCache&                     embree_scene_cache,
#endif
        const ShadingPoint*                         parent_shading_point
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , foundation::bvh::TraversalStatistics&     triangle_tree_stats
        , foundation::bvh::TraversalStatistics&     curve_tree_stats
#endif
        );

    // Visit a leaf.
    bool visit(
        const AssemblyTree::NodeType&               node,
        const ShadingRay&                           ray,
        const ShadingRay::RayInfoType&              ray_info,
        double&                                     distance
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , foundation::bvh::TraversalStatistics&     stats
#endif
        );

  private:
    ShadingPoint&                                   m_shading_point;
    const AssemblyTree&                             m_tree;
    TriangleTreeAccessCache&                        m_triangle_tree_cache;
    CurveTreeAccessCache&                           m_curve_tree_cache;
#ifdef APPLESEED_WITH_EMBREE
    EmbreeSceneAccessCache&                         m_embree_scene_cache;
#endif
    const ShadingPoint*                             m_parent_shading_point;
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    foundation::bvh::TraversalStatistics&           m_triangle_tree_stats;
    foundation::bvh::TraversalStatistics&           m_curve_tree_stats;
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
        TriangleTreeAccessCache&                    triangle_tree_cache,
        CurveTreeAccessCache&                       curve_tree_cache,
#ifdef APPLESEED_WITH_EMBREE
        EmbreeSceneAccessCache&                     embree_scene_cache,
#endif
        const ShadingPoint*                         parent_shading_point
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , foundation::bvh::TraversalStatistics&     triangle_tree_stats
        , foundation::bvh::TraversalStatistics&     curve_tree_stats
#endif
        );

    // Visit a leaf.
    bool visit(
        const AssemblyTree::NodeType&               node,
        const ShadingRay&                           ray,
        const ShadingRay::RayInfoType&              ray_info,
        double&                                     distance
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , foundation::bvh::TraversalStatistics&     stats
#endif
        );

  private:
    const AssemblyTree&                             m_tree;
    TriangleTreeAccessCache&                        m_triangle_tree_cache;
    CurveTreeAccessCache&                           m_curve_tree_cache;
#ifdef APPLESEED_WITH_EMBREE
    EmbreeSceneAccessCache&                         m_embree_scene_cache;
#endif
    const ShadingPoint*                             m_parent_shading_point;
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    foundation::bvh::TraversalStatistics&           m_triangle_tree_stats;
    foundation::bvh::TraversalStatistics&           m_curve_tree_stats;
#endif
};


//
// Assembly tree intersectors.
//

typedef foundation::bvh::Intersector<
    AssemblyTree,
    AssemblyLeafVisitor,
    ShadingRay
> AssemblyTreeIntersector;

typedef foundation::bvh::Intersector<
    AssemblyTree,
    AssemblyLeafProbeVisitor,
    ShadingRay
> AssemblyTreeProbeIntersector;


//
// AssemblyLeafVisitor class implementation.
//

inline AssemblyLeafVisitor::AssemblyLeafVisitor(
    ShadingPoint&                                   shading_point,
    const AssemblyTree&                             tree,
    TriangleTreeAccessCache&                        triangle_tree_cache,
    CurveTreeAccessCache&                           curve_tree_cache,
#ifdef APPLESEED_WITH_EMBREE
    EmbreeSceneAccessCache&                         embree_scene_cache,
#endif
    const ShadingPoint*                             parent_shading_point
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    , foundation::bvh::TraversalStatistics&         triangle_tree_stats
    , foundation::bvh::TraversalStatistics&         curve_tree_stats
#endif
    )
  : m_shading_point(shading_point)
  , m_tree(tree)
  , m_triangle_tree_cache(triangle_tree_cache)
  , m_curve_tree_cache(curve_tree_cache)
#ifdef APPLESEED_WITH_EMBREE
  , m_embree_scene_cache(embree_scene_cache)
#endif
  , m_parent_shading_point(parent_shading_point)
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
  , m_triangle_tree_stats(triangle_tree_stats)
  , m_curve_tree_stats(curve_tree_stats)
#endif
{
}


//
// AssemblyLeafProbeVisitor class implementation.
//

inline AssemblyLeafProbeVisitor::AssemblyLeafProbeVisitor(
    const AssemblyTree&                             tree,
    TriangleTreeAccessCache&                        triangle_tree_cache,
    CurveTreeAccessCache&                           curve_tree_cache,
#ifdef APPLESEED_WITH_EMBREE
    EmbreeSceneAccessCache&                         embree_scene_cache,
#endif
    const ShadingPoint*                             parent_shading_point
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    , foundation::bvh::TraversalStatistics&         triangle_tree_stats
    , foundation::bvh::TraversalStatistics&         curve_tree_stats
#endif
    )
  : m_tree(tree)
  , m_triangle_tree_cache(triangle_tree_cache)
  , m_curve_tree_cache(curve_tree_cache)
#ifdef APPLESEED_WITH_EMBREE
  , m_embree_scene_cache(embree_scene_cache)
#endif
  , m_parent_shading_point(parent_shading_point)
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
  , m_triangle_tree_stats(triangle_tree_stats)
  , m_curve_tree_stats(curve_tree_stats)
#endif
{
}

}   // namespace renderer
