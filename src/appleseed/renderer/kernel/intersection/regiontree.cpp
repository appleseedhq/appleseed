
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "regiontree.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/intersection/regioninfo.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/entity/entityvector.h"
#include "renderer/modeling/object/iregion.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/object/regionkit.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/utility/bbox.h"

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/split.h"
#include "foundation/math/transform.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/memory.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>
#include <limits>
#include <string>
#include <utility>

using namespace foundation;
using namespace std;

namespace renderer
{

namespace
{
    //
    // Leaf of an intermediate region tree.
    //

    class IntermRegionLeaf
      : public NonCopyable
    {
      public:
        // Constructor.
        explicit IntermRegionLeaf(const Assembly& assembly)
          : m_assembly(assembly)
        {
            m_extent.invalidate();
        }

        // Remove all regions from the leaf.
        void clear()
        {
            clear_keep_memory(m_regions);
        }

        // Insert a region into the leaf.
        void insert(const RegionInfo& region_info)
        {
            m_regions.push_back(region_info);
        }

        // Return the number of regions in the leaf.
        size_t get_size() const
        {
            return m_regions.size();
        }

        // Return the parent space bounding box of the leaf.
        GAABB3 get_bbox() const
        {
            GAABB3 bbox;
            bbox.invalidate();

            const size_t region_count = m_regions.size();
            for (size_t i = 0; i < region_count; ++i)
            {
                // Fetch the region info.
                const RegionInfo& region_info = m_regions[i];

                // Extend the bounding box of the leaf to include the
                // assembly space bounding box of the region.
                bbox.insert(region_info.get_region_parent_bbox());
            }

            return bbox;
        }

        // Return the size (in bytes) of this object in memory.
        size_t get_memory_size() const
        {
            return
                  sizeof(*this)
                + m_regions.capacity() * sizeof(RegionInfo);
        }

      private:
        friend class renderer::RegionTree;
        friend class IntermRegionTree;
        friend class IntermRegionLeafSplitter;

        typedef bsp::LeafInfo<GScalar, 3>   LeafInfoType;
        typedef Split<GScalar>              SplitType;

        const Assembly&     m_assembly;     // parent assembly
        GAABB3              m_extent;       // bounding box of the parent assembly leaf
        RegionInfoVector    m_regions;      // contents of the leaf
    };


    //
    // Intermediate region leaf factory.
    //

    class IntermRegionLeafFactory
      : public NonCopyable
    {
      public:
        // Constructor.
        explicit IntermRegionLeafFactory(const Assembly& assembly)
          : m_assembly(assembly)
        {
        }

        // Create a new leaf.
        IntermRegionLeaf* create_leaf()
        {
            return new IntermRegionLeaf(m_assembly);
        }

      private:
        const Assembly&     m_assembly;     // parent assembly
    };


    //
    // Intermediate region leaf splitter.
    //

    class IntermRegionLeafSplitter
      : public NonCopyable
    {
      public:
        // Constructor.
        IntermRegionLeafSplitter()
        {
            // Precompute 1/exp(i).
            for (size_t i = 0; i < RegionTreeMaxDepth; ++i)
                m_rcp_exp_depth[i] = 1.0 / exp(static_cast<double>(i));
        }

        // Return the splitting priority of a leaf.
        double get_priority(
            const IntermRegionLeaf&                 leaf,
            const IntermRegionLeaf::LeafInfoType&   leaf_info) const
        {
            const size_t size = leaf.m_regions.size();
            if (size > RegionTreeMaxLeafSize)
                return size * m_rcp_exp_depth[leaf_info.get_node_depth()];
            else return 0.0;
        }

        // Find a split.
        bool split(
            const IntermRegionLeaf&                 leaf,
            const IntermRegionLeaf::LeafInfoType&   leaf_info,
            IntermRegionLeaf::SplitType&            split)
        {
            assert(!leaf.m_regions.empty());

            const GAABB3& leaf_bbox = leaf_info.get_bbox();

            // Start with a middle split.
            split = IntermRegionLeaf::SplitType::middle(leaf_bbox);

            // Refine the split by aligning the splitting plane
            // with the bounding box wall which is the closest
            // to the middle split plane.

            const GScalar middle = split.m_abscissa;
            GScalar min_dist = numeric_limits<GScalar>::max();

            const size_t region_count = leaf.m_regions.size();
            for (size_t i = 0; i < region_count; ++i)
            {
                // Fetch the region info.
                const RegionInfo& region_info = leaf.m_regions[i];

                // Retrieve the assembly space bounding box of the region.
                const GAABB3& region_bbox = region_info.get_region_parent_bbox();

                // Compute distance of both walls to the middle split plane.
                const GScalar x1 = region_bbox.min[split.m_dimension];
                const GScalar x2 = region_bbox.max[split.m_dimension];
                const GScalar d1 = abs(x1 - middle);
                const GScalar d2 = abs(x2 - middle);

                // Keep track of the wall which is the closest to the middle split plane.
                if (min_dist > d1)
                {
                    min_dist = d1;
                    split.m_abscissa = x1;
                }
                if (min_dist > d2)
                {
                    min_dist = d2;
                    split.m_abscissa = x2;
                }
            }

            return
                   split.m_abscissa > leaf_bbox.min[split.m_dimension]
                && split.m_abscissa < leaf_bbox.max[split.m_dimension];
        }

        // Sort a leaf into child leaves, according to a given split.
        void sort(
            const IntermRegionLeaf&                 leaf,
            const IntermRegionLeaf::LeafInfoType&   leaf_info,
            const IntermRegionLeaf::SplitType&      split,
            IntermRegionLeaf&                       left_leaf,
            const IntermRegionLeaf::LeafInfoType&   left_leaf_info,
            IntermRegionLeaf&                       right_leaf,
            const IntermRegionLeaf::LeafInfoType&   right_leaf_info)
        {
            assert(!leaf.m_regions.empty());

            // Save the extent of the child leaves.
            left_leaf.m_extent = left_leaf_info.get_bbox();
            right_leaf.m_extent = right_leaf_info.get_bbox();

            const size_t dim = split.m_dimension;

            const size_t region_count = leaf.m_regions.size();
            for (size_t i = 0; i < region_count; ++i)
            {
                // Fetch the region info.
                const RegionInfo& region_info = leaf.m_regions[i];

                // Retrieve the assembly space bounding box of the region.
                const GAABB3& region_bbox = region_info.get_region_parent_bbox();

                // Insert the region into the appropriate leaves.
                if (region_bbox.max[dim] <= split.m_abscissa)
                {
                    left_leaf.insert(region_info);
                }
                else if (region_bbox.min[dim] >= split.m_abscissa)
                {
                    right_leaf.insert(region_info);
                }
                else
                {
                    left_leaf.insert(region_info);
                    right_leaf.insert(region_info);
                }
            }
        }

      private:
        double  m_rcp_exp_depth[RegionTreeMaxDepth];
    };


    //
    // Intermediate region tree builder.
    //

    class IntermRegionTree;

    typedef bsp::Builder<
        IntermRegionTree,
        IntermRegionLeafFactory,
        IntermRegionLeafSplitter
    > IntermRegionTreeBuilder;


    //
    // Intermediate region tree statistics.
    //

    typedef bsp::TreeStatistics<
        IntermRegionTree,
        IntermRegionTreeBuilder
    > IntermRegionTreeStatistics;


    //
    // Intermediate region tree.
    //

    class IntermRegionTree
      : public bsp::Tree<GScalar, 3, IntermRegionLeaf>
    {
      public:
        // Constructor, builds the tree for a given assembly.
        explicit IntermRegionTree(const RegionTree::Arguments& arguments)
        {
            // Retrieve the assembly for which to create the region tree.
            const Assembly& assembly = arguments.m_assembly;

            // Create the leaf factory.
            IntermRegionLeafFactory factory(assembly);

            // Create the root leaf of the region tree.
            auto_ptr<IntermRegionLeaf> root_leaf(factory.create_leaf());
            root_leaf->m_extent =
                compute_parent_bbox<GAABB3>(
                    assembly.object_instances().begin(),
                    assembly.object_instances().end());

            // Insert all regions of all object instances into the root leaf.
            for (size_t inst_index = 0; inst_index < assembly.object_instances().size(); ++inst_index)
            {
                // Retrieve the object instance and its transformation.
                const ObjectInstance* object_instance = assembly.object_instances().get_by_index(inst_index);
                assert(object_instance);
                const Transformd& transform = object_instance->get_transform();

                // Retrieve the object.
                Object& object = object_instance->get_object();

                // Retrieve the region kit of the object.
                Access<RegionKit> region_kit(&object.get_region_kit());

                // Insert all regions of the object into the root leaf.
                for (size_t region_index = 0; region_index < region_kit->size(); ++region_index)
                {
                    // Retrieve the region.
                    const IRegion* region = (*region_kit)[region_index];

                    // Compute the assembly space bounding box of the region.
                    const GAABB3 region_bbox =
                        transform.to_parent(region->compute_local_bbox());

                    // Insert the region into the root leaf.
                    root_leaf->insert(
                        RegionInfo(
                            inst_index,
                            region_index,
                            region_bbox));
                }
            }

            // Log a progress message.
            RENDERER_LOG_INFO(
                "building region tree for assembly #" FMT_UNIQUE_ID " (%s %s)...",
                arguments.m_assembly_uid,
                pretty_int(root_leaf->get_size()).c_str(),
                plural(root_leaf->get_size(), "region").c_str());

            // Build the region tree.
            IntermRegionLeafSplitter splitter;
            IntermRegionTreeBuilder builder;
            builder.build(
                *this,
                root_leaf,
                factory,
                splitter,
                RegionTreeMaxDuplication);

            // Collect and print triangle tree statistics.
            IntermRegionTreeStatistics tree_stats(*this, builder);
            RENDERER_LOG_DEBUG(
                "region tree #" FMT_UNIQUE_ID " statistics:",
                arguments.m_assembly_uid);
            tree_stats.print(global_logger());
        }

      private:
        friend class renderer::RegionTree;
    };
}


//
// RegionTree class implementation.
//

RegionTree::Arguments::Arguments(
    const Scene&    scene,
    const UniqueID  assembly_uid,
    const Assembly& assembly)
  : m_scene(scene)
  , m_assembly_uid(assembly_uid)
  , m_assembly(assembly)
{
}

RegionTree::RegionTree(const Arguments& arguments)
  : m_assembly_uid(arguments.m_assembly_uid)
{
    // Build the intermediate representation of the tree.
    IntermRegionTree interm_tree(arguments);

    // Copy tree bounding box.
    m_bbox = interm_tree.m_bbox;

    // Copy tree nodes.
    m_nodes = interm_tree.m_nodes;

    // Create final tree leaves.
    const size_t leaf_count = interm_tree.m_leaves.size();
    for (size_t i = 0; i < leaf_count; ++i)
    {
        // Fetch the intermediate representation of this leaf.
        const IntermRegionLeaf* interm_leaf = interm_tree.m_leaves[i];

        // Get a new unique ID for this triangle tree.
        const UniqueID triangle_tree_uid = new_guid();

        // Create the triangle tree factory.
        auto_ptr<ILazyFactory<TriangleTree> > triangle_tree_factory(
            new TriangleTreeFactory(
                TriangleTree::Arguments(
                    arguments.m_scene,
                    triangle_tree_uid,
                    interm_leaf->m_extent,
                    interm_leaf->m_assembly,
                    interm_leaf->m_regions)));

        // Create and store the triangle tree.
        m_triangle_trees.insert(
            make_pair(triangle_tree_uid, new Lazy<TriangleTree>(triangle_tree_factory)));

        // Create and store the leaf.
        m_leaves.push_back(new RegionLeaf(*this, triangle_tree_uid));
    }
}

RegionTree::~RegionTree()
{
    RENDERER_LOG_INFO(
        "deleting region tree for assembly #" FMT_UNIQUE_ID "...",
        m_assembly_uid);

    // Delete triangle trees.
    for (each<TriangleTreeContainer> i = m_triangle_trees; i; ++i)
        delete i->second;
}

void RegionTree::update_non_geometry(const bool enable_intersection_filters)
{
    for (each<TriangleTreeContainer> i = m_triangle_trees; i; ++i)
    {
        Access<TriangleTree> access(i->second);
        access->update_non_geometry(enable_intersection_filters);
    }
}


//
// RegionTreeFactory class implementation.
//

RegionTreeFactory::RegionTreeFactory(
    const RegionTree::Arguments& arguments)
  : m_arguments(arguments)
{
}

auto_ptr<RegionTree> RegionTreeFactory::create()
{
    return auto_ptr<RegionTree>(new RegionTree(m_arguments));
}


//
// RegionLeafVisitor class implementation.
//

double RegionLeafVisitor::visit(
    const RegionLeaf*               leaf,
    const ShadingRay&               ray,
    const ShadingRay::RayInfoType&  ray_info)
{
    assert(leaf);

    // Retrieve the triangle tree of this leaf.
    const TriangleTree* triangle_tree =
        m_triangle_tree_cache.access(
            leaf->get_triangle_tree_uid(),
            leaf->get_parent_tree().m_triangle_trees);

    if (triangle_tree)
    {
        // Check the intersection between the ray and the triangle tree.
        TriangleTreeIntersector intersector;
        TriangleLeafVisitor visitor(*triangle_tree, m_shading_point);
        if (triangle_tree->get_moving_triangle_count() > 0)
        {
            intersector.intersect_motion(
                *triangle_tree,
                ray,
                ray_info,
                ray.m_time.m_normalized,
                visitor
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
                , m_triangle_tree_stats
#endif
                );
        }
        else
        {
            intersector.intersect_no_motion(
                *triangle_tree,
                ray,
                ray_info,
                visitor
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
                , m_triangle_tree_stats
#endif
                );
        }
        visitor.read_hit_triangle_data();
    }

    // Return the distance to the closest intersection so far.
    return m_shading_point.m_ray.m_tmax;
}


//
// RegionLeafProbeVisitor class implementation.
//

double RegionLeafProbeVisitor::visit(
    const RegionLeaf*               leaf,
    const ShadingRay&               ray,
    const ShadingRay::RayInfoType&  ray_info)
{
    assert(leaf);

    // Retrieve the triangle tree of this leaf.
    const TriangleTree* triangle_tree =
        m_triangle_tree_cache.access(
            leaf->get_triangle_tree_uid(),
            leaf->get_parent_tree().m_triangle_trees);

    if (triangle_tree)
    {
        // Check the intersection between the ray and the triangle tree.
        TriangleTreeProbeIntersector intersector;
        TriangleLeafProbeVisitor visitor(*triangle_tree, ray.m_time.m_normalized, ray.m_flags);
        if (triangle_tree->get_moving_triangle_count() > 0)
        {
            intersector.intersect_motion(
                *triangle_tree,
                ray,
                ray_info,
                ray.m_time.m_normalized,
                visitor
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
                , m_triangle_tree_stats
#endif
                );
        }
        else
        {
            intersector.intersect_no_motion(
                *triangle_tree,
                ray,
                ray_info,
                visitor
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
                , m_triangle_tree_stats
#endif
                );
        }

        // Terminate traversal if there was a hit.
        if (visitor.hit())
        {
            m_hit = true;
            return ray.m_tmin;
        }
    }

    // Continue traversal.
    return ray.m_tmax;
}

}   // namespace renderer
