
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "assemblytree.h"

// appleseed.renderer headers.
#include "renderer/global/utility.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/geometry/iregion.h"
#include "renderer/modeling/geometry/object.h"
#include "renderer/modeling/geometry/regionkit.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"

// appleseed.foundation headers.
#include "foundation/math/intersection.h"
#include "foundation/math/permutation.h"
#include "foundation/math/sah.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <algorithm>
#include <utility>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// Assembly tree partitioner.
//

namespace
{

    class BboxSortPredicate
    {
      public:
        BboxSortPredicate(
            const vector<GAABB3>&   bboxes,
            const size_t            begin,
            const size_t            dim)
          : m_bboxes(bboxes)
          , m_begin(begin)
          , m_dim(dim)
        {
        }

        bool operator()(const size_t lhs, const size_t rhs) const
        {
            return
                  m_bboxes[m_begin + lhs].min[m_dim] + m_bboxes[m_begin + lhs].max[m_dim]
                < m_bboxes[m_begin + rhs].min[m_dim] + m_bboxes[m_begin + rhs].max[m_dim];
        }

      private:
        const vector<GAABB3>&   m_bboxes;
        const size_t            m_begin;
        const size_t            m_dim;
    };

    template <typename Vec>
    void ensure_size(Vec& v, const size_t size)
    {
        if (v.size() < size)
            v.resize(size);
    }

}   // anonymous namespace

class AssemblyTreePartitioner
  : public NonCopyable
{
  public:
    // Partition a set of items into two distinct sets.
    // Return end if the set is not to be partitioned.
    size_t partition(
        vector<UniqueID>&   items,
        vector<GAABB3>&     bboxes,
        const size_t        begin,
        const size_t        end,
        const GAABB3&       bbox)
    {
        const size_t count = end - begin;
        assert(count > 1);

        // Ensure that sufficient memory is allocated for the working arrays.
        ensure_size(m_indices, count);
        ensure_size(m_left_bboxes, count);
        ensure_size(m_temp_items, count);
        ensure_size(m_temp_bboxes, count);

        // Create the set of indices.
        for (size_t i = 0; i < count; ++i)
            m_indices[i] = i;

        GScalar best_split_cost = numeric_limits<GScalar>::max();
        size_t best_split_dim = 0;
        size_t best_split_pivot = 0;
        GAABB3 group_bbox;

        for (size_t dim = 0; dim < 3; ++dim)
        {
            // Sort the items according to their bounding boxes.
            BboxSortPredicate predicate(bboxes, begin, dim);
            sort(&m_indices[0], &m_indices[0] + count, predicate);

            // Left-to-right sweep to accumulate bounding boxes.
            group_bbox.invalidate();
            for (size_t i = 0; i < count; ++i)
            {
                group_bbox.insert(bboxes[begin + m_indices[i]]);
                m_left_bboxes[i] = group_bbox;
            }

            // Right-to-left sweep to accumulate bounding boxes and evaluate SAH.
            group_bbox.invalidate();
            for (size_t i = count - 1; i > 0; --i)
            {
                // Get left and right bounding boxes.
                const GAABB3& left_bbox = m_left_bboxes[i - 1];
                group_bbox.insert(bboxes[begin + m_indices[i]]);

                // Compute the cost of this partition.
                const GScalar left_cost = left_bbox.half_surface_area() * i;
                const GScalar right_cost = group_bbox.half_surface_area() * (count - i);
                const GScalar split_cost = left_cost + right_cost;

                // Keep track of the partition with the lowest cost.
                if (best_split_cost > split_cost)
                {
                    best_split_cost = split_cost;
                    best_split_dim = dim;
                    best_split_pivot = i;
                }
            }
        }

        // Don't split if the cost of the best partition is too high.
        const GScalar leaf_cost = bbox.half_surface_area() * count;
        if (best_split_cost >= leaf_cost)
            return end;

        // Sort the indices according to the item bounding boxes.
        BboxSortPredicate predicate(bboxes, begin, best_split_dim);
        sort(&m_indices[0], &m_indices[0] + count, predicate);

        // Reorder the items.
        small_item_reorder(&items[begin], &m_temp_items[0], &m_indices[0], count);
        small_item_reorder(&bboxes[begin], &m_temp_bboxes[0], &m_indices[0], count);

        return begin + best_split_pivot;
    }

  private:
    vector<size_t>      m_indices;
    vector<GAABB3>      m_left_bboxes;
    vector<UniqueID>    m_temp_items;
    vector<GAABB3>      m_temp_bboxes;
};


//
// Assembly tree builder.
//

typedef bvh::Builder<
        AssemblyTree,
        AssemblyTreePartitioner
    > AssemblyTreeBuilder;


//
// Assembly tree statistics.
//

typedef bvh::TreeStatistics<
        AssemblyTree,
        AssemblyTreeBuilder
    > AssemblyTreeStatistics;


//
// AssemblyTree class implementation.
//

// Constructor, builds the tree for a given scene.
AssemblyTree::AssemblyTree(const Scene& scene)
  : m_scene(scene)
{
    create_child_trees();
    build_assembly_tree();
}

// Destructor.
AssemblyTree::~AssemblyTree()
{
    // Log a progress message.
    RENDERER_LOG_INFO("deleting assembly bvh...");

    // Delete region trees.
    for (each<RegionTreeContainer> i = m_region_trees; i; ++i)
        delete i->second;
    m_region_trees.clear();
}

void AssemblyTree::create_child_trees()
{
    assert(m_region_trees.empty());

    vector<UniqueID> assemblies;
    collect_assemblies(assemblies);

    // Create one child tree (region tree or triangle tree) per assembly.
    for (const_each<vector<UniqueID> > i = assemblies; i; ++i)
    {
        const Assembly* assembly = m_scene.assemblies().get(*i);
        assert(assembly);

        if (assembly->is_flushable())
            create_region_tree(*assembly);
        else create_triangle_tree(*assembly);
    }
}

void AssemblyTree::collect_assemblies(vector<UniqueID>& assemblies) const
{
    for (const_each<AssemblyInstanceContainer> i = m_scene.assembly_instances(); i; ++i)
        assemblies.push_back(i->get_assembly_uid());

    sort(assemblies.begin(), assemblies.end());

    const vector<UniqueID>::const_iterator new_end =
        unique(assemblies.begin(), assemblies.end());

    assemblies.erase(new_end, assemblies.end());
}

void AssemblyTree::create_triangle_tree(const Assembly& assembly)
{
    // Compute the assembly space bounding box of the assembly.
    const GAABB3 assembly_bbox =
        compute_parent_bbox<GAABB3>(
            assembly.object_instances().begin(),
            assembly.object_instances().end());

    RegionInfoVector regions;
    collect_regions(assembly, regions);

    // Create the triangle tree factory.
    auto_ptr<ILazyFactory<TriangleTree> > triangle_tree_factory(
        new TriangleTreeFactory(
            TriangleTree::Arguments(
                assembly.get_uid(),
                assembly_bbox,
                assembly,
                regions)));

    // Create and store the triangle tree.
    m_triangle_trees.insert(
        make_pair(assembly.get_uid(), new Lazy<TriangleTree>(triangle_tree_factory)));
}

void AssemblyTree::collect_regions(const Assembly& assembly, RegionInfoVector& regions) const
{
    const ObjectInstanceContainer& object_instances = assembly.object_instances();

    // Collect all regions of all object instances of this assembly.
    for (size_t object_instance_index = 0;
         object_instance_index < object_instances.size();
         ++object_instance_index)
    {
        // Retrieve the object instance and its transformation.
        const ObjectInstance* object_instance =
            object_instances.get(object_instance_index);
        assert(object_instance);
        const Transformd& transform = object_instance->get_transform();

        // Retrieve the object.
        Object* object = assembly.objects().get(object_instance->get_object_index());
        assert(object);

        // Retrieve the region kit of the object.
        Access<RegionKit> region_kit(&object->get_region_kit());

        // Collect all regions of the object.
        for (size_t region_index = 0; region_index < region_kit->size(); ++region_index)
        {
            // Retrieve the region.
            const IRegion* region = (*region_kit)[region_index];

            // Compute the assembly space bounding box of the region.
            const GAABB3 region_bbox =
                transform.transform_to_parent(region->get_local_bbox());

            regions.push_back(
                RegionInfo(
                    object_instance_index,
                    region_index,
                    region_bbox));
        }
    }
}

void AssemblyTree::create_region_tree(const Assembly& assembly)
{
    // Create the region tree factory.
    auto_ptr<ILazyFactory<RegionTree> > region_tree_factory(
        new RegionTreeFactory(
            RegionTree::Arguments(
                assembly.get_uid(),
                assembly)));

    // Create and store the region tree.
    m_region_trees.insert(
        make_pair(assembly.get_uid(), new Lazy<RegionTree>(region_tree_factory)));
}

// Build the assembly tree.
void AssemblyTree::build_assembly_tree()
{
    // Insert all assembly instances of the scene into the tree.
    for (const_each<AssemblyInstanceContainer> i = m_scene.assembly_instances(); i; ++i)
    {
        // Retrieve the assembly instance.
        const AssemblyInstance& assembly_instance = *i;

        // Retrieve the assembly.
        const Assembly& assembly = assembly_instance.get_assembly();

        // Skip empty assemblies.
        if (assembly.object_instances().empty())
            continue;

        // Insert the assembly instance into the root leaf.
        insert(
            assembly_instance.get_uid(),
            assembly_instance.get_parent_bbox());
    }

    // Log a progress message.
    RENDERER_LOG_INFO(
        "building assembly bvh (%s %s)...",
        pretty_int(size()).c_str(),
        plural(size(), "assembly instance").c_str());

    // Build the assembly tree.
    AssemblyTreePartitioner partitioner;
    AssemblyTreeBuilder builder;
    builder.build(*this, partitioner);

    // Collect and print triangle tree statistics.
    AssemblyTreeStatistics tree_stats(*this, builder);
    RENDERER_LOG_DEBUG("assembly bvh statistics:");
    tree_stats.print(global_logger());
}


//
// AssemblyLeafVisitorBase class implementation.
//

// Transform a ray to the space of an assembly instance.
void AssemblyLeafVisitorBase::transform_ray_to_assembly_instance_space(
    const AssemblyInstance*             assembly_instance,
    const ShadingPoint*                 parent_shading_point,
    const ShadingRay::RayType&          input_ray,
    ShadingRay::RayType&                output_ray)
{
    assert(assembly_instance);

    // Retrieve the transformation of the assembly instance.
    const Transformd& transform = assembly_instance->get_transform();

    if (parent_shading_point &&
        parent_shading_point->m_assembly_instance == assembly_instance)
    {
        // The caller provided the previous intersection, and we are about
        // to intersect the assembly instance that contains the previous
        // intersection. Use the properly offset intersection point as the
        // origin of the child ray.
        output_ray.m_dir = transform.transform_vector_to_local(input_ray.m_dir);
        output_ray.m_org =
            dot(parent_shading_point->m_asm_geo_normal, output_ray.m_dir) > 0.0
                ? parent_shading_point->m_front_point
                : parent_shading_point->m_back_point;
    }
    else
    {
        // The caller didn't provide the previous intersection, or we are
        // about to intersect an assembly instance that does not contain
        // the previous intersection: proceed normally.
        output_ray.m_org = transform.transform_point_to_local(input_ray.m_org);
        output_ray.m_dir = transform.transform_vector_to_local(input_ray.m_dir);
    }
}


//
// AssemblyLeafVisitor class implementation.
//

// Visit a leaf.
bool AssemblyLeafVisitor::visit(
    const vector<UniqueID>&             items,
    const vector<GAABB3>&               bboxes,
    const size_t                        begin,
    const size_t                        end,
    const ShadingRay::RayType&          ray,
    const ShadingRay::RayInfoType&      /*ray_info*/,
    const double                        tmin,
    const double                        tmax,
    double&                             distance)
{
    assert(begin + 1 == end);

    // Retrieve the assembly instance.
    const AssemblyInstance* assembly_instance =
        m_tree.m_scene.assembly_instances().get(items[begin]);
    assert(assembly_instance);

    ShadingPoint result;
    result.m_ray.m_tmin = tmin;
    result.m_ray.m_tmax = tmax;
    result.m_ray.m_time = m_shading_point.m_ray.m_time;
    result.m_ray.m_flags = m_shading_point.m_ray.m_flags;

    // Transform the ray to assembly instance space.
    transform_ray_to_assembly_instance_space(
        assembly_instance,
        m_parent_shading_point,
        ray,
        result.m_ray);

    const RayInfo3d ray_info(result.m_ray);

    if (assembly_instance->get_assembly().is_flushable())
    {
        // Retrieve the region tree of this assembly.
        const RegionTree& region_tree =
            *m_region_tree_cache.access(
                assembly_instance->get_assembly_uid(),
                m_tree.m_region_trees);

        // Check the intersection between the ray and the region tree.
        RegionLeafVisitor visitor(
            result,
            m_triangle_tree_cache
#ifdef FOUNDATION_BSP_ENABLE_TRAVERSAL_STATS
            , m_triangle_bsp_stats
#endif
            );
        RegionLeafIntersector intersector;
#ifdef FOUNDATION_BSP_ENABLE_TRAVERSAL_STATS
        bsp::TraversalStatistics stats;
        intersector.intersect(
            region_tree,
            result.m_ray,
            ray_info,
            visitor,
            stats);
#else
        intersector.intersect(
            region_tree,
            result.m_ray,
            ray_info,
            visitor);
#endif
    }
    else
    {
        // Retrieve the triangle tree of this assembly.
        const TriangleTree* triangle_tree =
            m_triangle_tree_cache.access(
                assembly_instance->get_assembly_uid(),
                m_tree.m_triangle_trees);

        if (triangle_tree)
        {
            // Check the intersection between the ray and the triangle tree.
            TriangleLeafVisitor visitor(result);
            TriangleLeafIntersector intersector;
            intersector.intersect(
                *triangle_tree,
                result.m_ray,
                ray_info,
                visitor
#ifdef FOUNDATION_BSP_ENABLE_TRAVERSAL_STATS
                , m_triangle_bsp_stats
#endif
                );
            visitor.read_hit_triangle_data();
        }
    }

    // Keep track of the closest hit.
    if (result.m_hit)
    {
        assert(result.m_ray.m_tmax < m_shading_point.m_ray.m_tmax);
        m_shading_point.m_ray.m_tmax = result.m_ray.m_tmax;
        m_shading_point.m_hit = true;
        m_shading_point.m_bary = result.m_bary;
        m_shading_point.m_asm_instance_uid = assembly_instance->get_uid();
        m_shading_point.m_object_instance_index = result.m_object_instance_index;
        m_shading_point.m_region_index = result.m_region_index;
        m_shading_point.m_triangle_index = result.m_triangle_index;
        m_shading_point.m_triangle_support_plane = result.m_triangle_support_plane;
    }

    // Continue traversal.
    distance = m_shading_point.m_ray.m_tmax;
    return true;
}


//
// AssemblyLeafProbeVisitor class implementation.
//

// Visit a leaf.
bool AssemblyLeafProbeVisitor::visit(
    const vector<UniqueID>&             items,
    const vector<GAABB3>&               bboxes,
    const size_t                        begin,
    const size_t                        end,
    const ShadingRay::RayType&          ray,
    const ShadingRay::RayInfoType&      /*ray_info*/,
    const double                        tmin,
    const double                        tmax,
    double&                             distance)
{
    assert(begin + 1 == end);

    // Retrieve the assembly instance.
    const AssemblyInstance* assembly_instance =
        m_tree.m_scene.assembly_instances().get(items[begin]);
    assert(assembly_instance);

    ShadingRay::RayType local_ray;
    local_ray.m_tmin = tmin;
    local_ray.m_tmax = tmax;

    // Transform the ray to assembly instance space.
    transform_ray_to_assembly_instance_space(
        assembly_instance,
        m_parent_shading_point,
        ray,
        local_ray);

    const RayInfo3d local_ray_info(local_ray);

    if (assembly_instance->get_assembly().is_flushable())
    {
        // Retrieve the region tree of this assembly.
        const RegionTree& region_tree =
            *m_region_tree_cache.access(
                assembly_instance->get_assembly_uid(),
                m_tree.m_region_trees);

        // Check the intersection between the ray and the region tree.
        RegionLeafProbeVisitor visitor(
            m_triangle_tree_cache
#ifdef FOUNDATION_BSP_ENABLE_TRAVERSAL_STATS
            , m_triangle_bsp_stats
#endif
            );
        RegionLeafProbeIntersector intersector;
#ifdef FOUNDATION_BSP_ENABLE_TRAVERSAL_STATS
        bsp::TraversalStatistics stats;
        intersector.intersect(
            region_tree,
            local_ray,
            local_ray_info,
            visitor,
            stats);
#else
        intersector.intersect(
            region_tree,
            local_ray,
            local_ray_info,
            visitor);
#endif
        
        // Terminate traversal if there was a hit.
        if (visitor.hit())
        {
            m_hit = true;
            return false;
        }
    }
    else
    {
        // Retrieve the triangle tree of this leaf.
        const TriangleTree* triangle_tree =
            m_triangle_tree_cache.access(
                assembly_instance->get_assembly_uid(),
                m_tree.m_triangle_trees);

        if (triangle_tree)
        {
            // Check the intersection between the ray and the triangle tree.
            TriangleLeafProbeVisitor visitor;
            TriangleLeafProbeIntersector intersector;
            intersector.intersect(
                *triangle_tree,
                local_ray,
                local_ray_info,
                visitor
#ifdef FOUNDATION_BSP_ENABLE_TRAVERSAL_STATS
                , m_triangle_bsp_stats
#endif
                );

            // Terminate traversal if there was a hit.
            if (visitor.hit())
            {
                m_hit = true;
                return false;
            }
        }
    }

    // Continue traversal.
    distance = ray.m_tmax;
    return true;
}

}   // namespace renderer
