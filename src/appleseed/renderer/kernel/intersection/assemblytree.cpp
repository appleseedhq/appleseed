
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/object/iregion.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/object/regionkit.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/utility/bbox.h"

// appleseed.foundation headers.
#include "foundation/math/intersection.h"
#include "foundation/math/permutation.h"
#include "foundation/platform/system.h"
#include "foundation/platform/timer.h"
#include "foundation/utility/statistics.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <algorithm>
#include <utility>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// AssemblyTree class implementation.
//

AssemblyTree::AssemblyTree(Scene& scene)
  : TreeType(AlignedAllocator<void>(System::get_l1_data_cache_line_size()))
  , m_scene(scene)
{
    update();
}

AssemblyTree::~AssemblyTree()
{
    // Log a progress message.
    RENDERER_LOG_INFO("deleting assembly tree...");

    // Delete region trees.
    for (each<RegionTreeContainer> i = m_region_trees; i; ++i)
        delete i->second;
    m_region_trees.clear();

    // Delete triangle trees.
    for (each<TriangleTreeContainer> i = m_triangle_trees; i; ++i)
        delete i->second;
    m_triangle_trees.clear();

    // Delete curve trees.
    for (each<CurveTreeContainer> i = m_curve_trees; i; ++i)
        delete i->second;
    m_curve_trees.clear();
}

void AssemblyTree::update()
{
    rebuild_assembly_tree();
    update_child_trees();
}

size_t AssemblyTree::get_memory_size() const
{
    return
          TreeType::get_memory_size()
        - sizeof(*static_cast<const TreeType*>(this))
        + sizeof(*this)
        + m_items.capacity() * sizeof(AssemblyInstance*)
        + m_assembly_versions.size() * sizeof(pair<UniqueID, VersionID>);
}

void AssemblyTree::compute_cumulated_transforms(
    AssemblyInstanceContainer&              assembly_instances,
    const TransformSequence&                parent_transform_seq)
{
    for (each<AssemblyInstanceContainer> i = assembly_instances; i; ++i)
    {
        // Retrieve the assembly instance.
        AssemblyInstance& assembly_instance = *i;

        // Retrieve the assembly.
        const Assembly& assembly = assembly_instance.get_assembly();

        // Compute the cumulated transform sequence of this assembly instance.
        assembly_instance.cumulated_transform_sequence() = 
            assembly_instance.transform_sequence() * parent_transform_seq;
        assembly_instance.cumulated_transform_sequence().prepare();

        // Recurse into child assembly instances.
        compute_cumulated_transforms(
            assembly.assembly_instances(),
            assembly_instance.cumulated_transform_sequence());
    }
}

void AssemblyTree::collect_assembly_instances(
    const AssemblyInstanceContainer&    assembly_instances,
    AABBVector&                         assembly_instance_bboxes)
{
    for (const_each<AssemblyInstanceContainer> i = assembly_instances; i; ++i)
    {
        // Retrieve the assembly instance.
        const AssemblyInstance& assembly_instance = *i;

        // Retrieve the assembly.
        const Assembly& assembly = assembly_instance.get_assembly();

        // Recurse into child assembly instances.
        collect_assembly_instances(
            assembly.assembly_instances(),
            assembly_instance_bboxes);

        // Skip empty assemblies.
        if (assembly.object_instances().empty())
            continue;

        // Create and store an item for this assembly instance.
        m_items.push_back(
            Item(
                &assembly,
                &assembly_instance,
                assembly_instance.cumulated_transform_sequence()));

        // Compute and store the assembly instance bounding box.
        AABB3d assembly_instance_bbox(
            assembly_instance.cumulated_transform_sequence().to_parent(
                assembly.compute_non_hierarchical_local_bbox()));
        assembly_instance_bbox.robust_grow(1.0e-15);
        assembly_instance_bboxes.push_back(assembly_instance_bbox);
    }
}

void AssemblyTree::rebuild_assembly_tree()
{
    // Clear the current tree.
    clear();
    m_items.clear();

    Statistics statistics;
    
    compute_cumulated_transforms(
        m_scene.assembly_instances(),
        TransformSequence());

    // Collect all assembly instances of the scene.
    AABBVector assembly_instance_bboxes;
    collect_assembly_instances(
        m_scene.assembly_instances(),
        assembly_instance_bboxes);

    RENDERER_LOG_INFO(
        "building assembly tree (%s %s)...",
        pretty_int(m_items.size()).c_str(),
        plural(m_items.size(), "assembly instance").c_str());

    // Create the partitioner.
    typedef bvh::SAHPartitioner<AABBVector> Partitioner;
    Partitioner partitioner(
        assembly_instance_bboxes,
        AssemblyTreeMaxLeafSize,
        AssemblyTreeInteriorNodeTraversalCost,
        AssemblyTreeTriangleIntersectionCost);

    // Build the assembly tree.
    typedef bvh::Builder<AssemblyTree, Partitioner> Builder;
    Builder builder;
    builder.build<DefaultWallclockTimer>(*this, partitioner, m_items.size(), AssemblyTreeMaxLeafSize);
    statistics.insert_time("build time", builder.get_build_time());
    statistics.merge(bvh::TreeStatistics<AssemblyTree>(*this, AABB3d(m_scene.compute_bbox())));

    if (!m_items.empty())
    {
        const vector<size_t>& ordering = partitioner.get_item_ordering();
        assert(m_items.size() == ordering.size());

        // Reorder the items according to the tree ordering.
        ItemVector temp_assembly_instances(ordering.size());
        small_item_reorder(
            &m_items[0],
            &temp_assembly_instances[0],
            &ordering[0],
            ordering.size());

        // Store the items in the tree leaves whenever possible.
        store_items_in_leaves(statistics);
    }

    // Print assembly tree statistics.
    RENDERER_LOG_DEBUG("%s",
        StatisticsVector::make(
            "assembly tree statistics",
            statistics).to_string().c_str());
}

void AssemblyTree::store_items_in_leaves(Statistics& statistics)
{
    size_t leaf_count = 0;
    size_t fat_leaf_count = 0;

    const size_t node_count = m_nodes.size();

    for (size_t i = 0; i < node_count; ++i)
    {
        NodeType& node = m_nodes[i];

        if (node.is_leaf())
        {
            ++leaf_count;

            const size_t item_count = node.get_item_count();

            if (item_count <= NodeType::MaxUserDataSize / sizeof(Item))
            {
                ++fat_leaf_count;

                const size_t item_begin = node.get_item_index();
                Item* user_data = &node.get_user_data<Item>();

                for (size_t j = 0; j < item_count; ++j)
                    user_data[j] = m_items[item_begin + j];
            }
        }
    }

    statistics.insert_percent("fat leaves", fat_leaf_count, leaf_count);
}

void AssemblyTree::collect_unique_assemblies(AssemblyVector& assemblies) const
{
    assert(assemblies.empty());

    assemblies.reserve(m_items.size());

    for (const_each<ItemVector> i = m_items; i; ++i)
        assemblies.push_back(i->m_assembly);

    sort(assemblies.begin(), assemblies.end());

    assemblies.erase(
        unique(assemblies.begin(), assemblies.end()),
        assemblies.end());
}

namespace
{
    void collect_regions(const Assembly& assembly, RegionInfoVector& regions)
    {
        assert(regions.empty());

        const ObjectInstanceContainer& object_instances = assembly.object_instances();
        const size_t object_instance_count = object_instances.size();

        // Collect all regions of all object instances of this assembly.
        for (size_t obj_inst_index = 0; obj_inst_index < object_instance_count; ++obj_inst_index)
        {
            // Retrieve the object instance and its transformation.
            const ObjectInstance* object_instance = object_instances.get_by_index(obj_inst_index);
            assert(object_instance);
            const Transformd& transform = object_instance->get_transform();

            // Retrieve the object.
            Object& object = object_instance->get_object();

            // Retrieve the region kit of the object.
            Access<RegionKit> region_kit(&object.get_region_kit());

            // Collect all regions of the object.
            for (size_t region_index = 0; region_index < region_kit->size(); ++region_index)
            {
                // Retrieve the region.
                const IRegion* region = (*region_kit)[region_index];

                // Compute the assembly space bounding box of the region.
                const GAABB3 region_bbox =
                    transform.to_parent(region->compute_local_bbox());

                regions.push_back(
                    RegionInfo(
                        obj_inst_index,
                        region_index,
                        region_bbox));
            }
        }
    }

    Lazy<TriangleTree>* create_triangle_tree(const Scene& scene, const Assembly& assembly)
    {
        // Compute the assembly space bounding box of the assembly.
        const GAABB3 assembly_bbox =
            compute_parent_bbox<GAABB3>(
                assembly.object_instances().begin(),
                assembly.object_instances().end());

        RegionInfoVector regions;
        collect_regions(assembly, regions);

        auto_ptr<ILazyFactory<TriangleTree> > triangle_tree_factory(
            new TriangleTreeFactory(
                TriangleTree::Arguments(
                    scene,
                    assembly.get_uid(),
                    assembly_bbox,
                    assembly,
                    regions)));

        return new Lazy<TriangleTree>(triangle_tree_factory);
    }

    Lazy<RegionTree>* create_region_tree(const Scene& scene, const Assembly& assembly)
    {
        auto_ptr<ILazyFactory<RegionTree> > region_tree_factory(
            new RegionTreeFactory(
                RegionTree::Arguments(
                    scene,
                    assembly.get_uid(),
                    assembly)));

        return new Lazy<RegionTree>(region_tree_factory);
    }

    Lazy<CurveTree>* create_curve_tree(const Scene& scene, const Assembly& assembly)
    {
        // Compute the assembly space bounding box of the assembly.
        const GAABB3 assembly_bbox =
            compute_parent_bbox<GAABB3>(
                assembly.object_instances().begin(),
                assembly.object_instances().end());

        RegionInfoVector regions;
        collect_regions(assembly, regions);

        auto_ptr<ILazyFactory<CurveTree> > curve_tree_factory(
            new CurveTreeFactory(
                CurveTree::Arguments(
                    scene,
                    assembly.get_uid(),
                    assembly_bbox,
                    assembly,
                    regions)));

        return new Lazy<CurveTree>(curve_tree_factory);
    }
}

void AssemblyTree::update_child_trees()
{
    // Collect all assemblies in the scene.
    AssemblyVector assemblies;
    collect_unique_assemblies(assemblies);

    // Create or rebuild the child tree of each assembly.
    for (const_each<AssemblyVector> i = assemblies; i; ++i)
    {
        // Retrieve the assembly.
        const Assembly& assembly = **i;
        const UniqueID assembly_uid = assembly.get_uid();

        // Retrieve the current version ID of the assembly.
        const VersionID current_version_id = assembly.get_version_id();

        // Retrieve the stored version ID of the assembly.
        const AssemblyVersionMap::const_iterator stored_version_it =
            m_assembly_versions.find(assembly_uid);

        if (stored_version_it != m_assembly_versions.end())
        {
            if (stored_version_it->second == current_version_id)
            {
                // The child tree of this assembly is up-to-date wrt. the assembly's geometry.
                // Simply update the child tree.
                if (assembly.is_flushable())
                {
                    Update<RegionTree> access(m_region_trees.find(assembly_uid)->second);
                    if (access.get())
                        access->update_non_geometry();
                }
                else
                {
                    Update<TriangleTree> access(m_triangle_trees.find(assembly_uid)->second);
                    if (access.get())
                        access->update_non_geometry();
                }

                continue;
            }
            else
            {
                // The child tree is out-of-date wrt. the assembly's geometry: delete it.
                // It will get rebuilt from scratch lazily.
                if (assembly.is_flushable())
                {
                    const RegionTreeContainer::iterator it = m_region_trees.find(assembly_uid);
                    delete it->second;
                    m_region_trees.erase(it);
                }
                else
                {
                    const TriangleTreeContainer::iterator it = m_triangle_trees.find(assembly_uid);
                    delete it->second;
                    m_triangle_trees.erase(it);

                    const CurveTreeContainer::iterator cit = m_curve_trees.find(assembly_uid);
                    delete cit->second;
                    m_curve_trees.erase(cit);
                }
            }
        }

        // The assembly does not contain any geometry, nothing to do.
        if (assembly.object_instances().empty())
            continue;

        // The assembly does contains geometry, lazily build a new child tree.
        if (assembly.is_flushable())
        {
            m_region_trees.insert(
                make_pair(assembly_uid, create_region_tree(m_scene, assembly)));
        }
        else
        {
            m_triangle_trees.insert(
                make_pair(assembly_uid, create_triangle_tree(m_scene, assembly)));

            m_curve_trees.insert(
                make_pair(assembly_uid, create_curve_tree(m_scene, assembly)));
        }

        // Store the current version ID of the assembly.
        m_assembly_versions[assembly_uid] = current_version_id;
    }
}


//
// Utility function to transform a ray to the space of an assembly instance.
//

namespace
{
    void compute_assembly_instance_ray(
        const AssemblyInstance&     assembly_instance,
        const Transformd&           assembly_instance_transform,
        const ShadingPoint*         parent_sp,
        const ShadingRay&           input_ray,
        ShadingRay&                 output_ray)
    {
        // Transform the ray direction to assembly instance space.
        output_ray.m_dir = assembly_instance_transform.vector_to_local(input_ray.m_dir);

        // Compute the ray origin in assembly instance space.
        if (parent_sp &&
            parent_sp->get_assembly_instance().get_uid() == assembly_instance.get_uid() &&
            parent_sp->get_object_instance().get_ray_bias_method() == ObjectInstance::RayBiasMethodNone)
        {
            // The caller provided the previous intersection, and we are about
            // to intersect the assembly instance that contains the previous
            // intersection. Use the properly offset intersection point as the
            // origin of the child ray.
            output_ray.m_org = parent_sp->get_offset_point(output_ray.m_dir);
        }
        else
        {
            // The caller didn't provide the previous intersection, or we are
            // about to intersect an assembly instance that does not contain
            // the previous intersection: simply transform the ray origin to
            // assembly instance space.
            output_ray.m_org = assembly_instance_transform.point_to_local(input_ray.m_org);
        }

        // Copy the remaining members.
        output_ray.m_tmin = input_ray.m_tmin;
        output_ray.m_tmax = input_ray.m_tmax;
        output_ray.m_time = input_ray.m_time;
        output_ray.m_type = input_ray.m_type;
        output_ray.m_depth = input_ray.m_depth;
    }
}


//
// AssemblyLeafVisitor class implementation.
//

bool AssemblyLeafVisitor::visit(
    const AssemblyTree::NodeType&       node,
    const ShadingRay&                   ray,
    const ShadingRay::RayInfoType&      ray_info,
    double&                             distance
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    , bvh::TraversalStatistics&         stats
#endif
    )
{
    // Retrieve the assembly instances for this leaf.
    const size_t assembly_instance_index = node.get_item_index();
    const size_t assembly_instance_count = node.get_item_count();
    const AssemblyTree::Item* items =
        assembly_instance_count <= AssemblyTree::NodeType::MaxUserDataSize / sizeof(AssemblyTree::Item)
            ? &node.get_user_data<AssemblyTree::Item>()     // items are stored in the leaf node
            : &m_tree.m_items[assembly_instance_index];     // items are stored in the tree

    for (size_t i = 0; i < assembly_instance_count; ++i)
    {
        // Retrieve the assembly instance.
        const AssemblyTree::Item& item = items[i];

        // Evaluate the transformation of the assembly instance.
        Transformd tmp;
        const Transformd& assembly_instance_transform =
            item.m_transform_sequence.evaluate(ray.m_time, tmp);

        // Transform the ray to assembly instance space.
        ShadingPoint local_triangle_shading_point(ShadingPoint::PrimitiveTriangle);
        ShadingPoint local_curve_shading_point(ShadingPoint::PrimitiveCurve);

        compute_assembly_instance_ray(
            *item.m_assembly_instance,
            assembly_instance_transform,
            m_parent_shading_point,
            ray,
            local_triangle_shading_point.m_ray);

        const RayInfo3d local_ray_info(local_triangle_shading_point.m_ray);

        ShadingPoint& local_shading_point = local_triangle_shading_point;

        FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_intersected_items.insert(1));

        if (item.m_assembly->is_flushable())
        {
            // Retrieve the region tree of this assembly.
            const RegionTree& region_tree =
                *m_region_tree_cache.access(
                    item.m_assembly_uid,
                    m_tree.m_region_trees);

            // Check the intersection between the ray and the region tree.
            RegionLeafVisitor visitor(
                local_triangle_shading_point,
                m_triangle_tree_cache
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
                , m_triangle_tree_stats
#endif
                );
            RegionLeafIntersector intersector;
            intersector.intersect(
                region_tree,
                local_triangle_shading_point.m_ray,
                local_ray_info,
                visitor);
        }
        else
        {
            // Retrieve the triangle tree of this assembly.
            const TriangleTree* triangle_tree =
                m_triangle_tree_cache.access(
                    item.m_assembly_uid,
                    m_tree.m_triangle_trees);

            const CurveTree* curve_tree = m_curve_tree_cache.access(
                    item.m_assembly_uid,
                    m_tree.m_curve_trees);

            if (triangle_tree)
            {
                // Check the intersection between the ray and the triangle tree.
                TriangleTreeIntersector intersector;
                TriangleLeafVisitor visitor(*triangle_tree, local_triangle_shading_point);
                if (triangle_tree->get_moving_triangle_count() > 0)
                {
                    intersector.intersect_motion(
                        *triangle_tree,
                        local_triangle_shading_point.m_ray,
                        local_ray_info,
                        local_triangle_shading_point.m_ray.m_time,
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
                        local_triangle_shading_point.m_ray,
                        local_ray_info,
                        visitor
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
                        , m_triangle_tree_stats
#endif
                        );
                }
                visitor.read_hit_triangle_data();
            }

            // Check for intersection with curves.

            compute_assembly_instance_ray(
                *item.m_assembly_instance,
                assembly_instance_transform,
                m_parent_shading_point,
                ray,
                local_curve_shading_point.m_ray);
            const RayInfo3d local_ray_info(local_curve_shading_point.m_ray);

            if (curve_tree)
            {
                CurveTreeIntersector intersector;
                const Matrix4d xfm_matrix = BezierCurveIntersector<BezierCurve3d>::compute_curve_transform(ray);
                CurveLeafVisitor visitor(*curve_tree, xfm_matrix, local_curve_shading_point);

                intersector.intersect_no_motion(
                    *curve_tree,
                    local_curve_shading_point.m_ray,
                    local_ray_info,
                    visitor
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
                    , m_curve_tree_stats
#endif
                    );
            }
        }

        // Update local shading point to point to the curve shading point if its nearer than the nearest triangle.
        if (local_curve_shading_point.m_hit && local_curve_shading_point.m_ray.m_tmax < local_triangle_shading_point.m_ray.m_tmax)
        {
            local_shading_point = local_curve_shading_point;
        }

        // Keep track of the closest hit.
        if (local_shading_point.m_hit && local_shading_point.m_ray.m_tmax < m_shading_point.m_ray.m_tmax)
        {
            m_shading_point.m_ray.m_tmax = local_shading_point.m_ray.m_tmax;
            m_shading_point.m_hit = true;
            m_shading_point.m_bary = local_shading_point.m_bary;
            m_shading_point.m_assembly_instance = item.m_assembly_instance;
            m_shading_point.m_assembly_instance_transform = assembly_instance_transform;
            m_shading_point.m_object_instance_index = local_shading_point.m_object_instance_index;
            m_shading_point.m_region_index = local_shading_point.m_region_index;
            m_shading_point.m_primitive_index = local_shading_point.m_primitive_index;
            m_shading_point.m_primitive_type = local_shading_point.m_primitive_type;
            m_shading_point.m_triangle_support_plane = local_shading_point.m_triangle_support_plane;
        }
    }

    // Continue traversal.
    distance = m_shading_point.m_ray.m_tmax;
    return true;
}


//
// AssemblyLeafProbeVisitor class implementation.
//

bool AssemblyLeafProbeVisitor::visit(
    const AssemblyTree::NodeType&       node,
    const ShadingRay&                   ray,
    const ShadingRay::RayInfoType&      ray_info,
    double&                             distance
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    , bvh::TraversalStatistics&         stats
#endif
    )
{
    // Retrieve the assembly instances for this leaf.
    const size_t assembly_instance_count = node.get_item_count();
    const AssemblyTree::Item* items =
        assembly_instance_count <= AssemblyTree::NodeType::MaxUserDataSize / sizeof(AssemblyTree::Item)
            ? &node.get_user_data<AssemblyTree::Item>()     // items are stored in the leaf node
            : &m_tree.m_items[node.get_item_index()];       // items are stored in the tree

    for (size_t i = 0; i < assembly_instance_count; ++i)
    {
        // Retrieve the assembly instance.
        const AssemblyTree::Item& item = items[i];

        // Evaluate the transformation of the assembly instance.
        Transformd tmp;
        const Transformd& assembly_instance_transform =
            item.m_transform_sequence.evaluate(ray.m_time, tmp);

        // Transform the ray to assembly instance space.
        ShadingRay local_ray;
        compute_assembly_instance_ray(
            *item.m_assembly_instance,
            assembly_instance_transform,
            m_parent_shading_point,
            ray,
            local_ray);
        const RayInfo3d local_ray_info(local_ray);

        FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_intersected_items.insert(1));

        if (item.m_assembly->is_flushable())
        {
            // Retrieve the region tree of this assembly.
            const RegionTree& region_tree =
                *m_region_tree_cache.access(
                    item.m_assembly_uid,
                    m_tree.m_region_trees);

            // Check the intersection between the ray and the region tree.
            RegionLeafProbeVisitor visitor(
                m_triangle_tree_cache
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
                , m_triangle_tree_stats
#endif
                );
            RegionLeafProbeIntersector intersector;
            intersector.intersect(
                region_tree,
                local_ray,
                local_ray_info,
                visitor);
        
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
                    item.m_assembly_uid,
                    m_tree.m_triangle_trees);

            if (triangle_tree)
            {
                // Check the intersection between the ray and the triangle tree.
                TriangleTreeProbeIntersector intersector;
                TriangleLeafProbeVisitor visitor(*triangle_tree);
                if (triangle_tree->get_moving_triangle_count() > 0)
                {
                    intersector.intersect_motion(
                        *triangle_tree,
                        local_ray,
                        local_ray_info,
                        local_ray.m_time,
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
                        local_ray,
                        local_ray_info,
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
                    return false;
                }
            }

            // Retrieve the curve tree for this leaf.
            const CurveTree* curve_tree =
                m_curve_tree_cache.access(
                    item.m_assembly_uid,
                    m_tree.m_curve_trees);

            if (curve_tree)
            {
                // Check intersection between ray and curve tree.
                CurveTreeProbeIntersector intersector;
                const Matrix4d xfm_matrix = BezierCurveIntersector<BezierCurve3d>::compute_curve_transform(ray);
                CurveLeafProbeVisitor visitor(*curve_tree, xfm_matrix);

                intersector.intersect_no_motion(
                    *curve_tree,
                    local_ray,
                    local_ray_info,
                    visitor
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
                    , m_curve_tree_stats
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
    }

    // Continue traversal.
    distance = ray.m_tmax;
    return true;
}

}   // namespace renderer
