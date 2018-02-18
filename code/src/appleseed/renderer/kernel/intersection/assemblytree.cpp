
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
#include "assemblytree.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/kernel/intersection/regioninfo.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/entity/entityvector.h"
#include "renderer/modeling/object/curveobject.h"
#include "renderer/modeling/object/iregion.h"
#include "renderer/modeling/object/meshobject.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/object/proceduralobject.h"
#include "renderer/modeling/object/regionkit.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/utility/bbox.h"

// appleseed.foundation headers.
#include "foundation/math/beziercurve.h"
#include "foundation/math/permutation.h"
#include "foundation/math/ray.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/system.h"
#include "foundation/platform/timers.h"
#include "foundation/platform/types.h"
#include "foundation/utility/alignedallocator.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/lazy.h"
#include "foundation/utility/siphash.h"
#include "foundation/utility/statistics.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstring>
#include <set>
#include <utility>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// AssemblyTree class implementation.
//

AssemblyTree::AssemblyTree(const Scene& scene)
  : TreeType(AlignedAllocator<void>(System::get_l1_data_cache_line_size()))
  , m_scene(scene)
{
    update();
}

AssemblyTree::~AssemblyTree()
{
    RENDERER_LOG_INFO("deleting assembly tree...");
}

void AssemblyTree::update()
{
    rebuild_assembly_tree();
    update_tree_hierarchy();
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

void AssemblyTree::collect_assembly_instances(
    const AssemblyInstanceContainer&    assembly_instances,
    const TransformSequence&            parent_transform_seq,
    AABBVector&                         assembly_instance_bboxes)
{
    for (const_each<AssemblyInstanceContainer> i = assembly_instances; i; ++i)
    {
        // Retrieve the assembly instance.
        const AssemblyInstance& assembly_instance = *i;

        // Retrieve the assembly.
        const Assembly& assembly = assembly_instance.get_assembly();

        // Compute the cumulated transform sequence of this assembly instance.
        TransformSequence cumulated_transform_seq =
            assembly_instance.transform_sequence() * parent_transform_seq;
        cumulated_transform_seq.prepare();

        // Recurse into child assembly instances.
        collect_assembly_instances(
            assembly.assembly_instances(),
            cumulated_transform_seq,
            assembly_instance_bboxes);

        // Skip empty assemblies.
        if (assembly.object_instances().empty())
            continue;

        // Create and store an item for this assembly instance.
        m_items.emplace_back(
            &assembly,
            &assembly_instance,
            cumulated_transform_seq);

        // Compute and store the assembly instance bounding box.
        AABB3d assembly_instance_bbox(
            cumulated_transform_seq.to_parent(
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

    // Collect assembly instances and their bounding boxes.
    RENDERER_LOG_INFO("collecting assembly instances...");
    AABBVector assembly_instance_bboxes;
    collect_assembly_instances(
        m_scene.assembly_instances(),
        TransformSequence(),
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

void AssemblyTree::update_tree_hierarchy()
{
    // Collect all assemblies in the scene.
    AssemblyVector assemblies;
    collect_unique_assemblies(assemblies);

    // Delete child trees of assemblies that no longer exist.
    delete_unused_child_trees(assemblies);

    // Create or rebuild the child trees of each assembly.
    for (const_each<AssemblyVector> i = assemblies; i; ++i)
    {
        // Retrieve the assembly.
        const Assembly& assembly = **i;

        // Retrieve the current version ID of the assembly.
        const VersionID current_version_id = assembly.get_version_id();

        // Retrieve the stored version ID of the assembly.
        const AssemblyVersionMap::const_iterator stored_version_it =
            m_assembly_versions.find(assembly.get_uid());

        if (stored_version_it != m_assembly_versions.end())
        {
            if (stored_version_it->second == current_version_id)
            {
                // The child trees of this assembly are up-to-date.
                continue;
            }

            // The child trees of this assembly are out-of-date: delete them.
            delete_child_trees(assembly.get_uid());
        }

        // Lazily build new child trees.
        create_child_trees(assembly);

        // Store the current version ID of the assembly.
        m_assembly_versions[assembly.get_uid()] = current_version_id;
    }

    // Update child trees.
    update_region_trees();
    update_triangle_trees();
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

void AssemblyTree::delete_unused_child_trees(const AssemblyVector& assemblies)
{
    set<UniqueID> assembly_uids;

    for (const_each<AssemblyVector> i = assemblies; i; ++i)
        assembly_uids.insert((*i)->get_uid());

    for (AssemblyVersionMap::iterator
            i = m_assembly_versions.begin(),
            e = m_assembly_versions.end();
            i != e; )
    {
        if (assembly_uids.find(i->first) == assembly_uids.end())
        {
            delete_child_trees(i->first);
            m_assembly_versions.erase(i++);
        }
        else ++i;
    }
}

namespace
{
    bool has_object_instances_of_type(const Assembly& assembly, const char* model)
    {
        for (const_each<ObjectInstanceContainer> i = assembly.object_instances(); i; ++i)
        {
            if (strcmp(i->get_object().get_model(), model) == 0)
                return true;
        }

        return false;
    }

    uint64 hash_assembly_geometry(const Assembly& assembly, const char* model)
    {
        uint64 hash = 0;

        for (const_each<ObjectInstanceContainer> i = assembly.object_instances(); i; ++i)
        {
            const Object& object = i->get_object();

            if (strcmp(object.get_model(), model) == 0)
            {
                uint64 values[2 + 16];
                values[0] = hash;
                values[1] = object.get_uid();
                memcpy(&values[2], &i->get_transform().get_local_to_parent()[0], 16 * 8);
                hash = siphash24(&values, sizeof(values));
            }
        }

        return hash;
    }

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

                regions.emplace_back(
                    obj_inst_index,
                    region_index,
                    region_bbox);
            }
        }
    }
}

void AssemblyTree::create_child_trees(const Assembly& assembly)
{
    // Create a region or a triangle tree if there are mesh objects.
    if (has_object_instances_of_type(assembly, MeshObjectFactory().get_model()))
    {
        assembly.is_flushable()
            ? create_region_tree(assembly)
            : create_triangle_tree(assembly);
    }

    // Create a curve tree if there are curve objects.
    if (has_object_instances_of_type(assembly, CurveObjectFactory().get_model()))
        create_curve_tree(assembly);
}

void AssemblyTree::create_region_tree(const Assembly& assembly)
{
    const uint64 hash = hash_assembly_geometry(assembly, MeshObjectFactory().get_model());
    Lazy<RegionTree>* tree = m_region_tree_repository.acquire(hash);

    if (tree == nullptr)
    {
        unique_ptr<ILazyFactory<RegionTree>> region_tree_factory(
            new RegionTreeFactory(
                RegionTree::Arguments(
                    m_scene,
                    assembly.get_uid(),
                    assembly)));

        tree = new Lazy<RegionTree>(move(region_tree_factory));
        m_region_tree_repository.insert(hash, tree);
    }

    m_region_trees.insert(make_pair(assembly.get_uid(), tree));
}

void AssemblyTree::create_triangle_tree(const Assembly& assembly)
{
    const uint64 hash = hash_assembly_geometry(assembly, MeshObjectFactory().get_model());
    Lazy<TriangleTree>* tree = m_triangle_tree_repository.acquire(hash);

    if (tree == nullptr)
    {
        // Compute the assembly space bounding box of the assembly.
        const GAABB3 assembly_bbox =
            compute_parent_bbox<GAABB3>(
                assembly.object_instances().begin(),
                assembly.object_instances().end());

        RegionInfoVector regions;
        collect_regions(assembly, regions);

        unique_ptr<ILazyFactory<TriangleTree>> triangle_tree_factory(
            new TriangleTreeFactory(
                TriangleTree::Arguments(
                    m_scene,
                    assembly.get_uid(),
                    assembly_bbox,
                    assembly,
                    regions)));

        tree = new Lazy<TriangleTree>(move(triangle_tree_factory));
        m_triangle_tree_repository.insert(hash, tree);
    }

    m_triangle_trees.insert(make_pair(assembly.get_uid(), tree));
}

void AssemblyTree::create_curve_tree(const Assembly& assembly)
{
    const uint64 hash = hash_assembly_geometry(assembly, CurveObjectFactory().get_model());
    Lazy<CurveTree>* tree = m_curve_tree_repository.acquire(hash);

    if (tree == nullptr)
    {
        // Compute the assembly space bounding box of the assembly.
        const GAABB3 assembly_bbox =
            compute_parent_bbox<GAABB3>(
                assembly.object_instances().begin(),
                assembly.object_instances().end());

        unique_ptr<ILazyFactory<CurveTree>> curve_tree_factory(
            new CurveTreeFactory(
                CurveTree::Arguments(
                    m_scene,
                    assembly.get_uid(),
                    assembly_bbox,
                    assembly)));

        tree = new Lazy<CurveTree>(move(curve_tree_factory));
        m_curve_tree_repository.insert(hash, tree);
    }

    m_curve_trees.insert(make_pair(assembly.get_uid(), tree));
}

void AssemblyTree::delete_child_trees(const UniqueID assembly_id)
{
    delete_region_tree(assembly_id);
    delete_triangle_tree(assembly_id);
    delete_curve_tree(assembly_id);
}

void AssemblyTree::delete_region_tree(const UniqueID assembly_id)
{
    const RegionTreeContainer::iterator it = m_region_trees.find(assembly_id);
    if (it != m_region_trees.end())
    {
        m_region_tree_repository.release(it->second);
        m_region_trees.erase(it);
    }
}

void AssemblyTree::delete_triangle_tree(const UniqueID assembly_id)
{
    const TriangleTreeContainer::iterator it = m_triangle_trees.find(assembly_id);
    if (it != m_triangle_trees.end())
    {
        m_triangle_tree_repository.release(it->second);
        m_triangle_trees.erase(it);
    }
}

void AssemblyTree::delete_curve_tree(const UniqueID assembly_id)
{
    const CurveTreeContainer::iterator it = m_curve_trees.find(assembly_id);
    if (it != m_curve_trees.end())
    {
        m_curve_tree_repository.release(it->second);
        m_curve_trees.erase(it);
    }
}

namespace
{
    template <typename TreeType>
    struct UpdateTrees
    {
        void operator()(Lazy<TreeType>& tree, const size_t ref_count)
        {
            Access<TreeType> update(&tree);

            const bool enable_intersection_filters = ref_count == 1;
            update->update_non_geometry(enable_intersection_filters);
        }
    };
}

void AssemblyTree::update_region_trees()
{
    UpdateTrees<RegionTree> update_trees;
    m_region_tree_repository.for_each(update_trees);
}

void AssemblyTree::update_triangle_trees()
{
    UpdateTrees<TriangleTree> update_trees;
    m_triangle_tree_repository.for_each(update_trees);
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

        // todo: transform ray differentials.
        output_ray.m_has_differentials = false;

        // Copy the remaining members.
        output_ray.m_tmin = input_ray.m_tmin;
        output_ray.m_tmax = input_ray.m_tmax;
        output_ray.m_time = input_ray.m_time;
        output_ray.m_flags = input_ray.m_flags;
        output_ray.m_depth = input_ray.m_depth;
        output_ray.m_medium_count = input_ray.m_medium_count;
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
        const AssemblyInstance& assembly_instance = *item.m_assembly_instance;

        // Skip this assembly instance if it isn't visible for this ray.
        if (!(assembly_instance.get_vis_flags() & ray.m_flags))
            continue;

        FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_intersected_items.insert(1));

        // Evaluate the transformation of the assembly instance.
        const TransformSequence* assembly_instance_transform_seq =
            &item.m_transform_sequence;
        Transformd scratch;
        const Transformd& assembly_instance_transform =
            assembly_instance_transform_seq->evaluate(ray.m_time.m_absolute, scratch);

        // Transform the ray to assembly instance space.
        ShadingPoint local_shading_point;
        compute_assembly_instance_ray(
            assembly_instance,
            assembly_instance_transform,
            m_parent_shading_point,
            ray,
            local_shading_point.m_ray);
        const RayInfo3d local_ray_info(local_shading_point.m_ray);

        if (item.m_assembly->is_flushable())
        {
            // Retrieve the region tree of this assembly.
            const RegionTree& region_tree =
                *m_region_tree_cache.access(
                    item.m_assembly_uid,
                    m_tree.m_region_trees);

            // Check the intersection between the ray and the region tree.
            RegionLeafVisitor visitor(
                local_shading_point,
                m_triangle_tree_cache
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
                , m_triangle_tree_stats
#endif
                );
            RegionLeafIntersector intersector;
            intersector.intersect(
                region_tree,
                local_shading_point.m_ray,
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

            if (triangle_tree)
            {
                // Check the intersection between the ray and the triangle tree.
                TriangleTreeIntersector intersector;
                TriangleLeafVisitor visitor(*triangle_tree, local_shading_point);
                if (triangle_tree->get_moving_triangle_count() > 0)
                {
                    intersector.intersect_motion(
                        *triangle_tree,
                        local_shading_point.m_ray,
                        local_ray_info,
                        local_shading_point.m_ray.m_time.m_normalized,
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
                        local_shading_point.m_ray,
                        local_ray_info,
                        visitor
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
                        , m_triangle_tree_stats
#endif
                        );
                }
                visitor.read_hit_triangle_data();
            }
        }

        // Retrieve the curve tree of this assembly.
        const CurveTree* curve_tree =
            m_curve_tree_cache.access(
                item.m_assembly_uid,
                m_tree.m_curve_trees);

        if (curve_tree)
        {
            // Check the intersection between the ray and the curve tree.
            const GRay3 ray(local_shading_point.m_ray);
            const GRayInfo3 ray_info(local_ray_info);
            CurveMatrixType xfm_matrix;
            make_curve_projection_transform(xfm_matrix, ray);
            CurveLeafVisitor visitor(*curve_tree, xfm_matrix, local_shading_point);
            CurveTreeIntersector intersector;
            intersector.intersect_no_motion(
                *curve_tree,
                ray,
                ray_info,
                visitor
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
                , m_curve_tree_stats
#endif
                );
        }

        // Keep track of the closest hit.
        if (local_shading_point.hit_surface() && local_shading_point.m_ray.m_tmax < m_shading_point.m_ray.m_tmax)
        {
            m_shading_point.m_ray.m_tmax = local_shading_point.m_ray.m_tmax;
            m_shading_point.m_primitive_type = local_shading_point.m_primitive_type;
            m_shading_point.m_bary = local_shading_point.m_bary;
            m_shading_point.m_assembly_instance = item.m_assembly_instance;
            m_shading_point.m_assembly_instance_transform = assembly_instance_transform;
            m_shading_point.m_assembly_instance_transform_seq = assembly_instance_transform_seq;
            m_shading_point.m_object_instance_index = local_shading_point.m_object_instance_index;
            m_shading_point.m_region_index = local_shading_point.m_region_index;
            m_shading_point.m_primitive_index = local_shading_point.m_primitive_index;
            m_shading_point.m_triangle_support_plane = local_shading_point.m_triangle_support_plane;
        }

        // Check the intersection between the ray and procedural objects.
        if (item.m_assembly->has_render_data())
        {
            const ObjectInstanceArray& procedural_instances = item.m_assembly->get_render_data().m_procedural_objects;
            for (size_t j = 0, e = procedural_instances.size(); j < e; ++j)
            {
                // Retrieve the object and object instance.
                const ObjectInstance* object_instance = procedural_instances[j];
                const Transformd& object_instance_transform = object_instance->get_transform();
                const ProceduralObject& object = static_cast<const ProceduralObject&>(object_instance->get_object());

                // Transform the ray to object instance space.
                // todo: transform ray differentials.
                ShadingRay instance_local_ray;
                instance_local_ray.m_org = object_instance_transform.point_to_local(local_shading_point.m_ray.m_org);
                instance_local_ray.m_dir = object_instance_transform.vector_to_local(local_shading_point.m_ray.m_dir);
                instance_local_ray.m_has_differentials = false;
                instance_local_ray.m_tmin = local_shading_point.m_ray.m_tmin;
                instance_local_ray.m_tmax = local_shading_point.m_ray.m_tmax;
                instance_local_ray.m_time = local_shading_point.m_ray.m_time;
                instance_local_ray.m_flags = local_shading_point.m_ray.m_flags;
                instance_local_ray.m_depth = local_shading_point.m_ray.m_depth;
                instance_local_ray.m_medium_count = local_shading_point.m_ray.m_medium_count;

                // Ask the procedural object to intersect itself against the ray.
                ProceduralObject::IntersectionResult result;
                object.intersect(instance_local_ray, result);

                // Keep track of the closest hit.
                if (result.m_hit && result.m_distance < m_shading_point.m_ray.m_tmax)
                {
                    m_shading_point.m_ray.m_tmax = result.m_distance;
                    m_shading_point.m_primitive_type = ShadingPoint::PrimitiveProceduralSurface;
                    m_shading_point.m_bary = result.m_uv;
                    m_shading_point.m_assembly_instance = item.m_assembly_instance;
                    m_shading_point.m_assembly_instance_transform = assembly_instance_transform;
                    m_shading_point.m_assembly_instance_transform_seq = assembly_instance_transform_seq;
                    m_shading_point.m_object_instance_index = j;
                    m_shading_point.m_region_index = 0;
                    m_shading_point.m_primitive_index = 0;
                    m_shading_point.m_primitive_pa = result.m_material_slot;
                    m_shading_point.m_geometric_normal = object_instance_transform.normal_to_parent(result.m_geometric_normal);
                    m_shading_point.m_original_shading_normal = object_instance_transform.normal_to_parent(result.m_shading_normal);
                    m_shading_point.m_uv = result.m_uv;
                }
            }
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
        const AssemblyInstance& assembly_instance = *item.m_assembly_instance;

        // Skip this assembly instance if it isn't visible for this ray.
        if (!(assembly_instance.get_vis_flags() & ray.m_flags))
            continue;

        FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_intersected_items.insert(1));

        // Evaluate the transformation of the assembly instance.
        Transformd scratch;
        const Transformd& assembly_instance_transform =
            item.m_transform_sequence.evaluate(ray.m_time.m_absolute, scratch);

        // Transform the ray to assembly instance space.
        ShadingRay local_ray;
        compute_assembly_instance_ray(
            assembly_instance,
            assembly_instance_transform,
            m_parent_shading_point,
            ray,
            local_ray);
        const RayInfo3d local_ray_info(local_ray);

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
            // Retrieve the triangle tree of this assembly.
            const TriangleTree* triangle_tree =
                m_triangle_tree_cache.access(
                    item.m_assembly_uid,
                    m_tree.m_triangle_trees);

            if (triangle_tree)
            {
                // Check the intersection between the ray and the triangle tree.
                TriangleTreeProbeIntersector intersector;
                TriangleLeafProbeVisitor visitor(*triangle_tree, local_ray.m_time.m_normalized, local_ray.m_flags);
                if (triangle_tree->get_moving_triangle_count() > 0)
                {
                    intersector.intersect_motion(
                        *triangle_tree,
                        local_ray,
                        local_ray_info,
                        local_ray.m_time.m_normalized,
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
        }

        // Retrieve the curve tree of this assembly.
        const CurveTree* curve_tree =
            m_curve_tree_cache.access(
                item.m_assembly_uid,
                m_tree.m_curve_trees);

        if (curve_tree)
        {
            // Check intersection between ray and curve tree.
            const GRay3 ray(local_ray);
            const GRayInfo3 ray_info(local_ray_info);
            CurveMatrixType xfm_matrix;
            make_curve_projection_transform(xfm_matrix, ray);
            CurveLeafProbeVisitor visitor(*curve_tree, xfm_matrix);
            CurveTreeProbeIntersector intersector;
            intersector.intersect_no_motion(
                *curve_tree,
                ray,
                ray_info,
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

        // Check the intersection between the ray and procedural objects.
        if (item.m_assembly->has_render_data())
        {
            const ObjectInstanceArray& procedural_instances = item.m_assembly->get_render_data().m_procedural_objects;
            for (size_t j = 0, e = procedural_instances.size(); j < e; ++j)
            {
                // Retrieve the object and object instance.
                const ObjectInstance* object_instance = procedural_instances[j];
                const Transformd& object_instance_transform = object_instance->get_transform();
                const ProceduralObject& object = static_cast<const ProceduralObject&>(object_instance->get_object());

                // Transform the ray to object instance space.
                // todo: transform ray differentials.
                ShadingRay instance_local_ray;
                instance_local_ray.m_org = object_instance_transform.point_to_local(local_ray.m_org);
                instance_local_ray.m_dir = object_instance_transform.vector_to_local(local_ray.m_dir);
                instance_local_ray.m_has_differentials = false;
                instance_local_ray.m_tmin = local_ray.m_tmin;
                instance_local_ray.m_tmax = local_ray.m_tmax;
                instance_local_ray.m_time = local_ray.m_time;
                instance_local_ray.m_flags = local_ray.m_flags;
                instance_local_ray.m_depth = local_ray.m_depth;
                instance_local_ray.m_medium_count = local_ray.m_medium_count;

                // Ask the procedural object to intersect itself against the ray.
                if (object.intersect(instance_local_ray))
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
