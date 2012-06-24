
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

// Interface header.
#include "triangletree.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/intersection/triangleencoder.h"
#include "renderer/kernel/intersection/triangleitemhandler.h"
#include "renderer/kernel/intersection/trianglevertexinfo.h"
#include "renderer/kernel/tessellation/statictessellation.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/kernel/texturing/texturestore.h"
#include "renderer/modeling/input/source.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/object/iregion.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/object/regionkit.h"
#include "renderer/modeling/object/triangle.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/utility/bbox.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/area.h"
#include "foundation/math/permutation.h"
#include "foundation/math/treeoptimizer.h"
#include "foundation/platform/system.h"
#include "foundation/platform/timer.h"
#include "foundation/utility/memory.h"
#include "foundation/utility/statistics.h"

// Standard headers.
#include <algorithm>
#include <cassert>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// TriangleTree class implementation.
//

namespace
{
    void collect_static_triangles(
        const GAABB3&                   tree_bbox,
        const RegionInfo&               region_info,
        const StaticTriangleTess&       tess,
        const Transformd&               transform,
        vector<TriangleKey>&            triangle_keys,
        vector<TriangleVertexInfo>&     triangle_vertex_infos,
        vector<GVector3>&               triangle_vertices,
        vector<AABB3d>&                 triangle_bboxes)
    {
        const size_t triangle_count = tess.m_primitives.size();

        for (size_t i = 0; i < triangle_count; ++i)
        {
            // Fetch the triangle.
            const Triangle& triangle = tess.m_primitives[i];

            // Retrieve the object space vertices of the triangle.
            const GVector3& v0_os = tess.m_vertices[triangle.m_v0];
            const GVector3& v1_os = tess.m_vertices[triangle.m_v1];
            const GVector3& v2_os = tess.m_vertices[triangle.m_v2];

            // Transform triangle vertices to assembly space.
            const GVector3 v0 = transform.point_to_parent(v0_os);
            const GVector3 v1 = transform.point_to_parent(v1_os);
            const GVector3 v2 = transform.point_to_parent(v2_os);

            // Compute the bounding box of the triangle.
            GAABB3 triangle_bbox;
            triangle_bbox.invalidate();
            triangle_bbox.insert(v0);
            triangle_bbox.insert(v1);
            triangle_bbox.insert(v2);

            // Ignore degenerate triangles.
            if (square_area(v0, v1, v2) == GScalar(0.0))
                continue;

            // Ignore triangles that don't intersect the tree.
            if (!intersect(tree_bbox, v0, v1, v2))
                continue;

            // Store the triangle key.
            triangle_keys.push_back(
                TriangleKey(
                    region_info.get_object_instance_index(),
                    region_info.get_region_index(),
                    i));

            // Store the index of the first triangle vertex and the number of motion segments.
            triangle_vertex_infos.push_back(TriangleVertexInfo(triangle_vertices.size(), 0));

            // Store the triangle vertices.
            triangle_vertices.push_back(v0);
            triangle_vertices.push_back(v1);
            triangle_vertices.push_back(v2);

            // Store the triangle bounding box.
            triangle_bboxes.push_back(AABB3d(triangle_bbox));
        }
    }

    void collect_moving_triangles(
        const GAABB3&                   tree_bbox,
        const RegionInfo&               region_info,
        const StaticTriangleTess&       tess,
        const Transformd&               transform,
        const double                    time,
        vector<TriangleKey>&            triangle_keys,
        vector<TriangleVertexInfo>&     triangle_vertex_infos,
        vector<GVector3>&               triangle_vertices,
        vector<AABB3d>&                 triangle_bboxes)
    {
        const size_t motion_segment_count = tess.get_motion_segment_count();
        const size_t triangle_count = tess.m_primitives.size();

        vector<GAABB3> tri_pose_bboxes(motion_segment_count + 1);

        for (size_t i = 0; i < triangle_count; ++i)
        {
            // Fetch the triangle.
            const Triangle& triangle = tess.m_primitives[i];

            // Retrieve the object space vertices of the triangle.
            const GVector3& v0_os = tess.m_vertices[triangle.m_v0];
            const GVector3& v1_os = tess.m_vertices[triangle.m_v1];
            const GVector3& v2_os = tess.m_vertices[triangle.m_v2];

            // Transform triangle vertices to assembly space.
            const GVector3 v0 = transform.point_to_parent(v0_os);
            const GVector3 v1 = transform.point_to_parent(v1_os);
            const GVector3 v2 = transform.point_to_parent(v2_os);

            // Compute the bounding box of the triangle for each of its pose.
            tri_pose_bboxes[0].invalidate();
            tri_pose_bboxes[0].insert(v0);
            tri_pose_bboxes[0].insert(v1);
            tri_pose_bboxes[0].insert(v2);
            for (size_t m = 0; m < motion_segment_count; ++m)
            {
                tri_pose_bboxes[m + 1].invalidate();
                tri_pose_bboxes[m + 1].insert(transform.point_to_parent(tess.get_vertex_pose(triangle.m_v0, m)));
                tri_pose_bboxes[m + 1].insert(transform.point_to_parent(tess.get_vertex_pose(triangle.m_v1, m)));
                tri_pose_bboxes[m + 1].insert(transform.point_to_parent(tess.get_vertex_pose(triangle.m_v2, m)));
            }

            // Compute the bounding box of the triangle over its entire motion.
            const GAABB3 triangle_motion_bbox =
                compute_union<GAABB3>(tri_pose_bboxes.begin(), tri_pose_bboxes.end());

            // Ignore triangles that are degenerate over their entire motion.
            if (triangle_motion_bbox.rank() < 2)
                continue;

            // Ignore triangles that don't ever intersect the tree.
            if (!GAABB3::overlap(tree_bbox, triangle_motion_bbox))
                continue;

            // Store the triangle key.
            triangle_keys.push_back(
                TriangleKey(
                    region_info.get_object_instance_index(),
                    region_info.get_region_index(),
                    i));

            // Store the index of the first triangle vertex and the number of motion segments.
            triangle_vertex_infos.push_back(
                TriangleVertexInfo(
                    triangle_vertices.size(),
                    motion_segment_count));

            // Store the triangle vertices.
            triangle_vertices.push_back(v0);
            triangle_vertices.push_back(v1);
            triangle_vertices.push_back(v2);
            for (size_t m = 0; m < motion_segment_count; ++m)
            {
                triangle_vertices.push_back(transform.point_to_parent(tess.get_vertex_pose(triangle.m_v0, m)));
                triangle_vertices.push_back(transform.point_to_parent(tess.get_vertex_pose(triangle.m_v1, m)));
                triangle_vertices.push_back(transform.point_to_parent(tess.get_vertex_pose(triangle.m_v2, m)));
            }

            // Compute and store the bounding box of the triangle for the time value passed in argument.
            const GAABB3 triangle_midtime_bbox =
                interpolate<GAABB3>(tri_pose_bboxes.begin(), tri_pose_bboxes.end(), time);
            triangle_bboxes.push_back(AABB3d(triangle_midtime_bbox));
        }
    }

    void collect_triangles(
        const TriangleTree::Arguments&  arguments,
        const double                    time,
        vector<TriangleKey>&            triangle_keys,
        vector<TriangleVertexInfo>&     triangle_vertex_infos,
        vector<GVector3>&               triangle_vertices,
        vector<AABB3d>&                 triangle_bboxes)
    {
        const size_t region_count = arguments.m_regions.size();

        for (size_t i = 0; i < region_count; ++i)
        {
            // Fetch the region info.
            const RegionInfo& region_info = arguments.m_regions[i];

            // Retrieve the object instance and its transformation.
            const ObjectInstance* object_instance =
                arguments.m_assembly.object_instances().get_by_index(
                    region_info.get_object_instance_index());
            assert(object_instance);

            // Retrieve the object.
            Object& object = object_instance->get_object();

            // Retrieve the region kit of the object.
            Access<RegionKit> region_kit(&object.get_region_kit());

            // Retrieve the region.
            const IRegion* region = (*region_kit)[region_info.get_region_index()];

            // Retrieve the tessellation of the region.
            Access<StaticTriangleTess> tess(&region->get_static_triangle_tess());

            // Collect the triangles from this tessellation.
            if (tess->get_motion_segment_count() > 0)
            {
                collect_moving_triangles(
                    arguments.m_bbox,
                    region_info,
                    tess.ref(),
                    object_instance->get_transform(),
                    time,
                    triangle_keys,
                    triangle_vertex_infos,
                    triangle_vertices,
                    triangle_bboxes);
            }
            else
            {
                collect_static_triangles(
                    arguments.m_bbox,
                    region_info,
                    tess.ref(),
                    object_instance->get_transform(),
                    triangle_keys,
                    triangle_vertex_infos,
                    triangle_vertices,
                    triangle_bboxes);
            }
        }
    }
}

TriangleTree::Arguments::Arguments(
    const Scene&            scene,
    const UniqueID          triangle_tree_uid,
    const GAABB3&           bbox,
    const Assembly&         assembly,
    const RegionInfoVector& regions)
  : m_scene(scene)
  , m_triangle_tree_uid(triangle_tree_uid)
  , m_bbox(bbox)
  , m_assembly(assembly)
  , m_regions(regions)
{
}

TriangleTree::TriangleTree(const Arguments& arguments)
  : TreeType(AlignedAllocator<void>(System::get_l1_data_cache_line_size()))
  , m_triangle_tree_uid(arguments.m_triangle_tree_uid)
{
    Statistics statistics("triangle tree #" + to_string(arguments.m_triangle_tree_uid) + " statistics");

    const string DefaultAccelerationStructure = "bvh";

    string acceleration_structure =
        arguments.m_assembly.get_parameters().get_optional<string>(
            "acceleration_structure",
            DefaultAccelerationStructure);

    if (acceleration_structure != "bvh" && acceleration_structure != "sbvh")
    {
        RENDERER_LOG_DEBUG(
            "while building acceleration structure for assembly \"%s\": "
            "invalid acceleration structure \"%s\", using default value \"%s\".",
            arguments.m_assembly.get_name(),
            acceleration_structure.c_str(),
            DefaultAccelerationStructure.c_str());

        acceleration_structure = DefaultAccelerationStructure;
    }

    // Build the tree.
    if (acceleration_structure == "bvh")
        build_bvh(arguments, statistics);
    else build_sbvh(arguments, statistics);

    // Optimize the tree layout in memory.
    TreeOptimizer<NodeVectorType> tree_optimizer(m_nodes);
    tree_optimizer.optimize_node_layout(TriangleTreeSubtreeDepth);
    assert(m_nodes.size() == m_nodes.capacity());

    // Collect and print triangle tree statistics.
    statistics.add_size("node_array_alignment", "nodes alignment", alignment(&m_nodes[0]));
    bvh::TreeStatistics<TriangleTree> collect_statistics(
        statistics,
        *this,
        AABB3d(arguments.m_bbox));
    RENDERER_LOG_DEBUG("%s", statistics.to_string().c_str());

    // Create intersection filters.
    if (arguments.m_assembly.get_parameters().get_optional<bool>("enable_intersection_filters", true))
        create_intersection_filters(arguments);
    m_has_intersection_filters = !m_intersection_filters.empty();
}

TriangleTree::~TriangleTree()
{
    RENDERER_LOG_INFO(
        "deleting triangle tree #" FMT_UNIQUE_ID "...",
        m_triangle_tree_uid);

    // Delete intersection filters.
    for (size_t i = 0; i < m_intersection_filters.size(); ++i)
        delete m_intersection_filters[i];
}

size_t TriangleTree::get_memory_size() const
{
    return
          TreeType::get_memory_size()
        - sizeof(*static_cast<const TreeType*>(this))
        + sizeof(*this)
        + m_triangle_keys.capacity() * sizeof(TriangleKey)
        + m_leaf_data.capacity() * sizeof(uint8);
}

void TriangleTree::build_bvh(
    const Arguments&    arguments,
    Statistics&         statistics)
{
    // Collect triangles intersecting the bounding box of this tree.
    vector<TriangleKey> triangle_keys;
    vector<TriangleVertexInfo> triangle_vertex_infos;
    vector<GVector3> triangle_vertices;
    vector<AABB3d> triangle_bboxes;
    collect_triangles(
        arguments,
        0.5,
        triangle_keys,
        triangle_vertex_infos,
        triangle_vertices,
        triangle_bboxes);

    RENDERER_LOG_INFO(
        "building BVH triangle tree #" FMT_UNIQUE_ID " (%s %s)...",
        arguments.m_triangle_tree_uid,
        pretty_int(triangle_keys.size()).c_str(),
        plural(triangle_keys.size(), "triangle").c_str());

    // Create the partitioner.
    typedef bvh::SAHPartitioner<vector<AABB3d> > Partitioner;
    Partitioner partitioner(
        triangle_bboxes,
        TriangleTreeMaxLeafSize,
        TriangleTreeInteriorNodeTraversalCost,
        TriangleTreeTriangleIntersectionCost);

    // Build the tree.
    typedef bvh::Builder<TriangleTree, Partitioner> Builder;
    Builder builder;
    builder.build<DefaultWallclockTimer>(*this, partitioner, triangle_keys.size());
    statistics.add_time("build_time", "build time", builder.get_build_time());

    // Bounding boxes are no longer needed.
    clear_release_memory(triangle_bboxes);

    compute_motion_bboxes(
        partitioner.get_item_ordering(),
        triangle_vertex_infos,
        triangle_vertices,
        0);

    // Store triangles and triangle keys into the tree.
    store_triangles(
        partitioner.get_item_ordering(),
        triangle_vertex_infos,
        triangle_vertices,
        triangle_keys,
        statistics);
}

void TriangleTree::build_sbvh(
    const Arguments&    arguments,
    Statistics&         statistics)
{
    // Collect triangles intersecting the bounding box of this tree.
    vector<TriangleKey> triangle_keys;
    vector<TriangleVertexInfo> triangle_vertex_infos;
    vector<GVector3> triangle_vertices;
    vector<AABB3d> triangle_bboxes;
    collect_triangles(
        arguments,
        0.5,
        triangle_keys,
        triangle_vertex_infos,
        triangle_vertices,
        triangle_bboxes);

    RENDERER_LOG_INFO(
        "building SBVH triangle tree #" FMT_UNIQUE_ID " (%s %s)...",
        arguments.m_triangle_tree_uid,
        pretty_int(triangle_keys.size()).c_str(),
        plural(triangle_keys.size(), "triangle").c_str());

    // Create the partitioner.
    typedef bvh::SBVHPartitioner<TriangleItemHandler, vector<AABB3d> > Partitioner;
    TriangleItemHandler triangle_handler(
        triangle_vertex_infos,
        triangle_vertices,
        triangle_bboxes);
    Partitioner partitioner(
        triangle_handler,
        triangle_bboxes,
        TriangleTreeMaxLeafSize,
        TriangleTreeBinCount,
        TriangleTreeInteriorNodeTraversalCost,
        TriangleTreeTriangleIntersectionCost);

    // Create the root leaf.
    Partitioner::LeafType* root_leaf = partitioner.create_root_leaf();
    const AABB3d root_leaf_bbox = partitioner.compute_leaf_bbox(*root_leaf);

    // Build the tree.
    typedef bvh::SpatialBuilder<TriangleTree, Partitioner> Builder;
    Builder builder;
    builder.build<DefaultWallclockTimer>(
        *this,
        partitioner,
        root_leaf,
        root_leaf_bbox);
    statistics.add_time("build_time", "build time", builder.get_build_time());

    // Add splits statistics.
    const size_t spatial_splits = partitioner.get_spatial_split_count();
    const size_t object_splits = partitioner.get_object_split_count();
    const size_t total_splits = spatial_splits + object_splits; 
    statistics.add<string>(
        "splits", "splits",
        "spatial " + pretty_uint(spatial_splits) + " (" + pretty_percent(spatial_splits, total_splits) + ")  "
        "object " + pretty_uint(object_splits) + " (" + pretty_percent(object_splits, total_splits) + ")");

    // Bounding boxes are no longer needed.
    clear_release_memory(triangle_bboxes);

    compute_motion_bboxes(
        partitioner.get_item_ordering(),
        triangle_vertex_infos,
        triangle_vertices,
        0);

    // Store triangles and triangle keys into the tree.
    store_triangles(
        partitioner.get_item_ordering(),
        triangle_vertex_infos,
        triangle_vertices,
        triangle_keys,
        statistics);
}

void TriangleTree::store_triangles(
    const vector<size_t>&               triangle_indices,
    const vector<TriangleVertexInfo>&   triangle_vertex_infos,
    const vector<GVector3>&             triangle_vertices,
    const vector<TriangleKey>&          triangle_keys,
    Statistics&                         statistics)
{
    const size_t node_count = m_nodes.size();

    // Gather statistics.

    size_t leaf_count = 0;
    size_t fat_leaf_count = 0;
    size_t leaf_data_size = 0;

    for (size_t i = 0; i < node_count; ++i)
    {
        const NodeType& node = m_nodes[i];

        if (node.is_leaf())
        {
            ++leaf_count;

            const size_t item_begin = node.get_item_index();
            const size_t item_count = node.get_item_count();

            const size_t leaf_size =
                TriangleEncoder::compute_size(
                    triangle_vertex_infos,
                    triangle_indices,
                    item_begin,
                    item_count);

            if (leaf_size < NodeType::MaxUserDataSize)
                ++fat_leaf_count;
            else leaf_data_size += leaf_size;
        }
    }

    // Store triangle keys and triangles.

    m_triangle_keys.reserve(triangle_indices.size());
    m_leaf_data.resize(leaf_data_size);

    MemoryWriter leaf_data_writer(m_leaf_data.empty() ? 0 : &m_leaf_data[0]);

    for (size_t i = 0; i < node_count; ++i)
    {
        NodeType& node = m_nodes[i];

        if (node.is_leaf())
        {
            const size_t item_begin = node.get_item_index();
            const size_t item_count = node.get_item_count();

            node.set_item_index(m_triangle_keys.size());

            for (size_t j = 0; j < item_count; ++j)
            {
                const size_t triangle_index = triangle_indices[item_begin + j];
                m_triangle_keys.push_back(triangle_keys[triangle_index]);
            }

            const size_t leaf_size =
                TriangleEncoder::compute_size(
                    triangle_vertex_infos,
                    triangle_indices,
                    item_begin,
                    item_count);

            MemoryWriter user_data_writer(&node.get_user_data<uint8>());

            if (leaf_size <= NodeType::MaxUserDataSize - 4)
            {
                user_data_writer.write<uint32>(~0);

                TriangleEncoder::encode(
                    triangle_vertex_infos,
                    triangle_vertices,
                    triangle_indices,
                    item_begin,
                    item_count,
                    user_data_writer);
            }
            else
            {
                user_data_writer.write(static_cast<uint32>(leaf_data_writer.offset()));

                TriangleEncoder::encode(
                    triangle_vertex_infos,
                    triangle_vertices,
                    triangle_indices,
                    item_begin,
                    item_count,
                    leaf_data_writer);
            }
        }
    }

    statistics.add_percent("fat_leaves", "fat leaves", fat_leaf_count, leaf_count);
}

vector<GAABB3> TriangleTree::compute_motion_bboxes(
    const vector<size_t>&               triangle_indices,
    const vector<TriangleVertexInfo>&   triangle_vertex_infos,
    const vector<GVector3>&             triangle_vertices,
    const size_t                        node_index)
{
    NodeType& node = m_nodes[node_index];

    if (node.is_interior())
    {
        const vector<GAABB3> left_bboxes =
            compute_motion_bboxes(
                triangle_indices,
                triangle_vertex_infos,
                triangle_vertices,
                node.get_child_node_index() + 0);

        if (left_bboxes.size() > 1)
        {
            node.set_left_bbox(m_node_bboxes.size(), left_bboxes.size());
            m_node_bboxes.insert(m_node_bboxes.end(), left_bboxes.begin(), left_bboxes.end());
        }
        else node.set_left_bbox(0, 1);

        const vector<GAABB3> right_bboxes =
            compute_motion_bboxes(
                triangle_indices,
                triangle_vertex_infos,
                triangle_vertices,
                node.get_child_node_index() + 1);

        if (right_bboxes.size() > 1)
        {
            node.set_right_bbox(m_node_bboxes.size(), right_bboxes.size());
            m_node_bboxes.insert(m_node_bboxes.end(), right_bboxes.begin(), right_bboxes.end());
        }
        else node.set_right_bbox(0, 1);

        const size_t bbox_count = max(left_bboxes.size(), right_bboxes.size());
        vector<GAABB3> bboxes(bbox_count);

        for (size_t i = 0; i < bbox_count; ++i)
        {
            bboxes[i] = left_bboxes[i * left_bboxes.size() / bbox_count];
            bboxes[i].insert(right_bboxes[i * right_bboxes.size() / bbox_count]);
        }

        return bboxes;
    }
    else
    {
        const size_t item_begin = node.get_item_index();
        const size_t item_count = node.get_item_count();

        size_t max_motion_segment_count = 0;

        GAABB3 base_pose_bbox;
        base_pose_bbox.invalidate();

        for (size_t i = 0; i < item_count; ++i)
        {
            const size_t triangle_index = triangle_indices[item_begin + i];
            const TriangleVertexInfo& vertex_info = triangle_vertex_infos[triangle_index];

            assert(is_pow2(vertex_info.m_motion_segment_count + 1));

            if (max_motion_segment_count < vertex_info.m_motion_segment_count)
                max_motion_segment_count = vertex_info.m_motion_segment_count;

            base_pose_bbox.insert(triangle_vertices[vertex_info.m_vertex_index + 0]);
            base_pose_bbox.insert(triangle_vertices[vertex_info.m_vertex_index + 1]);
            base_pose_bbox.insert(triangle_vertices[vertex_info.m_vertex_index + 2]);
        }

        vector<GAABB3> bboxes(max_motion_segment_count + 1);
        bboxes[0] = base_pose_bbox;

        if (max_motion_segment_count > 0)
        {
            for (size_t m = 0; m < max_motion_segment_count - 1; ++m)
            {
                bboxes[m + 1].invalidate();

                const double time = static_cast<double>(m + 1) / max_motion_segment_count;

                for (size_t i = 0; i < item_count; ++i)
                {
                    const size_t triangle_index = triangle_indices[item_begin + i];
                    const TriangleVertexInfo& vertex_info = triangle_vertex_infos[triangle_index];

                    const size_t prev_pose_index = truncate<size_t>(time * vertex_info.m_motion_segment_count);
                    const size_t base_vertex_index = vertex_info.m_vertex_index + prev_pose_index * 3;
                    const GScalar k = static_cast<GScalar>(time * vertex_info.m_motion_segment_count - prev_pose_index);

                    bboxes[m + 1].insert(lerp(triangle_vertices[base_vertex_index + 0], triangle_vertices[base_vertex_index + 3], k));
                    bboxes[m + 1].insert(lerp(triangle_vertices[base_vertex_index + 1], triangle_vertices[base_vertex_index + 4], k));
                    bboxes[m + 1].insert(lerp(triangle_vertices[base_vertex_index + 2], triangle_vertices[base_vertex_index + 5], k));
                }
            }

            bboxes[max_motion_segment_count].invalidate();

            for (size_t i = 0; i < item_count; ++i)
            {
                const size_t triangle_index = triangle_indices[item_begin + i];
                const TriangleVertexInfo& vertex_info = triangle_vertex_infos[triangle_index];
                const size_t base_vertex_index = vertex_info.m_vertex_index + vertex_info.m_motion_segment_count * 3;

                bboxes[max_motion_segment_count].insert(triangle_vertices[base_vertex_index + 0]);
                bboxes[max_motion_segment_count].insert(triangle_vertices[base_vertex_index + 1]);
                bboxes[max_motion_segment_count].insert(triangle_vertices[base_vertex_index + 2]);
            }
        }

        return bboxes;
    }
}

void TriangleTree::create_intersection_filters(const Arguments& arguments)
{
    // Collect object instances.
    vector<size_t> object_instance_indices;
    object_instance_indices.reserve(arguments.m_regions.size());
    for (const_each<RegionInfoVector> i = arguments.m_regions; i; ++i)
        object_instance_indices.push_back(i->get_object_instance_index());

    // Unique the list of object instances.
    sort(object_instance_indices.begin(), object_instance_indices.end());
    object_instance_indices.erase(
        unique(object_instance_indices.begin(), object_instance_indices.end()),
        object_instance_indices.end());

    TextureStore texture_store(arguments.m_scene);
    TextureCache texture_cache(texture_store);

    size_t intersection_filter_count = 0;

    for (const_each<vector<size_t> > i = object_instance_indices; i; ++i)
    {
        // Retrieve the object instance.
        const size_t object_instance_index = *i;
        const ObjectInstance* object_instance =
            arguments.m_assembly.object_instances().get_by_index(object_instance_index);
        assert(object_instance);

        // No intersection filter for this object instance if it doesn't have any front materials.
        if (object_instance->get_front_materials().empty())
            continue;

        // No intersection filter for this object instance if its first front material doesn't have an alpha map.
        const Material* material = object_instance->get_front_materials()[0];
        const Source* alpha_map = material->get_alpha_map();
        if (alpha_map == 0)
            continue;

        // Create an intersection filter for this object instance.
        auto_ptr<IntersectionFilter> intersection_filter(
            new IntersectionFilter(
                arguments.m_scene,
                arguments.m_assembly,
                object_instance_index,
                texture_cache));

        // No intersection filter for this object instance if its alpha map is mostly opaque or semi-transparent.
        if (intersection_filter->get_transparent_pixel_ratio() < 5.0 / 100)
            continue;

        // Allocate the array of intersection filters.
        if (m_intersection_filters.empty())
            m_intersection_filters.resize(object_instance_indices.back() + 1);

        // Store the intersection filter.
        m_intersection_filters[object_instance_index] = intersection_filter.release();
        ++intersection_filter_count;
    }

    RENDERER_LOG_INFO(
        "created " FMT_SIZE_T " intersection filter%s for triangle tree #" FMT_UNIQUE_ID ".",
        intersection_filter_count,
        intersection_filter_count > 1 ? "s" : "",
        arguments.m_triangle_tree_uid);
}


//
// TriangleTreeFactory class implementation.
//

TriangleTreeFactory::TriangleTreeFactory(const TriangleTree::Arguments& arguments)
  : m_arguments(arguments)
{
}

auto_ptr<TriangleTree> TriangleTreeFactory::create()
{
    return auto_ptr<TriangleTree>(new TriangleTree(m_arguments));
}

}   // namespace renderer
