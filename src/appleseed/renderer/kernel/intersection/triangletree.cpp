
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

// Interface header.
#include "triangletree.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/intersection/intersectionfilter.h"
#include "renderer/kernel/intersection/triangleencoder.h"
#include "renderer/kernel/intersection/triangleitemhandler.h"
#include "renderer/kernel/intersection/trianglevertexinfo.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/kernel/tessellation/statictessellation.h"
#include "renderer/kernel/texturing/texturecache.h"
#include "renderer/kernel/texturing/texturestore.h"
#include "renderer/modeling/entity/entity.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/object/meshobject.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/object/triangle.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/utility/bbox.h"
#include "renderer/utility/messagecontext.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/area.h"
#include "foundation/math/intersection/aabbtriangle.h"
#include "foundation/math/scalar.h"
#include "foundation/math/transform.h"
#include "foundation/math/treeoptimizer.h"
#include "foundation/math/vector.h"
#include "foundation/memory/alignedallocator.h"
#include "foundation/memory/memory.h"
#include "foundation/platform/system.h"
#include "foundation/platform/timers.h"
#include "foundation/platform/types.h"
#include "foundation/string/string.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/makevector.h"
#include "foundation/utility/statistics.h"
#include "foundation/utility/stopwatch.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <set>
#include <string>

using namespace foundation;

namespace renderer
{

//
// TriangleTree class implementation.
//

namespace
{
    template <typename Vector>
    void increase_capacity(Vector* vec, const size_t count)
    {
        if (vec)
            vec->reserve(vec->capacity() + count);
    }

    template <typename Vector>
    void assert_empty(Vector* vec)
    {
        assert(vec == 0 || vec->empty());
    }

    template <typename AABBType>
    void collect_static_triangles(
        const GAABB3&                        tree_bbox,
        const ObjectInstance&                object_instance,
        const size_t                         object_instance_index,
        const StaticTriangleTess&            tess,
        const bool                           save_memory,
        std::vector<TriangleKey>*            triangle_keys,
        std::vector<TriangleVertexInfo>*     triangle_vertex_infos,
        std::vector<GVector3>*               triangle_vertices,
        std::vector<AABBType>*               triangle_bboxes,
        size_t&                              triangle_vertex_count)
    {
        const Transformd& transform = object_instance.get_transform();
        const size_t triangle_count = tess.m_primitives.size();

        if (save_memory)
        {
            // Reserve memory.
            increase_capacity(triangle_keys, triangle_count);
            increase_capacity(triangle_vertex_infos, triangle_count);
            increase_capacity(triangle_vertices, triangle_count * 3);
            increase_capacity(triangle_bboxes, triangle_count);
        }

        for (size_t i = 0; i < triangle_count; ++i)
        {
            // Fetch the triangle.
            const Triangle& triangle = tess.m_primitives[i];

            // Retrieve the object space vertices of the triangle.
            const GVector3& v0_os = tess.m_vertices[triangle.m_v0];
            const GVector3& v1_os = tess.m_vertices[triangle.m_v1];
            const GVector3& v2_os = tess.m_vertices[triangle.m_v2];

            // Ignore degenerate triangles.
            if (square_area(v0_os, v1_os, v2_os) == GScalar(0.0))
                continue;

            // Transform triangle vertices to assembly space.
            const GVector3 v0 = transform.point_to_parent(v0_os);
            const GVector3 v1 = transform.point_to_parent(v1_os);
            const GVector3 v2 = transform.point_to_parent(v2_os);

            // Ignore degenerate triangles.
            if (square_area(v0, v1, v2) == GScalar(0.0))
                continue;

            // Compute the bounding box of the triangle.
            GAABB3 triangle_bbox;
            triangle_bbox.invalidate();
            triangle_bbox.insert(v0);
            triangle_bbox.insert(v1);
            triangle_bbox.insert(v2);

            // Ignore triangles that don't intersect the tree.
            if (!intersect(tree_bbox, v0, v1, v2))
                continue;

            // Store the triangle key.
            if (triangle_keys)
            {
                triangle_keys->push_back(
                    TriangleKey(
                        object_instance_index,
                        i,
                        triangle.m_pa));
            }

            // Store the index of the first triangle vertex and the number of motion segments.
            if (triangle_vertex_infos)
            {
                triangle_vertex_infos->push_back(
                    TriangleVertexInfo(
                        triangle_vertex_count,
                        0,
                        object_instance.get_vis_flags()));
            }

            // Store the triangle vertices.
            if (triangle_vertices)
            {
                triangle_vertices->push_back(v0);
                triangle_vertices->push_back(v1);
                triangle_vertices->push_back(v2);
            }
            triangle_vertex_count += 3;

            // Store the triangle bounding box.
            if (triangle_bboxes)
                triangle_bboxes->push_back(AABBType(triangle_bbox));
        }
    }

    template <typename AABBType>
    void collect_moving_triangles(
        const GAABB3&                        tree_bbox,
        const ObjectInstance&                object_instance,
        const size_t                         object_instance_index,
        const StaticTriangleTess&            tess,
        const double                         time,
        const bool                           save_memory,
        std::vector<TriangleKey>*            triangle_keys,
        std::vector<TriangleVertexInfo>*     triangle_vertex_infos,
        std::vector<GVector3>*               triangle_vertices,
        std::vector<AABBType>*               triangle_bboxes,
        size_t&                              triangle_vertex_count)
    {
        const Transformd& transform = object_instance.get_transform();
        const size_t motion_segment_count = tess.get_motion_segment_count();
        const size_t triangle_count = tess.m_primitives.size();

        if (save_memory)
        {
            // Reserve memory.
            increase_capacity(triangle_keys, triangle_count);
            increase_capacity(triangle_vertex_infos, triangle_count);
            increase_capacity(triangle_vertices, triangle_count * 3 * (motion_segment_count + 1));
            increase_capacity(triangle_bboxes, triangle_count);
        }

        std::vector<GAABB3> tri_pose_bboxes(motion_segment_count + 1);

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

            // Compute the bounding box of the triangle for the time value passed in argument.
            const GAABB3 triangle_midtime_bbox =
                interpolate<GAABB3>(tri_pose_bboxes.begin(), tri_pose_bboxes.end(), time);

            // Ignore triangles with an empty bounding box.
            if (triangle_midtime_bbox.rank() < 2)
                continue;

            // Store the triangle key.
            if (triangle_keys)
            {
                triangle_keys->push_back(
                    TriangleKey(
                        object_instance_index,
                        i,
                        triangle.m_pa));
            }

            // Store the index of the first triangle vertex and the number of motion segments.
            if (triangle_vertex_infos)
            {
                triangle_vertex_infos->push_back(
                    TriangleVertexInfo(
                        triangle_vertex_count,
                        motion_segment_count,
                        object_instance.get_vis_flags()));
            }

            // Store the triangle vertices.
            if (triangle_vertices)
            {
                triangle_vertices->push_back(v0);
                triangle_vertices->push_back(v1);
                triangle_vertices->push_back(v2);
                for (size_t m = 0; m < motion_segment_count; ++m)
                {
                    triangle_vertices->push_back(transform.point_to_parent(tess.get_vertex_pose(triangle.m_v0, m)));
                    triangle_vertices->push_back(transform.point_to_parent(tess.get_vertex_pose(triangle.m_v1, m)));
                    triangle_vertices->push_back(transform.point_to_parent(tess.get_vertex_pose(triangle.m_v2, m)));
                }
            }
            triangle_vertex_count += (motion_segment_count + 1) * 3;

            // Store the bounding box of the triangle for the time value passed in argument.
            if (triangle_bboxes)
                triangle_bboxes->push_back(AABBType(triangle_midtime_bbox));
        }
    }

    template <typename AABBType>
    void collect_triangles(
        const TriangleTree::Arguments&       arguments,
        const double                         time,
        const bool                           save_memory,
        std::vector<TriangleKey>*            triangle_keys,
        std::vector<TriangleVertexInfo>*     triangle_vertex_infos,
        std::vector<GVector3>*               triangle_vertices,
        std::vector<AABBType>*               triangle_bboxes)
    {
        assert_empty(triangle_keys);
        assert_empty(triangle_vertex_infos);
        assert_empty(triangle_vertices);
        assert_empty(triangle_bboxes);

        size_t triangle_vertex_count = 0;

        for (size_t i = 0, e = arguments.m_assembly.object_instances().size(); i < e; ++i)
        {
            // Retrieve the object instance and its transformation.
            const ObjectInstance* object_instance =
                arguments.m_assembly.object_instances().get_by_index(i);
            assert(object_instance);

            // Retrieve the object.
            Object& object = object_instance->get_object();

            // Process only mesh objects.
            if (strcmp(object.get_model(), MeshObjectFactory().get_model()) != 0)
                continue;

            const MeshObject& mesh = static_cast<const MeshObject&>(object);
            const StaticTriangleTess& tess = mesh.get_static_triangle_tess();

            // Collect the triangles from this tessellation.
            if (tess.get_motion_segment_count() > 0)
            {
                collect_moving_triangles(
                    arguments.m_bbox,
                    *object_instance,
                    i,
                    tess,
                    time,
                    save_memory,
                    triangle_keys,
                    triangle_vertex_infos,
                    triangle_vertices,
                    triangle_bboxes,
                    triangle_vertex_count);
            }
            else
            {
                collect_static_triangles(
                    arguments.m_bbox,
                    *object_instance,
                    i,
                    tess,
                    save_memory,
                    triangle_keys,
                    triangle_vertex_infos,
                    triangle_vertices,
                    triangle_bboxes,
                    triangle_vertex_count);
            }
        }
    }
}

TriangleTree::Arguments::Arguments(
    const Project&          project,
    const UniqueID          triangle_tree_uid,
    const GAABB3&           bbox,
    const Assembly&         assembly)
  : m_project(project)
  , m_triangle_tree_uid(triangle_tree_uid)
  , m_bbox(bbox)
  , m_assembly(assembly)
{
}

TriangleTree::TriangleTree(const Arguments& arguments)
  : TreeType(AlignedAllocator<void>(System::get_l1_data_cache_line_size()))
  , m_arguments(arguments)
{
    // Retrieve construction parameters.
    const MessageContext message_context(
        format("while building triangle tree for assembly \"{0}\"", m_arguments.m_assembly.get_path()));
    const ParamArray& params = m_arguments.m_assembly.get_parameters().child("acceleration_structure");
    const std::string algorithm = params.get_optional<std::string>("algorithm", "bvh", make_vector("bvh", "sbvh"), message_context);
    const double time = params.get_optional<double>("time", 0.5);
    const bool save_memory = params.get_optional<bool>("save_temporary_memory", false);

    // Start stopwatch.
    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();

    // Build the tree.
    Statistics statistics;
    if (algorithm == "bvh")
        build_bvh(params, time, save_memory, statistics);
    else build_sbvh(params, time, save_memory, statistics);
    statistics.insert_time("total build time", stopwatch.measure().get_seconds());
    statistics.insert_size("nodes alignment", alignment(&m_nodes[0]));

#ifdef RENDERER_TRIANGLE_TREE_REORDER_NODES
    // Optimize the tree layout in memory.
    TreeOptimizer<NodeVectorType> tree_optimizer(m_nodes);
    tree_optimizer.optimize_node_layout(TriangleTreeSubtreeDepth);
    assert(m_nodes.size() == m_nodes.capacity());
#endif

    // Print triangle tree statistics.
    RENDERER_LOG_DEBUG("%s",
        StatisticsVector::make(
            "triangle tree #" + to_string(m_arguments.m_triangle_tree_uid) + " statistics",
            statistics).to_string().c_str());
}

TriangleTree::~TriangleTree()
{
    RENDERER_LOG_INFO(
        "deleting triangle tree #" FMT_UNIQUE_ID "...",
        m_arguments.m_triangle_tree_uid);

    delete_intersection_filters();
}

void TriangleTree::update_non_geometry(const bool enable_intersection_filters)
{
    if (enable_intersection_filters &&
        m_arguments.m_assembly.get_parameters().get_optional<bool>("enable_intersection_filters", true))
        update_intersection_filters();
    else delete_intersection_filters();
}

size_t TriangleTree::get_memory_size() const
{
    return
          TreeType::get_memory_size()
        - sizeof(*static_cast<const TreeType*>(this))
        + sizeof(*this)
        + m_triangle_keys.capacity() * sizeof(TriangleKey)
        + m_leaf_data.capacity() * sizeof(std::uint8_t);
}

namespace
{
    template <typename Vector>
    void print_vector_stats(const char* label, const Vector& vec)
    {
        const size_t ItemSize = sizeof(typename Vector::value_type);
        const size_t overhead = vec.capacity() - vec.size();

        RENDERER_LOG_DEBUG(
            "%s: size %s (%s)  capacity %s (%s)  overhead %s (%s)",
            label,
            pretty_uint(vec.size()).c_str(),
            pretty_size(vec.size() * ItemSize).c_str(),
            pretty_uint(vec.capacity()).c_str(),
            pretty_size(vec.capacity() * ItemSize).c_str(),
            pretty_uint(overhead).c_str(),
            pretty_size(overhead * ItemSize).c_str());
    }

    #define RENDERER_LOG_VECTOR_STATS(vec) print_vector_stats(#vec, vec)

    size_t count_static_triangles(const std::vector<TriangleVertexInfo>& info)
    {
        size_t count = 0;

        for (size_t i = 0; i < info.size(); ++i)
        {
            if (info[i].m_motion_segment_count == 0)
                ++count;
        }

        return count;
    }
}

void TriangleTree::build_bvh(
    const ParamArray&   params,
    const double        time,
    const bool          save_memory,
    Statistics&         statistics)
{
    Stopwatch<DefaultWallclockTimer> stopwatch;

    // Collect triangles intersecting the bounding box of this tree.
    RENDERER_LOG_INFO(
        "collecting geometry for triangle tree #" FMT_UNIQUE_ID " from assembly \"%s\"...",
        m_arguments.m_triangle_tree_uid,
        m_arguments.m_assembly.get_path().c_str());
    stopwatch.start();
    std::vector<TriangleKey> triangle_keys;
    std::vector<TriangleVertexInfo> triangle_vertex_infos;
    std::vector<GAABB3> triangle_bboxes;
    collect_triangles(
        m_arguments,
        time,
        save_memory,
        &triangle_keys,
        &triangle_vertex_infos,
        nullptr,
        &triangle_bboxes);
    const double collection_time = stopwatch.measure().get_seconds();

    // Store the number of static and moving triangles.
    m_static_triangle_count = count_static_triangles(triangle_vertex_infos);
    m_moving_triangle_count = triangle_vertex_infos.size() - m_static_triangle_count;

    // Print statistics about the input geometry.
    RENDERER_LOG_INFO(
        "building triangle tree #" FMT_UNIQUE_ID " (bvh, %s %s, %s %s)...",
        m_arguments.m_triangle_tree_uid,
        pretty_uint(m_static_triangle_count).c_str(),
        plural(m_static_triangle_count, "static triangle").c_str(),
        pretty_uint(m_moving_triangle_count).c_str(),
        plural(m_moving_triangle_count, "moving triangle").c_str());

    // Retrieving the partitioner parameters.
    const size_t max_leaf_size = params.get_optional<size_t>("max_leaf_size", TriangleTreeDefaultMaxLeafSize);
    const GScalar interior_node_traversal_cost = params.get_optional<GScalar>("interior_node_traversal_cost", TriangleTreeDefaultInteriorNodeTraversalCost);
    const GScalar triangle_intersection_cost = params.get_optional<GScalar>("triangle_intersection_cost", TriangleTreeDefaultTriangleIntersectionCost);

    // Create the partitioner.
    typedef bvh::SAHPartitioner<std::vector<GAABB3>> Partitioner;
    Partitioner partitioner(
        triangle_bboxes,
        max_leaf_size,
        interior_node_traversal_cost,
        triangle_intersection_cost);

    // Build the tree.
    typedef bvh::Builder<TriangleTree, Partitioner> Builder;
    Builder builder;
    builder.build<DefaultWallclockTimer>(
        *this,
        partitioner,
        triangle_keys.size(),
        max_leaf_size);
    statistics.merge(
        bvh::TreeStatistics<TriangleTree>(*this, AABB3d(m_arguments.m_bbox)));

    stopwatch.start();

    // Bounding boxes are no longer needed.
    clear_release_memory(triangle_bboxes);

    // Collect triangle vertices.
    std::vector<GVector3> triangle_vertices;
    collect_triangles<GAABB3>(
        m_arguments,
        time,
        save_memory,
        nullptr,
        nullptr,
        &triangle_vertices,
        nullptr);

    // Compute and propagate motion bounding boxes.
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

    const double store_time = stopwatch.measure().get_seconds();

    statistics.insert_time("collection time", collection_time);
    statistics.insert_time("partition time", builder.get_build_time());
    statistics.insert_time("store time", store_time);
}

void TriangleTree::build_sbvh(
    const ParamArray&   params,
    const double        time,
    const bool          save_memory,
    Statistics&         statistics)
{
    Stopwatch<DefaultWallclockTimer> stopwatch;

    // Collect triangles intersecting the bounding box of this tree.
    RENDERER_LOG_INFO(
        "collecting geometry for triangle tree #" FMT_UNIQUE_ID " from assembly \"%s\"...",
        m_arguments.m_triangle_tree_uid,
        m_arguments.m_assembly.get_path().c_str());
    std::vector<TriangleKey> triangle_keys;
    std::vector<TriangleVertexInfo> triangle_vertex_infos;
    std::vector<GVector3> triangle_vertices;
    std::vector<AABB3d> triangle_bboxes;
    stopwatch.start();
    collect_triangles(
        m_arguments,
        time,
        save_memory,
        &triangle_keys,
        &triangle_vertex_infos,
        &triangle_vertices,
        &triangle_bboxes);
    const double collection_time = stopwatch.measure().get_seconds();

    // Store the number of static and moving triangles.
    m_static_triangle_count = count_static_triangles(triangle_vertex_infos);
    m_moving_triangle_count = triangle_vertex_infos.size() - m_static_triangle_count;

    // Print statistics about the input geometry.
    RENDERER_LOG_INFO(
        "building triangle tree #" FMT_UNIQUE_ID " (sbvh, %s %s, %s %s)...",
        m_arguments.m_triangle_tree_uid,
        pretty_uint(m_static_triangle_count).c_str(),
        plural(m_static_triangle_count, "static triangle").c_str(),
        pretty_uint(m_moving_triangle_count).c_str(),
        plural(m_moving_triangle_count, "moving triangle").c_str());

    // Retrieving the partitioner parameters.
    const size_t max_leaf_size = params.get_optional<size_t>("max_leaf_size", TriangleTreeDefaultMaxLeafSize);
    const size_t bin_count = params.get_optional<size_t>("bin_count", TriangleTreeDefaultBinCount);
    const GScalar interior_node_traversal_cost = params.get_optional<GScalar>("interior_node_traversal_cost", TriangleTreeDefaultInteriorNodeTraversalCost);
    const GScalar triangle_intersection_cost = params.get_optional<GScalar>("triangle_intersection_cost", TriangleTreeDefaultTriangleIntersectionCost);

    // Create the partitioner.
    typedef bvh::SBVHPartitioner<TriangleItemHandler, std::vector<AABB3d>> Partitioner;
    TriangleItemHandler triangle_handler(
        triangle_vertex_infos,
        triangle_vertices,
        triangle_bboxes);
    Partitioner partitioner(
        triangle_handler,
        triangle_bboxes,
        max_leaf_size,
        bin_count,
        interior_node_traversal_cost,
        triangle_intersection_cost);

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
    statistics.merge(bvh::TreeStatistics<TriangleTree>(*this, AABB3d(m_arguments.m_bbox)));

    // Add splits statistics.
    const size_t spatial_splits = partitioner.get_spatial_split_count();
    const size_t object_splits = partitioner.get_object_split_count();
    const size_t total_splits = spatial_splits + object_splits;
    statistics.insert(
        "splits",
        "spatial " + pretty_uint(spatial_splits) + " (" + pretty_percent(spatial_splits, total_splits) + ")  "
        "object " + pretty_uint(object_splits) + " (" + pretty_percent(object_splits, total_splits) + ")");

    stopwatch.start();

    // Bounding boxes are no longer needed.
    clear_release_memory(triangle_bboxes);

    // Compute and propagate motion bounding boxes.
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

    const double store_time = stopwatch.measure().get_seconds();

    statistics.insert_time("collection time", collection_time);
    statistics.insert_time("partition time", builder.get_build_time());
    statistics.insert_time("store time", store_time);
}

namespace
{
#ifdef APPLESEED_USE_SSE

    //
    // If the bounding box is seen as a flat array of scalars, the swizzle()
    // function converts the bounding box from
    //
    //   min.x  min.y  min.z  max.x  max.y  max.z
    //
    // to
    //
    //   min.x  max.x  min.y  max.y  min.z  max.z
    //

    template <typename T, size_t N>
    AABB<T, N> swizzle(const AABB<T, N>& bbox)
    {
        AABB<T, N> result;
        T* flat_result = &result[0][0];

        for (size_t i = 0; i < N; ++i)
        {
            flat_result[i * 2 + 0] = bbox[0][i];
            flat_result[i * 2 + 1] = bbox[1][i];
        }

        return result;
    }

#else

    //
    // The swizzle() function has no effect when SSE is disabled.
    //

    template <typename T, size_t N>
    AABB<T, N> swizzle(const AABB<T, N>& bbox)
    {
        return bbox;
    }

#endif
}

std::vector<GAABB3> TriangleTree::compute_motion_bboxes(
    const std::vector<size_t>&               triangle_indices,
    const std::vector<TriangleVertexInfo>&   triangle_vertex_infos,
    const std::vector<GVector3>&             triangle_vertices,
    const size_t                             node_index)
{
    NodeType& node = m_nodes[node_index];

    if (node.is_interior())
    {
        const std::vector<GAABB3> left_bboxes =
            compute_motion_bboxes(
                triangle_indices,
                triangle_vertex_infos,
                triangle_vertices,
                node.get_child_node_index() + 0);

        const std::vector<GAABB3> right_bboxes =
            compute_motion_bboxes(
                triangle_indices,
                triangle_vertex_infos,
                triangle_vertices,
                node.get_child_node_index() + 1);

        node.set_left_bbox_count(left_bboxes.size());
        node.set_right_bbox_count(right_bboxes.size());

        if (left_bboxes.size() > 1)
        {
            node.set_left_bbox_index(m_node_bboxes.size());

            for (std::vector<GAABB3>::const_iterator i = left_bboxes.begin(); i != left_bboxes.end(); ++i)
                m_node_bboxes.push_back(swizzle(AABB3d(*i)));
        }

        if (right_bboxes.size() > 1)
        {
            node.set_right_bbox_index(m_node_bboxes.size());

            for (std::vector<GAABB3>::const_iterator i = right_bboxes.begin(); i != right_bboxes.end(); ++i)
                m_node_bboxes.push_back(swizzle(AABB3d(*i)));
        }

        const size_t bbox_count = std::max(left_bboxes.size(), right_bboxes.size());
        std::vector<GAABB3> bboxes(bbox_count);

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

        std::vector<GAABB3> bboxes(max_motion_segment_count + 1);
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

void TriangleTree::store_triangles(
    const std::vector<size_t>&               triangle_indices,
    const std::vector<TriangleVertexInfo>&   triangle_vertex_infos,
    const std::vector<GVector3>&             triangle_vertices,
    const std::vector<TriangleKey>&          triangle_keys,
    Statistics&                              statistics)
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

    MemoryWriter leaf_data_writer(m_leaf_data.empty() ? nullptr : &m_leaf_data[0]);

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

            MemoryWriter user_data_writer(&node.get_user_data<std::uint8_t>());

            if (leaf_size <= NodeType::MaxUserDataSize - sizeof(std::uint32_t))
            {
                user_data_writer.write<std::uint32_t>(~std::uint32_t(0));

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
                user_data_writer.write(static_cast<std::uint32_t>(leaf_data_writer.offset()));

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

    statistics.insert_percent("fat leaves", fat_leaf_count, leaf_count);
}

namespace
{
    struct FilterKey
    {
        Object*         m_object;
        MaterialArray   m_materials;

        FilterKey()
        {
        }

        FilterKey(
            Object*                 object,
            const MaterialArray&    materials)
          : m_object(object)
          , m_materials(materials)
        {
        }

        bool operator==(const FilterKey& rhs) const
        {
            if (m_object != rhs.m_object)
                return false;

            if (m_materials.size() != rhs.m_materials.size())
                return false;

            const size_t material_count = m_materials.size();

            for (size_t i = 0; i < material_count; ++i)
            {
                if (m_materials[i] != rhs.m_materials[i])
                    return false;
            }

            return true;
        }

        bool operator<(const FilterKey& rhs) const
        {
            if (m_object < rhs.m_object)
                return true;
            else if (m_object > rhs.m_object)
                return false;

            if (m_materials.size() < rhs.m_materials.size())
                return true;
            else if (m_materials.size() > rhs.m_materials.size())
                return false;

            const size_t material_count = m_materials.size();

            for (size_t i = 0; i < material_count; ++i)
            {
                if (m_materials[i] < rhs.m_materials[i])
                    return true;
                else if (m_materials[i] > rhs.m_materials[i])
                    return false;
            }

            return false;
        }

        bool has_alpha_maps() const
        {
            // Use the uncached versions of get_alpha_map() since at this point
            // on_frame_begin() hasn't been called on the object or materials,
            // when intersection filters are updated on existing triangle trees
            // prior to rendering.

            if (m_object->get_uncached_alpha_map())
                return true;

            const size_t material_count = m_materials.size();

            for (size_t i = 0; i < material_count; ++i)
            {
                if (m_materials[i] && m_materials[i]->get_uncached_alpha_map())
                    return true;
            }

            return false;
        }
    };

    // Hash a filter key.
    std::uint64_t hash(const FilterKey& key)
    {
        std::uint64_t h = key.m_object->compute_signature();

        for (size_t i = 0; i < key.m_materials.size(); ++i)
        {
            const Material* material = key.m_materials[i];

            if (material)
                h = Entity::combine_signatures(h, material->compute_signature());
        }

        return h;
    }

    typedef std::set<size_t> IndexSet;
    typedef std::set<FilterKey> FilterKeySet;
    typedef std::map<size_t, const FilterKey*> IndexToFilterKeyMap;

    // Collect the set of object instances contained in an assembly.
    void collect_object_instances(
        const Assembly&                     assembly,
        IndexSet&                           object_instances)
    {
        for (size_t i = 0, e = assembly.object_instances().size(); i < e; ++i)
        {
            // Retrieve the object instance and its transformation.
            const ObjectInstance* object_instance =
                assembly.object_instances().get_by_index(i);
            assert(object_instance);

            // Retrieve the object.
            Object& object = object_instance->get_object();

            // Process only mesh objects.
            if (strcmp(object.get_model(), MeshObjectFactory().get_model()) == 0)
                object_instances.insert(i);
        }
    }

    // Create filter keys for a set of object instances, and establish an (object instance) -> (filter key) mapping.
    void create_filter_keys(
        const ObjectInstanceContainer&      object_instances,
        const IndexSet&                     object_instance_indices,
        FilterKeySet&                       filter_keys,
        IndexToFilterKeyMap&                object_instances_to_filter_keys)
    {
        for (const_each<IndexSet> i = object_instance_indices; i; ++i)
        {
            const size_t object_instance_index = *i;
            const ObjectInstance* object_instance =
                object_instances.get_by_index(object_instance_index);

            // We create intersection filters for front materials, assuming materials will be the same on the back
            // side of the object instance. This assumption is checked in renderer::ObjectInstance::on_frame_begin()
            // and a warning is issued if it does not appear to hold. The reason is that the direction of shadow
            // rays is unpredictable: an object with different alpha transparency on its front and back faces would
            // render differently with different light transport algorithms, or if the direction of shadow rays
            // (which is an implementation detail) changes.
            const FilterKeySet::const_iterator filter_key_it =
                filter_keys.insert(
                    FilterKey(
                        &object_instance->get_object(),
                        object_instance->get_front_materials())).first;

            const FilterKey& filter_key = *filter_key_it;
            object_instances_to_filter_keys[object_instance_index] = &filter_key;
        }
    }

    // Create intersection filters for filter keys that don't already have one.
    void create_missing_intersection_filters(
        TextureCache&                       texture_cache,
        const FilterKeySet&                 filter_keys,
        IntersectionFilterRepository&       filters)
    {
        for (const_each<FilterKeySet> i = filter_keys; i; ++i)
        {
            const FilterKey& filter_key = *i;

            // Don't create intersection filters for keys that don't reference any alpha map.
            if (!filter_key.has_alpha_maps())
                continue;

            // Update existing intersection filters.
            const std::uint64_t filter_key_hash = hash(filter_key);
            const IntersectionFilterRepository::const_iterator it = filters.find(filter_key_hash);
            if (it != filters.end())
            {
                it->second->update(*filter_key.m_object, filter_key.m_materials, texture_cache);
                RENDERER_LOG_DEBUG(
                    "updated intersection filter with filter key hash 0x" FMT_UINT64_HEX ".",
                    filter_key_hash);
                continue;
            }

            // Create an intersection filter for this key.
            std::unique_ptr<IntersectionFilter> intersection_filter(
                new IntersectionFilter(
                    *filter_key.m_object,
                    filter_key.m_materials,
                    texture_cache));

            // Discard intersection filters that don't have any alpha masks.
            if (!intersection_filter->has_alpha_masks())
                continue;

            RENDERER_LOG_DEBUG(
                "created intersection filter for object \"%s\" with " FMT_SIZE_T " material%s "
                "(masks: %s, uvs: %s, filter key hash: 0x" FMT_UINT64_HEX ").",
                filter_key.m_object->get_path().c_str(),
                filter_key.m_materials.size(),
                filter_key.m_materials.size() > 1 ? "s" : "",
                pretty_size(intersection_filter->get_masks_memory_size()).c_str(),
                pretty_size(intersection_filter->get_masks_memory_size()).c_str(),
                filter_key_hash);

            // Store this intersection filter.
            filters[filter_key_hash] = intersection_filter.release();
        }
    }

    // Delete and remove intersection filters that don't correspond to any filter key.
    void delete_unreferenced_intersection_filters(
        const FilterKeySet&                 filter_keys,
        IntersectionFilterRepository&       repository)
    {
        if (repository.empty())
            return;

        std::set<std::uint64_t> hashes;

        for (const_each<FilterKeySet> i = filter_keys; i; ++i)
            hashes.insert(hash(*i));

        for (IntersectionFilterRepository::iterator i = repository.begin(); i != repository.end(); )
        {
            const std::uint64_t filter_key_hash = i->first;

            if (hashes.find(filter_key_hash) == hashes.end())
            {
                delete i->second;
                repository.erase(i++);

                RENDERER_LOG_DEBUG(
                    "deleted intersection filter with filter key hash 0x" FMT_UINT64_HEX ".",
                    filter_key_hash);
            }
            else ++i;
        }
    }

    void map_object_instances_to_intersection_filters(
        const IndexSet&                          object_instances,
        const IndexToFilterKeyMap&               object_instances_to_filter_keys,
        const IntersectionFilterRepository&      repository,
        std::vector<const IntersectionFilter*>&  filters)
    {
        if (!object_instances.empty())
        {
            const size_t max_object_instance_index =
                *max_element(object_instances.begin(), object_instances.end());

            filters.assign(max_object_instance_index + 1, nullptr);

            if (!repository.empty())
            {
                for (const_each<IndexToFilterKeyMap> i = object_instances_to_filter_keys; i; ++i)
                {
                    const size_t object_instance_index = i->first;
                    const FilterKey* filter_key = i->second;

                    const IntersectionFilterRepository::const_iterator it =
                        repository.find(hash(*filter_key));

                    if (it != repository.end())
                        filters[object_instance_index] = it->second;
                }
            }
        }
    }
}

void TriangleTree::update_intersection_filters()
{
    // Collect object instances.
    IndexSet object_instances;
    collect_object_instances(m_arguments.m_assembly, object_instances);

    // Create filter keys and map object instances to filter keys.
    FilterKeySet filter_keys;
    IndexToFilterKeyMap object_instances_to_filter_keys;
    create_filter_keys(
        m_arguments.m_assembly.object_instances(),
        object_instances,
        filter_keys,
        object_instances_to_filter_keys);

    // Create missing intersection filters and update existing ones.
    TextureCache texture_cache(m_arguments.m_project.get_texture_store());
    create_missing_intersection_filters(
        texture_cache,
        filter_keys,
        m_intersection_filters_repository);

    // Delete intersection filters that are no longer referenced.
    delete_unreferenced_intersection_filters(
        filter_keys,
        m_intersection_filters_repository);

    // Map object instances to intersection filters.
    map_object_instances_to_intersection_filters(
        object_instances,
        object_instances_to_filter_keys,
        m_intersection_filters_repository,
        m_intersection_filters);
}

void TriangleTree::delete_intersection_filters()
{
    for (const_each<IntersectionFilterRepository> i = m_intersection_filters_repository; i; ++i)
        delete i->second;

    m_intersection_filters_repository.clear();
    m_intersection_filters.clear();
}


//
// TriangleTreeFactory class implementation.
//

TriangleTreeFactory::TriangleTreeFactory(const TriangleTree::Arguments& arguments)
  : m_arguments(arguments)
{
}

std::unique_ptr<TriangleTree> TriangleTreeFactory::create()
{
    return std::unique_ptr<TriangleTree>(new TriangleTree(m_arguments));
}


//
// Utility class to convert a triangle to the desired precision if necessary,
// but avoid any work (in particular, no copy) if the source triangle already
// has the desired precision and can be used in-place.
//

namespace
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

bool TriangleLeafVisitor::visit(
    const TriangleTree::NodeType&           node,
    const Ray3d&                            ray,
    const RayInfo3d&                        ray_info,
    double&                                 distance
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    , bvh::TraversalStatistics&             stats
#endif
    )
{
    // Retrieve the pointer to the data of this leaf.
    const std::uint8_t* user_data = &node.get_user_data<std::uint8_t>();
    const std::uint32_t leaf_data_index = *reinterpret_cast<const std::uint32_t*>(user_data);
    const std::uint8_t* leaf_data =
        leaf_data_index == ~std::uint32_t(0)
            ? user_data + sizeof(std::uint32_t)         // triangles are stored in the leaf node
            : &m_tree.m_leaf_data[leaf_data_index];     // triangles are stored in the tree
    MemoryReader reader(leaf_data);

    // Sequentially intersect all triangles of the leaf.
    for (size_t triangle_index = node.get_item_index(),
                triangle_count = node.get_item_count();
                triangle_count--;
                triangle_index++)
    {
        FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_intersected_items.insert(1));

        // Retrieve the triangle's visibility flags.
        const std::uint32_t vis_flags = reader.read<std::uint32_t>();

        // Retrieve the number of motion segments for this triangle.
        const std::uint32_t motion_segment_count = reader.read<std::uint32_t>();

        // todo: get rid of this test by sorting triangles by their number of motion segments.
        if (motion_segment_count == 0)
        {
            // Check visibility flags.
            if (!(vis_flags & m_shading_point.m_ray.m_flags))
            {
                reader += sizeof(GTriangleType);
                continue;
            }

            // Read the triangle, converting it to the right format if necessary.
            const GTriangleType& triangle = reader.read<GTriangleType>();
            const TriangleReader triangle_reader(triangle);

            // Intersect the triangle.
            double t, u, v;
            if (triangle_reader.m_triangle.intersect(ray, t, u, v))
            {
                // Optionally filter intersections.
                if (m_has_intersection_filters)
                {
                    const TriangleKey& triangle_key = m_tree.m_triangle_keys[triangle_index];
                    const IntersectionFilter* filter =
                        m_tree.m_intersection_filters[triangle_key.get_object_instance_index()];
                    if (filter && !filter->accept(triangle_key, u, v))
                        continue;
                }

                m_hit_triangle = &triangle;
                m_hit_triangle_index = triangle_index;
                m_shading_point.m_ray.m_tmax = t;
                m_shading_point.m_bary[0] = static_cast<float>(u);
                m_shading_point.m_bary[1] = static_cast<float>(v);
            }
        }
        else
        {
            // Size in bytes of one motion step (i.e. one triangle).
            const size_t TriangleSize = 3 * sizeof(GVector3);

            // Check visibility flags.
            if (!(vis_flags & m_shading_point.m_ray.m_flags))
            {
                reader += (motion_segment_count + 1) * TriangleSize;
                continue;
            }

            // Advance to the motion step immediately before the ray time.
            const double base_time = m_shading_point.m_ray.m_time.m_normalized * motion_segment_count;
            const size_t base_index = truncate<size_t>(base_time);
            reader += base_index * TriangleSize;

            // Fetch and interpolate the triangle's vertices of the motion steps surrounding the ray time.
            const GScalar frac = static_cast<GScalar>(base_time - base_index);
            const GScalar one_minus_frac = GScalar(1.0) - frac;
            GVector3 v0 = reader.read<GVector3>() * one_minus_frac;
            GVector3 v1 = reader.read<GVector3>() * one_minus_frac;
            GVector3 v2 = reader.read<GVector3>() * one_minus_frac;
            v0 += reader.read<GVector3>() * frac;
            v1 += reader.read<GVector3>() * frac;
            v2 += reader.read<GVector3>() * frac;

            // Skip the remaining motion steps of this triangle.
            reader += (motion_segment_count - base_index - 1) * TriangleSize;

            // Build the triangle and convert it to the right format if necessary.
            const GTriangleType triangle(v0, v1, v2);
            const TriangleReader reader(triangle);

            // Intersect the triangle.
            double t, u, v;
            if (reader.m_triangle.intersect(ray, t, u, v))
            {
                // Optionally filter intersections.
                if (m_has_intersection_filters)
                {
                    const TriangleKey& triangle_key = m_tree.m_triangle_keys[triangle_index];
                    const IntersectionFilter* filter =
                        m_tree.m_intersection_filters[triangle_key.get_object_instance_index()];
                    if (filter && !filter->accept(triangle_key, u, v))
                        continue;
                }

                m_interpolated_triangle = triangle;
                m_hit_triangle = &m_interpolated_triangle;
                m_hit_triangle_index = triangle_index;
                m_shading_point.m_ray.m_tmax = t;
                m_shading_point.m_bary[0] = static_cast<float>(u);
                m_shading_point.m_bary[1] = static_cast<float>(v);
            }
        }
    }

    // Continue traversal.
    distance = m_shading_point.m_ray.m_tmax;
    return true;
}

void TriangleLeafVisitor::read_hit_triangle_data() const
{
    if (m_hit_triangle)
    {
        // Record a hit.
        m_shading_point.m_primitive_type = ShadingPoint::PrimitiveTriangle;

        // Copy the triangle key.
        const TriangleKey& triangle_key = m_tree.m_triangle_keys[m_hit_triangle_index];
        m_shading_point.m_object_instance_index = triangle_key.get_object_instance_index();
        m_shading_point.m_primitive_index = triangle_key.get_triangle_index();

        // Compute and store the support plane of the hit triangle.
        const TriangleReader reader(*m_hit_triangle);
        m_shading_point.m_triangle_support_plane.initialize(reader.m_triangle);
    }
}


//
// TriangleLeafProbeVisitor class implementation.
//

bool TriangleLeafProbeVisitor::visit(
    const TriangleTree::NodeType&           node,
    const Ray3d&                            ray,
    const RayInfo3d&                        ray_info,
    double&                                 distance
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    , bvh::TraversalStatistics&             stats
#endif
    )
{
    // Retrieve the pointer to the data of this leaf.
    const std::uint8_t* user_data = &node.get_user_data<std::uint8_t>();
    const std::uint32_t leaf_data_index = *reinterpret_cast<const std::uint32_t*>(user_data);
    const std::uint8_t* leaf_data =
        leaf_data_index == ~std::uint32_t(0)
            ? user_data + sizeof(std::uint32_t)         // triangles are stored in the leaf node
            : &m_tree.m_leaf_data[leaf_data_index];     // triangles are stored in the tree
    MemoryReader reader(leaf_data);

    // Sequentially intersect triangles until a hit is found.
    for (size_t triangle_count = node.get_item_count(); triangle_count--; )
    {
        FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_intersected_items.insert(1));

        // Retrieve the triangle's visibility flags.
        const std::uint32_t vis_flags = reader.read<std::uint32_t>();

        // Retrieve the number of motion segments for this triangle.
        const std::uint32_t motion_segment_count = reader.read<std::uint32_t>();

        // todo: get rid of this test by sorting triangles by their number of motion segments.
        if (motion_segment_count == 0)
        {
            // Check visibility flags.
            if (!(vis_flags & m_ray_flags))
            {
                reader += sizeof(GTriangleType);
                continue;
            }

            // Read the triangle, converting it to the right format if necessary.
            const GTriangleType& triangle = reader.read<GTriangleType>();
            const TriangleReader triangle_reader(triangle);

            // Intersect the triangle.
            if (triangle_reader.m_triangle.intersect(ray))
            {
                m_hit = true;
                return false;
            }
        }
        else
        {
            // Size in bytes of one motion step (i.e. one triangle).
            const size_t TriangleSize = 3 * sizeof(GVector3);

            // Check visibility flags.
            if (!(vis_flags & m_ray_flags))
            {
                reader += (motion_segment_count + 1) * TriangleSize;
                continue;
            }

            // Advance to the motion step immediately before the ray time.
            const double base_time = m_ray_time * motion_segment_count;
            const size_t base_index = truncate<size_t>(base_time);
            reader += base_index * TriangleSize;

            // Fetch and interpolate the triangle's vertices of the motion steps surrounding the ray time.
            const GScalar frac = static_cast<GScalar>(base_time - base_index);
            const GScalar one_minus_frac = GScalar(1.0) - frac;
            GVector3 v0 = reader.read<GVector3>() * one_minus_frac;
            GVector3 v1 = reader.read<GVector3>() * one_minus_frac;
            GVector3 v2 = reader.read<GVector3>() * one_minus_frac;
            v0 += reader.read<GVector3>() * frac;
            v1 += reader.read<GVector3>() * frac;
            v2 += reader.read<GVector3>() * frac;

            // Build the triangle and convert it to the right format if necessary.
            const GTriangleType triangle(v0, v1, v2);
            const TriangleReader triangle_reader(triangle);

            // Intersect the triangle.
            if (triangle_reader.m_triangle.intersect(ray))
            {
                m_hit = true;
                return false;
            }

            // Skip the remaining motion steps of this triangle.
            reader += (motion_segment_count - base_index - 1) * TriangleSize;
        }
    }

    // Continue traversal.
    distance = ray.m_tmax;
    return true;
}

}   // namespace renderer
