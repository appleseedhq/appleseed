
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
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/math/area.h"
#include "foundation/math/permutation.h"
#include "foundation/math/treeoptimizer.h"
#ifndef APPLESEED_FOUNDATION_USE_SSE
#include "foundation/platform/sse.h"
#endif
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
    void collect_triangles(
        const TriangleTree::Arguments&  arguments,
        vector<TriangleKey>&            triangle_keys,
        vector<TriangleVertexInfo>&     triangle_vertex_infos,
        vector<GVector3>&               triangle_vertices,
        vector<AABB3d>&                 triangle_bboxes)
    {
        const size_t region_count = arguments.m_regions.size();
        for (size_t region_index = 0; region_index < region_count; ++region_index)
        {
            // Fetch the region info.
            const RegionInfo& region_info = arguments.m_regions[region_index];

            // Retrieve the object instance and its transformation.
            const ObjectInstance* object_instance =
                arguments.m_assembly.object_instances().get_by_index(
                    region_info.get_object_instance_index());
            assert(object_instance);
            const Transformd& transform = object_instance->get_transform();

            // Retrieve the object.
            Object& object = object_instance->get_object();

            // Retrieve the region kit of the object.
            Access<RegionKit> region_kit(&object.get_region_kit());

            // Retrieve the region.
            const IRegion* region = (*region_kit)[region_info.get_region_index()];

            // Retrieve the tessellation of the region.
            Access<StaticTriangleTess> tess(&region->get_static_triangle_tess());
            const size_t motion_segment_count = tess->get_motion_segment_count();

            // Collect all triangles of the region that intersect the bounding box of the tree.
            const size_t triangle_count = tess->m_primitives.size();
            for (size_t triangle_index = 0; triangle_index < triangle_count; ++triangle_index)
            {
                // Fetch the triangle.
                const Triangle& triangle = tess->m_primitives[triangle_index];

                // Retrieve object space vertices of the triangle.
                const GVector3& v0_os = tess->m_vertices[triangle.m_v0];
                const GVector3& v1_os = tess->m_vertices[triangle.m_v1];
                const GVector3& v2_os = tess->m_vertices[triangle.m_v2];

                // Transform triangle vertices to assembly space.
                const GVector3 v0 = transform.transform_point_to_parent(v0_os);
                const GVector3 v1 = transform.transform_point_to_parent(v1_os);
                const GVector3 v2 = transform.transform_point_to_parent(v2_os);

                // Compute the bounding box of the triangle in assembly space.
                GAABB3 triangle_bbox;
                triangle_bbox.invalidate();
                triangle_bbox.insert(v0);
                triangle_bbox.insert(v1);
                triangle_bbox.insert(v2);

                if (motion_segment_count > 0)
                {
                    // Update the bounding of the triangle to enclose its full motion over the shutter interval.
                    for (size_t ms_index = 0; ms_index < motion_segment_count; ++ms_index)
                    {
                        triangle_bbox.insert(transform.transform_point_to_parent(tess->get_vertex_pose(triangle.m_v0, ms_index)));
                        triangle_bbox.insert(transform.transform_point_to_parent(tess->get_vertex_pose(triangle.m_v1, ms_index)));
                        triangle_bbox.insert(transform.transform_point_to_parent(tess->get_vertex_pose(triangle.m_v2, ms_index)));
                    }

                    // Ignore triangles that are degenerate over the entire shutter interval.
                    if (triangle_bbox.rank() < 2)
                        continue;

                    // Ignore triangles that don't intersect the tree.
                    if (!GAABB3::overlap(arguments.m_bbox, triangle_bbox))
                        continue;
                }
                else
                {
                    // Ignore degenerate triangles.
                    if (square_area(v0, v1, v2) == GScalar(0.0))
                        continue;

                    // Ignore triangles that don't intersect the tree.
                    if (!intersect(arguments.m_bbox, v0, v1, v2))
                        continue;
                }

                // Store the triangle key.
                triangle_keys.push_back(
                    TriangleKey(
                        region_info.get_object_instance_index(),
                        region_info.get_region_index(),
                        triangle_index));

                // Store the index of the first triangle vertex and the number of motion segments.
                triangle_vertex_infos.push_back(
                    TriangleVertexInfo(
                        triangle_vertices.size(),
                        motion_segment_count));

                // Store the triangle vertices.
                triangle_vertices.push_back(v0);
                triangle_vertices.push_back(v1);
                triangle_vertices.push_back(v2);
                for (size_t ms_index = 0; ms_index < motion_segment_count; ++ms_index)
                {
                    triangle_vertices.push_back(transform.transform_point_to_parent(tess->get_vertex_pose(triangle.m_v0, ms_index)));
                    triangle_vertices.push_back(transform.transform_point_to_parent(tess->get_vertex_pose(triangle.m_v1, ms_index)));
                    triangle_vertices.push_back(transform.transform_point_to_parent(tess->get_vertex_pose(triangle.m_v2, ms_index)));
                }

                // Store the triangle bounding box.
                triangle_bboxes.push_back(AABB3d(triangle_bbox));
            }
        }
    }

    class TriangleItemHandler
    {
      public:
        TriangleItemHandler(
            const vector<TriangleVertexInfo>&   triangle_vertex_infos,
            const vector<GVector3>&             triangle_vertices,
            const vector<AABB3d>&               triangle_bboxes)
          : m_triangle_vertex_infos(triangle_vertex_infos)
          , m_triangle_vertices(triangle_vertices)
          , m_triangle_bboxes(triangle_bboxes)
        {
        }

        double get_bbox_grow_eps() const
        {
            return 2.0e-9;
        }

        AABB3d clip(
            const size_t    item_index,
            const size_t    dimension,
            const double    slab_min,
            const double    slab_max) const
        {
            const TriangleVertexInfo& vertex_info = m_triangle_vertex_infos[item_index];

            if (vertex_info.m_motion_segment_count > 0)
            {
                AABB3d triangle_bbox = m_triangle_bboxes[item_index];

                if (triangle_bbox.min[dimension] < slab_min)
                    triangle_bbox.min[dimension] = slab_min;

                if (triangle_bbox.max[dimension] > slab_max)
                    triangle_bbox.max[dimension] = slab_max;

                return triangle_bbox;
            }

#ifdef APPLESEED_FOUNDATION_USE_SSE

            SSE_ALIGN const Vector3d v0(m_triangle_vertices[vertex_info.m_vertex_index + 0]);
            SSE_ALIGN const Vector3d v1(m_triangle_vertices[vertex_info.m_vertex_index + 1]);
            SSE_ALIGN const Vector3d v2(m_triangle_vertices[vertex_info.m_vertex_index + 2]);

            const double v0d = v0[dimension];
            const double v1d = v1[dimension];
            const double v2d = v2[dimension];

            const int v0_ge_min = v0d >= slab_min ? 1 : 0;
            const int v0_le_max = v0d <= slab_max ? 1 : 0;
            const int v1_ge_min = v1d >= slab_min ? 1 : 0;
            const int v1_le_max = v1d <= slab_max ? 1 : 0;
            const int v2_ge_min = v2d >= slab_min ? 1 : 0;
            const int v2_le_max = v2d <= slab_max ? 1 : 0;

            sse2d bbox_min_xy = set1pd(+numeric_limits<double>::max());
            sse2d bbox_min_zz = set1pd(+numeric_limits<double>::max());
            sse2d bbox_max_xy = set1pd(-numeric_limits<double>::max());
            sse2d bbox_max_zz = set1pd(-numeric_limits<double>::max());

            const sse2d v0_xy = loadpd(&v0.x);
            const sse2d v0_zz = set1pd(v0.z);
            const sse2d v1_xy = loadpd(&v1.x);
            const sse2d v1_zz = set1pd(v1.z);
            const sse2d v2_xy = loadpd(&v2.x);
            const sse2d v2_zz = set1pd(v2.z);

            if (v0_ge_min & v0_le_max)
            {
                bbox_min_xy = minpd(bbox_min_xy, v0_xy);
                bbox_max_xy = maxpd(bbox_max_xy, v0_xy);
                bbox_min_zz = minpd(bbox_min_zz, v0_zz);
                bbox_max_zz = maxpd(bbox_max_zz, v0_zz);
            }

            if (v1_ge_min & v1_le_max)
            {
                bbox_min_xy = minpd(bbox_min_xy, v1_xy);
                bbox_max_xy = maxpd(bbox_max_xy, v1_xy);
                bbox_min_zz = minpd(bbox_min_zz, v1_zz);
                bbox_max_zz = maxpd(bbox_max_zz, v1_zz);
            }

            if (v2_ge_min & v2_le_max)
            {
                bbox_min_xy = minpd(bbox_min_xy, v2_xy);
                bbox_max_xy = maxpd(bbox_max_xy, v2_xy);
                bbox_min_zz = minpd(bbox_min_zz, v2_zz);
                bbox_max_zz = maxpd(bbox_max_zz, v2_zz);
            }

            const int v0v1_cross_min = v0_ge_min ^ v1_ge_min;
            const int v0v1_cross_max = v0_le_max ^ v1_le_max;
            const int v1v2_cross_min = v1_ge_min ^ v2_ge_min;
            const int v1v2_cross_max = v1_le_max ^ v2_le_max;
            const int v2v0_cross_min = v2_ge_min ^ v0_ge_min;
            const int v2v0_cross_max = v2_le_max ^ v0_le_max;

            if (v0v1_cross_min | v0v1_cross_max)
            {
                const double rcp_v0v1 = 1.0 / (v1[dimension] - v0[dimension]);

                if (v0v1_cross_min)
                {
                    const double t = (slab_min - v0[dimension]) * rcp_v0v1;
                    assert(t >= 0.0 && t <= 1.0);

                    const sse2d mt = set1pd(t);
                    const sse2d mt1 = set1pd(1.0 - t);
                    const sse2d p_xy = addpd(mulpd(v0_xy, mt1), mulpd(v1_xy, mt));
                    const sse2d p_zz = addpd(mulpd(v0_zz, mt1), mulpd(v1_zz, mt));

                    bbox_min_xy = minpd(bbox_min_xy, p_xy);
                    bbox_max_xy = maxpd(bbox_max_xy, p_xy);
                    bbox_min_zz = minpd(bbox_min_zz, p_zz);
                    bbox_max_zz = maxpd(bbox_max_zz, p_zz);
                }

                if (v0v1_cross_max)
                {
                    const double t = (slab_max - v0[dimension]) * rcp_v0v1;
                    assert(t >= 0.0 && t <= 1.0);

                    const sse2d mt = set1pd(t);
                    const sse2d mt1 = set1pd(1.0 - t);
                    const sse2d p_xy = addpd(mulpd(v0_xy, mt1), mulpd(v1_xy, mt));
                    const sse2d p_zz = addpd(mulpd(v0_zz, mt1), mulpd(v1_zz, mt));

                    bbox_min_xy = minpd(bbox_min_xy, p_xy);
                    bbox_max_xy = maxpd(bbox_max_xy, p_xy);
                    bbox_min_zz = minpd(bbox_min_zz, p_zz);
                    bbox_max_zz = maxpd(bbox_max_zz, p_zz);
                }
            }

            if (v1v2_cross_min | v1v2_cross_max)
            {
                const double rcp_v1v2 = 1.0 / (v2[dimension] - v1[dimension]);

                if (v1v2_cross_min)
                {
                    const double t = (slab_min - v1[dimension]) * rcp_v1v2;
                    assert(t >= 0.0 && t <= 1.0);

                    const sse2d mt = set1pd(t);
                    const sse2d mt1 = set1pd(1.0 - t);
                    const sse2d p_xy = addpd(mulpd(v1_xy, mt1), mulpd(v2_xy, mt));
                    const sse2d p_zz = addpd(mulpd(v1_zz, mt1), mulpd(v2_zz, mt));

                    bbox_min_xy = minpd(bbox_min_xy, p_xy);
                    bbox_max_xy = maxpd(bbox_max_xy, p_xy);
                    bbox_min_zz = minpd(bbox_min_zz, p_zz);
                    bbox_max_zz = maxpd(bbox_max_zz, p_zz);
                }

                if (v1v2_cross_max)
                {
                    const double t = (slab_max - v1[dimension]) * rcp_v1v2;
                    assert(t >= 0.0 && t <= 1.0);

                    const sse2d mt = set1pd(t);
                    const sse2d mt1 = set1pd(1.0 - t);
                    const sse2d p_xy = addpd(mulpd(v1_xy, mt1), mulpd(v2_xy, mt));
                    const sse2d p_zz = addpd(mulpd(v1_zz, mt1), mulpd(v2_zz, mt));

                    bbox_min_xy = minpd(bbox_min_xy, p_xy);
                    bbox_max_xy = maxpd(bbox_max_xy, p_xy);
                    bbox_min_zz = minpd(bbox_min_zz, p_zz);
                    bbox_max_zz = maxpd(bbox_max_zz, p_zz);
                }
            }

            if (v2v0_cross_min | v2v0_cross_max)
            {
                const double rcp_v2v0 = 1.0 / (v0[dimension] - v2[dimension]);

                if (v2v0_cross_min)
                {
                    const double t = (slab_min - v2[dimension]) * rcp_v2v0;
                    assert(t >= 0.0 && t <= 1.0);

                    const sse2d mt = set1pd(t);
                    const sse2d mt1 = set1pd(1.0 - t);
                    const sse2d p_xy = addpd(mulpd(v2_xy, mt1), mulpd(v0_xy, mt));
                    const sse2d p_zz = addpd(mulpd(v2_zz, mt1), mulpd(v0_zz, mt));

                    bbox_min_xy = minpd(bbox_min_xy, p_xy);
                    bbox_max_xy = maxpd(bbox_max_xy, p_xy);
                    bbox_min_zz = minpd(bbox_min_zz, p_zz);
                    bbox_max_zz = maxpd(bbox_max_zz, p_zz);
                }

                if (v2v0_cross_max)
                {
                    const double t = (slab_max - v2[dimension]) * rcp_v2v0;
                    assert(t >= 0.0 && t <= 1.0);

                    const sse2d mt = set1pd(t);
                    const sse2d mt1 = set1pd(1.0 - t);
                    const sse2d p_xy = addpd(mulpd(v2_xy, mt1), mulpd(v0_xy, mt));
                    const sse2d p_zz = addpd(mulpd(v2_zz, mt1), mulpd(v0_zz, mt));

                    bbox_min_xy = minpd(bbox_min_xy, p_xy);
                    bbox_max_xy = maxpd(bbox_max_xy, p_xy);
                    bbox_min_zz = minpd(bbox_min_zz, p_zz);
                    bbox_max_zz = maxpd(bbox_max_zz, p_zz);
                }
            }

            SSE_ALIGN AABB3d bbox;

            storepd(&bbox.min.x, bbox_min_xy);
            storesd(&bbox.min.z, bbox_min_zz);
            storeupd(&bbox.max.x, bbox_max_xy);
            storesd(&bbox.max.z, bbox_max_zz);

            if (bbox.min[dimension] < slab_min)
                bbox.min[dimension] = slab_min;

            if (bbox.max[dimension] > slab_max)
                bbox.max[dimension] = slab_max;

#else

            const Vector3d v0(m_triangle_vertices[vertex_info.m_vertex_index + 0]);
            const Vector3d v1(m_triangle_vertices[vertex_info.m_vertex_index + 1]);
            const Vector3d v2(m_triangle_vertices[vertex_info.m_vertex_index + 2]);

            const int v0_ge_min = v0[dimension] >= slab_min ? 1 : 0;
            const int v0_le_max = v0[dimension] <= slab_max ? 1 : 0;
            const int v1_ge_min = v1[dimension] >= slab_min ? 1 : 0;
            const int v1_le_max = v1[dimension] <= slab_max ? 1 : 0;
            const int v2_ge_min = v2[dimension] >= slab_min ? 1 : 0;
            const int v2_le_max = v2[dimension] <= slab_max ? 1 : 0;

            AABB3d bbox;
            bbox.invalidate();

            if (v0_ge_min & v0_le_max)
                bbox.insert(v0);

            if (v1_ge_min & v1_le_max)
                bbox.insert(v1);

            if (v2_ge_min & v2_le_max)
                bbox.insert(v2);

            if (v0_ge_min != v1_ge_min)
                bbox.insert(segment_plane_intersection(v0, v1, dimension, slab_min));

            if (v0_le_max != v1_le_max)
                bbox.insert(segment_plane_intersection(v0, v1, dimension, slab_max));

            if (v1_ge_min != v2_ge_min)
                bbox.insert(segment_plane_intersection(v1, v2, dimension, slab_min));

            if (v1_le_max != v2_le_max)
                bbox.insert(segment_plane_intersection(v1, v2, dimension, slab_max));

            if (v2_ge_min != v0_ge_min)
                bbox.insert(segment_plane_intersection(v2, v0, dimension, slab_min));

            if (v2_le_max != v0_le_max)
                bbox.insert(segment_plane_intersection(v2, v0, dimension, slab_max));

#endif

            return bbox;
        }

        bool intersect(
            const size_t    item_index,
            const AABB3d&   bbox) const
        {
            const TriangleVertexInfo& vertex_info = m_triangle_vertex_infos[item_index];
            return
                vertex_info.m_motion_segment_count > 0
                    ? AABB3d::overlap(bbox, m_triangle_bboxes[item_index])
                    : foundation::intersect(
                          bbox,
                          Vector3d(m_triangle_vertices[vertex_info.m_vertex_index + 0]),
                          Vector3d(m_triangle_vertices[vertex_info.m_vertex_index + 1]),
                          Vector3d(m_triangle_vertices[vertex_info.m_vertex_index + 2]));
        }

      private:
        const vector<TriangleVertexInfo>&   m_triangle_vertex_infos;
        const vector<GVector3>&             m_triangle_vertices;
        const vector<AABB3d>&               m_triangle_bboxes;

        static Vector3d segment_plane_intersection(
            const Vector3d& a,
            const Vector3d& b,
            const size_t    d,
            const double    x)
        {
            const double ab = b[d] - a[d];

            if (ab == 0.0)
                return a;

            const double t = (x - a[d]) / ab;

            assert(t >= 0.0 && t <= 1.0);

            Vector3d result;
            result = a * (1.0 - t) + b * t;
            result[d] = x;

            return result;
        }
    };

    struct LeafEncoder
    {
        static size_t compute_size(
            const vector<TriangleVertexInfo>&   triangle_vertex_infos,
            const vector<size_t>&               triangle_indices,
            const size_t                        item_begin,
            const size_t                        item_count)
        {
            size_t size = 0;

            for (size_t i = 0; i < item_count; ++i)
            {
                const size_t triangle_index = triangle_indices[item_begin + i];
                const TriangleVertexInfo& vertex_info = triangle_vertex_infos[triangle_index];

                size += sizeof(uint32);         // motion segment count

                if (vertex_info.m_motion_segment_count == 0)
                    size += sizeof(GTriangleType);
                else size += (vertex_info.m_motion_segment_count + 1) * 3 * sizeof(GVector3);
            }

            return size;
        }

        static void encode(
            const vector<TriangleVertexInfo>&   triangle_vertex_infos,
            const vector<GVector3>&             triangle_vertices,
            const vector<size_t>&               triangle_indices,
            const size_t                        item_begin,
            const size_t                        item_count,
            MemoryWriter&                       writer)
        {
            for (size_t i = 0; i < item_count; ++i)
            {
                const size_t triangle_index = triangle_indices[item_begin + i];
                const TriangleVertexInfo& vertex_info = triangle_vertex_infos[triangle_index];

                writer.write(static_cast<uint32>(vertex_info.m_motion_segment_count));

                if (vertex_info.m_motion_segment_count == 0)
                {
                    writer.write(
                        GTriangleType(
                            triangle_vertices[vertex_info.m_vertex_index + 0],
                            triangle_vertices[vertex_info.m_vertex_index + 1],
                            triangle_vertices[vertex_info.m_vertex_index + 2]));
                }
                else
                {
                    writer.write(
                        &triangle_vertices[vertex_info.m_vertex_index],
                        (vertex_info.m_motion_segment_count + 1) * 3 * sizeof(GVector3));
                }
            }
        }
    };
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
                LeafEncoder::compute_size(
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
                LeafEncoder::compute_size(
                    triangle_vertex_infos,
                    triangle_indices,
                    item_begin,
                    item_count);

            MemoryWriter user_data_writer(&node.get_user_data<uint8>());

            if (leaf_size <= NodeType::MaxUserDataSize - 4)
            {
                user_data_writer.write<uint32>(~0);

                LeafEncoder::encode(
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

                LeafEncoder::encode(
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
