
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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
#include "renderer/modeling/object/iregion.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/object/regionkit.h"
#include "renderer/modeling/object/triangle.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/objectinstance.h"

// appleseed.foundation headers.
#include "foundation/math/area.h"
#include "foundation/math/permutation.h"
#include "foundation/math/treeoptimizer.h"
#include "foundation/platform/timer.h"
#include "foundation/utility/memory.h"

// Standard headers.
#include <cassert>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// Specializations.
//

typedef bvh::SAHPartitioner<
    TriangleTree
> TriangleTreePartitioner;

typedef bvh::Builder<
    TriangleTree,
    TriangleTreePartitioner
> TriangleTreeBuilder;

typedef bvh::TreeStatistics<
    TriangleTree,
    TriangleTreeBuilder
> TriangleTreeStatistics;


//
// TriangleTree class implementation.
//

TriangleTree::Arguments::Arguments(
    const UniqueID      triangle_tree_uid,
    const GAABB3&       bbox,
    const Assembly&     assembly,
    const RegionInfoVector& regions)
  : m_triangle_tree_uid(triangle_tree_uid)
  , m_bbox(bbox)
  , m_assembly(assembly)
  , m_regions(regions)
{
}

TriangleTree::TriangleTree(const Arguments& arguments)
  : m_triangle_tree_uid(arguments.m_triangle_tree_uid)
{
    // Collect triangles intersecting the bounding box of this tree.
    collect_triangles(arguments);

    // Log a progress message.
    RENDERER_LOG_INFO(
        "building triangle tree #" FMT_UNIQUE_ID " (%s %s)...",
        arguments.m_triangle_tree_uid,
        pretty_int(size()).c_str(),
        plural(size(), "triangle").c_str());

    // Build the triangle tree.
    TriangleTreePartitioner partitioner(
        TriangleTreeMaxLeafSize,
        TriangleTreeInteriorNodeTraversalCost,
        TriangleTreeTriangleIntersectionCost);
    TriangleTreeBuilder builder;
    builder.build<DefaultWallclockTimer>(*this, partitioner);

    // Get rid of the triangle bounding boxes as we no longer need them.
    clear_release_memory(m_bboxes);

    if (!m_triangles.empty())
    {
        const vector<size_t>& ordering = partitioner.get_item_ordering();

        assert(m_triangle_keys.size() == ordering.size());
        assert(m_triangles.size() == ordering.size());

        // Reorder the triangle keys according to the tree ordering.
        vector<TriangleKey> temp_triangle_keys(ordering.size());
        small_item_reorder(
            &m_triangle_keys[0],
            &temp_triangle_keys[0],
            &ordering[0],
            ordering.size());
        clear_release_memory(temp_triangle_keys);

        // Reorder the triangles according to the tree ordering.
        vector<size_t> tags(ordering.size());
        large_item_reorder(
            &m_triangles[0],
            &tags[0],
            &ordering[0],
            ordering.size());

        // Store triangles in the tree leaves whenever possible.
        move_triangles_to_leaves();

        // Optimize the tree layout in memory.
        TreeOptimizer<NodeType> tree_optimizer;
        tree_optimizer.optimize_node_layout(m_nodes, TriangleTreeSubtreeDepth);
    }

    // Collect and print triangle tree statistics.
    TriangleTreeStatistics tree_stats(*this, builder);
    RENDERER_LOG_DEBUG(
        "triangle tree #" FMT_UNIQUE_ID " statistics:",
        arguments.m_triangle_tree_uid);
    tree_stats.print(global_logger());
}

TriangleTree::~TriangleTree()
{
    RENDERER_LOG_INFO(
        "deleting triangle tree #" FMT_UNIQUE_ID "...",
        m_triangle_tree_uid);
}

size_t TriangleTree::get_memory_size() const
{
    return
          TreeType::get_memory_size()
        - sizeof(*static_cast<const TreeType*>(this))
        + sizeof(*this)
        + m_triangle_keys.capacity() * sizeof(TriangleKey)
        + m_triangles.capacity() * sizeof(GTriangleType);
}

void TriangleTree::collect_triangles(const Arguments& arguments)
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

            // Calculate the (square of the) area of this triangle.
            const GScalar triangle_square_area = square_area(v0, v1, v2);

            // Ignore degenerate triangles.
            if (triangle_square_area == GScalar(0.0))
                continue;

            // Insert this triangle into the tree if it intersects the tree's bounding box.
            if (intersect(arguments.m_bbox, v0, v1, v2))
            {
                // Insert the bounding box of the triangle into the tree's root leaf.
                GAABB3 triangle_bbox;
                triangle_bbox.invalidate();
                triangle_bbox.insert(v0);
                triangle_bbox.insert(v1);
                triangle_bbox.insert(v2);
                insert(AABB3d(triangle_bbox));

                // Store the triangle key.
                m_triangle_keys.push_back(
                    TriangleKey(
                        region_info.get_object_instance_index(),
                        region_info.get_region_index(),
                        triangle_index));

                // Store the triangle.
                m_triangles.push_back(GTriangleType(v0, v1, v2));
            }
        }
    }
}

void TriangleTree::move_triangles_to_leaves()
{
    size_t leaf_count = 0;
    size_t fat_leaf_count = 0;

    size_t tree_triangle_count = 0;

    const size_t node_count = m_nodes.size();

    for (size_t i = 0; i < node_count; ++i)
    {
        NodeType& node = m_nodes[i];

        if (node.is_leaf())
        {
            ++leaf_count;

            const size_t item_begin = node.get_item_index();
            const size_t item_count = node.get_item_count();

            if (item_count <= NodeType::MaxUserDataSize / sizeof(GTriangleType))
            {
                ++fat_leaf_count;

                GTriangleType* user_data = &node.get_user_data<GTriangleType>();

                for (size_t j = 0; j < item_count; ++j)
                    user_data[j] = m_triangles[item_begin + j];
            }
            else
            {
                node.set_item_index(tree_triangle_count);

                for (size_t j = 0; j < item_count; ++j)
                    m_triangles[tree_triangle_count++] = m_triangles[item_begin + j];
            }
        }
    }

    m_triangles.resize(tree_triangle_count);
    shrink_to_fit(m_triangles);

    RENDERER_LOG_DEBUG(
        "fat triangle tree leaves: %s",
        pretty_percent(fat_leaf_count, leaf_count).c_str());
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
