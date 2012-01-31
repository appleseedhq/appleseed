
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
#include "renderer/kernel/shading/shadingpoint.h"
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
#include "foundation/platform/timer.h"

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
    const UniqueID                  triangle_tree_uid,
    const GAABB3&                   bbox,
    const Assembly&                 assembly,
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
        "building triangle bvh #" FMT_UNIQUE_ID " (%s %s)...",
        arguments.m_triangle_tree_uid,
        pretty_int(size()).c_str(),
        plural(size(), "triangle").c_str());

    // Build the triangle tree.
    TriangleTreePartitioner partitioner(TriangleTreeMaxLeafSize);
    TriangleTreeBuilder builder;
    builder.build<DefaultWallclockTimer>(*this, partitioner);

    // Reorder the triangles according to the BVH ordering.
    vector<size_t> item_ordering(builder.get_item_ordering());
    assert(m_triangles.size() == item_ordering.size());
    large_item_reorder(
        &m_triangles[0],
        &item_ordering[0],
        m_triangles.size());

    // Collect and print triangle tree statistics.
    TriangleTreeStatistics tree_stats(*this, builder);
    RENDERER_LOG_DEBUG(
        "triangle bvh #" FMT_UNIQUE_ID " statistics:",
        arguments.m_triangle_tree_uid);
    tree_stats.print(global_logger());
}

TriangleTree::~TriangleTree()
{
    RENDERER_LOG_INFO(
        "deleting triangle bvh tree #" FMT_UNIQUE_ID "...",
        m_triangle_tree_uid);
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

            // Insert this triangle into the root leaf if it intersects the bounding box of the tree.
            if (intersect(arguments.m_bbox, v0, v1, v2))
            {
                const TriangleKey triangle_key(
                    region_info.get_object_instance_index(),
                    region_info.get_region_index(),
                    triangle_index);

                GAABB3 triangle_bbox;
                triangle_bbox.invalidate();
                triangle_bbox.insert(v0);
                triangle_bbox.insert(v1);
                triangle_bbox.insert(v2);

                insert(triangle_key, triangle_bbox);

                const GTriangleType triangle(v0, v1, v2);

                m_triangles.push_back(triangle);
            }
        }
    }
}


//
// TriangleTreeFactory class implementation.
//

TriangleTreeFactory::TriangleTreeFactory(
    const TriangleTree::Arguments&  arguments)
  : m_arguments(arguments)
{
}

auto_ptr<TriangleTree> TriangleTreeFactory::create()
{
    return auto_ptr<TriangleTree>(new TriangleTree(m_arguments));
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
    const vector<TriangleKey>&      items,
    const vector<GAABB3>&           bboxes,
    const size_t                    begin,
    const size_t                    end,
    const ShadingRay::RayType&      ray,
    const ShadingRay::RayInfoType&  ray_info,
    const double                    tmin,
    const double                    tmax,
    double&                         distance)
{
    // Sequentially intersect all triangles of this leaf.
    for (size_t i = begin; i < end; ++i)
    {
        const TriangleReader reader(m_tree.m_triangles[i]);

        // Intersect the triangle.
        double t, u, v;
        if (reader.m_triangle.intersect(m_shading_point.m_ray, t, u, v))
        {
            m_hit_triangle_index = i;
            m_shading_point.m_ray.m_tmax = t;
            m_shading_point.m_bary[0] = u;
            m_shading_point.m_bary[1] = v;
        }
    }

    // Continue traversal.
    distance = m_shading_point.m_ray.m_tmax;
    return true;
}

void TriangleLeafVisitor::read_hit_triangle_data() const
{
    if (m_hit_triangle_index != ~0)
    {
        // Record a hit.
        m_shading_point.m_hit = true;

        // Copy the triangle key.
        const TriangleKey& triangle_key = m_tree.m_items[m_hit_triangle_index];
        m_shading_point.m_object_instance_index = triangle_key.get_object_instance_index();
        m_shading_point.m_region_index = triangle_key.get_region_index();
        m_shading_point.m_triangle_index = triangle_key.get_triangle_index();

        // Compute and store the support plane of the hit triangle.
        const TriangleReader reader(m_tree.m_triangles[m_hit_triangle_index]);
        m_shading_point.m_triangle_support_plane.initialize(reader.m_triangle);
    }
}


//
// TriangleLeafProbeVisitor class implementation.
//

bool TriangleLeafProbeVisitor::visit(
    const vector<TriangleKey>&      items,
    const vector<GAABB3>&           bboxes,
    const size_t                    begin,
    const size_t                    end,
    const ShadingRay::RayType&      ray,
    const ShadingRay::RayInfoType&  ray_info,
    const double                    tmin,
    const double                    tmax,
    double&                         distance)
{
    // Sequentially intersect all triangles of this leaf.
    for (size_t i = begin; i < end; ++i)
    {
        const TriangleReader reader(m_tree.m_triangles[i]);

        // Intersect the triangle.
        if (reader.m_triangle.intersect(ray))
        {
            m_hit = true;
            return false;
        }
    }

    // Continue traversal.
    distance = ray.m_tmax;
    return true;
}

}   // namespace renderer
