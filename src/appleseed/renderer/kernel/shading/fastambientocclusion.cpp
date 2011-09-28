
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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
#include "fastambientocclusion.h"

// appleseed.renderer headers.
#include "renderer/kernel/tessellation/statictessellation.h"
#include "renderer/modeling/object/iregion.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/object/regionkit.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/utility/bbox.h"

// appleseed.foundation headers.
#include "foundation/math/intersection.h"
#include "foundation/math/sampling.h"
#include "foundation/math/transform.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/lazy.h"

// Standard headers.
#include <algorithm>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// AOVoxelTree class implementation.
//

// Constructor, build the tree for a given scene.
AOVoxelTree::AOVoxelTree(
    const Scene&    scene,
    const GScalar   max_extent_fraction)
{
    assert(max_extent_fraction > GScalar(0.0));

    // Print a progress message.
    RENDERER_LOG_INFO("building ambient occlusion voxel tree...");

    // Compute the bounding box of the scene, in world space.
    const GAABB3 scene_bbox =
        compute_parent_bbox<GAABB3>(
            scene.assembly_instances().begin(),
            scene.assembly_instances().end());

    // Compute the maximum extent of a leaf, in world space.
    const GScalar max_extent = max_extent_fraction * max_value(scene_bbox.extent());

    // Build the tree.
    BuilderType builder(m_tree, scene_bbox, max_extent);
    build(scene, builder);
    builder.complete();

    // Print statistics.
    TreeStatisticsType tree_stats(m_tree, builder);
    RENDERER_LOG_DEBUG("ambient occlusion voxel tree statistics:");
    tree_stats.print(global_logger());
}

// Dump all solid leaves of the tree to disk, as an .obj mesh file.
void AOVoxelTree::dump_solid_leaves_to_disk(const string& filename) const
{
    RENDERER_LOG_INFO(
        "writing ambient occlusion voxel tree file %s...",
        filename.c_str());

    if (m_tree.dump_solid_leaves_to_disk(filename))
    {
        RENDERER_LOG_INFO(
            "wrote ambient occlusion voxel tree file %s",
            filename.c_str());
    }
    else
    {
        RENDERER_LOG_ERROR(
            "failed to write ambient occlusion voxel tree file %s: i/o error",
            filename.c_str());
    }
}

// Dump the entire tree to disk, in proprietary binary format.
void AOVoxelTree::dump_tree_to_disk(const string& filename) const
{
    RENDERER_LOG_INFO(
        "writing ambient occlusion voxel tree file %s...",
        filename.c_str());

    if (m_tree.dump_tree_to_disk(filename))
    {
        RENDERER_LOG_INFO(
            "wrote ambient occlusion voxel tree file %s",
            filename.c_str());
    }
    else
    {
        RENDERER_LOG_ERROR(
            "failed to write ambient occlusion voxel tree file %s: i/o error",
            filename.c_str());
    }
}

namespace
{
    //
    // A triangle-bounding box intersection predicate.
    //

    class TriangleIntersector
    {
      public:
        // Constructor.
        TriangleIntersector(
            const GVector3& v0,
            const GVector3& v1,
            const GVector3& v2)
          : m_v0(v0)
          , m_v1(v1)
          , m_v2(v2)
        {
            // Compute and store the bounding box of the triangle.
            m_triangle_bbox.invalidate();
            m_triangle_bbox.insert(m_v0);
            m_triangle_bbox.insert(m_v1);
            m_triangle_bbox.insert(m_v2);
        }

        // Return whether the item intersect a given bounding box.
        bool intersect(const GAABB3& bbox) const
        {
            if (m_triangle_bbox.overlaps(bbox))
                return foundation::intersect(bbox, m_v0, m_v1, m_v2);
            else return false;
        }

      private:
        const GVector3  m_v0;
        const GVector3  m_v1;
        const GVector3  m_v2;
        GAABB3          m_triangle_bbox;

    };
}

// Build the tree.
void AOVoxelTree::build(
    const Scene&    scene,
    BuilderType&    builder)
{
    // Loop over the assembly instances of the scene.
    for (const_each<AssemblyInstanceContainer> i = scene.assembly_instances(); i; ++i)
    {
        // Retrieve the assembly instance.
        const AssemblyInstance& assembly_instance = *i;

        // Retrieve the assembly.
        const Assembly& assembly = assembly_instance.get_assembly();

        // Loop over the object instances of the assembly.
        for (const_each<ObjectInstanceContainer> i = assembly.object_instances(); i; ++i)
        {
            // Retrieve the object instance.
            const ObjectInstance& object_instance = *i;

            // Compute the object space to world space transformation.
            const Transformd& transform =
                assembly_instance.get_transform() * object_instance.get_transform();

            // Retrieve the object.
            Object& object = object_instance.get_object();

            // Retrieve the region kit of the object.
            Access<RegionKit> region_kit(&object.get_region_kit());

            // Loop over the regions of the object.
            const size_t region_count = region_kit->size();
            for (size_t region_index = 0; region_index < region_count; ++region_index)
            {
                // Retrieve the region.
                const IRegion* region = (*region_kit)[region_index];

                // Retrieve the tessellation of the region.
                Access<StaticTriangleTess> tess(&region->get_static_triangle_tess());

                // Push all triangles of the region into the tree.
                const size_t triangle_count = tess->m_primitives.size();
                for (size_t triangle_index = 0; triangle_index < triangle_count; ++triangle_index)
                {
                    // Fetch the triangle.
                    const Triangle& triangle = tess->m_primitives[triangle_index];

                    // Retrieve object instance space vertices of the triangle.
                    const GVector3& v0_os = tess->m_vertices[triangle.m_v0];
                    const GVector3& v1_os = tess->m_vertices[triangle.m_v1];
                    const GVector3& v2_os = tess->m_vertices[triangle.m_v2];

                    // Transform triangle vertices to world space.
                    const GVector3 v0(transform.transform_point_to_parent(v0_os));
                    const GVector3 v1(transform.transform_point_to_parent(v1_os));
                    const GVector3 v2(transform.transform_point_to_parent(v2_os));

                    // Push the triangle into the tree.
                    TriangleIntersector intersector(v0, v1, v2);
                    builder.push(intersector);
                }
            }
        }
    }
}


//
// AOVoxelTreeIntersector class implementation.
//

// Constructor, binds the intersector to a given tree.
AOVoxelTreeIntersector::AOVoxelTreeIntersector(const AOVoxelTree& tree)
  : m_tree(tree)
{
}

// Destructor.
AOVoxelTreeIntersector::~AOVoxelTreeIntersector()
{
#ifdef FOUNDATION_VOXEL_ENABLE_TRAVERSAL_STATS
    RENDERER_LOG_DEBUG("ambient occlusion voxel traversal statistics:");
    m_traversal_stats.print(global_logger());
#endif
}

// Trace a world space ray through the voxel tree.
bool AOVoxelTreeIntersector::trace(
    ShadingRay::RayType ray,
    const bool          solid,
    double&             distance) const
{
    // Compute ray info once for the entire traversal.
    const ShadingRay::RayInfoType ray_info(ray);

    // Retrieve the voxel tree.
    const AOVoxelTree::TreeType& tree = m_tree.m_tree;
    const AABB3d tree_bbox(tree.get_bbox());

    // Clip the ray against the bounding box of the tree.
    if (clip(ray, ray_info, tree_bbox))
    {
        // Intersect the ray with the tree.
        IntersectorType intersector;
        const bool hit =
            intersector.intersect(
                tree,
                ray,
                ray_info,
                solid,
                distance
#ifdef FOUNDATION_VOXEL_ENABLE_TRAVERSAL_STATS
                , m_traversal_stats
#endif
                );
        if (!hit && !solid)
            return intersect(ray, ray_info, tree_bbox, distance);
        else return hit;
    }

    // No intersection.
    return false;
}


//
// Compute fast ambient occlusion at a given point in space.
//

double compute_fast_ambient_occlusion(
    const SamplingContext&          sampling_context,
    const AOVoxelTreeIntersector&   intersector,
    const Vector3d&                 point,
    const Vector3d&                 geometric_normal,
    const Basis3d&                  basis,
    const double                    max_distance,
    const size_t                    sample_count,
    double&                         min_distance)
{
    // Create a sampling context.
    SamplingContext child_context = sampling_context.split(2, sample_count);

    // Construct an ambient occlusion ray.
    ShadingRay::RayType ao_ray;
    ao_ray.m_org = point;
    ao_ray.m_tmin = 0.0;
    ao_ray.m_tmax = max_distance;

    size_t computed_samples = 0;
    size_t occluded_samples = 0;

    min_distance = max_distance;

    for (size_t i = 0; i < sample_count; ++i)
    {
        // Generate a cosine-weighted direction over the unit hemisphere.
        const Vector2d s = child_context.next_vector2<2>();
        ao_ray.m_dir = sample_hemisphere_cosine(s);

        // Transform the direction to world space.
        ao_ray.m_dir = basis.transform_to_parent(ao_ray.m_dir);

        // Don't cast rays on or below the geometric surface.
        if (dot(ao_ray.m_dir, geometric_normal) <= 0.0)
            continue;

        // Count the number of computed samples.
        ++computed_samples;

        // Trace the ambient occlusion ray and count the number of occluded samples.
        double distance;
        if (intersector.trace(ao_ray, true, distance))
        {
            ++occluded_samples;
            min_distance = min(min_distance, distance);
        }
    }

    // Return occlusion as a scalar between 0.0 and 1.0.
    return static_cast<double>(occluded_samples) / computed_samples;
}

}   // namespace renderer
