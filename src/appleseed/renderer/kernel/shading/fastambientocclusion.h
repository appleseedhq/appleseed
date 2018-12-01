
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

#pragma once

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/shading/shadingray.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/vector.h"
#include "foundation/math/voxel.h"

// Standard headers.
#include <cstddef>
#include <string>

// Forward declarations.
namespace renderer  { class Scene; }

namespace renderer
{

//
// Voxel tree for fast ambient occlusion.
//

class AOVoxelTree
{
  public:
    // Constructor, build the tree for a given scene.
    AOVoxelTree(
        const Scene&        scene,
        const GScalar       max_extent_fraction);

    // Return the maximum leaf node diagonal length.
    GScalar get_max_diag_length() const;

    // Dump all solid leaves of the tree to disk, as an .obj mesh file.
    void dump_solid_leaves_to_disk(const std::string& filename) const;

    // Dump the entire tree to disk, in proprietary binary format.
    void dump_tree_to_disk(const std::string& filename) const;

  private:
    friend class AOVoxelTreeIntersector;

    // Types.
    typedef foundation::voxel::Tree<GScalar, 3> TreeType;
    typedef foundation::voxel::Builder<TreeType> BuilderType;
    typedef foundation::voxel::TreeStatistics<
        TreeType,
        BuilderType
    > TreeStatisticsType;

    // Voxel tree.
    TreeType                m_tree;

    // Build the tree.
    void build(
        const Scene&        scene,
        BuilderType&        builder);
};


//
// Ambient occlusion voxel tree intersector.
//

class AOVoxelTreeIntersector
{
  public:
    // Constructor, binds the intersector to a given tree.
    explicit AOVoxelTreeIntersector(const AOVoxelTree& tree);

    // Destructor.
    ~AOVoxelTreeIntersector();

    // Trace a world space ray through the voxel tree.
    bool trace(
        ShadingRay::RayType ray,
        const bool          solid,
        double&             distance) const;

  private:
    // Types.
    typedef foundation::voxel::Intersector<
        double,
        AOVoxelTree::TreeType
    > IntersectorType;

    // Voxel tree.
    const AOVoxelTree&      m_tree;

    // Intersection statistics.
#ifdef FOUNDATION_VOXEL_ENABLE_TRAVERSAL_STATS
    mutable foundation::voxel::TraversalStatistics m_traversal_stats;
#endif
};


//
// Compute fast ambient occlusion at a given point in space.
//
// todo: implement optional computation of the mean unoccluded direction.
//

double compute_fast_ambient_occlusion(
    const SamplingContext&          sampling_context,
    const AOVoxelTreeIntersector&   intersector,
    const foundation::Vector3d&     point,
    const foundation::Vector3d&     geometric_normal,
    const foundation::Basis3d&      shading_basis,
    const double                    max_distance,
    const size_t                    sample_count,
    double&                         min_distance);


//
// AOVoxelTree class implementation.
//

inline GScalar AOVoxelTree::get_max_diag_length() const
{
    return m_tree.get_max_diag_length();
}

}   // namespace renderer
