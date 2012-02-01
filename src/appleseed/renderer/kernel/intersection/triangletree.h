
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

#ifndef APPLESEED_RENDERER_KERNEL_INTERSECTION_TRIANGLETREE_H
#define APPLESEED_RENDERER_KERNEL_INTERSECTION_TRIANGLETREE_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/kernel/intersection/probevisitorbase.h"
#include "renderer/kernel/intersection/regioninfo.h"
#include "renderer/kernel/intersection/trianglekey.h"
#include "renderer/kernel/shading/shadingray.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/bvh.h"
#include "foundation/utility/lazy.h"
#include "foundation/utility/poolallocator.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <cstddef>
#include <map>
#include <memory>
#include <vector>

// Forward declarations.
namespace renderer  { class Assembly; }
namespace renderer  { class ShadingPoint; }

namespace renderer
{

//
// Triangle tree.
//

class TriangleTree
  : public foundation::bvh::Tree<GScalar, 3, TriangleKey>
{
  public:
    // Construction arguments.
    struct Arguments
    {
        const foundation::UniqueID      m_triangle_tree_uid;
        const GAABB3                    m_bbox;
        const Assembly&                 m_assembly;
        const RegionInfoVector          m_regions;

        // Constructor.
        Arguments(
            const foundation::UniqueID  triangle_tree_uid,
            const GAABB3&               bbox,
            const Assembly&             assembly,
            const RegionInfoVector&     regions);
    };

    // Constructor, builds the tree for a given set of regions.
    explicit TriangleTree(const Arguments& arguments);

    // Destructor.
    ~TriangleTree();

  private:
    friend class TriangleLeafVisitor;
    friend class TriangleLeafProbeVisitor;

    const foundation::UniqueID          m_triangle_tree_uid;
    std::vector<GTriangleType>          m_triangles;

    void collect_triangles(const Arguments& arguments);
};


//
// Triangle tree factory.
//

class TriangleTreeFactory
  : public foundation::ILazyFactory<TriangleTree>
{
  public:
    // Constructor.
    explicit TriangleTreeFactory(
        const TriangleTree::Arguments&  arguments);

    // Create the triangle tree.
    virtual std::auto_ptr<TriangleTree> create();

  private:
    const TriangleTree::Arguments       m_arguments;
};


//
// Some additional types.
//

// Triangle tree container and iterator types.
typedef std::map<
    foundation::UniqueID,
    foundation::Lazy<TriangleTree>*
> TriangleTreeContainer;
typedef TriangleTreeContainer::iterator TriangleTreeIterator;
typedef TriangleTreeContainer::const_iterator TriangleTreeConstIterator;

// Triangle tree access cache type.
typedef foundation::AccessCacheMap<
    TriangleTreeContainer,
    TriangleTreeAccessCacheSize,
    1,
    foundation::PoolAllocator<void, TriangleTreeAccessCacheSize>
> TriangleTreeAccessCache;


//
// Triangle leaf visitor, used during tree intersection.
//

class TriangleLeafVisitor
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    TriangleLeafVisitor(
        const TriangleTree&                 tree,
        ShadingPoint&                       shading_point);

    // Visit a leaf.
    bool visit(
        const std::vector<TriangleKey>&     items,
        const std::vector<GAABB3>&          bboxes,
        const size_t                        begin,
        const size_t                        end,
        const ShadingRay::RayType&          ray,
        const ShadingRay::RayInfoType&      ray_info,
        const double                        tmin,
        const double                        tmax,
        double&                             distance);

    // Read additional data about the triangle that was hit, if any.
    void read_hit_triangle_data() const;

  private:
    const TriangleTree&     m_tree;
    ShadingPoint&           m_shading_point;
    size_t                  m_hit_triangle_index;
};


//
// Triangle leaf visitor for probe rays, only return boolean answers
// (whether an intersection was found or not).
//

class TriangleLeafProbeVisitor
  : public ProbeVisitorBase
{
  public:
    // Constructor.
    explicit TriangleLeafProbeVisitor(
        const TriangleTree&                 tree);

    // Visit a leaf.
    bool visit(
        const std::vector<TriangleKey>&     items,
        const std::vector<GAABB3>&          bboxes,
        const size_t                        begin,
        const size_t                        end,
        const ShadingRay::RayType&          ray,
        const ShadingRay::RayInfoType&      ray_info,
        const double                        tmin,
        const double                        tmax,
        double&                             distance);

  private:
    const TriangleTree&     m_tree;
};


//
// Triangle tree intersectors.
//

typedef foundation::bvh::Intersector<
    double,
    TriangleTree,
    TriangleLeafVisitor
> TriangleLeafIntersector;

typedef foundation::bvh::Intersector<
    double,
    TriangleTree,
    TriangleLeafProbeVisitor
> TriangleLeafProbeIntersector;


//
// TriangleLeafVisitor class implementation.
//

inline TriangleLeafVisitor::TriangleLeafVisitor(
    const TriangleTree&                     tree,
    ShadingPoint&                           shading_point)
  : m_tree(tree)
  , m_shading_point(shading_point)
  , m_hit_triangle_index(~0)
{
}


//
// TriangleLeafProbeVisitor class implementation.
//

inline TriangleLeafProbeVisitor::TriangleLeafProbeVisitor(
    const TriangleTree&                     tree)
  : m_tree(tree)
{
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_INTERSECTION_TRIANGLETREE_H
