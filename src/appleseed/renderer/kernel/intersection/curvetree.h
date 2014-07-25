
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Srinath Ravichandran, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_INTERSECTION_CURVETREE_H
#define APPLESEED_RENDERER_KERNEL_INTERSECTION_CURVETREE_H

// appleseed.renderer headers.
#include "renderer/kernel/intersection/curvekey.h"
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/kernel/intersection/probevisitorbase.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/object/curveobject.h"
#include "renderer/global/globaltypes.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/aabb.h"
#include "foundation/math/bvh.h"
#include "foundation/math/beziercurve.h"
#include "foundation/math/matrix.h"
#include "foundation/utility/alignedvector.h"
#include "foundation/utility/lazy.h"
#include "foundation/utility/poolallocator.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <cstddef>
#include <map>
#include <memory>
#include <vector>

// Forward declarations.
namespace foundation    { class Statistics; }
namespace renderer      { class Assembly; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Scene; }

namespace renderer
{

//
// Curve tree.
//

class CurveTree
  : public foundation::bvh::Tree<
               foundation::AlignedVector<
                   foundation::bvh::Node<foundation::AABB3d>
               >
           >
{
  public:
    // Construction arguments.
    struct Arguments
    {
        const Scene&                            m_scene;
        const foundation::UniqueID              m_curve_tree_uid;
        const GAABB3                            m_bbox;
        const Assembly&                         m_assembly;

        // Constructor.
        Arguments(
            const Scene&                        scene,
            const foundation::UniqueID          curve_tree_uid,
            const GAABB3&                       bbox,
            const Assembly&                     assembly);
    };
 
    // Constructor, builds the tree for a given assembly.
    explicit CurveTree(const Arguments& arguments);
    
    void build_bvh(        
        const ParamArray&                       params,
        const double                            time,
        foundation::Statistics&                 statistics);

  private:
    friend class CurveLeafVisitor;
    friend class CurveLeafProbeVisitor;

    const Arguments                             m_arguments;

    std::vector<foundation::BezierCurve1d>      m_curves1;
    std::vector<foundation::BezierCurve2d>      m_curves2;
    std::vector<foundation::BezierCurve3d>      m_curves3;

    std::vector<CurveKey>                       m_curve_keys;
};


//
// Curve tree factory.
//

class CurveTreeFactory
  : public foundation::ILazyFactory<CurveTree>
{
  public:
    // Constructor.
    explicit CurveTreeFactory(
        const CurveTree::Arguments&  arguments);

    // Create the curve tree.
    virtual std::auto_ptr<CurveTree> create();

  private:
    const CurveTree::Arguments       m_arguments;
};


//
// Some additional types.
//

// Curve tree container and iterator types.
typedef std::map<
    foundation::UniqueID,
    foundation::Lazy<CurveTree>*
> CurveTreeContainer;
typedef CurveTreeContainer::iterator CurveTreeIterator;
typedef CurveTreeContainer::const_iterator CurveTreeConstIterator;

// Curve tree access cache type.
typedef foundation::AccessCacheMap<
    CurveTreeContainer,
    CurveTreeAccessCacheLines,
    CurveTreeAccessCacheWays,
    foundation::PoolAllocator<void, CurveTreeAccessCacheLines * CurveTreeAccessCacheWays>
> CurveTreeAccessCache;


//
// Curve leaf visitor, used during tree intersection.
//

class CurveLeafVisitor
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    CurveLeafVisitor(
        const CurveTree&                        tree,
        const foundation::Matrix4d&             xfm_matrix,
        ShadingPoint&                           shading_point);

    // Visit a leaf.
    bool visit(
        const CurveTree::NodeType&              node,
        const ShadingRay&                       ray,
        const ShadingRay::RayInfoType&          ray_info,
        double&                                 distance
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , foundation::bvh::TraversalStatistics& stats
#endif
        );

  private:
    const CurveTree&                m_tree;
    const foundation::Matrix4d&     m_xfm_matrix;
    ShadingPoint&                   m_shading_point;
};


//
// Curve leaf visitor for probe rays, only return boolean answers
// (whether an intersection was found or not).
//

class CurveLeafProbeVisitor
  : public ProbeVisitorBase
{
  public:
    // Constructor.
    CurveLeafProbeVisitor(
        const CurveTree&                        tree,
        const foundation::Matrix4d&             xfm_matrix);

    // Visit a leaf.
    bool visit(
        const CurveTree::NodeType&              node,
        const ShadingRay&                       ray,
        const ShadingRay::RayInfoType&          ray_info,
        double&                                 distance
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , foundation::bvh::TraversalStatistics& stats
#endif
        );

  private:
    const CurveTree&                m_tree;
    const foundation::Matrix4d&     m_xfm_matrix;
};


//
// Curve tree intersectors.
//

typedef foundation::bvh::Intersector<
    CurveTree,
    CurveLeafVisitor,
    ShadingRay,
    CurveTreeStackSize
> CurveTreeIntersector;

typedef foundation::bvh::Intersector<
    CurveTree,
    CurveLeafProbeVisitor,
    ShadingRay,
    CurveTreeStackSize
> CurveTreeProbeIntersector;


//
// CurveLeafVisitor class implementation.
//

inline CurveLeafVisitor::CurveLeafVisitor(
    const CurveTree&                            tree,
    const foundation::Matrix4d&                 xfm_matrix,
    ShadingPoint&                               shading_point)
  : m_tree(tree)
  , m_xfm_matrix(xfm_matrix)
  , m_shading_point(shading_point)
{
}

inline bool CurveLeafVisitor::visit(
    const CurveTree::NodeType&                  node,
    const ShadingRay&                           ray,
    const ShadingRay::RayInfoType&              ray_info,
    double&                                     distance
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    , foundation::bvh::TraversalStatistics&     stats
#endif
    )
{
    const size_t curve_index = node.get_item_index();
    const size_t curve_count = node.get_item_count();

    // Sequentially intersect all curves of the leaf.
    for (size_t i = 0; i < curve_count; ++i)
    {
        // For now we intersect curves as they are stored in memory (no floating point format conversion).
        const GCurveType& curve = m_tree.m_curves3[curve_index + i];

        // Intersect the curve.
        double t;
        if (CurveIntersector::intersect(curve, m_shading_point.m_ray, m_xfm_matrix, t))
        {
            const CurveKey& key = m_tree.m_curve_keys[curve_index + i];
            m_shading_point.m_ray.m_tmax = t;
            m_shading_point.m_hit = true;
            m_shading_point.m_object_instance_index = key.get_object_instance_index();
            m_shading_point.m_primitive_index = key.get_curve_index();
        }
    }

    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_intersected_items.insert(curve_count));

    // Continue traversal.
    distance = m_shading_point.m_ray.m_tmax;
    return true;
}


//
// CurveLeafProbeVisitor class implementation.
//

inline CurveLeafProbeVisitor::CurveLeafProbeVisitor(
    const CurveTree&                            tree,
    const foundation::Matrix4d&                 xfm_matrix)
  : m_tree(tree)
  , m_xfm_matrix(xfm_matrix)
{
}

inline bool CurveLeafProbeVisitor::visit(
    const CurveTree::NodeType&                  node,
    const ShadingRay&                           ray,
    const ShadingRay::RayInfoType&              ray_info,
    double&                                     distance
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    , foundation::bvh::TraversalStatistics&     stats
#endif
    )
{
    const size_t curve_index = node.get_item_index();
    const size_t curve_count = node.get_item_count();

    // Sequentially intersect all curves of the leaf.
    for (size_t i = 0; i < curve_count; ++i)
    {
        // For now we intersect curves as they are stored in memory (no floating point format conversion).
        const GCurveType& curve = m_tree.m_curves3[curve_index + i];

        // Intersect the curve.
        // todo: we need a variante of CurveIntersector::intersect() that does not compute or return t.
        double t;
        if (CurveIntersector::intersect(curve, ray, m_xfm_matrix, t))
        {
            FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_intersected_items.insert(i + 1));
            m_hit = true;
            return false;
        }
    }

    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_intersected_items.insert(curve_count));

    // Continue traversal.
    distance = ray.m_tmax;
    return true;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_INTERSECTION_CURVETREE_H
