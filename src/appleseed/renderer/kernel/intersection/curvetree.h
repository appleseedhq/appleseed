
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
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/curvekey.h"
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/kernel/intersection/probevisitorbase.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/bvh.h"
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
                   foundation::bvh::Node<GAABB3>
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

    const Arguments         m_arguments;
    std::vector<CurveType1> m_curves1;
    std::vector<CurveType3> m_curves3;
    std::vector<CurveKey>   m_curve_keys;

    void collect_curves(std::vector<GAABB3>& curve_bboxes);
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
        const CurveMatrixType&                  xfm_matrix,
        ShadingPoint&                           shading_point);

    // Visit a leaf.
    bool visit(
        const CurveTree::NodeType&              node,
        const GRay3&                            ray,
        const GRayInfo3&                        ray_info,
        GScalar&                                distance
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , foundation::bvh::TraversalStatistics& stats
#endif
        );

  private:
    const CurveTree&                            m_tree;
    const CurveMatrixType&                      m_xfm_matrix;
    ShadingPoint&                               m_shading_point;
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
        const CurveMatrixType&                  xfm_matrix);

    // Visit a leaf.
    bool visit(
        const CurveTree::NodeType&              node,
        const GRay3&                            ray,
        const GRayInfo3&                        ray_info,
        GScalar&                                distance
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , foundation::bvh::TraversalStatistics& stats
#endif
        );

  private:
    const CurveTree&                            m_tree;
    const CurveMatrixType&                      m_xfm_matrix;
};


//
// Curve tree intersectors.
//

typedef foundation::bvh::Intersector<
    CurveTree,
    CurveLeafVisitor,
    GRay3,
    CurveTreeStackSize
> CurveTreeIntersector;

typedef foundation::bvh::Intersector<
    CurveTree,
    CurveLeafProbeVisitor,
    GRay3,
    CurveTreeStackSize
> CurveTreeProbeIntersector;


//
// CurveLeafVisitor class implementation.
//

inline CurveLeafVisitor::CurveLeafVisitor(
    const CurveTree&                            tree,
    const CurveMatrixType&                      xfm_matrix,
    ShadingPoint&                               shading_point)
  : m_tree(tree)
  , m_xfm_matrix(xfm_matrix)
  , m_shading_point(shading_point)
{
}

inline bool CurveLeafVisitor::visit(
    const CurveTree::NodeType&                  node,
    const GRay3&                                ray,
    const GRayInfo3&                            ray_info,
    GScalar&                                    distance
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    , foundation::bvh::TraversalStatistics&     stats
#endif
    )
{
    const size_t curve_index = node.get_item_index();
    const size_t curve_count = node.get_item_count();
    const foundation::Vector4u& curve_data = node.get_user_data<foundation::Vector4u>();
    const size_t degree1_curve_count = curve_data[0];
    const size_t degree1_curve_offset = curve_data[1];
    const size_t degree3_curve_count = curve_data[2];
    const size_t degree3_curve_offset = curve_data[3];

    size_t curve_num = 0;
    size_t hit_curve_index = 0;

    // Intersection params.
    GScalar u, v, t = ray.m_tmax;
    bool intersected = false;
    ShadingPoint::PrimitiveType m_type = ShadingPoint::PrimitiveNone;

    // Sequentially intersect all curves of the leaf.
    for (size_t i = 0; i < degree1_curve_count; i++)
    {
        const CurveType1& curve = m_tree.m_curves1[degree1_curve_offset + i];
        if (Curve1IntersectorType::intersect(curve, ray, m_xfm_matrix, u, v, t))
        {
            intersected = true;
            m_type = ShadingPoint::PrimitiveCurve1;
            hit_curve_index = curve_index + curve_num;
        }
        curve_num++;
    }

    for (size_t i = 0; i < degree3_curve_count; i++)
    {
        const CurveType3& curve = m_tree.m_curves3[degree3_curve_offset + i];
        if (Curve3IntersectorType::intersect(curve, ray, m_xfm_matrix, u, v, t))
        {
            intersected = true;
            m_type = ShadingPoint::PrimitiveCurve3;
            hit_curve_index = curve_index + curve_num;
        }
        curve_num++;
    }

    if (intersected)
    {
        m_shading_point.m_primitive_type = m_type;
        m_shading_point.m_ray.m_tmax = static_cast<double>(t);
        m_shading_point.m_bary[0] = static_cast<double>(u);
        m_shading_point.m_bary[1] = static_cast<double>(v);
        m_shading_point.m_object_instance_index = m_tree.m_curve_keys[hit_curve_index].get_object_instance_index();
        m_shading_point.m_primitive_index = m_tree.m_curve_keys[hit_curve_index].get_curve_index();
    }

    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_intersected_items.insert(curve_count));

    // Continue traversal.
    distance = static_cast<GScalar>(m_shading_point.m_ray.m_tmax);
    return true;
}


//
// CurveLeafProbeVisitor class implementation.
//

inline CurveLeafProbeVisitor::CurveLeafProbeVisitor(
    const CurveTree&                            tree,
    const CurveMatrixType&                      xfm_matrix)
  : m_tree(tree)
  , m_xfm_matrix(xfm_matrix)
{
}

inline bool CurveLeafProbeVisitor::visit(
    const CurveTree::NodeType&                  node,
    const GRay3&                                ray,
    const GRayInfo3&                            ray_info,
    GScalar&                                    distance
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    , foundation::bvh::TraversalStatistics&     stats
#endif
    )
{
    const size_t curve_index = node.get_item_index();
    const size_t curve_count = node.get_item_count();
    const foundation::Vector4u& curve_data = node.get_user_data<foundation::Vector4u>();
    const size_t degree1_curve_count = curve_data[0];
    const size_t degree1_curve_offset = curve_data[1];
    const size_t degree3_curve_count = curve_data[2];
    const size_t degree3_curve_offset = curve_data[3];

    for (size_t i = 0; i < degree1_curve_count; i++)
    {
        const CurveType1& curve = m_tree.m_curves1[degree1_curve_offset + i];
        if (Curve1IntersectorType::intersect(curve, ray, m_xfm_matrix))
        {
            m_hit = true;
            return false;
        }
    }

    for (size_t i = 0; i < degree3_curve_count; i++)
    {
        const CurveType3& curve = m_tree.m_curves3[degree3_curve_offset + i];
        if (Curve3IntersectorType::intersect(curve, ray, m_xfm_matrix))
        {
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
