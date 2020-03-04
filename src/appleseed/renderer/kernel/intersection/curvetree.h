
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Srinath Ravichandran, The appleseedhq Organization
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
#include "renderer/kernel/intersection/curvekey.h"
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/kernel/intersection/probevisitorbase.h"
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/kernel/shading/shadingray.h"

// appleseed.foundation headers.
#include "foundation/containers/alignedvector.h"
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/bvh.h"
#include "foundation/memory/poolallocator.h"
#include "foundation/utility/lazy.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <cstddef>
#include <cstdint>
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

  private:
    friend class CurveLeafVisitor;
    friend class CurveLeafProbeVisitor;

    struct LeafUserData
    {
        std::uint32_t       m_curve1_offset;
        std::uint32_t       m_curve1_count;
        std::uint32_t       m_curve3_offset;
        std::uint32_t       m_curve3_count;
    };

    const Arguments         m_arguments;
    std::vector<Curve1Type> m_curves1;
    std::vector<Curve3Type> m_curves3;
    std::vector<CurveKey>   m_curve_keys;

    void collect_curves(std::vector<GAABB3>& curve_bboxes);

    void build_bvh(
        const ParamArray&                       params,
        const double                            time,
        foundation::Statistics&                 statistics);

    // Reorder curve keys to match a given ordering.
    void reorder_curve_keys(const std::vector<size_t>& ordering);

    // Reorder curves to match a given ordering.
    void reorder_curves(const std::vector<size_t>& ordering);

    // Reorder curve keys in leaf nodes so that all degree-1 curve keys come before degree-3 ones.
    void reorder_curve_keys_in_leaf_nodes();
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
    std::unique_ptr<CurveTree> create() override;

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
    const CurveTree::LeafUserData& user_data = node.get_user_data<CurveTree::LeafUserData>();

    size_t curve_index = node.get_item_index();
    size_t hit_curve_index = ~size_t(0);
    GScalar u, v, t = ray.m_tmax;

    for (std::uint32_t i = 0; i < user_data.m_curve1_count; ++i, ++curve_index)
    {
        const Curve1Type& curve = m_tree.m_curves1[user_data.m_curve1_offset + i];
        if (Curve1IntersectorType::intersect(curve, ray, m_xfm_matrix, u, v, t))
        {
            m_shading_point.m_primitive_type = ShadingPoint::PrimitiveCurve1;
            m_shading_point.m_ray.m_tmax = static_cast<double>(t);
            m_shading_point.m_bary[0] = static_cast<float>(u);
            m_shading_point.m_bary[1] = static_cast<float>(v);
            hit_curve_index = curve_index;
        }
    }

    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_intersected_items.insert(curve1_curve_count));

    for (std::uint32_t i = 0; i < user_data.m_curve3_count; ++i, ++curve_index)
    {
        const Curve3Type& curve = m_tree.m_curves3[user_data.m_curve3_offset + i];
        if (Curve3IntersectorType::intersect(curve, ray, m_xfm_matrix, u, v, t))
        {
            m_shading_point.m_primitive_type = ShadingPoint::PrimitiveCurve3;
            m_shading_point.m_ray.m_tmax = static_cast<double>(t);
            m_shading_point.m_bary[0] = static_cast<float>(u);
            m_shading_point.m_bary[1] = static_cast<float>(v);
            hit_curve_index = curve_index;
        }
    }

    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_intersected_items.insert(curve3_curve_count));

    if (hit_curve_index != ~size_t(0))
    {
        const CurveKey& curve_key = m_tree.m_curve_keys[hit_curve_index];
        m_shading_point.m_object_instance_index = curve_key.get_object_instance_index();
        m_shading_point.m_primitive_index = curve_key.get_curve_index_object();
    }

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
    const CurveTree::LeafUserData& user_data = node.get_user_data<CurveTree::LeafUserData>();

    for (std::uint32_t i = 0; i < user_data.m_curve1_count; ++i)
    {
        const Curve1Type& curve = m_tree.m_curves1[user_data.m_curve1_offset + i];
        if (Curve1IntersectorType::intersect(curve, ray, m_xfm_matrix))
        {
            FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_intersected_items.insert(i + 1));
            m_hit = true;
            return false;
        }
    }

    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_intersected_items.insert(curve1_curve_count));

    for (std::uint32_t i = 0; i < user_data.m_curve3_count; ++i)
    {
        const Curve3Type& curve = m_tree.m_curves3[user_data.m_curve3_offset + i];
        if (Curve3IntersectorType::intersect(curve, ray, m_xfm_matrix))
        {
            FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_intersected_items.insert(i + 1));
            m_hit = true;
            return false;
        }
    }

    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_intersected_items.insert(curve3_curve_count));

    // Continue traversal.
    distance = ray.m_tmax;
    return true;
}

}   // namespace renderer
