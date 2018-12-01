
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

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/bvh/bvh_statistics.h"
#include "foundation/math/intersection/rayaabb.h"
#include "foundation/math/ray.h"
#include "foundation/math/scalar.h"
#ifdef APPLESEED_USE_SSE
#include "foundation/platform/compiler.h"
#include "foundation/platform/sse.h"
#endif

// Standard headers.
#include <cassert>
#include <cstddef>

// Enable or disable BVH traversal statistics.
#undef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS

#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
#define FOUNDATION_BVH_TRAVERSAL_STATS(x) x
#else
#define FOUNDATION_BVH_TRAVERSAL_STATS(x)
#endif

namespace foundation {
namespace bvh {

//
// BVH intersector.
//
// The Visitor class must conform to the following prototype:
//
//      class Visitor
//        : public foundation::NonCopyable
//      {
//        public:
//          // Return whether BVH traversal should continue or not.
//          // 'distance' should be set to the distance to the closest hit so far.
//          bool visit(
//              const NodeType&             node,
//              const RayType&              ray,
//              const RayInfoType&          ray_info,
//              ValueType&                  distance
//      #ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
//              , TraversalStatistics&      stats
//      #endif
//              );
//      };
//

template <
    typename Tree,
    typename Visitor,
    typename Ray,
    size_t StackSize = 64,
    size_t N = Tree::NodeType::AABBType::Dimension
>
class Intersector
  : public NonCopyable
{
  public:
    typedef typename Tree::NodeType NodeType;
    typedef typename NodeType::ValueType ValueType;
    typedef Ray RayType;
    typedef RayInfo<ValueType, NodeType::Dimension> RayInfoType;

    // Intersect a ray with a given BVH without motion.
    void intersect_no_motion(
        const Tree&             tree,
        const RayType&          ray,
        const RayInfoType&      ray_info,
        Visitor&                visitor
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , TraversalStatistics&  stats
#endif
        ) const;

    // Intersect a ray with a given BVH with motion.
    void intersect_motion(
        const Tree&             tree,
        const RayType&          ray,
        const RayInfoType&      ray_info,
        const ValueType         ray_time,
        Visitor&                visitor
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , TraversalStatistics&  stats
#endif
        ) const;
};


//
// Intersector class implementation.
//

#if defined(__GNUC__) && ((__GNUC__ > 4) || ((__GNUC__ == 4) && (__GNUC_MINOR__ >= 7)))
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmaybe-uninitialized"
#endif

template <
    typename Tree,
    typename Visitor,
    typename Ray,
    size_t StackSize,
    size_t N
>
void Intersector<Tree, Visitor, Ray, StackSize, N>::intersect_no_motion(
    const Tree&                 tree,
    const RayType&              ray,
    const RayInfoType&          ray_info,
    Visitor&                    visitor
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    , TraversalStatistics&      stats
#endif
    ) const
{
    // Make sure the tree was built.
    assert(!tree.m_nodes.empty());

    // Node stack.
    const NodeType* stack[StackSize];
    const NodeType** stack_ptr = stack;

    // Current node.
    const NodeType* node_ptr = &tree.m_nodes[0];

    // Initialize traversal statistics.
    FOUNDATION_BVH_TRAVERSAL_STATS(++stats.m_traversal_count);
    FOUNDATION_BVH_TRAVERSAL_STATS(size_t visited_nodes = 0);
    FOUNDATION_BVH_TRAVERSAL_STATS(size_t visited_leaves = 0);
    FOUNDATION_BVH_TRAVERSAL_STATS(size_t intersected_bboxes = 0);
    FOUNDATION_BVH_TRAVERSAL_STATS(size_t discarded_nodes = 0);

    // Traverse the tree and intersect leaf nodes.
    ValueType ray_tmax = ray.m_tmax;
    while (true)
    {
        // Fetch the node.
        FOUNDATION_BVH_TRAVERSAL_STATS(++visited_nodes);

        if (node_ptr->is_interior())
        {
            FOUNDATION_BVH_TRAVERSAL_STATS(intersected_bboxes += 2);

            ValueType tmin[2];

            // Intersect the left bounding box.
            const size_t hit_left =
                foundation::intersect(ray, ray_info, node_ptr->get_left_bbox(), tmin[0]) && tmin[0] < ray_tmax ? 1 : 0;

            // Intersect the right bounding box.
            const size_t hit_right =
                foundation::intersect(ray, ray_info, node_ptr->get_right_bbox(), tmin[1]) && tmin[1] < ray_tmax ? 1 : 0;

            node_ptr = &tree.m_nodes[node_ptr->get_child_node_index()];
            node_ptr += hit_right;

            if (hit_left ^ hit_right)
            {
                // Continue with the left or right child node.
                FOUNDATION_BVH_TRAVERSAL_STATS(++discarded_nodes);
                continue;
            }

            if (hit_left | hit_right)
            {
                // Push the far child node to the stack, continue with the near child node.
                const int far_index = tmin[0] < tmin[1] ? 1 : 0;
                *stack_ptr++ = node_ptr + far_index - 1;
                node_ptr -= far_index;
                continue;
            }

            FOUNDATION_BVH_TRAVERSAL_STATS(discarded_nodes += 2);

            // Terminate traversal if the node stack is empty.
            if (stack_ptr == stack)
                break;

            // Pop the top node from the stack.
            node_ptr = *--stack_ptr;
            continue;
        }
        else
        {
            // Visit the leaf.
            FOUNDATION_BVH_TRAVERSAL_STATS(++visited_leaves);
            ValueType distance;
#ifndef NDEBUG
            distance = ValueType(-1.0);
#endif
            const bool proceed =
                visitor.visit(
                    *node_ptr,
                    ray,
                    ray_info,
                    distance
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
                    , stats
#endif
                    );
            assert(!proceed || distance >= ValueType(0.0));

            // Terminate traversal if the visitor decided so.
            if (!proceed)
                break;

            // Keep track of the distance to the closest intersection.
            if (ray_tmax > distance)
                ray_tmax = distance;

            // Terminate traversal if the node stack is empty.
            if (stack_ptr == stack)
                break;

            // Pop the top node from the stack.
            node_ptr = *--stack_ptr;
        }
    }

    // Store traversal statistics.
    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_visited_nodes.insert(visited_nodes));
    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_visited_leaves.insert(visited_leaves));
    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_intersected_bboxes.insert(intersected_bboxes));
    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_discarded_nodes.insert(discarded_nodes));
}

#if defined(__GNUC__) && ((__GNUC__ > 4) || ((__GNUC__ == 4) && (__GNUC_MINOR__ >= 7)))
#pragma GCC diagnostic pop
#endif

template <
    typename Tree,
    typename Visitor,
    typename Ray,
    size_t StackSize,
    size_t N
>
void Intersector<Tree, Visitor, Ray, StackSize, N>::intersect_motion(
    const Tree&                 tree,
    const RayType&              ray,
    const RayInfoType&          ray_info,
    const ValueType             ray_time,
    Visitor&                    visitor
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    , TraversalStatistics&      stats
#endif
    ) const
{
    // Make sure the tree was built.
    assert(!tree.m_nodes.empty());

    // Node stack.
    const NodeType* stack[StackSize];
    const NodeType** stack_ptr = stack;

    // Current node.
    const NodeType* node_ptr = &tree.m_nodes[0];

    // Initialize traversal statistics.
    FOUNDATION_BVH_TRAVERSAL_STATS(++stats.m_traversal_count);
    FOUNDATION_BVH_TRAVERSAL_STATS(size_t visited_nodes = 0);
    FOUNDATION_BVH_TRAVERSAL_STATS(size_t visited_leaves = 0);
    FOUNDATION_BVH_TRAVERSAL_STATS(size_t intersected_bboxes = 0);
    FOUNDATION_BVH_TRAVERSAL_STATS(size_t discarded_nodes = 0);

    // Traverse the tree and intersect leaf nodes.
    ValueType ray_tmax = ray.m_tmax;
    while (true)
    {
        // Fetch the node.
        FOUNDATION_BVH_TRAVERSAL_STATS(++visited_nodes);

        if (node_ptr->is_interior())
        {
            FOUNDATION_BVH_TRAVERSAL_STATS(intersected_bboxes += 2);

            ValueType tmin[2];

            // Intersect the left bounding box.
            size_t hit_left;
            const size_t left_motion_segment_count = node_ptr->get_left_bbox_count() - 1;
            if (left_motion_segment_count > 0)
            {
                const size_t prev_index = truncate<size_t>(ray_time * left_motion_segment_count);
                const size_t base_index = node_ptr->get_left_bbox_index() + prev_index;

                const typename NodeType::AABBType left_bbox =
                    lerp(
                        tree.m_node_bboxes[base_index],
                        tree.m_node_bboxes[base_index + 1],
                        static_cast<ValueType>(ray_time * left_motion_segment_count - prev_index));

                hit_left = (foundation::intersect(ray, ray_info, left_bbox, tmin[0]) && tmin[0] < ray_tmax) ? 1 : 0;
            }
            else
            {
                hit_left = (foundation::intersect(ray, ray_info, node_ptr->get_left_bbox(), tmin[0]) && tmin[0] < ray_tmax) ? 1 : 0;
            }

            // Intersect the right bounding box.
            size_t hit_right;
            const size_t right_motion_segment_count = node_ptr->get_right_bbox_count() - 1;
            if (right_motion_segment_count > 0)
            {
                const size_t prev_index = truncate<size_t>(ray_time * right_motion_segment_count);
                const size_t base_index = node_ptr->get_right_bbox_index() + prev_index;

                const typename NodeType::AABBType right_bbox =
                    lerp(
                        tree.m_node_bboxes[base_index],
                        tree.m_node_bboxes[base_index + 1],
                        static_cast<ValueType>(ray_time * right_motion_segment_count - prev_index));

                hit_right = (foundation::intersect(ray, ray_info, right_bbox, tmin[1]) && tmin[1] < ray_tmax) ? 1 : 0;
            }
            else
            {
                hit_right = (foundation::intersect(ray, ray_info, node_ptr->get_right_bbox(), tmin[1]) && tmin[1] < ray_tmax) ? 1 : 0;
            }

            node_ptr = &tree.m_nodes[node_ptr->get_child_node_index()];
            node_ptr += hit_right;

            if (hit_left ^ hit_right)
            {
                // Continue with the left or right child node.
                FOUNDATION_BVH_TRAVERSAL_STATS(++discarded_nodes);
                continue;
            }

            if (hit_left | hit_right)
            {
                // Push the far child node to the stack, continue with the near child node.
                const int far_index = tmin[0] < tmin[1] ? 1 : 0;
                *stack_ptr++ = node_ptr + far_index - 1;
                node_ptr -= far_index;
                continue;
            }

            FOUNDATION_BVH_TRAVERSAL_STATS(discarded_nodes += 2);

            // Terminate traversal if the node stack is empty.
            if (stack_ptr == stack)
                break;

            // Pop the top node from the stack.
            node_ptr = *--stack_ptr;
            continue;
        }
        else
        {
            // Visit the leaf.
            FOUNDATION_BVH_TRAVERSAL_STATS(++visited_leaves);
            ValueType distance;
#ifndef NDEBUG
            distance = ValueType(-1.0);
#endif
            const bool proceed =
                visitor.visit(
                    *node_ptr,
                    ray,
                    ray_info,
                    distance
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
                    , stats
#endif
                    );
            assert(!proceed || distance >= ValueType(0.0));

            // Terminate traversal if the visitor decided so.
            if (!proceed)
                break;

            // Keep track of the distance to the closest intersection.
            if (ray_tmax > distance)
                ray_tmax = distance;

            // Terminate traversal if the node stack is empty.
            if (stack_ptr == stack)
                break;

            // Pop the top node from the stack.
            node_ptr = *--stack_ptr;
        }
    }

    // Store traversal statistics.
    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_visited_nodes.insert(visited_nodes));
    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_visited_leaves.insert(visited_leaves));
    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_intersected_bboxes.insert(intersected_bboxes));
    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_discarded_nodes.insert(discarded_nodes));
}

#ifdef APPLESEED_USE_SSE

template <
    typename Tree,
    typename Visitor,
    size_t StackSize
>
class Intersector<Tree, Visitor, Ray3d, StackSize, 3>
  : public NonCopyable
{
  public:
    typedef typename Tree::NodeType NodeType;
    typedef double ValueType;
    typedef Ray3d RayType;
    typedef RayInfo3d RayInfoType;

    // Intersect a ray with a given BVH without motion.
    void intersect_no_motion(
        const Tree&             tree,
        const RayType&          ray,
        const RayInfoType&      ray_info,
        Visitor&                visitor
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , TraversalStatistics&  stats
#endif
        ) const;

    // Intersect a ray with a given BVH with motion.
    void intersect_motion(
        const Tree&             tree,
        const RayType&          ray,
        const RayInfoType&      ray_info,
        const ValueType         ray_time,
        Visitor&                visitor
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , TraversalStatistics&  stats
#endif
        ) const;
};

template <
    typename Tree,
    typename Visitor,
    size_t StackSize
>
void Intersector<Tree, Visitor, Ray3d, StackSize, 3>::intersect_no_motion(
    const Tree&                 tree,
    const RayType&              ray,
    const RayInfoType&          ray_info,
    Visitor&                    visitor
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    , TraversalStatistics&      stats
#endif
    ) const
{
    // Make sure the tree was built.
    assert(!tree.m_nodes.empty());

    // Load the ray into SSE registers.
    const __m128d org_x = _mm_set1_pd(ray.m_org.x);
    const __m128d org_y = _mm_set1_pd(ray.m_org.y);
    const __m128d org_z = _mm_set1_pd(ray.m_org.z);
    const __m128d rcp_dir_x = _mm_set1_pd(ray_info.m_rcp_dir.x);
    const __m128d rcp_dir_y = _mm_set1_pd(ray_info.m_rcp_dir.y);
    const __m128d rcp_dir_z = _mm_set1_pd(ray_info.m_rcp_dir.z);
    const __m128d ray_tmin = _mm_set1_pd(ray.m_tmin);

    // Node stack.
    const NodeType* stack[StackSize];
    const NodeType** stack_ptr = stack;

    // Current node.
    const NodeType* node_ptr = &tree.m_nodes[0];

    // Initialize traversal statistics.
    FOUNDATION_BVH_TRAVERSAL_STATS(++stats.m_traversal_count);
    FOUNDATION_BVH_TRAVERSAL_STATS(size_t visited_nodes = 0);
    FOUNDATION_BVH_TRAVERSAL_STATS(size_t visited_leaves = 0);
    FOUNDATION_BVH_TRAVERSAL_STATS(size_t intersected_bboxes = 0);
    FOUNDATION_BVH_TRAVERSAL_STATS(size_t discarded_nodes = 0);

    // Traverse the tree and intersect leaf nodes.
    ValueType rtmax = ray.m_tmax;
    while (true)
    {
        // Fetch the node.
        FOUNDATION_BVH_TRAVERSAL_STATS(++visited_nodes);

        if (node_ptr->is_interior())
        {
            FOUNDATION_BVH_TRAVERSAL_STATS(intersected_bboxes += 2);

            const __m128d xl1 = _mm_mul_pd(rcp_dir_x, _mm_sub_pd(_mm_load_pd(node_ptr->m_bbox_data + 0 + 2 * (1 - ray_info.m_sgn_dir.x)), org_x));
            const __m128d xl2 = _mm_mul_pd(rcp_dir_x, _mm_sub_pd(_mm_load_pd(node_ptr->m_bbox_data + 0 + 2 * (    ray_info.m_sgn_dir.x)), org_x));
            const __m128d yl1 = _mm_mul_pd(rcp_dir_y, _mm_sub_pd(_mm_load_pd(node_ptr->m_bbox_data + 4 + 2 * (1 - ray_info.m_sgn_dir.y)), org_y));
            const __m128d yl2 = _mm_mul_pd(rcp_dir_y, _mm_sub_pd(_mm_load_pd(node_ptr->m_bbox_data + 4 + 2 * (    ray_info.m_sgn_dir.y)), org_y));
            const __m128d zl1 = _mm_mul_pd(rcp_dir_z, _mm_sub_pd(_mm_load_pd(node_ptr->m_bbox_data + 8 + 2 * (1 - ray_info.m_sgn_dir.z)), org_z));
            const __m128d zl2 = _mm_mul_pd(rcp_dir_z, _mm_sub_pd(_mm_load_pd(node_ptr->m_bbox_data + 8 + 2 * (    ray_info.m_sgn_dir.z)), org_z));

            const __m128d ray_tmax = _mm_set1_pd(rtmax);
            const __m128d tmin = _mm_max_pd(zl1, _mm_max_pd(yl1, _mm_max_pd(xl1, ray_tmin)));
            const __m128d tmax = _mm_min_pd(zl2, _mm_min_pd(yl2, _mm_min_pd(xl2, ray_tmax)));

            const int hits =
                _mm_movemask_pd(
                    _mm_or_pd(
                        _mm_cmpgt_pd(tmin, tmax),
                        _mm_or_pd(
                            _mm_cmplt_pd(tmax, ray_tmin),
                            _mm_cmpge_pd(tmin, ray_tmax)))) ^ 3;

            const size_t hit_left = hits & 1;
            const size_t hit_right = hits >> 1;

            node_ptr = &tree.m_nodes[node_ptr->get_child_node_index()];
            node_ptr += hit_right;

            if (hit_left ^ hit_right)
            {
                // Continue with the left or right child node.
                FOUNDATION_BVH_TRAVERSAL_STATS(++discarded_nodes);
                continue;
            }

            if (hits)
            {
                // Push the far child node to the stack, continue with the near child node.
                const int far_index =
                    _mm_movemask_pd(
                        _mm_cmplt_pd(
                            tmin,
                            _mm_shuffle_pd(tmin, tmin, _MM_SHUFFLE2(1, 1))));
                *stack_ptr++ = node_ptr + far_index - 1;
                node_ptr -= far_index;
                continue;
            }

            FOUNDATION_BVH_TRAVERSAL_STATS(discarded_nodes += 2);

            // Terminate traversal if the node stack is empty.
            if (stack_ptr == stack)
                break;

            // Pop the top node from the stack.
            node_ptr = *--stack_ptr;
            continue;
        }
        else
        {
            // Visit the leaf.
            FOUNDATION_BVH_TRAVERSAL_STATS(++visited_leaves);
            ValueType distance;
#ifndef NDEBUG
            distance = ValueType(-1.0);
#endif
            const bool proceed =
                visitor.visit(
                    *node_ptr,
                    ray,
                    ray_info,
                    distance
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
                    , stats
#endif
                    );
            assert(!proceed || distance >= ValueType(0.0));

            // Terminate traversal if the visitor decided so.
            if (!proceed)
                break;

            // Keep track of the distance to the closest intersection.
            if (rtmax > distance)
                rtmax = distance;

            // Terminate traversal if the node stack is empty.
            if (stack_ptr == stack)
                break;

            // Pop the top node from the stack.
            node_ptr = *--stack_ptr;
        }
    }

    // Store traversal statistics.
    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_visited_nodes.insert(visited_nodes));
    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_visited_leaves.insert(visited_leaves));
    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_intersected_bboxes.insert(intersected_bboxes));
    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_discarded_nodes.insert(discarded_nodes));
}

template <
    typename Tree,
    typename Visitor,
    size_t StackSize
>
void Intersector<Tree, Visitor, Ray3d, StackSize, 3>::intersect_motion(
    const Tree&                 tree,
    const RayType&              ray,
    const RayInfoType&          ray_info,
    const ValueType             ray_time,
    Visitor&                    visitor
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
    , TraversalStatistics&      stats
#endif
    ) const
{
    // Make sure the tree was built.
    assert(!tree.m_nodes.empty());

    // Load the ray into SSE registers.
    const __m128d org_x = _mm_set1_pd(ray.m_org.x);
    const __m128d org_y = _mm_set1_pd(ray.m_org.y);
    const __m128d org_z = _mm_set1_pd(ray.m_org.z);
    const __m128d rcp_dir_x = _mm_set1_pd(ray_info.m_rcp_dir.x);
    const __m128d rcp_dir_y = _mm_set1_pd(ray_info.m_rcp_dir.y);
    const __m128d rcp_dir_z = _mm_set1_pd(ray_info.m_rcp_dir.z);
    const __m128d ray_tmin = _mm_set1_pd(ray.m_tmin);
    const __m128d mray_time = _mm_set1_pd(ray_time);

    // Load constants.
    const __m128d one = _mm_set1_pd(1.0);

    // Node stack.
    const NodeType* stack[StackSize];
    const NodeType** stack_ptr = stack;

    // Current node.
    const NodeType* node_ptr = &tree.m_nodes[0];

    // Initialize traversal statistics.
    FOUNDATION_BVH_TRAVERSAL_STATS(++stats.m_traversal_count);
    FOUNDATION_BVH_TRAVERSAL_STATS(size_t visited_nodes = 0);
    FOUNDATION_BVH_TRAVERSAL_STATS(size_t visited_leaves = 0);
    FOUNDATION_BVH_TRAVERSAL_STATS(size_t intersected_bboxes = 0);
    FOUNDATION_BVH_TRAVERSAL_STATS(size_t discarded_nodes = 0);

    // Traverse the tree and intersect leaf nodes.
    ValueType rtmax = ray.m_tmax;
    while (true)
    {
        // Fetch the node.
        FOUNDATION_BVH_TRAVERSAL_STATS(++visited_nodes);

        if (node_ptr->is_interior())
        {
            FOUNDATION_BVH_TRAVERSAL_STATS(intersected_bboxes += 2);

            __m128d tmin, tmax;

            const __m128d ray_tmax = _mm_set1_pd(rtmax);

            const NodeType* base_child_node_ptr = &tree.m_nodes[node_ptr->get_child_node_index()];
            const size_t left_motion_segment_count = node_ptr->get_left_bbox_count() - 1;
            const size_t right_motion_segment_count = node_ptr->get_right_bbox_count() - 1;

            if (left_motion_segment_count > 0 && right_motion_segment_count > 0)
            {
                const __m128d left_t = _mm_mul_pd(mray_time, _mm_set1_pd(static_cast<double>(left_motion_segment_count)));
                const int left_prev_index = _mm_cvttsd_si32(left_t);
                const size_t left_base_index = node_ptr->get_left_bbox_index() + left_prev_index;
                const __m128d left_w2 = _mm_sub_pd(left_t, _mm_set1_pd(left_prev_index));
                const __m128d left_w1 = _mm_sub_pd(one, left_w2);

                const __m128d right_t = _mm_mul_pd(mray_time, _mm_set1_pd(static_cast<double>(right_motion_segment_count)));
                const int right_prev_index = _mm_cvttsd_si32(right_t);
                const size_t right_base_index = node_ptr->get_right_bbox_index() + right_prev_index;
                const __m128d right_w2 = _mm_sub_pd(right_t, _mm_set1_pd(right_prev_index));
                const __m128d right_w1 = _mm_sub_pd(one, right_w2);

                const double* base_bbox = &tree.m_node_bboxes[0][0][0];
                const double* left_bbox = base_bbox + left_base_index * 6;
                const double* right_bbox = base_bbox + right_base_index * 6;

                const __m128d mleftbbx = _mm_add_pd(_mm_mul_pd(_mm_load_pd(left_bbox + 0), left_w1), _mm_mul_pd(_mm_load_pd(left_bbox + 6), left_w2));
                const __m128d mrightbbx = _mm_add_pd(_mm_mul_pd(_mm_load_pd(right_bbox + 0), right_w1), _mm_mul_pd(_mm_load_pd(right_bbox + 6), right_w2));
                const __m128d bbox_x[2] = { _mm_shuffle_pd(mleftbbx, mrightbbx, _MM_SHUFFLE2(0, 0)), _mm_shuffle_pd(mleftbbx, mrightbbx, _MM_SHUFFLE2(1, 1)) };

                const __m128d mleftbby = _mm_add_pd(_mm_mul_pd(_mm_load_pd(left_bbox + 2), left_w1), _mm_mul_pd(_mm_load_pd(left_bbox + 8), left_w2));
                const __m128d mrightbby = _mm_add_pd(_mm_mul_pd(_mm_load_pd(right_bbox + 2), right_w1), _mm_mul_pd(_mm_load_pd(right_bbox + 8), right_w2));
                const __m128d bbox_y[2] = { _mm_shuffle_pd(mleftbby, mrightbby, _MM_SHUFFLE2(0, 0)), _mm_shuffle_pd(mleftbby, mrightbby, _MM_SHUFFLE2(1, 1)) };

                const __m128d mleftbbz = _mm_add_pd(_mm_mul_pd(_mm_load_pd(left_bbox + 4), left_w1), _mm_mul_pd(_mm_load_pd(left_bbox + 10), left_w2));
                const __m128d mrightbbz = _mm_add_pd(_mm_mul_pd(_mm_load_pd(right_bbox + 4), right_w1), _mm_mul_pd(_mm_load_pd(right_bbox + 10), right_w2));
                const __m128d bbox_z[2] = { _mm_shuffle_pd(mleftbbz, mrightbbz, _MM_SHUFFLE2(0, 0)), _mm_shuffle_pd(mleftbbz, mrightbbz, _MM_SHUFFLE2(1, 1)) };

                const __m128d xl1 = _mm_mul_pd(rcp_dir_x, _mm_sub_pd(bbox_x[1 - ray_info.m_sgn_dir.x], org_x));
                const __m128d yl1 = _mm_mul_pd(rcp_dir_y, _mm_sub_pd(bbox_y[1 - ray_info.m_sgn_dir.y], org_y));
                const __m128d zl1 = _mm_mul_pd(rcp_dir_z, _mm_sub_pd(bbox_z[1 - ray_info.m_sgn_dir.z], org_z));

                const __m128d xl2 = _mm_mul_pd(rcp_dir_x, _mm_sub_pd(bbox_x[    ray_info.m_sgn_dir.x], org_x));
                const __m128d yl2 = _mm_mul_pd(rcp_dir_y, _mm_sub_pd(bbox_y[    ray_info.m_sgn_dir.y], org_y));
                const __m128d zl2 = _mm_mul_pd(rcp_dir_z, _mm_sub_pd(bbox_z[    ray_info.m_sgn_dir.z], org_z));

                tmin = _mm_max_pd(zl1, _mm_max_pd(yl1, _mm_max_pd(xl1, ray_tmin)));
                tmax = _mm_min_pd(zl2, _mm_min_pd(yl2, _mm_min_pd(xl2, ray_tmax)));
            }
            else
            {
                APPLESEED_SIMD4_ALIGN double bbox_data[12];

                // Fetch the left bounding box.
                if (left_motion_segment_count > 0)
                {
                    const __m128d t = _mm_mul_pd(mray_time, _mm_set1_pd(static_cast<double>(left_motion_segment_count)));
                    const int prev_index = _mm_cvttsd_si32(t);
                    const __m128d w2 = _mm_sub_pd(t, _mm_set1_pd(prev_index));
                    const __m128d w1 = _mm_sub_pd(one, w2);

                    const size_t base_index = node_ptr->get_left_bbox_index() + prev_index;
                    const double* bbox = &tree.m_node_bboxes[0][0][0] + base_index * 6;

                    const __m128d x = _mm_add_pd(_mm_mul_pd(_mm_load_pd(bbox + 0), w1), _mm_mul_pd(_mm_load_pd(bbox + 6), w2));
                    _mm_storel_pd(bbox_data + 0, x);
                    _mm_storeh_pd(bbox_data + 2, x);

                    const __m128d y = _mm_add_pd(_mm_mul_pd(_mm_load_pd(bbox + 2), w1), _mm_mul_pd(_mm_load_pd(bbox + 8), w2));
                    _mm_storel_pd(bbox_data + 4, y);
                    _mm_storeh_pd(bbox_data + 6, y);

                    const __m128d z = _mm_add_pd(_mm_mul_pd(_mm_load_pd(bbox + 4), w1), _mm_mul_pd(_mm_load_pd(bbox + 10), w2));
                    _mm_storel_pd(bbox_data +  8, z);
                    _mm_storeh_pd(bbox_data + 10, z);
                }
                else
                {
                    bbox_data[ 0] = node_ptr->m_bbox_data[ 0];
                    bbox_data[ 2] = node_ptr->m_bbox_data[ 2];
                    bbox_data[ 4] = node_ptr->m_bbox_data[ 4];
                    bbox_data[ 6] = node_ptr->m_bbox_data[ 6];
                    bbox_data[ 8] = node_ptr->m_bbox_data[ 8];
                    bbox_data[10] = node_ptr->m_bbox_data[10];
                }

                // Fetch the right bounding box.
                if (right_motion_segment_count > 0)
                {
                    const __m128d t = _mm_mul_pd(mray_time, _mm_set1_pd(static_cast<double>(right_motion_segment_count)));
                    const int prev_index = _mm_cvttsd_si32(t);
                    const __m128d w2 = _mm_sub_pd(t, _mm_set1_pd(prev_index));
                    const __m128d w1 = _mm_sub_pd(one, w2);

                    const size_t base_index = node_ptr->get_right_bbox_index() + prev_index;
                    const double* bbox = &tree.m_node_bboxes[0][0][0] + base_index * 6;

                    const __m128d x = _mm_add_pd(_mm_mul_pd(_mm_load_pd(bbox + 0), w1), _mm_mul_pd(_mm_load_pd(bbox + 6), w2));
                    _mm_storel_pd(bbox_data + 1, x);
                    _mm_storeh_pd(bbox_data + 3, x);

                    const __m128d y = _mm_add_pd(_mm_mul_pd(_mm_load_pd(bbox + 2), w1), _mm_mul_pd(_mm_load_pd(bbox + 8), w2));
                    _mm_storel_pd(bbox_data + 5, y);
                    _mm_storeh_pd(bbox_data + 7, y);

                    const __m128d z = _mm_add_pd(_mm_mul_pd(_mm_load_pd(bbox + 4), w1), _mm_mul_pd(_mm_load_pd(bbox + 10), w2));
                    _mm_storel_pd(bbox_data +  9, z);
                    _mm_storeh_pd(bbox_data + 11, z);
                }
                else
                {
                    bbox_data[ 1] = node_ptr->m_bbox_data[ 1];
                    bbox_data[ 3] = node_ptr->m_bbox_data[ 3];
                    bbox_data[ 5] = node_ptr->m_bbox_data[ 5];
                    bbox_data[ 7] = node_ptr->m_bbox_data[ 7];
                    bbox_data[ 9] = node_ptr->m_bbox_data[ 9];
                    bbox_data[11] = node_ptr->m_bbox_data[11];
                }

                const __m128d xl1 = _mm_mul_pd(rcp_dir_x, _mm_sub_pd(_mm_load_pd(bbox_data + 0 + 2 * (1 - ray_info.m_sgn_dir.x)), org_x));
                const __m128d xl2 = _mm_mul_pd(rcp_dir_x, _mm_sub_pd(_mm_load_pd(bbox_data + 0 + 2 * (    ray_info.m_sgn_dir.x)), org_x));
                const __m128d yl1 = _mm_mul_pd(rcp_dir_y, _mm_sub_pd(_mm_load_pd(bbox_data + 4 + 2 * (1 - ray_info.m_sgn_dir.y)), org_y));
                const __m128d yl2 = _mm_mul_pd(rcp_dir_y, _mm_sub_pd(_mm_load_pd(bbox_data + 4 + 2 * (    ray_info.m_sgn_dir.y)), org_y));
                const __m128d zl1 = _mm_mul_pd(rcp_dir_z, _mm_sub_pd(_mm_load_pd(bbox_data + 8 + 2 * (1 - ray_info.m_sgn_dir.z)), org_z));
                const __m128d zl2 = _mm_mul_pd(rcp_dir_z, _mm_sub_pd(_mm_load_pd(bbox_data + 8 + 2 * (    ray_info.m_sgn_dir.z)), org_z));

                tmin = _mm_max_pd(zl1, _mm_max_pd(yl1, _mm_max_pd(xl1, ray_tmin)));
                tmax = _mm_min_pd(zl2, _mm_min_pd(yl2, _mm_min_pd(xl2, ray_tmax)));
            }

            const int hits =
                _mm_movemask_pd(
                    _mm_or_pd(
                        _mm_cmpgt_pd(tmin, tmax),
                        _mm_or_pd(
                            _mm_cmplt_pd(tmax, ray_tmin),
                            _mm_cmpge_pd(tmin, ray_tmax)))) ^ 3;

            const size_t hit_left = hits & 1;
            const size_t hit_right = hits >> 1;

            node_ptr = base_child_node_ptr + hit_right;

            if (hit_left ^ hit_right)
            {
                // Continue with the left or right child node.
                FOUNDATION_BVH_TRAVERSAL_STATS(++discarded_nodes);
                continue;
            }

            if (hits)
            {
                // Push the far child node to the stack, continue with the near child node.
                const int far_index =
                    _mm_movemask_pd(
                        _mm_cmplt_pd(
                            tmin,
                            _mm_shuffle_pd(tmin, tmin, _MM_SHUFFLE2(1, 1))));
                *stack_ptr++ = node_ptr + far_index - 1;
                node_ptr -= far_index;
                continue;
            }

            FOUNDATION_BVH_TRAVERSAL_STATS(discarded_nodes += 2);

            // Terminate traversal if the node stack is empty.
            if (stack_ptr == stack)
                break;

            // Pop the top node from the stack.
            node_ptr = *--stack_ptr;
            continue;
        }
        else
        {
            // Visit the leaf.
            FOUNDATION_BVH_TRAVERSAL_STATS(++visited_leaves);
            ValueType distance;
#ifndef NDEBUG
            distance = ValueType(-1.0);
#endif
            const bool proceed =
                visitor.visit(
                    *node_ptr,
                    ray,
                    ray_info,
                    distance
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
                    , stats
#endif
                    );
            assert(!proceed || distance >= ValueType(0.0));

            // Terminate traversal if the visitor decided so.
            if (!proceed)
                break;

            // Keep track of the distance to the closest intersection.
            if (rtmax > distance)
                rtmax = distance;

            // Terminate traversal if the node stack is empty.
            if (stack_ptr == stack)
                break;

            // Pop the top node from the stack.
            node_ptr = *--stack_ptr;
        }
    }

    // Store traversal statistics.
    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_visited_nodes.insert(visited_nodes));
    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_visited_leaves.insert(visited_leaves));
    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_intersected_bboxes.insert(intersected_bboxes));
    FOUNDATION_BVH_TRAVERSAL_STATS(stats.m_discarded_nodes.insert(discarded_nodes));
}

#endif  // APPLESEED_USE_SSE

}   // namespace bvh
}   // namespace foundation
