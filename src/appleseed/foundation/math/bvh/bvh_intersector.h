
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

#ifndef APPLESEED_FOUNDATION_MATH_BVH_BVH_INTERSECTOR_H
#define APPLESEED_FOUNDATION_MATH_BVH_BVH_INTERSECTOR_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/bvh/bvh_statistics.h"
#include "foundation/math/intersection.h"
#include "foundation/math/ray.h"
#include "foundation/math/scalar.h"
#ifdef APPLESEED_FOUNDATION_USE_SSE
#include "foundation/math/fp.h"
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

    // Intersect a ray with a given BVH.
    void intersect(
        const Tree&             tree,
        const RayType&          ray,
        const RayInfoType&      ray_info,
        Visitor&                visitor
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , TraversalStatistics&  stats
#endif
        ) const;

    // Intersect a ray with a given BVH with motion.
    void intersect(
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

template <
    typename Tree,
    typename Visitor,
    typename Ray,
    size_t StackSize,
    size_t N
>
void Intersector<Tree, Visitor, Ray, StackSize, N>::intersect(
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
            int hits = 0;

            // Intersect the left bounding box.
            if (foundation::intersect(ray, ray_info, node_ptr->get_left_bbox(), tmin[0]) && tmin[0] < ray_tmax)
                hits |= 1;

            // Intersect the right bounding box.
            if (foundation::intersect(ray, ray_info, node_ptr->get_right_bbox(), tmin[1]) && tmin[1] < ray_tmax)
                hits |= 2;

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
                const int far = tmin[0] < tmin[1] ? 1 : 0;
                *stack_ptr++ = node_ptr + far - 1;
                node_ptr -= far;
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

template <
    typename Tree,
    typename Visitor,
    typename Ray,
    size_t StackSize,
    size_t N
>
void Intersector<Tree, Visitor, Ray, StackSize, N>::intersect(
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
            int hits = 0;

            // Intersect the left bounding box.
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

                if (foundation::intersect(ray, ray_info, left_bbox, tmin[0]) && tmin[0] < ray_tmax)
                    hits |= 1;
            }
            else
            {
                if (foundation::intersect(ray, ray_info, node_ptr->get_left_bbox(), tmin[0]) && tmin[0] < ray_tmax)
                    hits |= 1;
            }

            // Intersect the right bounding box.
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

                if (foundation::intersect(ray, ray_info, right_bbox, tmin[1]) && tmin[1] < ray_tmax)
                    hits |= 2;
            }
            else
            {
                if (foundation::intersect(ray, ray_info, node_ptr->get_right_bbox(), tmin[1]) && tmin[1] < ray_tmax)
                    hits |= 2;
            }

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
                const int far = tmin[0] < tmin[1] ? 1 : 0;
                *stack_ptr++ = node_ptr + far - 1;
                node_ptr -= far;
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

#ifdef APPLESEED_FOUNDATION_USE_SSE

template <
    typename Tree,
    typename Visitor,
    typename Ray,
    size_t StackSize
>
class Intersector<Tree, Visitor, Ray, StackSize, 3>
  : public NonCopyable
{
  public:
    typedef typename Tree::NodeType NodeType;
    typedef typename NodeType::ValueType ValueType;
    typedef Ray RayType;
    typedef RayInfo<ValueType, NodeType::Dimension> RayInfoType;

    // Intersect a ray with a given BVH.
    void intersect(
        const Tree&             tree,
        const RayType&          ray,
        const RayInfoType&      ray_info,
        Visitor&                visitor
#ifdef FOUNDATION_BVH_ENABLE_TRAVERSAL_STATS
        , TraversalStatistics&  stats
#endif
        ) const;

    // Intersect a ray with a given BVH with motion.
    void intersect(
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
    typename Ray,
    size_t StackSize
>
void Intersector<Tree, Visitor, Ray, StackSize, 3>::intersect(
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
    const __m128d mrox = _mm_set1_pd(ray.m_org.x);
    const __m128d mroy = _mm_set1_pd(ray.m_org.y);
    const __m128d mroz = _mm_set1_pd(ray.m_org.z);
    const __m128d mrrcpdx = _mm_set1_pd(ray_info.m_rcp_dir.x);
    const __m128d mrrcpdy = _mm_set1_pd(ray_info.m_rcp_dir.y);
    const __m128d mrrcpdz = _mm_set1_pd(ray_info.m_rcp_dir.z);
    const __m128d mraytmin = _mm_set1_pd(ray.m_tmin);

    // Load constants.
    const __m128d mposinf = _mm_set1_pd(FP<double>::pos_inf());
    const __m128d mneginf = _mm_set1_pd(FP<double>::neg_inf());

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

            const __m128d mbbminx2d = _mm_load_pd(node_ptr->m_bbox_data + 0);
            const __m128d mbbmaxx2d = _mm_load_pd(node_ptr->m_bbox_data + 2);
            const __m128d mx1 = _mm_mul_pd(mrrcpdx, _mm_sub_pd(mbbminx2d, mrox));
            const __m128d mx2 = _mm_mul_pd(mrrcpdx, _mm_sub_pd(mbbmaxx2d, mrox));

            __m128d mtmax = _mm_max_pd(_mm_min_pd(mx1, mposinf), _mm_min_pd(mx2, mposinf));
            __m128d mtmin = _mm_min_pd(_mm_max_pd(mx1, mneginf), _mm_max_pd(mx2, mneginf));

            const __m128d mbbminy2d = _mm_load_pd(node_ptr->m_bbox_data + 4);
            const __m128d mbbmaxy2d = _mm_load_pd(node_ptr->m_bbox_data + 6);
            const __m128d my1 = _mm_mul_pd(mrrcpdy, _mm_sub_pd(mbbminy2d, mroy));
            const __m128d my2 = _mm_mul_pd(mrrcpdy, _mm_sub_pd(mbbmaxy2d, mroy));

            mtmax = _mm_min_pd(mtmax, _mm_max_pd(_mm_min_pd(my1, mposinf), _mm_min_pd(my2, mposinf)));
            mtmin = _mm_max_pd(mtmin, _mm_min_pd(_mm_max_pd(my1, mneginf), _mm_max_pd(my2, mneginf)));

            const __m128d mbbminz2d = _mm_load_pd(node_ptr->m_bbox_data + 8);
            const __m128d mbbmaxz2d = _mm_load_pd(node_ptr->m_bbox_data + 10);
            const __m128d mz1 = _mm_mul_pd(mrrcpdz, _mm_sub_pd(mbbminz2d, mroz));
            const __m128d mz2 = _mm_mul_pd(mrrcpdz, _mm_sub_pd(mbbmaxz2d, mroz));

            mtmax = _mm_min_pd(mtmax, _mm_max_pd(_mm_min_pd(mz1, mposinf), _mm_min_pd(mz2, mposinf)));
            mtmin = _mm_max_pd(mtmin, _mm_min_pd(_mm_max_pd(mz1, mneginf), _mm_max_pd(mz2, mneginf)));

            const __m128d mraytmax = _mm_set1_pd(ray_tmax);
            const int hits =
                _mm_movemask_pd(
                    _mm_or_pd(
                        _mm_cmpgt_pd(mtmin, mtmax),
                        _mm_or_pd(
                            _mm_cmplt_pd(mtmax, mraytmin),
                            _mm_cmpge_pd(mtmin, mraytmax)))) ^ 3;

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
                const int far =
                    _mm_movemask_pd(
                        _mm_cmplt_pd(
                            mtmin,
                            _mm_shuffle_pd(mtmin, mtmin, _MM_SHUFFLE2(1, 1))));
                *stack_ptr++ = node_ptr + far - 1;
                node_ptr -= far;
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

template <
    typename Tree,
    typename Visitor,
    typename Ray,
    size_t StackSize
>
void Intersector<Tree, Visitor, Ray, StackSize, 3>::intersect(
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
    const __m128d mrox = _mm_set1_pd(ray.m_org.x);
    const __m128d mroy = _mm_set1_pd(ray.m_org.y);
    const __m128d mroz = _mm_set1_pd(ray.m_org.z);
    const __m128d mrrcpdx = _mm_set1_pd(ray_info.m_rcp_dir.x);
    const __m128d mrrcpdy = _mm_set1_pd(ray_info.m_rcp_dir.y);
    const __m128d mrrcpdz = _mm_set1_pd(ray_info.m_rcp_dir.z);
    const __m128d mraytmin = _mm_set1_pd(ray.m_tmin);
    const __m128d mraytime = _mm_set1_pd(ray_time);

    // Load constants.
    const __m128d mone = _mm_set1_pd(1.0);
    const __m128d mposinf = _mm_set1_pd(FP<double>::pos_inf());
    const __m128d mneginf = _mm_set1_pd(FP<double>::neg_inf());

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

            __m128d mtmax, mtmin;

            const NodeType* base_child_node_ptr = &tree.m_nodes[node_ptr->get_child_node_index()];
            const size_t left_motion_segment_count = node_ptr->get_left_bbox_count() - 1;
            const size_t right_motion_segment_count = node_ptr->get_right_bbox_count() - 1;

            if (left_motion_segment_count > 0 && right_motion_segment_count > 0)
            {
                const __m128d left_t = _mm_mul_pd(mraytime, _mm_set1_pd(static_cast<double>(left_motion_segment_count)));
                const int left_prev_index = _mm_cvttsd_si32(left_t);
                const size_t left_base_index = node_ptr->get_left_bbox_index() + left_prev_index;
                const __m128d left_w2 = _mm_sub_pd(left_t, _mm_set1_pd(left_prev_index));
                const __m128d left_w1 = _mm_sub_pd(mone, left_w2);

                const __m128d right_t = _mm_mul_pd(mraytime, _mm_set1_pd(static_cast<double>(right_motion_segment_count)));
                const int right_prev_index = _mm_cvttsd_si32(right_t);
                const size_t right_base_index = node_ptr->get_right_bbox_index() + right_prev_index;
                const __m128d right_w2 = _mm_sub_pd(right_t, _mm_set1_pd(right_prev_index));
                const __m128d right_w1 = _mm_sub_pd(mone, right_w2);

                const double* base_bbox = &tree.m_node_bboxes[0][0][0];
                const double* left_bbox = base_bbox + left_base_index * 6;
                const double* right_bbox = base_bbox + right_base_index * 6;

                const __m128d mleftbbx = _mm_add_pd(_mm_mul_pd(_mm_load_pd(left_bbox + 0), left_w1), _mm_mul_pd(_mm_load_pd(left_bbox + 6), left_w2));
                const __m128d mrightbbx = _mm_add_pd(_mm_mul_pd(_mm_load_pd(right_bbox + 0), right_w1), _mm_mul_pd(_mm_load_pd(right_bbox + 6), right_w2));
                const __m128d mbbminx2d = _mm_shuffle_pd(mleftbbx, mrightbbx, _MM_SHUFFLE2(0, 0));
                const __m128d mbbmaxx2d = _mm_shuffle_pd(mleftbbx, mrightbbx, _MM_SHUFFLE2(1, 1));
                const __m128d mx1 = _mm_mul_pd(mrrcpdx, _mm_sub_pd(mbbminx2d, mrox));
                const __m128d mx2 = _mm_mul_pd(mrrcpdx, _mm_sub_pd(mbbmaxx2d, mrox));

                mtmax = _mm_max_pd(_mm_min_pd(mx1, mposinf), _mm_min_pd(mx2, mposinf));
                mtmin = _mm_min_pd(_mm_max_pd(mx1, mneginf), _mm_max_pd(mx2, mneginf));

                const __m128d mleftbby = _mm_add_pd(_mm_mul_pd(_mm_load_pd(left_bbox + 2), left_w1), _mm_mul_pd(_mm_load_pd(left_bbox + 8), left_w2));
                const __m128d mrightbby = _mm_add_pd(_mm_mul_pd(_mm_load_pd(right_bbox + 2), right_w1), _mm_mul_pd(_mm_load_pd(right_bbox + 8), right_w2));
                const __m128d mbbminy2d = _mm_shuffle_pd(mleftbby, mrightbby, _MM_SHUFFLE2(0, 0));
                const __m128d mbbmaxy2d = _mm_shuffle_pd(mleftbby, mrightbby, _MM_SHUFFLE2(1, 1));
                const __m128d my1 = _mm_mul_pd(mrrcpdy, _mm_sub_pd(mbbminy2d, mroy));
                const __m128d my2 = _mm_mul_pd(mrrcpdy, _mm_sub_pd(mbbmaxy2d, mroy));

                mtmax = _mm_min_pd(mtmax, _mm_max_pd(_mm_min_pd(my1, mposinf), _mm_min_pd(my2, mposinf)));
                mtmin = _mm_max_pd(mtmin, _mm_min_pd(_mm_max_pd(my1, mneginf), _mm_max_pd(my2, mneginf)));

                const __m128d mleftbbz = _mm_add_pd(_mm_mul_pd(_mm_load_pd(left_bbox + 4), left_w1), _mm_mul_pd(_mm_load_pd(left_bbox + 10), left_w2));
                const __m128d mrightbbz = _mm_add_pd(_mm_mul_pd(_mm_load_pd(right_bbox + 4), right_w1), _mm_mul_pd(_mm_load_pd(right_bbox + 10), right_w2));
                const __m128d mbbminz2d = _mm_shuffle_pd(mleftbbz, mrightbbz, _MM_SHUFFLE2(0, 0));
                const __m128d mbbmaxz2d = _mm_shuffle_pd(mleftbbz, mrightbbz, _MM_SHUFFLE2(1, 1));
                const __m128d mz1 = _mm_mul_pd(mrrcpdz, _mm_sub_pd(mbbminz2d, mroz));
                const __m128d mz2 = _mm_mul_pd(mrrcpdz, _mm_sub_pd(mbbmaxz2d, mroz));

                mtmax = _mm_min_pd(mtmax, _mm_max_pd(_mm_min_pd(mz1, mposinf), _mm_min_pd(mz2, mposinf)));
                mtmin = _mm_max_pd(mtmin, _mm_min_pd(_mm_max_pd(mz1, mneginf), _mm_max_pd(mz2, mneginf)));
            }
            else
            {
                SSE_ALIGN double bbox_data[12];

                // Fetch the left bounding box.
                if (left_motion_segment_count > 0)
                {
                    const __m128d t = _mm_mul_pd(mraytime, _mm_set1_pd(static_cast<double>(left_motion_segment_count)));
                    const int prev_index = _mm_cvttsd_si32(t);
                    const __m128d w2 = _mm_sub_pd(t, _mm_set1_pd(prev_index));
                    const __m128d w1 = _mm_sub_pd(mone, w2);

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
                    const __m128d t = _mm_mul_pd(mraytime, _mm_set1_pd(static_cast<double>(right_motion_segment_count)));
                    const int prev_index = _mm_cvttsd_si32(t);
                    const __m128d w2 = _mm_sub_pd(t, _mm_set1_pd(prev_index));
                    const __m128d w1 = _mm_sub_pd(mone, w2);

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

                const __m128d mbbminx2d = _mm_load_pd(bbox_data + 0);
                const __m128d mbbmaxx2d = _mm_load_pd(bbox_data + 2);
                const __m128d mx1 = _mm_mul_pd(mrrcpdx, _mm_sub_pd(mbbminx2d, mrox));
                const __m128d mx2 = _mm_mul_pd(mrrcpdx, _mm_sub_pd(mbbmaxx2d, mrox));

                mtmax = _mm_max_pd(_mm_min_pd(mx1, mposinf), _mm_min_pd(mx2, mposinf));
                mtmin = _mm_min_pd(_mm_max_pd(mx1, mneginf), _mm_max_pd(mx2, mneginf));

                const __m128d mbbminy2d = _mm_load_pd(bbox_data + 4);
                const __m128d mbbmaxy2d = _mm_load_pd(bbox_data + 6);
                const __m128d my1 = _mm_mul_pd(mrrcpdy, _mm_sub_pd(mbbminy2d, mroy));
                const __m128d my2 = _mm_mul_pd(mrrcpdy, _mm_sub_pd(mbbmaxy2d, mroy));

                mtmax = _mm_min_pd(mtmax, _mm_max_pd(_mm_min_pd(my1, mposinf), _mm_min_pd(my2, mposinf)));
                mtmin = _mm_max_pd(mtmin, _mm_min_pd(_mm_max_pd(my1, mneginf), _mm_max_pd(my2, mneginf)));

                const __m128d mbbminz2d = _mm_load_pd(bbox_data + 8);
                const __m128d mbbmaxz2d = _mm_load_pd(bbox_data + 10);
                const __m128d mz1 = _mm_mul_pd(mrrcpdz, _mm_sub_pd(mbbminz2d, mroz));
                const __m128d mz2 = _mm_mul_pd(mrrcpdz, _mm_sub_pd(mbbmaxz2d, mroz));

                mtmax = _mm_min_pd(mtmax, _mm_max_pd(_mm_min_pd(mz1, mposinf), _mm_min_pd(mz2, mposinf)));
                mtmin = _mm_max_pd(mtmin, _mm_min_pd(_mm_max_pd(mz1, mneginf), _mm_max_pd(mz2, mneginf)));
            }

            const __m128d mraytmax = _mm_set1_pd(ray_tmax);
            const int hits =
                _mm_movemask_pd(
                    _mm_or_pd(
                        _mm_cmpgt_pd(mtmin, mtmax),
                        _mm_or_pd(
                            _mm_cmplt_pd(mtmax, mraytmin),
                            _mm_cmpge_pd(mtmin, mraytmax)))) ^ 3;

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
                const int far =
                    _mm_movemask_pd(
                        _mm_cmplt_pd(
                            mtmin,
                            _mm_shuffle_pd(mtmin, mtmin, _MM_SHUFFLE2(1, 1))));
                *stack_ptr++ = node_ptr + far - 1;
                node_ptr -= far;
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

#endif

}       // namespace bvh
}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_BVH_BVH_INTERSECTOR_H
