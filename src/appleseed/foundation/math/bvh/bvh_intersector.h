
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
    const sse2d mrox = set1pd(ray.m_org.x);
    const sse2d mroy = set1pd(ray.m_org.y);
    const sse2d mroz = set1pd(ray.m_org.z);
    const sse2d mrrcpdx = set1pd(ray_info.m_rcp_dir.x);
    const sse2d mrrcpdy = set1pd(ray_info.m_rcp_dir.y);
    const sse2d mrrcpdz = set1pd(ray_info.m_rcp_dir.z);
    const sse2d mraytmin = set1pd(ray.m_tmin);

    // Load constants.
    const sse2d mposinf = set1pd(FP<double>::pos_inf());
    const sse2d mneginf = set1pd(FP<double>::neg_inf());

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

            const sse2d mbbminx2d = loadpd(node_ptr->m_bbox_data + 0);
            const sse2d mbbmaxx2d = loadpd(node_ptr->m_bbox_data + 2);
            const sse2d mx1 = mulpd(mrrcpdx, subpd(mbbminx2d, mrox));
            const sse2d mx2 = mulpd(mrrcpdx, subpd(mbbmaxx2d, mrox));

            sse2d mtmax = maxpd(minpd(mx1, mposinf), minpd(mx2, mposinf));
            sse2d mtmin = minpd(maxpd(mx1, mneginf), maxpd(mx2, mneginf));

            const sse2d mbbminy2d = loadpd(node_ptr->m_bbox_data + 4);
            const sse2d mbbmaxy2d = loadpd(node_ptr->m_bbox_data + 6);
            const sse2d my1 = mulpd(mrrcpdy, subpd(mbbminy2d, mroy));
            const sse2d my2 = mulpd(mrrcpdy, subpd(mbbmaxy2d, mroy));

            mtmax = minpd(mtmax, maxpd(minpd(my1, mposinf), minpd(my2, mposinf)));
            mtmin = maxpd(mtmin, minpd(maxpd(my1, mneginf), maxpd(my2, mneginf)));

            const sse2d mbbminz2d = loadpd(node_ptr->m_bbox_data + 8);
            const sse2d mbbmaxz2d = loadpd(node_ptr->m_bbox_data + 10);
            const sse2d mz1 = mulpd(mrrcpdz, subpd(mbbminz2d, mroz));
            const sse2d mz2 = mulpd(mrrcpdz, subpd(mbbmaxz2d, mroz));

            mtmax = minpd(mtmax, maxpd(minpd(mz1, mposinf), minpd(mz2, mposinf)));
            mtmin = maxpd(mtmin, minpd(maxpd(mz1, mneginf), maxpd(mz2, mneginf)));

            const sse2d mraytmax = set1pd(ray_tmax);
            const int hits =
                movemaskpd(
                    orpd(
                        cmpgtpd(mtmin, mtmax),
                        orpd(
                            cmpltpd(mtmax, mraytmin),
                            cmpgepd(mtmin, mraytmax)))) ^ 3;

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
                    movemaskpd(
                        cmpltpd(
                            mtmin,
                            shufflepd(mtmin, mtmin, _MM_SHUFFLE2(1, 1))));
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
    const sse2d mrox = set1pd(ray.m_org.x);
    const sse2d mroy = set1pd(ray.m_org.y);
    const sse2d mroz = set1pd(ray.m_org.z);
    const sse2d mrrcpdx = set1pd(ray_info.m_rcp_dir.x);
    const sse2d mrrcpdy = set1pd(ray_info.m_rcp_dir.y);
    const sse2d mrrcpdz = set1pd(ray_info.m_rcp_dir.z);
    const sse2d mraytmin = set1pd(ray.m_tmin);
    const sse2d mraytime = set1pd(ray_time);

    // Load constants.
    const sse2d mone = set1pd(1.0);
    const sse2d mposinf = set1pd(FP<double>::pos_inf());
    const sse2d mneginf = set1pd(FP<double>::neg_inf());

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

            sse2d mtmax, mtmin;

            const NodeType* base_child_node_ptr = &tree.m_nodes[node_ptr->get_child_node_index()];
            const size_t left_motion_segment_count = node_ptr->get_left_bbox_count() - 1;
            const size_t right_motion_segment_count = node_ptr->get_right_bbox_count() - 1;

            if (left_motion_segment_count > 0 && right_motion_segment_count > 0)
            {
                const sse2d left_t = mulpd(mraytime, set1pd(left_motion_segment_count));
                const int left_prev_index = _mm_cvttsd_si32(left_t);
                const size_t left_base_index = node_ptr->get_left_bbox_index() + left_prev_index;
                const sse2d left_w2 = subpd(left_t, set1pd(left_prev_index));
                const sse2d left_w1 = subpd(mone, left_w2);

                const sse2d right_t = mulpd(mraytime, set1pd(right_motion_segment_count));
                const int right_prev_index = _mm_cvttsd_si32(right_t);
                const size_t right_base_index = node_ptr->get_right_bbox_index() + right_prev_index;
                const sse2d right_w2 = subpd(right_t, set1pd(right_prev_index));
                const sse2d right_w1 = subpd(mone, right_w2);

                const double* base_bbox = &tree.m_node_bboxes[0][0][0];
                const double* left_bbox = base_bbox + left_base_index * 6;
                const double* right_bbox = base_bbox + right_base_index * 6;

                const sse2d mleftbbx = addpd(mulpd(loadpd(left_bbox + 0), left_w1), mulpd(loadpd(left_bbox + 6), left_w2));
                const sse2d mrightbbx = addpd(mulpd(loadpd(right_bbox + 0), right_w1), mulpd(loadpd(right_bbox + 6), right_w2));
                const sse2d mbbminx2d = shufflepd(mleftbbx, mrightbbx, _MM_SHUFFLE2(0, 0));
                const sse2d mbbmaxx2d = shufflepd(mleftbbx, mrightbbx, _MM_SHUFFLE2(1, 1));
                const sse2d mx1 = mulpd(mrrcpdx, subpd(mbbminx2d, mrox));
                const sse2d mx2 = mulpd(mrrcpdx, subpd(mbbmaxx2d, mrox));

                mtmax = maxpd(minpd(mx1, mposinf), minpd(mx2, mposinf));
                mtmin = minpd(maxpd(mx1, mneginf), maxpd(mx2, mneginf));

                const sse2d mleftbby = addpd(mulpd(loadpd(left_bbox + 2), left_w1), mulpd(loadpd(left_bbox + 8), left_w2));
                const sse2d mrightbby = addpd(mulpd(loadpd(right_bbox + 2), right_w1), mulpd(loadpd(right_bbox + 8), right_w2));
                const sse2d mbbminy2d = shufflepd(mleftbby, mrightbby, _MM_SHUFFLE2(0, 0));
                const sse2d mbbmaxy2d = shufflepd(mleftbby, mrightbby, _MM_SHUFFLE2(1, 1));
                const sse2d my1 = mulpd(mrrcpdy, subpd(mbbminy2d, mroy));
                const sse2d my2 = mulpd(mrrcpdy, subpd(mbbmaxy2d, mroy));

                mtmax = minpd(mtmax, maxpd(minpd(my1, mposinf), minpd(my2, mposinf)));
                mtmin = maxpd(mtmin, minpd(maxpd(my1, mneginf), maxpd(my2, mneginf)));

                const sse2d mleftbbz = addpd(mulpd(loadpd(left_bbox + 4), left_w1), mulpd(loadpd(left_bbox + 10), left_w2));
                const sse2d mrightbbz = addpd(mulpd(loadpd(right_bbox + 4), right_w1), mulpd(loadpd(right_bbox + 10), right_w2));
                const sse2d mbbminz2d = shufflepd(mleftbbz, mrightbbz, _MM_SHUFFLE2(0, 0));
                const sse2d mbbmaxz2d = shufflepd(mleftbbz, mrightbbz, _MM_SHUFFLE2(1, 1));
                const sse2d mz1 = mulpd(mrrcpdz, subpd(mbbminz2d, mroz));
                const sse2d mz2 = mulpd(mrrcpdz, subpd(mbbmaxz2d, mroz));

                mtmax = minpd(mtmax, maxpd(minpd(mz1, mposinf), minpd(mz2, mposinf)));
                mtmin = maxpd(mtmin, minpd(maxpd(mz1, mneginf), maxpd(mz2, mneginf)));
            }
            else
            {
                SSE_ALIGN double bbox_data[12];

                // Fetch the left bounding box.
                if (left_motion_segment_count > 0)
                {
                    const sse2d t = mulpd(mraytime, set1pd(left_motion_segment_count));
                    const int prev_index = _mm_cvttsd_si32(t);
                    const sse2d w2 = subpd(t, set1pd(prev_index));
                    const sse2d w1 = subpd(mone, w2);

                    const size_t base_index = node_ptr->get_left_bbox_index() + prev_index;
                    const double* bbox = &tree.m_node_bboxes[0][0][0] + base_index * 6;

                    const sse2d x = addpd(mulpd(loadpd(bbox + 0), w1), mulpd(loadpd(bbox + 6), w2));
                    _mm_storel_pd(bbox_data + 0, x);
                    _mm_storeh_pd(bbox_data + 2, x);

                    const sse2d y = addpd(mulpd(loadpd(bbox + 2), w1), mulpd(loadpd(bbox + 8), w2));
                    _mm_storel_pd(bbox_data + 4, y);
                    _mm_storeh_pd(bbox_data + 6, y);

                    const sse2d z = addpd(mulpd(loadpd(bbox + 4), w1), mulpd(loadpd(bbox + 10), w2));
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
                    const sse2d t = mulpd(mraytime, set1pd(right_motion_segment_count));
                    const int prev_index = _mm_cvttsd_si32(t);
                    const sse2d w2 = subpd(t, set1pd(prev_index));
                    const sse2d w1 = subpd(mone, w2);

                    const size_t base_index = node_ptr->get_right_bbox_index() + prev_index;
                    const double* bbox = &tree.m_node_bboxes[0][0][0] + base_index * 6;

                    const sse2d x = addpd(mulpd(loadpd(bbox + 0), w1), mulpd(loadpd(bbox + 6), w2));
                    _mm_storel_pd(bbox_data + 1, x);
                    _mm_storeh_pd(bbox_data + 3, x);

                    const sse2d y = addpd(mulpd(loadpd(bbox + 2), w1), mulpd(loadpd(bbox + 8), w2));
                    _mm_storel_pd(bbox_data + 5, y);
                    _mm_storeh_pd(bbox_data + 7, y);

                    const sse2d z = addpd(mulpd(loadpd(bbox + 4), w1), mulpd(loadpd(bbox + 10), w2));
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

                const sse2d mbbminx2d = loadpd(bbox_data + 0);
                const sse2d mbbmaxx2d = loadpd(bbox_data + 2);
                const sse2d mx1 = mulpd(mrrcpdx, subpd(mbbminx2d, mrox));
                const sse2d mx2 = mulpd(mrrcpdx, subpd(mbbmaxx2d, mrox));

                mtmax = maxpd(minpd(mx1, mposinf), minpd(mx2, mposinf));
                mtmin = minpd(maxpd(mx1, mneginf), maxpd(mx2, mneginf));

                const sse2d mbbminy2d = loadpd(bbox_data + 4);
                const sse2d mbbmaxy2d = loadpd(bbox_data + 6);
                const sse2d my1 = mulpd(mrrcpdy, subpd(mbbminy2d, mroy));
                const sse2d my2 = mulpd(mrrcpdy, subpd(mbbmaxy2d, mroy));

                mtmax = minpd(mtmax, maxpd(minpd(my1, mposinf), minpd(my2, mposinf)));
                mtmin = maxpd(mtmin, minpd(maxpd(my1, mneginf), maxpd(my2, mneginf)));

                const sse2d mbbminz2d = loadpd(bbox_data + 8);
                const sse2d mbbmaxz2d = loadpd(bbox_data + 10);
                const sse2d mz1 = mulpd(mrrcpdz, subpd(mbbminz2d, mroz));
                const sse2d mz2 = mulpd(mrrcpdz, subpd(mbbmaxz2d, mroz));

                mtmax = minpd(mtmax, maxpd(minpd(mz1, mposinf), minpd(mz2, mposinf)));
                mtmin = maxpd(mtmin, minpd(maxpd(mz1, mneginf), maxpd(mz2, mneginf)));
            }

            const sse2d mraytmax = set1pd(ray_tmax);
            const int hits =
                movemaskpd(
                    orpd(
                        cmpgtpd(mtmin, mtmax),
                        orpd(
                            cmpltpd(mtmax, mraytmin),
                            cmpgepd(mtmin, mraytmax)))) ^ 3;

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
                    movemaskpd(
                        cmpltpd(
                            mtmin,
                            shufflepd(mtmin, mtmin, _MM_SHUFFLE2(1, 1))));
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
