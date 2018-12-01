
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
#include "foundation/math/aabb.h"
#include "foundation/math/scalar.h"
#include "foundation/math/split.h"
#include "foundation/platform/timers.h"
#include "foundation/utility/stopwatch.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <memory>
#include <queue>

namespace foundation {
namespace bsp {

// todo: limit the size of the priority queue of leaf nodes?
// todo: introduce a global splitting threshold below which
//       leaves would not be split further?


//
// The LeafInfo structure contains information about leaves of a BSP tree.
// These data are not stored into the tree, but are computed and updated
// during the splitting procedure.
//

template <typename T, size_t N>
class LeafInfo
{
  public:
    // Value type and dimension.
    typedef T ValueType;
    static const size_t Dimension = N;

    // AABB and leaf info type.
    typedef AABB<T, N> AABBType;
    typedef LeafInfo<T, N> LeafInfoType;

    // Constructor.
    LeafInfo(
        const size_t        node_depth,         // depth of the leaf node in the tree
        const AABBType&     leaf_bbox);         // bounding box of the leaf

    // Return the depth of the leaf node in the tree.
    size_t get_node_depth() const;

    // Return the bounding box of the leaf.
    const AABBType& get_bbox() const;

  private:
    size_t                  m_node_depth;       // depth of the leaf node
    AABBType                m_leaf_bbox;        // bounding box of the leaf
};


//
// The LeafRecord structure allows to keep track of the nodes to split.
//

template <typename T, size_t N>
class LeafRecord
{
  public:
    // LeafInfo type.
    typedef LeafInfo<T, N> LeafInfoType;

    // Constructor.
    LeafRecord(
        const size_t        node_index,         // index of the leaf node in Tree::m_nodes
        const LeafInfoType& leaf_info,          // additional leaf information
        const double        leaf_priority);     // splitting priority of the leaf

    // Sorting predicate.
    bool operator<(const LeafRecord& rhs) const;

    // Return the index of the leaf node in Tree::m_nodes.
    size_t get_node_index() const;

    // Return the leaf info.
    const LeafInfoType& get_leaf_info() const;

  private:
    size_t                  m_node_index;       // index of the leaf node in Tree::m_nodes
    LeafInfoType            m_leaf_info;        // additional leaf information
    double                  m_leaf_priority;    // splitting priority of the leaf
};


//
// BSP tree builder.
//
// The LeafFactory class must conform to the following prototype:
//
//      class LeafFactory
//        : public foundation::NonCopyable
//      {
//        public:
//          // Create a new leaf.
//          Leaf* create_leaf();
//      };
//
//
// The LeafSplitter class must conform to the following prototype:
//
//      class LeafSplitter
//        : public foundation::NonCopyable
//      {
//        public:
//          // Return the splitting priority of a leaf.
//          double get_priority(
//              const Leaf&             leaf,
//              const LeafInfoType&     leaf_info) const;
//
//          // Find a split.
//          bool split(
//              const Leaf&             leaf,
//              const LeafInfoType&     leaf_info,
//              SplitType&              split);
//
//          // Sort a leaf into child leaves, according to a given split.
//          void sort(
//              const Leaf&             leaf,
//              const LeafInfoType&     leaf_info,
//              const SplitType&        split,
//              Leaf&                   left_leaf,
//              const LeafInfoType&     left_leaf_info,
//              Leaf&                   right_leaf,
//              const LeafInfoType&     right_leaf_info);
//      };
//

template <
    typename Tree,
    typename LeafFactory,
    typename LeafSplitter,
    typename Timer = DefaultWallclockTimer
>
class Builder
  : public NonCopyable
{
  public:
    // Types.
    typedef typename Tree::ValueType ValueType;
    typedef typename Tree::AABBType AABBType;
    typedef typename Tree::NodeType NodeType;
    typedef typename Tree::LeafType LeafType;
    typedef Tree TreeType;

    // Constructor.
    Builder();

    // Build a BSP tree for a given root leaf.
    void build(
        TreeType&                   tree,
        std::unique_ptr<LeafType>   root_leaf,
        LeafFactory&                factory,
        LeafSplitter&               splitter,
        const double                max_duplication_rate = 2.0);

    // Return the construction time.
    double get_build_time() const;

  private:
    typedef Split<ValueType> SplitType;
    typedef LeafInfo<ValueType, Tree::Dimension> LeafInfoType;
    typedef LeafRecord<ValueType, Tree::Dimension> LeafRecordType;
    typedef std::priority_queue<LeafRecordType> LeafQueue;

    double m_build_time;

    // Insert a leaf into a leaf queue.
    static void insert_leaf_record(
        TreeType&                   tree,
        LeafSplitter&               splitter,
        LeafQueue&                  leaf_queue,
        const LeafInfoType&         leaf_info,
        const size_t                leaf_index,
        const size_t                node_index);

    // Create the root of the tree.
    void create_root(
        TreeType&                   tree,
        std::unique_ptr<LeafType>   root_leaf);

    // Subdivide the tree.
    void subdivide(
        TreeType&                   tree,
        LeafFactory&                factory,
        LeafSplitter&               splitter,
        const double                max_duplication_rate);
};


//
// LeafInfo class implementation.
//

template <typename T, size_t N>
inline LeafInfo<T, N>::LeafInfo(
    const size_t        node_depth,     // depth of the leaf node in the tree
    const AABBType&     leaf_bbox)      // bounding box of the leaf
  : m_node_depth(node_depth)
  , m_leaf_bbox(leaf_bbox)
{
}

template <typename T, size_t N>
inline size_t LeafInfo<T, N>::get_node_depth() const
{
    return m_node_depth;
}

template <typename T, size_t N>
inline const AABB<T, N>& LeafInfo<T, N>::get_bbox() const
{
    return m_leaf_bbox;
}


//
// LeafRecord class implementation.
//

template <typename T, size_t N>
inline LeafRecord<T, N>::LeafRecord(
    const size_t        node_index,     // index of the leaf node in Tree::m_nodes
    const LeafInfoType& leaf_info,      // additional leaf information
    const double        leaf_priority)  // splitting priority of the leaf
  : m_node_index(node_index)
  , m_leaf_info(leaf_info)
  , m_leaf_priority(leaf_priority)
{
}

template <typename T, size_t N>
inline bool LeafRecord<T, N>::operator<(const LeafRecord& rhs) const
{
    return m_leaf_priority < rhs.m_leaf_priority;
}

template <typename T, size_t N>
inline size_t LeafRecord<T, N>::get_node_index() const
{
    return m_node_index;
}

template <typename T, size_t N>
inline const LeafInfo<T, N>& LeafRecord<T, N>::get_leaf_info() const
{
    return m_leaf_info;
}


//
// Builder class implementation.
//

// Return the appropriate epsilon factor for enlarging bounding boxes.
template <typename U> U get_bbox_grow_eps();            // intentionally left unimplemented
template <> inline float get_bbox_grow_eps<float>()     { return 1.0e-4f; }
template <> inline double get_bbox_grow_eps<double>()   { return 1.0e-9;  }

template <
    typename Tree,
    typename LeafFactory,
    typename LeafSplitter,
    typename Timer
>
Builder<Tree, LeafFactory, LeafSplitter, Timer>::Builder()
  : m_build_time(0.0)
{
}

template <
    typename Tree,
    typename LeafFactory,
    typename LeafSplitter,
    typename Timer
>
void Builder<Tree, LeafFactory, LeafSplitter, Timer>::build(
    TreeType&                   tree,
    std::unique_ptr<LeafType>   root_leaf,
    LeafFactory&                factory,
    LeafSplitter&               splitter,
    const double                max_duplication_rate)
{
    // Start stopwatch.
    Stopwatch<Timer> stopwatch;
    stopwatch.start();

    // Clear the BSP tree.
    tree.clear();

    // Initialize the bounding box of the tree.
    tree.m_bbox = root_leaf->get_bbox();

    // Slightly enlarge the bounding box of the tree to avoid
    // missing intersections with entities lying exactly on a
    // wall of the bounding box.
    if (tree.m_bbox.is_valid())
    {
        const ValueType eps = get_bbox_grow_eps<ValueType>();
        tree.m_bbox.robust_grow(eps);
    }

    // Create the root of the tree.
    create_root(tree, std::move(root_leaf));

    // Subdivide the tree.
    subdivide(tree, factory, splitter, max_duplication_rate);

    // Measure and save construction time.
    stopwatch.measure();
    m_build_time = stopwatch.get_seconds();
}

template <
    typename Tree,
    typename LeafFactory,
    typename LeafSplitter,
    typename Timer
>
double Builder<Tree, LeafFactory, LeafSplitter, Timer>::get_build_time() const
{
    return m_build_time;
}

template <
    typename Tree,
    typename LeafFactory,
    typename LeafSplitter,
    typename Timer
>
void Builder<Tree, LeafFactory, LeafSplitter, Timer>::insert_leaf_record(
    TreeType&                   tree,
    LeafSplitter&               splitter,
    LeafQueue&                  leaf_queue,
    const LeafInfoType&         leaf_info,
    const size_t                leaf_index,
    const size_t                node_index)
{
    // Compute the splitting priority of the leaf.
    const LeafType& leaf = *tree.m_leaves[leaf_index];
    const double leaf_priority = splitter.get_priority(leaf, leaf_info);

    // Insert the leaf into the leaf queue if it needs to be split.
    if (leaf_priority > 0.0)
    {
        const LeafRecordType leaf_record(
            node_index,
            leaf_info,
            leaf_priority);
        leaf_queue.push(leaf_record);
    }
}

template <
    typename Tree,
    typename LeafFactory,
    typename LeafSplitter,
    typename Timer
>
void Builder<Tree, LeafFactory, LeafSplitter, Timer>::create_root(
    TreeType&                   tree,
    std::unique_ptr<LeafType>   root_leaf)
{
    assert(tree.m_leaves.empty());
    assert(tree.m_nodes.empty());

    // Store the root leaf.
    tree.m_leaves.push_back(root_leaf.release());

    // Create the root node.
    NodeType root_node;
    root_node.make_leaf();
    root_node.set_leaf_index(0);
    root_node.set_leaf_size(tree.m_leaves.front()->get_size());
    tree.m_nodes.push_back(root_node);
}

template <
    typename Tree,
    typename LeafFactory,
    typename LeafSplitter,
    typename Timer
>
void Builder<Tree, LeafFactory, LeafSplitter, Timer>::subdivide(
    TreeType&                   tree,
    LeafFactory&                factory,
    LeafSplitter&               splitter,
    const double                max_duplication_rate)
{
    // Create the leaf queue, keeping track of leaves that needs
    // splitting, and ordering them by splitting priority.
    LeafQueue leaf_queue;

    // Insert the root leaf into the leaf queue.
    const LeafInfoType root_leaf_info(0, tree.m_bbox);
    insert_leaf_record(
        tree,               // tree
        splitter,           // leaf splitter
        leaf_queue,         // leaf queue
        root_leaf_info,     // leaf info
        0,                  // leaf index
        0);                 // node index

    // Temporary, working leaves.
    LeafType* left_leaf = factory.create_leaf();
    LeafType* right_leaf = factory.create_leaf();

    // Initially, the tree contains as many objects as the root leaf.
    size_t tree_size = tree.m_leaves.front()->get_size();

    // Compute maximum tree size, given the maximum duplication rate.
    const size_t max_tree_size = truncate<size_t>(tree_size * max_duplication_rate);

    // Subdivide until there is no node to split anymore,
    // or until the maximum tree size has been reached.
    while (!leaf_queue.empty() && tree_size < max_tree_size)
    {
        // Get leaf record and leaf info of next leaf to split.
        const LeafRecordType leaf_record = leaf_queue.top();
        const LeafInfoType& leaf_info = leaf_record.get_leaf_info();
        leaf_queue.pop();

        // Get leaf node.
        NodeType& node = tree.m_nodes[leaf_record.get_node_index()];
        assert(node.is_leaf());

        // Get leaf.
        const size_t leaf_index = node.get_leaf_index();
        LeafType& leaf = *tree.m_leaves[leaf_index];

        // Attempt to split the leaf.
        SplitType split;
        if (splitter.split(leaf, leaf_info, split))
        {
            // Compute the indices of the child leaves and child nodes.
            const size_t left_leaf_index  = leaf_index;
            const size_t right_leaf_index = tree.m_leaves.size();
            const size_t left_node_index  = tree.m_nodes.size();
            const size_t right_node_index = left_node_index + 1;

            // Compute the depth of the child leaves.
            const size_t child_depth = leaf_info.get_node_depth() + 1;

            // Compute the bounding box of the child leaves.
            AABBType left_leaf_bbox;
            AABBType right_leaf_bbox;
            split_bbox(
                leaf_info.get_bbox(),
                split,
                left_leaf_bbox,
                right_leaf_bbox);

            // Create LeafInfo for left and right leaves.
            const LeafInfoType left_leaf_info(child_depth, left_leaf_bbox);
            const LeafInfoType right_leaf_info(child_depth, right_leaf_bbox);

            // Sort the elements of the leaf into the child leaves.
            splitter.sort(
                leaf,
                leaf_info,
                split,
                *left_leaf,
                left_leaf_info,
                *right_leaf,
                right_leaf_info);

            // Count the number of objects in the parent, left and right leaves.
            const size_t leaf_size = leaf.get_size();
            const size_t left_leaf_size = left_leaf->get_size();
            const size_t right_leaf_size = right_leaf->get_size();
            assert(left_leaf_size + right_leaf_size >= leaf_size);

            // Update the tree size.
            tree_size += left_leaf_size + right_leaf_size;
            if (tree_size > leaf_size)
                tree_size -= leaf_size;
            else tree_size = 0;

            // Replace the parent leaf with its left child.
            std::swap(tree.m_leaves[leaf_index], left_leaf);

            // Append the right child to the leaf vector.
            tree.m_leaves.push_back(right_leaf);

            // Recycle the left leaf (which is really the parent leaf)
            // and create a new right leaf.
            left_leaf->clear();
            right_leaf = factory.create_leaf();

            // Transform the leaf node to an interior node.
            node.make_interior();
            node.set_child_node_index(left_node_index);
            node.set_split_dim(split.m_dimension);
            node.set_split_abs(split.m_abscissa);

            // Create the left node.
            NodeType left_node;
            left_node.make_leaf();
            left_node.set_leaf_index(left_leaf_index);
            left_node.set_leaf_size(left_leaf_size);
            tree.m_nodes.push_back(left_node);

            // Create the right node.
            NodeType right_node;
            right_node.make_leaf();
            right_node.set_leaf_index(right_leaf_index);
            right_node.set_leaf_size(right_leaf_size);
            tree.m_nodes.push_back(right_node);

            // Insert the left leaf into the leaf queue.
            insert_leaf_record(
                tree,                   // tree
                splitter,               // leaf splitter
                leaf_queue,             // leaf queue
                left_leaf_info,         // leaf info
                left_leaf_index,        // leaf index
                left_node_index);       // node index

            // Insert the right leaf into the leaf queue.
            insert_leaf_record(
                tree,                   // tree
                splitter,               // leaf splitter
                leaf_queue,             // leaf queue
                right_leaf_info,        // leaf info
                right_leaf_index,       // leaf index
                right_node_index);      // node index
        }
    }

    delete left_leaf;
    delete right_leaf;
}

}   // namespace bsp
}   // namespace foundation
