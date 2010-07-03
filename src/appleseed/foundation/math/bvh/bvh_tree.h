
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

#ifndef APPLESEED_FOUNDATION_MATH_BVH_BVH_TREE_H
#define APPLESEED_FOUNDATION_MATH_BVH_BVH_TREE_H

// appleseed.foundation headers.
#include "foundation/core/concepts.h"
#include "foundation/math/bvh/bvh_node.h"
#include "foundation/math/aabb.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <vector>

namespace foundation {
namespace bvh {

//
// Bounding Volume Hierarchy (BVH).
//

template <typename T, size_t N, typename Item>
class Tree
  : public NonCopyable
{
  public:
    // Value type and dimension.
    typedef T ValueType;
    static const size_t Dimension = N;

    // Item, AABB, node and tree types.
    typedef Item ItemType;
    typedef AABB<T, N> AABBType;
    typedef Node<T, N> NodeType;
    typedef Tree<T, N, Item> TreeType;

    // Constructor.
    Tree();

    // Clear the tree.
    void clear();

    // Insert an item into the tree.
    void insert(
        const ItemType& item,
        const AABBType& bbox);

    // Return the number of items in the tree.
    size_t size() const;

    // Return the bounding box of the tree.
    const AABBType& get_bbox() const;

    // Return the size (in bytes) of this object in memory.
    size_t get_memory_size() const;

  protected:
    template <
        typename Tree,
        typename Partitioner,
        typename Timer
    >
    friend class Builder;

    template <
        typename T_,
        typename Tree,
        typename Visitor,
        size_t StackSize,
        size_t SortSize
    >
    friend class Intersector;

    template <typename Tree, typename Builder>
    friend class TreeStatistics;

    typedef std::vector<NodeType> NodeVector;
    typedef std::vector<ItemType> ItemVector;
    typedef std::vector<AABBType> AABBVector;

    ItemVector  m_items;        // items
    AABBVector  m_bboxes;       // bounding box of each item
    AABBType    m_bbox;         // bounding box of the tree
    NodeVector  m_nodes;        // nodes of the tree
};


//
// Tree class implementation.
//

// Return the appropriate epsilon factor for enlarging an item bounding box.
template <typename U> U get_item_bbox_grow_eps();           // intentionally left unimplemented
template <> inline float get_item_bbox_grow_eps<float>()    { return 1.0e-6f; }
template <> inline double get_item_bbox_grow_eps<double>()  { return 1.0e-15;  }

// Constructor.
template <typename T, size_t N, typename Item>
Tree<T, N, Item>::Tree()
{
    clear();
}

// Clear the tree.
template <typename T, size_t N, typename Item>
void Tree<T, N, Item>::clear()
{
    m_items.clear();
    m_bboxes.clear();
    m_bbox.invalidate();
    m_nodes.clear();
}

// Insert an item into the tree.
template <typename T, size_t N, typename Item>
void Tree<T, N, Item>::insert(
    const ItemType& item,
    const AABBType& bbox)
{
    assert(bbox.is_valid());

    AABBType enlarged_bbox = bbox;
    enlarged_bbox.robust_grow(get_item_bbox_grow_eps<T>());

    m_items.push_back(item);
    m_bboxes.push_back(enlarged_bbox);
    m_bbox.insert(enlarged_bbox);

    assert(m_items.size() == m_bboxes.size());
}

// Return the number of items in the tree.
template <typename T, size_t N, typename Item>
inline size_t Tree<T, N, Item>::size() const
{
    return m_items.size();
}

// Return the bounding box of the tree.
template <typename T, size_t N, typename Item>
inline const AABB<T, N>& Tree<T, N, Item>::get_bbox() const
{
    return m_bbox;
}

// Return the size (in bytes) of this object in memory.
template <typename T, size_t N, typename Item>
size_t Tree<T, N, Item>::get_memory_size() const
{
    size_t mem_size = sizeof(*this);
    mem_size += m_items.capacity() * sizeof(ItemType);
    mem_size += m_bboxes.capacity() * sizeof(AABBType);
    mem_size += m_nodes.capacity() * sizeof(NodeType);
    return mem_size;
}

}       // namespace bvh
}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_BVH_BVH_TREE_H
