
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_INTERSECTION_TREEREPOSITORY_H
#define APPLESEED_RENDERER_KERNEL_INTERSECTION_TREEREPOSITORY_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/platform/types.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/lazy.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <map>
#include <utility>

namespace renderer
{

//
// A collection of trees indexed by their 64-bit key (typically a hash of their content).
//

template <typename TreeType>
class TreeRepository
  : public foundation::NonCopyable
{
  public:
    typedef foundation::Lazy<TreeType> LazyTreeType;

    ~TreeRepository();

    void insert(const foundation::uint64 key, LazyTreeType* tree);

    LazyTreeType* acquire(const foundation::uint64 key);
    void release(LazyTreeType* tree);

    template <typename Func>
    void for_each(Func& func);

  private:
    struct TreeInfo
    {
        LazyTreeType*   m_tree;
        size_t          m_ref;
    };

    typedef std::map<foundation::uint64, TreeInfo> TreeContainer;
    typedef std::map<LazyTreeType*, foundation::uint64> TreeIndex;

    TreeContainer       m_trees;
    TreeIndex           m_index;
};


//
// TreeRepository class implementation.
//

template <typename TreeType>
TreeRepository<TreeType>::~TreeRepository()
{
    for (foundation::each<TreeContainer> i = m_trees; i; ++i)
        delete i->second.m_tree;
}

template <typename TreeType>
void TreeRepository<TreeType>::insert(const foundation::uint64 key, LazyTreeType* tree)
{
    assert(m_trees.find(key) == m_trees.end());

    TreeInfo info;
    info.m_tree = tree;
    info.m_ref = 1;

    m_trees.insert(std::make_pair(key, info));
    m_index.insert(std::make_pair(tree, key));
}

template <typename TreeType>
typename TreeRepository<TreeType>::LazyTreeType* TreeRepository<TreeType>::acquire(const foundation::uint64 key)
{
    const typename TreeContainer::iterator i = m_trees.find(key);

    if (i == m_trees.end())
        return 0;

    ++i->second.m_ref;
    return i->second.m_tree;
}

template <typename TreeType>
void TreeRepository<TreeType>::release(LazyTreeType* tree)
{
    const typename TreeIndex::iterator i = m_index.find(tree);
    assert(i != m_index.end());

    const typename TreeContainer::iterator t = m_trees.find(i->second);
    assert(t != m_trees.end());

    assert(t->second.m_ref > 0);
    --t->second.m_ref;

    if (t->second.m_ref == 0)
    {
        delete t->second.m_tree;
        m_trees.erase(t);
        m_index.erase(i);
    }
}

template <typename TreeType>
template <typename Func>
void TreeRepository<TreeType>::for_each(Func& func)
{
    for (foundation::each<TreeContainer> i = m_trees; i; ++i)
        func(*(i->second.m_tree), i->second.m_ref);
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_INTERSECTION_TREEREPOSITORY_H
