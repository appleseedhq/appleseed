
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
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
#include "foundation/utility/foreach.h"
#include "foundation/utility/lazy.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdint>
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

    void insert(const std::uint64_t key, LazyTreeType* tree);

    LazyTreeType* acquire(const std::uint64_t key);
    void release(LazyTreeType* tree);

    template <typename Func>
    void for_each(Func& func);

  private:
    struct TreeInfo
    {
        LazyTreeType*   m_tree;
        size_t          m_ref;
    };

    typedef std::map<std::uint64_t, TreeInfo> TreeContainer;
    typedef std::map<LazyTreeType*, std::uint64_t> TreeIndex;

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
void TreeRepository<TreeType>::insert(const std::uint64_t key, LazyTreeType* tree)
{
    assert(m_trees.find(key) == m_trees.end());

    TreeInfo info;
    info.m_tree = tree;
    info.m_ref = 1;

    m_trees.insert(std::make_pair(key, info));
    m_index.insert(std::make_pair(tree, key));
}

template <typename TreeType>
typename TreeRepository<TreeType>::LazyTreeType* TreeRepository<TreeType>::acquire(const std::uint64_t key)
{
    const typename TreeContainer::iterator i = m_trees.find(key);

    if (i == m_trees.end())
        return nullptr;

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

}   // namespace renderer
