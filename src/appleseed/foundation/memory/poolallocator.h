
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
#include "foundation/core/concepts/singleton.h"
#include "foundation/platform/thread.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <memory>

namespace foundation
{

//
// A standard-conformant, thread-safe, fixed-size object allocator.
//
// Note that memory allocated through this allocator is never returned
// to the system, and thus is never made available for other uses.
//

namespace impl
{
    template <
        size_t ItemSize,    // in bytes
        size_t ItemsPerPage
    >
    class Pool
      : public Singleton<Pool<ItemSize, ItemsPerPage>>
    {
      public:
        // Allocate a memory block.
        void* allocate()
        {
            Spinlock::ScopedLock lock(m_spinlock);

            if (Node* node = m_free_head)
            {
                // Return the first node from the list of free nodes.
                m_free_head = m_free_head->m_next;
                return node;
            }
            else
            {
                // The current page is full, allocate a new page of nodes.
                if (m_page_index == ItemsPerPage)
                {
                    m_page = new Node[ItemsPerPage];
                    m_page_index = 0;
                }

                // Return the next node from the page.
                return &m_page[m_page_index++];
            }
        }

        // Return a memory block to the pool.
        void deallocate(void* p)
        {
            Spinlock::ScopedLock lock(m_spinlock);

            assert(p);

            Node* node = static_cast<Node*>(p);

            // Insert this node at the beginning of the list of free nodes.
            node->m_next = m_free_head;
            m_free_head = node;
        }

      private:
        friend class Singleton<Pool<ItemSize, ItemsPerPage>>;

        union Node
        {
            std::uint8_t    m_item[ItemSize];   // the actual storage for one item
            Node*           m_next;             // pointer to the next free node
        };

        Spinlock    m_spinlock;
        Node*       m_page;
        size_t      m_page_index;
        Node*       m_free_head;

        // Constructor.
        Pool()
          : m_page(nullptr)
          , m_page_index(ItemsPerPage)
          , m_free_head(nullptr)
        {
        }
    };
}

template <
    typename    T,
    size_t      ItemsPerPage,
    typename    FallBackAllocator = std::allocator<T>
>
class PoolAllocator
{
  public:
    typedef T                   value_type;
    typedef value_type*         pointer;
    typedef const value_type*   const_pointer;
    typedef value_type&         reference;
    typedef const value_type&   const_reference;
    typedef size_t              size_type;
    typedef std::ptrdiff_t      difference_type;

    template <typename U>
    struct rebind
    {
        typedef PoolAllocator<
            U,
            ItemsPerPage,
            typename FallBackAllocator::template rebind<U>::other
        > other;
    };

    explicit PoolAllocator(FallBackAllocator allocator = FallBackAllocator())
      : m_pool(Pool::instance())
      , m_fallback_alloc(allocator)
    {
    }

    template <typename U>
    PoolAllocator(const PoolAllocator<U, ItemsPerPage, typename FallBackAllocator::template rebind<U>::other>& rhs)
      : m_pool(Pool::instance())
      , m_fallback_alloc(rhs.m_fallback_alloc)
    {
    }

    PoolAllocator(const PoolAllocator& rhs)
      : m_pool(rhs.m_pool)
      , m_fallback_alloc(rhs.m_fallback_alloc)
    {
    }

    PoolAllocator& operator=(const PoolAllocator& rhs)
    {
        m_fallback_alloc = rhs.m_fallback_alloc;
        return *this;
    }

    pointer address(reference x) const
    {
        return &x;
    }

    const_pointer address(const_reference x) const
    {
        return &x;
    }

    pointer allocate(size_type n, const_pointer hint = nullptr)
    {
        return n == 1
            ? static_cast<pointer>(m_pool.allocate())
            : m_fallback_alloc.allocate(n, hint);
    }

    void deallocate(pointer p, size_type n)
    {
        if (p && n == 1)
            m_pool.deallocate(p);
        else m_fallback_alloc.deallocate(p, n);
    }

    size_type max_size() const
    {
        return std::numeric_limits<size_type>::max() / sizeof(T);
    }

    void construct(pointer p, const_reference x)
    {
        new(p) value_type(x);
    }

    void destroy(pointer p)
    {
        p->~value_type();
    }

  private:
    // Allow allocators of different types to access each other private members.
    template <typename, size_t, typename>
    friend class PoolAllocator;

    typedef impl::Pool<sizeof(value_type), ItemsPerPage> Pool;

    Pool&               m_pool;
    FallBackAllocator   m_fallback_alloc;
};

// A partial specialization for the void value type is required for rebinding
// to another, different value type.
template <
    size_t      ItemsPerPage,
    typename    FallBackAllocator
>
class PoolAllocator<void, ItemsPerPage, FallBackAllocator>
{
  public:
    typedef void                value_type;
    typedef value_type*         pointer;
    typedef const value_type*   const_pointer;

    template <typename U>
    struct rebind
    {
        typedef PoolAllocator<
            U,
            ItemsPerPage,
            typename FallBackAllocator::template rebind<U>::other
        > other;
    };

    explicit PoolAllocator(FallBackAllocator allocator = FallBackAllocator())
      : m_fallback_alloc(allocator)
    {
    }

    template <typename U>
    PoolAllocator(const PoolAllocator<U, ItemsPerPage, typename FallBackAllocator::template rebind<U>::other>& rhs)
      : m_fallback_alloc(rhs.m_fallback_alloc)
    {
    }

  private:
    // Allow allocators of different types to access each other private members.
    template <typename, size_t, typename>
    friend class PoolAllocator;

    FallBackAllocator m_fallback_alloc;
};

template <
    typename    T,
    size_t      ItemsPerPage,
    typename    FallBackAllocator
>
inline bool operator==(
    const PoolAllocator<T, ItemsPerPage, FallBackAllocator>&,
    const PoolAllocator<T, ItemsPerPage, FallBackAllocator>&)
{
    // Pool allocators for the same type, with the same number of items per
    // page and the same fall back allocators are considered equal.
    return true;
}

template <
    typename    LhsT,
    typename    RhsT,
    size_t      LhsItemsPerPage,
    size_t      RhsItemsPerPage,
    typename    LhsFallBackAllocator,
    typename    RhsFallBackAllocator
>
inline bool operator==(
    const PoolAllocator<LhsT, LhsItemsPerPage, LhsFallBackAllocator>&,
    const PoolAllocator<RhsT, RhsItemsPerPage, RhsFallBackAllocator>&)
{
    // Pool allocators for different types, with different numbers of items per
    // page or with different fall back allocators are not considered equal.
    return false;
}

template <
    typename    LhsT,
    typename    RhsT,
    size_t      LhsItemsPerPage,
    size_t      RhsItemsPerPage,
    typename    LhsFallBackAllocator,
    typename    RhsFallBackAllocator
>
inline bool operator!=(
    const PoolAllocator<LhsT, LhsItemsPerPage, LhsFallBackAllocator>& lhs,
    const PoolAllocator<RhsT, RhsItemsPerPage, RhsFallBackAllocator>& rhs)
{
    return !operator==(lhs, rhs);
}

}   // namespace foundation
