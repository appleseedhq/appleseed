
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

#ifndef APPLESEED_FOUNDATION_UTILITY_POOLALLOCATOR_H
#define APPLESEED_FOUNDATION_UTILITY_POOLALLOCATOR_H

// appleseed.foundation headers.
#include "foundation/core/concepts/singleton.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <limits>
#include <memory>

namespace foundation
{

//
// A standard-conformant, single-threaded, fixed-size object allocator.
// The implementation is essentially the same as boost's quick_allocator.
//
// Note that memory allocated through is allocator is never returned to
// the system, and thus is never made available for other uses.
//

namespace impl
{
    template <
        size_t ItemSize,    // in bytes
        size_t PageSize     // in bytes
    >
    class Pool
      : public Singleton<Pool<ItemSize, PageSize> >
    {
      public:
        // Constructor.
        Pool()
          : m_page(0)
          , m_free_head(0)
          , m_free_index(ItemsPerPage)
        {
        }

        // Allocate a memory block.
        void* allocate()
        {
            if (Node* node = m_free_head)
            {
                m_free_head = m_free_head->m_next;
                return node;
            }
            else
            {
                if (m_free_index == ItemsPerPage)
                {
                    m_page = new Node[ItemsPerPage];
                    m_free_index = 0;
                }

                return &m_page[m_free_index++];
            }
        }

        // Return a memory block to the pool.
        void deallocate(void* p)
        {
            assert(p);
            Node* node = static_cast<Node*>(p);
            node->m_next = m_free_head;
            m_free_head = node;
        }

      private:
        enum { ItemsPerPage = PageSize / ItemSize };

        union Node
        {
            // todo: handle alignment.
            uint8   m_item[ItemSize];
            Node*   m_next;
        };

        Node*       m_page;
        Node*       m_free_head;
        size_t      m_free_index;
    };
}

template <
    typename    T,
    size_t      PageSize = 512,     // in bytes
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
    typedef ptrdiff_t           difference_type;

    template <typename U>
    struct rebind
    {
        typedef PoolAllocator<
            U,
            PageSize,
            typename FallBackAllocator::template rebind<U>::other
        > other;
    };

    // Constructors.
    explicit PoolAllocator(FallBackAllocator allocator = FallBackAllocator())
      : m_pool(Pool::instance())
      , m_fallback_alloc(allocator)
    {
    }
    template <typename U>
    PoolAllocator(const PoolAllocator<U, PageSize, typename FallBackAllocator::template rebind<U>::other>& rhs)
      : m_pool(Pool::instance())
      , m_fallback_alloc(rhs.m_fallback_alloc)
    {
    }

    void operator=(const PoolAllocator& rhs)
    {
        m_fallback_alloc = rhs.m_fallback_alloc;
    }

    pointer address(reference x) const
    {
        return &x;
    }
    const_pointer address(const_reference x) const
    {
        return &x;
    }

    pointer allocate(size_type n, const_pointer hint = 0)
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
    template <typename, size_t, typename>
    friend class PoolAllocator;

    typedef impl::Pool<sizeof(value_type), PageSize> Pool;

    Pool&               m_pool;
    FallBackAllocator   m_fallback_alloc;
};

// Partial specialization for T = void.
template <
    size_t      PageSize,
    typename    FallBackAllocator
>
class PoolAllocator<void, PageSize, FallBackAllocator>
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
            PageSize,
            typename FallBackAllocator::template rebind<U>::other
        > other;
    };

    // Constructors.
    explicit PoolAllocator(FallBackAllocator allocator = FallBackAllocator())
      : m_fallback_alloc(allocator)
    {
    }
    template <typename U>
    PoolAllocator(const PoolAllocator<U, PageSize, typename FallBackAllocator::template rebind<U>::other>& rhs)
      : m_fallback_alloc(rhs.m_fallback_alloc)
    {
    }

  private:
    FallBackAllocator m_fallback_alloc;
};

template <
    typename    LhsT,
    typename    RhsT,
    size_t      PageSize,
    typename    FallBackAllocator
>
inline bool operator==(
    const PoolAllocator<LhsT, PageSize, FallBackAllocator>&,
    const PoolAllocator<RhsT, PageSize, FallBackAllocator>&)
{
    // Pool allocators with same page sizes and fall back allocators are always equal.
    return true;
}

template <
    typename    LhsT,
    typename    RhsT,
    size_t      LhsPageSize,
    size_t      RhsPageSize,
    typename    LhsFallBackAllocator,
    typename    RhsFallBackAllocator
>
inline bool operator==(
    const PoolAllocator<LhsT, LhsPageSize, LhsFallBackAllocator>&,
    const PoolAllocator<RhsT, RhsPageSize, RhsFallBackAllocator>&)
{
    // Pool allocators with different page sizes or fall back allocators are never equal.
    return false;
}

template <
    typename    LhsT,
    typename    RhsT,
    size_t      LhsPageSize,
    size_t      RhsPageSize,
    typename    LhsFallBackAllocator,
    typename    RhsFallBackAllocator
>
inline bool operator!=(
    const PoolAllocator<LhsT, LhsPageSize, LhsFallBackAllocator>& lhs,
    const PoolAllocator<RhsT, RhsPageSize, RhsFallBackAllocator>& rhs)
{
    return !operator==(lhs, rhs);
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_POOLALLOCATOR_H
