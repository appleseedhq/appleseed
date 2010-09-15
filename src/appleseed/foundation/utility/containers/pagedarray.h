
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

#ifndef APPLESEED_FOUNDATION_UTILITY_CONTAINERS_PAGEDARRAY_H
#define APPLESEED_FOUNDATION_UTILITY_CONTAINERS_PAGEDARRAY_H

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstring>
#include <stdexcept>
#include <vector>

namespace foundation
{

// todo: implement iterators (at the moment, they are only declared).
// todo: improve STL conformance.
// todo: implement constant time insertion and deletion in the middle.


//
// Paged array with STL-like interface. Very similar to std::deque, with control
// over the page size. The page size is the number of elements contained in one
// page.
//

template <typename Type, size_t PageSize>
class PagedArray
{
  private:
    // Page.
    struct Page
    {
        Type m_elements[PageSize];

        // Constructors.
        Page();                                     // leave all elements uninitialized
        Page(const Page& rhs);                      // copy constructor
        Page(Type val);                             // set all components to 'val'
    };

    // Page vector.
    typedef std::vector<Page*> PageVector;

  public:
    // STL-like types.
    typedef Type value_type;                        // data type
    typedef value_type& reference;                  // reference to an element
    typedef const value_type& const_reference;      // reference to a const element
    typedef value_type* pointer;                    // pointer to an element
    typedef const value_type* const_pointer;        // pointer to a const element
    typedef size_t size_type;                       // element count and indexing

    // Out of range exception, thrown by at() methods.
    struct ExceptionOutOfRange
      : public std::out_of_range
      , public Exception
    {
        ExceptionOutOfRange()
          : std::out_of_range("invalid element index") {}
    };

    // Iterator.
    class iterator
    {
      public:
        // Copy constructor.
        iterator(const iterator& rhs);

        // Prefix and postfix increment and decrement operators.
        iterator& operator++();                     // prefix increment
        iterator  operator++(int) const;            // postfix increment
        iterator& operator--();                     // prefix decrement
        iterator  operator--(int) const;            // postfix decrement

        // Comparison operators.
        bool operator==(const iterator& rhs) const;
        bool operator!=(const iterator& rhs) const;

        // Dereference operators.
        pointer operator*();
        pointer operator->();

      private:
        friend class PagedArray;

        typedef PagedArray<Type, PageSize>          ArrayType;
        typedef typename ArrayType::PageVector      PageVector;
        typedef typename PageVector::iterator       PageVectorIterator;

        const ArrayType&            m_array;        // parent array
        PageVectorIterator          m_page_it;      // page iterator
        size_t                      m_elem_it;      // element iterator

        // Initializing constructor.
        iterator(
            const ArrayType&        array,
            PageVectorIterator      page_it,
            size_t                  elem_it);
    };

    // Constant iterator.
    class const_iterator
    {
      public:
        // Copy constructor.
        const_iterator(const const_iterator& rhs);

        // Prefix and postfix increment and decrement operators.
        const_iterator& operator++();               // prefix increment
        const_iterator  operator++(int) const;      // postfix increment
        const_iterator& operator--();               // prefix decrement
        const_iterator  operator--(int) const;      // postfix decrement

        // Comparison operators.
        bool operator==(const const_iterator& rhs) const;
        bool operator!=(const const_iterator& rhs) const;

        // Dereference operators.
        const_pointer operator*() const;
        const_pointer operator->() const;

      private:
        friend class PagedArray;

        typedef PagedArray<Type, PageSize>          ArrayType;
        typedef typename ArrayType::PageVector      PageVector;
        typedef typename PageVector::const_iterator PageVectorConstIterator;

        const ArrayType&            m_array;        // parent array
        PageVectorConstIterator     m_page_it;      // page iterator
        size_t                      m_elem_it;      // element iterator

        // Initializing constructor.
        const_iterator(
            const ArrayType&        array,
            PageVectorConstIterator page_it,
            size_t                  elem_it);
    };

    // Constructors.
    PagedArray();
    PagedArray(const PagedArray& rhs);

    // Erases the elements of the vector.
    void clear();

    // Tests if the vector is empty.
    bool empty() const;

    // Returns the number of elements in the vector.
    size_type size() const;

    // Specifies a new size for a vector.
    void resize(size_type new_size, Type val = Type());

    // Adds an element to the end of the vector and returns a reference to it.
    // All references returned by push_back() stay valid as long as the vector
    // itself exists.
    reference push_back(const Type& val);

    // Unchecked array subscripting to elements.
    reference operator[](size_type i);
    const_reference operator[](size_type i) const;

    // Exception-checked access to elements.
    reference at(size_type i);
    const_reference at(size_type i) const;

    // Returns a random-access iterator to the first element in the vector.
    iterator begin();
    const_iterator begin() const;

    // Returns a random-access iterator that points just beyond the end of the vector.
    iterator end();
    const_iterator end() const;

  private:
    friend class iterator;
    friend class const_iterator;

    size_t          m_page_index;                   // index of the current page
    size_t          m_elem_index;                   // index of the current element in the current page
    PageVector      m_pages;                        // paged array elements
};


//
// PagedArray class implementation.
//


// Constructors.

template <typename Type, size_t PageSize>
inline PagedArray<Type, PageSize>::PagedArray()
  : m_page_index(0)     // start on first page
  , m_elem_index(0)     // and on first element
{
    // Allocate first page.
    m_pages.push_back(new Page());
}

template <typename Type, size_t PageSize>
inline PagedArray<Type, PageSize>::PagedArray(const PagedArray& rhs)
  : m_page_index(rhs.m_page_index)
  , m_elem_index(rhs.m_elem_index)
{
    // Insert copies of the source pages.
    for (size_t i = 0; i <= rhs.m_page_index; ++i)
        m_pages.push_back(new Page(*rhs.m_pages[i]));
}

// Erases the elements of the vector.
template <typename Type, size_t PageSize>
inline void PagedArray<Type, PageSize>::clear()
{
    // Reset the indices, but keep all pages allocated.
    m_page_index = 0;
    m_elem_index = 0;
}

// Tests if the vector is empty.
template <typename Type, size_t PageSize>
inline bool PagedArray<Type, PageSize>::empty() const
{
    return m_page_index == 0 && m_elem_index == 0;
}

// Returns the number of elements in the vector.
template <typename Type, size_t PageSize>
inline typename PagedArray<Type, PageSize>::size_type
PagedArray<Type, PageSize>::size() const
{
    return PageSize * m_page_index + m_elem_index;
}

// Specifies a new size for a vector.
template <typename Type, size_t PageSize>
inline void PagedArray<Type, PageSize>::resize(size_type new_size, Type val)
{
    // Compute new page index and element index.
    const size_t new_page_index = new_size / PageSize;
    const size_t new_elem_index = new_size % PageSize;

    // Allocate missing pages.
    for (size_t i = m_page_index; i < new_page_index; ++i)
        m_pages.push_back(new Page());

    // Initialize new elements.
    for (size_t p = m_page_index; p <= new_page_index; ++p)
    {
        // Determine the range of elements to initialize in this page.
        const size_t begin = (p == m_page_index) ? m_elem_index : 0;
        const size_t end = (p == new_page_index) ? new_elem_index : PageSize;

        // Initialize new elements of this page.
        Page *page = m_pages[p];
        for (size_t i = begin; i < end; ++i)
            page->m_elements[i] = val;
    }

    // Move cursor to the end of the array.
    m_page_index = new_page_index;
    m_elem_index = new_elem_index;
}

// Adds an element to the end of the vector and returns a reference to it.
// All references returned by push_back() stay valid as long as the vector
// itself exists.
template <typename Type, size_t PageSize>
inline typename PagedArray<Type, PageSize>::reference
PagedArray<Type, PageSize>::push_back(const Type& val)
{
    // Get a reference to the new element and initialize the element.
    assert(m_elem_index < PageSize);
    Type& ref = m_pages[m_page_index]->m_elements[m_elem_index];
    ref = val;

    // Increase element index and create a new page if necessary.
    if (++m_elem_index == PageSize)
    {
        if (++m_page_index == m_pages.size())
            m_pages.push_back(new Page());
        m_elem_index = 0;
    }

    return ref;
}

// Unchecked array subscripting to elements.

template <typename Type, size_t PageSize>
inline typename PagedArray<Type, PageSize>::reference
PagedArray<Type, PageSize>::operator[](size_type i)
{
    const size_t pi = i / PageSize;
    const size_t ei = i % PageSize;
    assert(pi <= m_page_index);
    assert(pi <  m_page_index || ei < m_elem_index);
    return m_pages[pi]->m_elements[ei];
}

template <typename Type, size_t PageSize>
inline typename PagedArray<Type, PageSize>::const_reference
PagedArray<Type, PageSize>::operator[](size_type i) const
{
    const size_t pi = i / PageSize;
    const size_t ei = i % PageSize;
    assert(pi <= m_page_index);
    assert(pi <  m_page_index || ei < m_elem_index);
    return m_pages[pi]->m_elements[ei];
}

// Exception-checked access to elements.

template <typename Type, size_t PageSize>
inline typename PagedArray<Type, PageSize>::reference
PagedArray<Type, PageSize>::at(size_type i)
{
    const size_t pi = i / PageSize;
    const size_t ei = i % PageSize;

    if (pi >  m_page_index)
        throw ExceptionOutOfRange();

    if (pi == m_page_index &&
        ei >= m_elem_index)
        throw ExceptionOutOfRange();

    return m_pages[pi]->m_elements[ei];
}

template <typename Type, size_t PageSize>
inline typename PagedArray<Type, PageSize>::const_reference
PagedArray<Type, PageSize>::at(size_type i) const
{
    return (const PagedArray<Type, PageSize>*)this->at(i);
}

// Returns a random-access iterator to the first element in the vector.

template <typename Type, size_t PageSize>
inline typename PagedArray<Type, PageSize>::iterator
PagedArray<Type, PageSize>::begin()
{
    return iterator(*this, m_pages.begin(), 0);
}

template <typename Type, size_t PageSize>
inline typename PagedArray<Type, PageSize>::const_iterator
PagedArray<Type, PageSize>::begin() const
{
    return const_iterator(*this, m_pages.begin(), 0);
}

// Returns a random-access iterator that points just beyond the end of the vector.

template <typename Type, size_t PageSize>
inline typename PagedArray<Type, PageSize>::iterator
PagedArray<Type, PageSize>::end()
{
}

template <typename Type, size_t PageSize>
inline typename PagedArray<Type, PageSize>::const_iterator
PagedArray<Type, PageSize>::end() const
{
}


//
// PagedArray::Page class implementation.
//


// Constructors.

template <typename Type, size_t PageSize>
inline PagedArray<Type, PageSize>::Page::Page()
{
}

template <typename Type, size_t PageSize>
inline PagedArray<Type, PageSize>::Page::Page(const Page& rhs)
{
    std::memcpy(m_elements, rhs.m_elements, PageSize * sizeof(Type));
}

template <typename Type, size_t PageSize>
inline PagedArray<Type, PageSize>::Page::Page(Type val)
{
    for (size_t i = 0; i < PageSize; ++i)
        m_elements[i] = val;
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_CONTAINERS_PAGEDARRAY_H
