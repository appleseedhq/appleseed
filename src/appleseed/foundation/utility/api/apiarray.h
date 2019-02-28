
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

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstring>
#include <iterator>
#include <vector>

namespace foundation
{

//
// A minimalist array class that can safely cross DLL boundaries.
//

#define APPLESEED_DECLARE_APIARRAY(ArrayName, ArrayType)                \
    class APPLESEED_DLLSYMBOL ArrayName                                 \
    {                                                                   \
      public:                                                           \
        /* Types. */                                                    \
        typedef ArrayType value_type;                                   \
        typedef value_type& reference;                                  \
        typedef const value_type& const_reference;                      \
        typedef size_t size_type;                                       \
                                                                        \
        /* Constructors. */                                             \
        ArrayName();                                                    \
        ArrayName(const ArrayName& rhs);                                \
        ArrayName(                                                      \
            const size_type     size,                                   \
            const value_type*   values);                                \
                                                                        \
        /* Destructor. */                                               \
        ~ArrayName();                                                   \
                                                                        \
        /* Assignment operator. */                                      \
        ArrayName& operator=(const ArrayName& rhs);                     \
                                                                        \
        /* Comparison operators. */                                     \
        bool operator==(const ArrayName& rhs) const;                    \
        bool operator!=(const ArrayName& rhs) const;                    \
                                                                        \
        /* Returns the size of the vector. */                           \
        size_type size() const;                                         \
                                                                        \
        /* Tests if the vector is empty. */                             \
        bool empty() const;                                             \
                                                                        \
        /* Clears the vector. */                                        \
        void clear();                                                   \
                                                                        \
        /* Reserves memory for a given number of elements. */           \
        void reserve(const size_type count);                            \
                                                                        \
        /* Specifies a new size for a vector. */                        \
        void resize(const size_type new_size);                          \
                                                                        \
        /* Adds an element to the end of the vector. */                 \
        void push_back(const value_type& val);                          \
                                                                        \
        /* Append a vector to the end of the vector. */                 \
        void append(const ArrayName& rhs);                              \
                                                                        \
        /* Returns the vector element at a specified position. */       \
        reference operator[](const size_type pos);                      \
        const_reference operator[](const size_type pos) const;          \
                                                                        \
      private:                                                          \
        /* Private implementation. */                                   \
        struct Impl;                                                    \
        Impl* impl;                                                     \
    }

#define APPLESEED_DEFINE_APIARRAY(ArrayName)                            \
    struct ArrayName::Impl                                              \
      : public std::vector<value_type>                                  \
    {                                                                   \
    };                                                                  \
                                                                        \
    ArrayName::ArrayName()                                              \
      : impl(new Impl())                                                \
    {                                                                   \
    }                                                                   \
                                                                        \
    ArrayName::ArrayName(const ArrayName& rhs)                          \
      : impl(new Impl(*rhs.impl))                                       \
    {                                                                   \
    }                                                                   \
                                                                        \
    ArrayName::ArrayName(                                               \
        const size_type     size,                                       \
        const value_type*   values)                                     \
      : impl(new Impl())                                                \
    {                                                                   \
        assert(size > 0);                                               \
        assert(values);                                                 \
        impl->reserve(size);                                            \
        std::copy(values, values + size, std::back_inserter(*impl));    \
    }                                                                   \
                                                                        \
    ArrayName::~ArrayName()                                             \
    {                                                                   \
        delete impl;                                                    \
    }                                                                   \
                                                                        \
    ArrayName& ArrayName::operator=(const ArrayName& rhs)               \
    {                                                                   \
        *impl = *rhs.impl;                                              \
        return *this;                                                   \
    }                                                                   \
                                                                        \
    bool ArrayName::operator==(const ArrayName& rhs) const              \
    {                                                                   \
        return *impl == *rhs.impl;                                      \
    }                                                                   \
                                                                        \
    bool ArrayName::operator!=(const ArrayName& rhs) const              \
    {                                                                   \
        return *impl != *rhs.impl;                                      \
    }                                                                   \
                                                                        \
    ArrayName::size_type ArrayName::size() const                        \
    {                                                                   \
        return impl->size();                                            \
    }                                                                   \
                                                                        \
    bool ArrayName::empty() const                                       \
    {                                                                   \
        return impl->empty();                                           \
    }                                                                   \
                                                                        \
    void ArrayName::clear()                                             \
    {                                                                   \
        impl->clear();                                                  \
    }                                                                   \
                                                                        \
    void ArrayName::reserve(const size_type count)                      \
    {                                                                   \
        impl->reserve(count);                                           \
    }                                                                   \
                                                                        \
    void ArrayName::resize(const size_type new_size)                    \
    {                                                                   \
        impl->resize(new_size);                                         \
    }                                                                   \
                                                                        \
    void ArrayName::push_back(const value_type& val)                    \
    {                                                                   \
        impl->push_back(val);                                           \
    }                                                                   \
                                                                        \
    void ArrayName::append(const ArrayName& rhs)                        \
    {                                                                   \
        impl->insert(impl->end(), rhs.impl->begin(), rhs.impl->end());  \
    }                                                                   \
                                                                        \
    ArrayName::reference                                                \
    ArrayName::operator[](const size_type pos)                          \
    {                                                                   \
        return (*impl)[pos];                                            \
    }                                                                   \
                                                                        \
    ArrayName::const_reference                                          \
    ArrayName::operator[](const size_type pos) const                    \
    {                                                                   \
        return (*impl)[pos];                                            \
    }


//
// Utility function to convert between an array and a std::vector (works both ways).
//

template <typename U, typename V>
U array_vector(const V& v)
{
    const size_t n = v.size();

    U u;
    u.reserve(n);

    for (size_t i = 0; i < n; ++i)
        u.push_back(v[i]);

    return u;
}

}   // namespace foundation
