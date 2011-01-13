
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

#ifndef APPLESEED_FOUNDATION_UTILITY_CONTAINERS_ARRAY_H
#define APPLESEED_FOUNDATION_UTILITY_CONTAINERS_ARRAY_H

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstring>
#include <vector>

//
// On Windows, define FOUNDATIONDLL to __declspec(dllexport) when building the DLL
// and to __declspec(dllimport) when building an application using the DLL.
// Other platforms don't use this export mechanism and the symbol FOUNDATIONDLL is
// defined to evaluate to nothing.
//

#ifndef FOUNDATIONDLL
#ifdef _WIN32
#ifdef APPLESEED_FOUNDATION_EXPORTS
#define FOUNDATIONDLL __declspec(dllexport)
#else
#define FOUNDATIONDLL __declspec(dllimport)
#endif
#else
#define FOUNDATIONDLL
#endif
#endif

namespace foundation
{

//
// An array of elements of a given type with minimalistic functionalities.
//
// The array can be passed safely across DLL boundaries.
//

// Declare the array.
#define DECLARE_ARRAY(ArrayName, ArrayType)                             \
    class FOUNDATIONDLL ArrayName                                       \
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
        /* Returns the vector element at a specified position. */       \
        reference operator[](const size_type pos);                      \
        const_reference operator[](const size_type pos) const;          \
                                                                        \
      private:                                                          \
        /* Private implementation. */                                   \
        struct Impl;                                                    \
        Impl* impl;                                                     \
    }

// Define the array.
#define DEFINE_ARRAY(ArrayName)                                         \
    struct ArrayName::Impl                                              \
      : public std::vector<value_type>                                  \
    {                                                                   \
    };                                                                  \
                                                                        \
    /* Constructors. */                                                 \
    ArrayName::ArrayName()                                              \
      : impl(new Impl())                                                \
    {                                                                   \
    }                                                                   \
    ArrayName::ArrayName(const ArrayName& rhs)                          \
      : impl(new Impl(*rhs.impl))                                       \
    {                                                                   \
    }                                                                   \
    ArrayName::ArrayName(                                               \
        const size_type     size,                                       \
        const value_type*   values)                                     \
      : impl(new Impl())                                                \
    {                                                                   \
        assert(size > 0);                                               \
        assert(values);                                                 \
        impl->resize(size);                                             \
        std::memcpy(                                                    \
            &impl->front(),                                             \
            values,                                                     \
            size * sizeof(value_type));                                 \
    }                                                                   \
                                                                        \
    /* Destructor. */                                                   \
    ArrayName::~ArrayName()                                             \
    {                                                                   \
        delete impl;                                                    \
    }                                                                   \
                                                                        \
    /* Assignment operator. */                                          \
    ArrayName& ArrayName::operator=(const ArrayName& rhs)               \
    {                                                                   \
        *impl = *rhs.impl;                                              \
        return *this;                                                   \
    }                                                                   \
                                                                        \
    /* Returns the size of the vector. */                               \
    ArrayName::size_type ArrayName::size() const                        \
    {                                                                   \
        return impl->size();                                            \
    }                                                                   \
                                                                        \
    /* Tests if the vector is empty. */                                 \
    bool ArrayName::empty() const                                       \
    {                                                                   \
        return impl->empty();                                           \
    }                                                                   \
                                                                        \
    /* Clears the vector. */                                            \
    void ArrayName::clear()                                             \
    {                                                                   \
        impl->clear();                                                  \
    }                                                                   \
                                                                        \
    /* Reserves memory for a given number of elements. */               \
    void ArrayName::reserve(const size_type count)                      \
    {                                                                   \
        impl->reserve(count);                                           \
    }                                                                   \
                                                                        \
    /* Specifies a new size for a vector. */                            \
    void ArrayName::resize(const size_type new_size)                    \
    {                                                                   \
        impl->resize(new_size);                                         \
    }                                                                   \
                                                                        \
    /* Adds an element to the end of the vector. */                     \
    void ArrayName::push_back(const value_type& val)                    \
    {                                                                   \
        impl->push_back(val);                                           \
    }                                                                   \
                                                                        \
    /* Returns the vector element at a specified position. */           \
    ArrayName::reference                                                \
    ArrayName::operator[](const size_type pos)                          \
    {                                                                   \
        return (*impl)[pos];                                            \
    }                                                                   \
    ArrayName::const_reference                                          \
    ArrayName::operator[](const size_type pos) const                    \
    {                                                                   \
        return (*impl)[pos];                                            \
    }


//
// Utility function to convert between an array and a std::vector.
// Works both ways.
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

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_CONTAINERS_ARRAY_H
