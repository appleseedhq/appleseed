
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
#include "foundation/memory/alignedallocator.h"

// Standard headers.
#include <vector>

namespace foundation
{

//
// foundation::AlignedVector is a partial specialization of std::vector for
// the foundation::AlignedAllocator memory allocator.
//
// foundation::AlignedVector also works around a particularity in Visual Studio's
// implementation of the STL where std::vector::resize() takes the value of new
// elements by value (as it should according to C++03) instead of by constant
// reference (as it should according to C++11), preventing using "overly-aligned"
// types with std::vector.
//
// References:
//
//   std::vector of Aligned Elements
//   http://thetweaker.wordpress.com/2010/05/05/stdvector-of-aligned-elements/
//
//   std::vector of Aligned Elements Revisited
//   http://thetweaker.wordpress.com/2010/08/15/stdvector-of-aligned-elements-revisited/
//
//   StackOverflow: Self-contained, STL-compatible implementation of std::vector
//   http://stackoverflow.com/questions/9409591/self-contained-stl-compatible-implementation-of-stdvector
//
//   Eigen implementation of this workaround
//   https://bitbucket.org/eigen/eigen/src/69228ecab94b/Eigen/src/StlSupport/details.h
//   https://bitbucket.org/eigen/eigen/src/69228ecab94b/Eigen/src/StlSupport/StdVector.h
//

template <typename T>
struct VectorElementWrapper
  : public T
{
    VectorElementWrapper() {}
    VectorElementWrapper(const T& rhs) : T(rhs) {}
};

#ifdef _MSC_VER
#define ALIGNED_VECTOR_BASE std::vector<VectorElementWrapper<T>, AlignedAllocator<VectorElementWrapper<T>>>
#else
#define ALIGNED_VECTOR_BASE std::vector<T, AlignedAllocator<T>>
#endif

template <typename T>
class AlignedVector
  : public ALIGNED_VECTOR_BASE
{
  public:
    typedef ALIGNED_VECTOR_BASE Base;

    AlignedVector()
    {
    }

    explicit AlignedVector(const typename Base::allocator_type& allocator)
      : Base(allocator)
    {
    }

    explicit AlignedVector(const typename Base::size_type count)
      : Base(count)
    {
    }

    AlignedVector(
        const typename Base::size_type          count,
        const typename Base::value_type&        value)
      : Base(count, value)
    {
    }

    AlignedVector(
        const typename Base::size_type          count,
        const typename Base::value_type&        value,
        const typename Base::allocator_type&    allocator)
      : Base(count, value, allocator)
    {
    }

    AlignedVector(const Base& rhs)
      : Base(rhs)
    {
    }

    template <typename Iterator>
    AlignedVector(Iterator first, Iterator last)
      : Base(first, last)
    {
    }

    template <typename Iterator>
    AlignedVector(
        Iterator                                first,
        Iterator                                last,
        const typename Base::allocator_type&    allocator)
      : Base(first, last, allocator)
    {
    }

    AlignedVector(Base&& rhs)
      : Base(rhs)
    {
    }
};

#undef ALIGNED_VECTOR_BASE

}   // namespace foundation
