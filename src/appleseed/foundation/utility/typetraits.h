
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

// Standard headers.
#include <cstddef>
#include <cstdint>

namespace foundation
{

//
// Conversion of a numeric type to another numeric type of the same size.
//

namespace impl
{
    template <size_t Size>
    struct TypeConv;

    template <>
    struct TypeConv<4>
    {
        typedef std::int32_t Int;
        typedef std::uint32_t UInt;
        typedef float Scalar;
    };

    template <>
    struct TypeConv<8>
    {
        typedef std::int64_t Int;
        typedef std::uint64_t UInt;
        typedef double Scalar;
    };
}

template <typename T>
struct TypeConv : public impl::TypeConv<sizeof(T)> {};


//
// Check whether a type is a pointer type.
//
// Reference:
//
//   http://semantics.org/once_weakly/w02_SFINAE.pdf
//

template <typename T> struct IsPointer                          { enum { R = false }; };
template <typename T> struct IsPointer<T*>                      { enum { R = true }; };
template <typename T> struct IsPointer<T* const>                { enum { R = true }; };
template <typename T> struct IsPointer<T* volatile>             { enum { R = true }; };
template <typename T> struct IsPointer<T* const volatile>       { enum { R = true }; };


//
// Dereference a type at compile-time.
//
// Reference:
//
//   http://semantics.org/once_weakly/w02_SFINAE.pdf
//

template <typename T> struct PointerDeref                       { typedef T R; };
template <typename T> struct PointerDeref<T*>                   { typedef T R; };
template <typename T> struct PointerDeref<T* const >            { typedef T R; };
template <typename T> struct PointerDeref<T* const volatile>    { typedef T R; };


//
// Check whether a derived class inherits from a base class.
//
// References:
//
//   http://www.boost.org/doc/libs/1_35_0/libs/type_traits/doc/html/boost_typetraits/reference/is_base_of.html
//   http://cboard.cprogramming.com/cplusplus-programming/116506-check-compile-time-whether-one-template-parameter-derived-another.html#post867680
//

namespace impl
{
    struct false_type { typedef false_type type; const static bool value = false; };
    struct true_type { typedef true_type type; const static bool value = true; };

    template <size_t N> struct test_result { typedef false_type type; };
    template <> struct test_result<1> { typedef true_type type; };

    struct Yes { char dummy[1]; };
    struct No  { char dummy[2]; };

    template <typename Base, typename Derived>
    class IsBase
    {
      private:
        static Yes is_base(Base*);
        static No is_base(...);

      public:
        typedef typename test_result<sizeof(is_base((Derived*)nullptr))>::type type;
    };

    template <typename Base, typename Derived>
    class IsNotBase
    {
      private:
        static No is_not_base(Base*);
        static Yes is_not_base(...);

      public:
        typedef typename test_result<sizeof(is_not_base((Derived*)nullptr))>::type type;
    };
}

template <typename Base, typename Derived>
struct IsBase
  : public impl::IsBase<Base, Derived>::type
{
};

template <typename Base, typename Derived>
struct IsNotBase
  : public impl::IsNotBase<Base, Derived>::type
{
};

}   // namespace foundation
