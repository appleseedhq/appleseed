
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

namespace foundation
{

//
// The binary_cast() function allows to interpret a variable of a given type
// as a variable of another type. The size of the types must be equal.
//
// In the following example, the variables f, g and i all have the exact same
// binary representation in memory:
//
//   const float f = 1.0f;
//   const std::uint32_t i = binary_cast<std::uint32_t>(f);     // i == 0x3F800000L
//   const float g = binary_cast<float>(i);                     // f == g
//

template <typename Target, typename Source>
Target binary_cast(Source s);


//
// Implementation.
//

// If this symbol is defined, an unsafe but potentially faster implementation
// of binary_cast<>() is used.  Otherwise, a safe implementation obeying the
// strict aliasing rule is used.  MUST BE UNDEFINED when compiling with gcc.
#undef FOUNDATION_USE_UNSAFE_BINARY_CAST

#ifdef FOUNDATION_USE_UNSAFE_BINARY_CAST

template <typename Target, typename Source>
inline Target binary_cast(Source s)
{
    static_assert(
        sizeof(Target) == sizeof(Source),
        "foundation::binary_cast() expects the source and target types to have the same size");

    return *static_cast<const Target*>(static_cast<const void*>(&s));
}

#else

template <typename Target, typename Source>
inline Target binary_cast(Source s)
{
    static_assert(
        sizeof(Target) == sizeof(Source),
        "foundation::binary_cast() expects the source and target types to have the same size");

    union
    {
        Source  m_source;
        Target  m_target;
    } u;

    u.m_source = s;
    return u.m_target;
}

#endif

}   // namespace foundation
