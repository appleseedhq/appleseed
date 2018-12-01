
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

namespace foundation
{

//
// A countof() macro returning the number of elements in a C array.
//
// Reference:
//
//   http://blogs.msdn.com/the1/archive/2004/05/07/128242.aspx
//

// A function taking a reference to an array of N elements of type T in argument,
// and returning a reference to an array of N elements of type T. The function is
// only declared, and never defined.
template <typename T, size_t N>
char (&count_of_helper(T (&array)[N]))[N];

// The number of elements in an array is equal to the size of the array returned
// by the helper function declared above, when applied to the input array.
#define COUNT_OF(array) (sizeof(foundation::count_of_helper(array)))

// Introduce countof() as a synonym to COUNT_OF(), for convenience and symmetry
// with the sizeof() operator.
#define countof COUNT_OF

}   // namespace foundation
