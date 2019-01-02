
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
#include <algorithm>
#include <cstddef>

namespace foundation
{

//
// Import std::min() and std::max() into the foundation namespace, otherwise they'll be shadowed.
//

using std::min;
using std::max;


//
// Return the lesser of a set of objects.
//

template <typename T>
T min(const T a, const T b, const T c);

template <typename T>
T min(const T a, const T b, const T c, const T d);


//
// Return the greater of a set of objects.
//

template <typename T>
T max(const T a, const T b, const T c);

template <typename T>
T max(const T a, const T b, const T c, const T d);


//
// Return both the lesser and greater of a set of objects.
//

template <typename T>
void minmax(const T a, const T b, T& min, T& max);

template <typename T>
void minmax(const T a, const T b, const T c, T& min, T& max);


//
// Return the lesser (resp. greater) of two objects with the exact
// same semantics as the SSE instructions 'minps' (resp. 'maxps').
//
// An important property guaranteed by these functions is that if
// a, b or both are NaN, b will always be returned. Thus, if b is
// never NaN, the min/max of a and b is guaranteed not to be NaN.
//
// References:
//
//   http://www.sesp.cse.clrc.ac.uk/html/SoftwareTools/vtune/users_guide/mergedProjects/analyzer_ec/mergedProjects/reference_olh/mergedProjects/instructions/instruct32_hh/vc174.htm
//   http://www.sesp.cse.clrc.ac.uk/html/SoftwareTools/vtune/users_guide/mergedProjects/analyzer_ec/mergedProjects/reference_olh/mergedProjects/instructions/instruct32_hh/vc169.htm
//

template <typename T>
T ssemin(const T a, const T b);

template <typename T>
T ssemax(const T a, const T b);


//
// Return the index of the lesser or greater of a set of objects.
//

template <typename T>
size_t min_index(const T& a, const T& b);

template <typename T>
size_t min_index(const T& a, const T& b, const T& c);

template <typename T>
size_t max_index(const T& a, const T& b);

template <typename T>
size_t max_index(const T& a, const T& b, const T& c);


//
// Implementation.
//

template <typename T>
inline T min(const T a, const T b, const T c)
{
    return std::min(std::min(a, b), c);
}

template <typename T>
inline T min(const T a, const T b, const T c, const T d)
{
    return std::min(std::min(std::min(a, b), c), d);
}

template <typename T>
inline T max(const T a, const T b, const T c)
{
    return std::max(std::max(a, b), c);
}

template <typename T>
inline T max(const T a, const T b, const T c, const T d)
{
    return std::max(std::max(std::max(a, b), c), d);
}

template <typename T>
inline void minmax(const T a, const T b, T& min, T& max)
{
    if (a < b)
    {
        min = a;
        max = b;
    }
    else
    {
        min = b;
        max = a;
    }
}

template <typename T>
inline void minmax(const T a, const T b, const T c, T& min, T& max)
{
    minmax(a, b, min, max);

    if (c < min) min = c;
    if (c > max) max = c;
}

template <typename T>
inline T ssemin(const T a, const T b)
{
    return a < b ? a : b;
}

template <typename T>
inline T ssemax(const T a, const T b)
{
    return a > b ? a : b;
}

template <typename T>
inline size_t min_index(const T& a, const T& b)
{
    return a < b ? 0 : 1;
}

template <typename T>
inline size_t min_index(const T& a, const T& b, const T& c)
{
    if (a < b)
        return a < c ? 0 : 2;
    else return b < c ? 1 : 2;
}

template <typename T>
inline size_t max_index(const T& a, const T& b)
{
    return a > b ? 0 : 1;
}

template <typename T>
inline size_t max_index(const T& a, const T& b, const T& c)
{
    if (a > b)
        return a > c ? 0 : 2;
    else return b > c ? 1 : 2;
}

}   // namespace foundation
