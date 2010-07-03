
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

#ifndef APPLESEED_FOUNDATION_MATH_FASTMATH_H
#define APPLESEED_FOUNDATION_MATH_FASTMATH_H

// appleseed.foundation headers.
#include "foundation/platform/types.h"
#include "foundation/utility/casts.h"

// Standard headers.
#include <cassert>
#include <cmath>

namespace foundation
{

//
// Fast power and logarithm approximations.
//

// Fast approximation of the base 2 logarithm.
float fast_log2(const float x);

// Fast approximation of the base 2 logarithm, more accurate than fast_log2().
float fast_log2_refined(const float x);

// Fast approximation of 2^x.
float fast_pow2(const float x);

// Fast approximation of a^b.
float fast_pow(const float a, const float b);

// Fast approximation of a^b, more accurate than fast_pow().
float fast_pow_refined(const float a, const float b);

// Fast approximation of the square root.
float fast_sqrt(const float x);

// Fast approximation of the reciprocal square root.
float fast_rcp_sqrt(const float x);
double fast_rcp_sqrt(const double x);


//
// Fast power and logarithm approximations implementation.
//
// References:
//
//   Fast log2 and pow2:
//     Production Rendering, Springer-Verlag, 2004
//
//   Fast square root:
//     http://en.wikipedia.org/wiki/Methods_of_computing_square_roots
//
//   Fast reciprocal square root:
//     http://www.lomont.org/Math/Papers/2003/InvSqrt.pdf
//

// Fast approximation of the base 2 logarithm.
inline float fast_log2(const float x)
{
    assert(x > 0.0f);
    float y = binary_cast<int32>(x);
    y *= 0.1192092896e-6f;                  // y *= pow(2.0f, -23)
    y -= 127.0f;
    return y;
}

// Fast approximation of the base 2 logarithm, more accurate than fast_log2().
inline float fast_log2_refined(const float x)
{
    assert(x > 0.0f);
    float y = binary_cast<int32>(x);
    y *= 0.1192092896e-6f;                  // y *= pow(2.0f, -23)
    y -= 127.0f;
    float z = y - std::floor(y);
    z = (z - z * z) * 0.346607f;
    return y + z;
}

// Fast approximation of 2^x.
inline float fast_pow2(const float x)
{
    float y = x - std::floor(x);
    y = (y - y * y) * 0.33971f;
    float z = x + 127.0f - y;
    z *= 8388608.0f;                        // z *= pow(2.0f, 23)
    return z;
}

// Fast approximation of a^b.
inline float fast_pow(const float a, const float b)
{
    assert(a > 0.0f);
    return fast_pow2(b * fast_log2(a));
}

// Fast approximation of a^b, more accurate than fast_pow().
inline float fast_pow_refined(const float a, const float b)
{
    assert(a > 0.0f);
    return fast_pow2(b * fast_log2_refined(a));
}

// Fast approximation of the square root.
inline float fast_sqrt(const float x)
{
    assert(x >= 0.0f);
    int32 i = binary_cast<int32>(x);
    i -= 1 << 23;                           // remove last bit to not let it go to mantissa
    i = i >> 1;                             // divide by 2
    i += 1 << 29;                           // add 64 to exponent
    return binary_cast<float>(i);
}

// Fast approximation of the reciprocal square root, single precision.
inline float fast_rcp_sqrt(const float x)
{
    assert(x >= 0.0f);
    const float xhalf = 0.5f * x;
    int32 i = binary_cast<int32>(x);
    i = 0x5F375A86L - (i >> 1);             // gives initial guess
    float z = binary_cast<float>(i);
    z = z * (1.5f - xhalf * z * z);         // Newton step, repeating increases accuracy
    return z;
}

// Fast approximation of the reciprocal square root, double precision.
inline double fast_rcp_sqrt(const double x)
{
    assert(x >= 0.0);
    const double xhalf = 0.5 * x;
    int64 i = binary_cast<int64>(x);
    i = 0x5FE6EC85E7DE30DALL - (i >> 1);    // gives initial guess
    double z = binary_cast<double>(i);
    z = z * (1.5 - xhalf * z * z);          // Newton step, repeating increases accuracy
    return z;
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_FASTMATH_H
