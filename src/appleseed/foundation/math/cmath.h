
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_MATH_CMATH_H
#define APPLESEED_FOUNDATION_MATH_CMATH_H

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"

// Standard headers.
#include <cmath>

namespace foundation
{

template <typename T>
struct cmath
{
    static T acos(const T x) { return std::acos(x); }
    static T asin(const T x) { return std::asin(x); }
    static T atan(const T x) { return std::atan(x); }
    static T atan2(const T x, const T y) { return std::atan2(x, y); }

    static T cos(const T x) { return std::cos(x); }
    static T sin(const T x) { return std::sin(x); }
    static T tan(const T x) { return std::tan(x); }

    static T pow(const T x, const T y) { return std::pow(x, y); }
    static T sqrt(const T x) { return std::sqrt(x); }
    static T exp(const T x) { return std::exp(x); }
    static T log(const T x) { return std::log(x); }
    static T log10(const T x) { return std::log10(x); }

    static T ceil(const T x) { return std::ceil(x); }
    static T floor(const T x) { return std::floor(x); }

    static T fabs(const T x) { return std::fabs(x); }
    static T fmod(const T x, const T y) { return std::fmod(x, y); }
};

#if defined __CUDACC__
    template <>
    struct cmath<float>
    {
        APPLESEED_DEVICE_INLINE
        static float acos(const float x) { return acosf(x); }

        APPLESEED_DEVICE_INLINE
        static float asin(const float x) { return asinf(x); }

        APPLESEED_DEVICE_INLINE
        static float atan(const float x) { return atanf(x); }

        APPLESEED_DEVICE_INLINE
        static float atan2(const float x, const float y) { return atan2f(x, y); }

        APPLESEED_DEVICE_INLINE
        static float cos(const float x) { return cosf(x); }

        APPLESEED_DEVICE_INLINE
        static float sin(const float x) { return sinf(x); }

        APPLESEED_DEVICE_INLINE
        static float tan(const float x) { return tanf(x); }

        APPLESEED_DEVICE_INLINE
        static float pow(const float x, const float y) { return powf(x, y); }

        APPLESEED_DEVICE_INLINE
        static float sqrt(const float x) { return sqrtf(x); }

        APPLESEED_DEVICE_INLINE
        static float exp(const float x) { return expf(x); }

        APPLESEED_DEVICE_INLINE
        static float log(const float x) { return logf(x); }

        APPLESEED_DEVICE_INLINE
        static float log10(const float x) { return log10f(x); }

        APPLESEED_DEVICE_INLINE
        static float ceil(const float x) { return ceilf(x); }

        APPLESEED_DEVICE_INLINE
        static float floor(const float x) { return floorf(x); }

        APPLESEED_DEVICE_INLINE
        static float fabs(const float x) { return fabsf(x); }

        APPLESEED_DEVICE_INLINE
        static float fmod(const float x, const float y) { return fmodf(x, y); }
    };
#endif

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_CMATH_H
