
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

#ifndef APPLESEED_FOUNDATION_MATH_RNG_DISTRIBUTION_H
#define APPLESEED_FOUNDATION_MATH_RNG_DISTRIBUTION_H

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cassert>
#include <limits>

namespace foundation
{

//
// Distribution adapters.
//

// Returns a random number in the integer interval [0, 0x7fffffff].
template <typename RNG> int32 rand_int31(RNG& rng);

// Returns a random number in the integer interval [min, max].
template <typename RNG> int32 rand_int1(RNG& rng, const int32 min, const int32 max);

// Returns a random number in the real interval [0,1].
template <typename RNG> float rand_float1(RNG& rng);
template <typename RNG> double rand_double1(RNG& rng);

// Returns a random number in the real interval [min, max].
template <typename RNG> float rand_float1(RNG& rng, const float min, const float max);
template <typename RNG> double rand_double1(RNG& rng, const double min, const double max);

// Returns a random number in the real interval [0,1).
template <typename RNG> float rand_float2(RNG& rng);
template <typename RNG> double rand_double2(RNG& rng);

// Returns a random number in the real interval [min, max).
template <typename RNG> float rand_float2(RNG& rng, const float min, const float max);
template <typename RNG> double rand_double2(RNG& rng, const double min, const double max);

// Returns a random number in the real interval (0,1).
template <typename RNG> float rand_float3(RNG& rng);
template <typename RNG> double rand_double3(RNG& rng);

// Returns a random number in the real interval (min, max).
template <typename RNG> float rand_float3(RNG& rng, const float min, const float max);
template <typename RNG> double rand_double3(RNG& rng, const double min, const double max);

// Returns a random number in the real interval [0,1) with 53-bit resolution.
template <typename RNG> double rand_double2_res53(RNG& rng);


//
// Implementation.
//

template <typename RNG>
inline int32 rand_int31(RNG& rng)
{
    return static_cast<int32>(rng.rand_uint32() >> 1);
}

template <typename RNG>
inline int32 rand_int1(RNG& rng, const int32 min, const int32 max)
{
    assert(min <= max);
    const double x = rand_double2(
        rng,
        static_cast<double>(min),
        static_cast<double>(max) + 1);
    return truncate<int32>(x);
}

template <typename RNG>
inline float rand_float1(RNG& rng)
{
    return rng.rand_uint32() * (1.0f / 4294967295UL);
}

template <typename RNG>
inline double rand_double1(RNG& rng)
{
    return rng.rand_uint32() * (1.0 / 4294967295UL);
}

template <typename RNG>
inline float rand_float1(RNG& rng, const float min, const float max)
{
    assert(min <= max);
    const float x = rand_float1(rng);
    return (1.0f - x) * min + x * max;
}

template <typename RNG>
inline double rand_double1(RNG& rng, const double min, const double max)
{
    assert(min <= max);
    const double x = rand_double1(rng);
    return (1.0 - x) * min + x * max;
}

template <typename RNG>
inline float rand_float2(RNG& rng)
{
    return rng.rand_uint32() * 2.3283063e-010f;
}

template <typename RNG>
inline double rand_double2(RNG& rng)
{
    return rng.rand_uint32() * (1.0 / 4294967296.0);
}

template <typename RNG>
inline float rand_float2(RNG& rng, const float min, const float max)
{
    assert(min <= max);
    const float x = rand_float2(rng);
    return (1.0f - x) * min + x * max;
}

template <typename RNG>
inline double rand_double2(RNG& rng, const double min, const double max)
{
    assert(min <= max);
    const double x = rand_double2(rng);
    return (1.0 - x) * min + x * max;
}

template <typename RNG>
inline float rand_float3(RNG& rng)
{
    return rng.rand_uint32() * 2.3283060e-010f + std::numeric_limits<float>::epsilon();
}

template <typename RNG>
inline double rand_double3(RNG& rng)
{
    return rng.rand_uint32() * 2.3283064370807963e-10 + std::numeric_limits<double>::epsilon();
}

template <typename RNG>
inline float rand_float3(RNG& rng, const float min, const float max)
{
    assert(min <= max);
    const float x = rand_float3(rng);
    return (1.0f - x) * min + x * max;
}

template <typename RNG>
inline double rand_double3(RNG& rng, const double min, const double max)
{
    assert(min <= max);
    const double x = rand_double3(rng);
    return (1.0 - x) * min + x * max;
}

template <typename RNG>
inline double rand_double2_res53(RNG& rng)
{
    const uint32 a = rng.rand_uint32() >> 5;
    const uint32 b = rng.rand_uint32() >> 6;
    return (a * 67108864.0 + b) * (1.0 / 9007199254740992.0);
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_RNG_DISTRIBUTION_H
