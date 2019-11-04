
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
#include "foundation/math/scalar.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <limits>

namespace foundation
{

//
// Distribution adapters.
//
// Interesting references:
//
//   Uniform random floats: How to generate a double-precision floating-point number
//   in [0, 1] uniformly at random given a uniform random source of bits.
//   http://mumble.net/~campbell/tmp/random_real.c
//
//   Doubling the speed of std::uniform_int_distribution in the GNU C++ library
//   https://lemire.me/blog/2019/09/28/doubling-the-speed-of-stduniform_int_distribution-in-the-gnu-c-library/
//

// Return a random number in the integer interval [0, 0x7FFFFFFF].
template <typename RNG> std::int32_t rand_int31(RNG& rng);

// Return a random number in the integer interval [min, max].
template <typename RNG> std::int32_t rand_int1(RNG& rng, const std::int32_t min, const std::int32_t max);

// Return a random number in the real interval [0,1].
template <typename RNG> float rand_float1(RNG& rng);
template <typename RNG> double rand_double1(RNG& rng);
template <typename T, typename RNG> T rand1(RNG& rng);

// Return a random number in the real interval [min, max].
template <typename RNG> float rand_float1(RNG& rng, const float min, const float max);
template <typename RNG> double rand_double1(RNG& rng, const double min, const double max);
template <typename RNG> float rand1(RNG& rng, const float min, const float max);
template <typename RNG> double rand1(RNG& rng, const double min, const double max);

// Return a random number in the real interval [0,1).
template <typename RNG> float rand_float2(RNG& rng);
template <typename RNG> double rand_double2(RNG& rng);
template <typename T, typename RNG> T rand2(RNG& rng);

// Return a random number in the real interval [min, max).
template <typename RNG> float rand_float2(RNG& rng, const float min, const float max);
template <typename RNG> double rand_double2(RNG& rng, const double min, const double max);
template <typename RNG> float rand2(RNG& rng, const float min, const float max);
template <typename RNG> double rand2(RNG& rng, const double min, const double max);

// Return a random number in the real interval (0,1).
template <typename RNG> float rand_float3(RNG& rng);
template <typename RNG> double rand_double3(RNG& rng);
template <typename T, typename RNG> T rand3(RNG& rng);

// Return a random number in the real interval (min, max).
template <typename RNG> float rand_float3(RNG& rng, const float min, const float max);
template <typename RNG> double rand_double3(RNG& rng, const double min, const double max);
template <typename RNG> float rand3(RNG& rng, const float min, const float max);
template <typename RNG> double rand3(RNG& rng, const double min, const double max);

// Return a random number in the real interval [0,1) with 53-bit resolution.
template <typename RNG> double rand_double2_res53(RNG& rng);


//
// Random vectors.
//

// Return a random vector whose components are in the real interval [0,1].
template <typename VectorType, typename RNG>
VectorType rand_vector1(RNG& rng);

// Return a random vector whose components are in the real interval [0,1).
template <typename VectorType, typename RNG>
VectorType rand_vector2(RNG& rng);

// Return a random vector whose components are in the real interval (0,1).
template <typename VectorType, typename RNG>
VectorType rand_vector3(RNG& rng);


//
// Implementation.
//

template <typename RNG>
inline std::int32_t rand_int31(RNG& rng)
{
    const std::int32_t result = static_cast<std::int32_t>(rng.rand_uint32() >> 1);

    assert(result >= 0);
    assert(result <= 0x7FFFFFFFu);

    return result;
}

template <typename RNG>
inline std::int32_t rand_int1(RNG& rng, const std::int32_t min, const std::int32_t max)
{
    assert(min <= max);

    const double x =
        rand_double2(
            rng,
            static_cast<double>(min),
            static_cast<double>(max) + 1);

    const std::int32_t result = truncate<std::int32_t>(x);

    assert(result >= min);
    assert(result <= max);

    return result;
}

template <typename RNG>
inline float rand_float1(RNG& rng)
{
    const float result = rng.rand_uint32() * (1.0f / 0xFFFFFFFFu);

    assert(result >= 0.0f);
    assert(result <= 1.0f);

    return result;
}

template <typename RNG>
inline double rand_double1(RNG& rng)
{
    const double result = rng.rand_uint32() * (1.0 / 0xFFFFFFFFu);

    assert(result >= 0.0);
    assert(result <= 1.0);

    return result;
}

template <typename T, typename RNG>
struct Rand1Helper;

template <typename RNG>
struct Rand1Helper<float, RNG>
{
    float operator()(RNG& rng) { return rand_float1(rng); }
};

template <typename RNG>
struct Rand1Helper<double, RNG>
{
    double operator()(RNG& rng) { return rand_double1(rng); }
};

template <typename T, typename RNG>
inline T rand1(RNG& rng)
{
    Rand1Helper<T, RNG> helper;
    return helper(rng);
}

template <typename RNG>
inline float rand_float1(RNG& rng, const float min, const float max)
{
    assert(min <= max);

    const float result = lerp(min, max, rand_float1(rng));

    assert(result >= min);
    assert(result <= max);

    return result;
}

template <typename RNG>
inline double rand_double1(RNG& rng, const double min, const double max)
{
    assert(min <= max);

    const double result = lerp(min, max, rand_double1(rng));

    assert(result >= min);
    assert(result <= max);

    return result;
}

template <typename RNG>
inline float rand1(RNG& rng, const float min, const float max)
{
    return rand_float1(rng, min, max);
}

template <typename RNG>
inline double rand1(RNG& rng, const double min, const double max)
{
    return rand_double1(rng, min, max);
}

template <typename RNG>
inline float rand_float2(RNG& rng)
{
    const float result = rng.rand_uint32() * Rcp2Pow32<float>();

    assert(result >= 0.0f);
    assert(result < 1.0f);

    return result;
}

template <typename RNG>
inline double rand_double2(RNG& rng)
{
    const double result = rng.rand_uint32() * Rcp2Pow32<double>();

    assert(result >= 0.0);
    assert(result < 1.0);

    return result;
}

template <typename T, typename RNG>
struct Rand2Helper;

template <typename RNG>
struct Rand2Helper<float, RNG>
{
    float operator()(RNG& rng) { return rand_float2(rng); }
};

template <typename RNG>
struct Rand2Helper<double, RNG>
{
    double operator()(RNG& rng) { return rand_double2(rng); }
};

template <typename T, typename RNG>
inline T rand2(RNG& rng)
{
    Rand2Helper<T, RNG> helper;
    return helper(rng);
}

template <typename RNG>
inline float rand_float2(RNG& rng, const float min, const float max)
{
    assert(min <= max);

    const float result = lerp(min, max, rand_float2(rng));

    assert(result >= min);
    assert(result < max);

    return result;
}

template <typename RNG>
inline double rand_double2(RNG& rng, const double min, const double max)
{
    assert(min <= max);

    const double result = lerp(min, max, rand_double2(rng));

    assert(result >= min);
    assert(result < max);

    return result;
}

template <typename RNG>
inline float rand2(RNG& rng, const float min, const float max)
{
    return rand_float2(rng, min, max);
}

template <typename RNG>
inline double rand2(RNG& rng, const double min, const double max)
{
    return rand_double2(rng, min, max);
}

template <typename RNG>
inline float rand_float3(RNG& rng)
{
    const float result = rng.rand_uint32() * 2.3283060e-010f + std::numeric_limits<float>::epsilon();

    assert(result > 0.0f);
    assert(result < 1.0f);

    return result;
}

template <typename RNG>
inline double rand_double3(RNG& rng)
{
    const double result = rng.rand_uint32() * 2.3283064370807963e-10 + std::numeric_limits<double>::epsilon();

    assert(result > 0.0);
    assert(result < 1.0);

    return result;
}

template <typename T, typename RNG>
struct Rand3Helper;

template <typename RNG>
struct Rand3Helper<float, RNG>
{
    float operator()(RNG& rng) { return rand_float3(rng); }
};

template <typename RNG>
struct Rand3Helper<double, RNG>
{
    double operator()(RNG& rng) { return rand_double3(rng); }
};

template <typename T, typename RNG>
inline T rand3(RNG& rng)
{
    Rand3Helper<T, RNG> helper;
    return helper(rng);
}

template <typename RNG>
inline float rand_float3(RNG& rng, const float min, const float max)
{
    assert(min <= max);

    const float result = lerp(min, max, rand_float3(rng));

    assert(result > min);
    assert(result < max);

    return result;
}

template <typename RNG>
inline double rand_double3(RNG& rng, const double min, const double max)
{
    assert(min <= max);

    const double result = lerp(min, max, rand_double3(rng));

    assert(result > min);
    assert(result < max);

    return result;
}

template <typename RNG>
inline float rand3(RNG& rng, const float min, const float max)
{
    return rand_float3(rng, min, max);
}

template <typename RNG>
inline double rand3(RNG& rng, const double min, const double max)
{
    return rand_double3(rng, min, max);
}

template <typename RNG>
inline double rand_double2_res53(RNG& rng)
{
    const std::uint32_t a = rng.rand_uint32() >> 5;
    const std::uint32_t b = rng.rand_uint32() >> 6;

    const double result = (a * 67108864.0 + b) * (1.0 / 9007199254740992.0);

    assert(result >= 0.0);
    assert(result < 1.0);

    return result;
}

template <typename VectorType, typename RNG>
inline VectorType rand_vector1(RNG& rng)
{
    VectorType v;

    for (size_t i = 0; i < VectorType::Dimension; ++i)
        v[i] = rand1<typename VectorType::ValueType>(rng);

    return v;
}

template <typename VectorType, typename RNG>
inline VectorType rand_vector2(RNG& rng)
{
    VectorType v;

    for (size_t i = 0; i < VectorType::Dimension; ++i)
        v[i] = rand2<typename VectorType::ValueType>(rng);

    return v;
}

template <typename VectorType, typename RNG>
inline VectorType rand_vector3(RNG& rng)
{
    VectorType v;

    for (size_t i = 0; i < VectorType::Dimension; ++i)
        v[i] = rand3<typename VectorType::ValueType>(rng);

    return v;
}

}   // namespace foundation
