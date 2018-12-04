
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
#include "foundation/math/fp.h"
#include "foundation/math/scalar.h"
#include "foundation/platform/compiler.h"
#ifdef APPLESEED_USE_SSE
#include "foundation/platform/sse.h"
#endif

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>

namespace foundation
{

//
// A spectrum represented by a fixed set of equally spaced samples.
//

template <typename T, size_t N>
class RegularSpectrum
{
  public:
    // Value type and number of samples.
    typedef T ValueType;
    static const size_t Samples = N;

    // Number of stored samples such that the size of the sample array is a multiple of 16 bytes.
    static const size_t StoredSamples = (((N * sizeof(T)) + 15) & ~15) / sizeof(T);

    // Constructors.
#ifdef APPLESEED_USE_SSE
    RegularSpectrum();
#else
#if APPLESEED_COMPILER_CXX_DEFAULTED_FUNCTIONS
    RegularSpectrum() = default;                            // leave all components uninitialized
#else
    RegularSpectrum() {}                                    // leave all components uninitialized
#endif
#endif
    explicit RegularSpectrum(const ValueType val);          // set all components to `val`

    // Construct a spectrum from another spectrum of a different type.
    template <typename U>
    RegularSpectrum(const RegularSpectrum<U, N>& rhs);

    // Construct a spectrum from an array of N scalars.
    static RegularSpectrum from_array(const ValueType* rhs);

    // Set all components to a given value.
    void set(const ValueType val);

    // Unchecked array subscripting.
    ValueType& operator[](const size_t i);
    const ValueType& operator[](const size_t i) const;

  private:
    APPLESEED_SIMD4_ALIGN ValueType m_samples[StoredSamples];
};

// Exact inequality and equality tests.
template <typename T, size_t N> bool operator!=(const RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs);
template <typename T, size_t N> bool operator==(const RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs);

// Return whether all components of a spectrum are exactly zero.
template <typename T, size_t N> bool is_zero(const RegularSpectrum<T, N>& s);

// Approximate equality tests.
template <typename T, size_t N> bool feq(const RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs);
template <typename T, size_t N> bool feq(const RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs, const T eps);

// Approximate zero tests.
template <typename T, size_t N> bool fz(const RegularSpectrum<T, N>& s);
template <typename T, size_t N> bool fz(const RegularSpectrum<T, N>& s, const T eps);

// Spectrum arithmetic.
template <typename T, size_t N> RegularSpectrum<T, N>  operator+ (const RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs);
template <typename T, size_t N> RegularSpectrum<T, N>  operator- (const RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs);
template <typename T, size_t N> RegularSpectrum<T, N>  operator- (const RegularSpectrum<T, N>& lhs);
template <typename T, size_t N> RegularSpectrum<T, N>  operator* (const RegularSpectrum<T, N>& lhs, const T rhs);
template <typename T, size_t N> RegularSpectrum<T, N>  operator* (const T lhs, const RegularSpectrum<T, N>& rhs);
template <typename T, size_t N> RegularSpectrum<T, N>  operator* (const RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs);
template <typename T, size_t N> RegularSpectrum<T, N>  operator/ (const RegularSpectrum<T, N>& lhs, const T rhs);
template <typename T, size_t N> RegularSpectrum<T, N>  operator/ (const RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs);
template <typename T, size_t N> RegularSpectrum<T, N>& operator+=(RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs);
template <typename T, size_t N> RegularSpectrum<T, N>& operator-=(RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs);
template <typename T, size_t N> RegularSpectrum<T, N>& operator*=(RegularSpectrum<T, N>& lhs, const T rhs);
template <typename T, size_t N> RegularSpectrum<T, N>& operator*=(RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs);
template <typename T, size_t N> RegularSpectrum<T, N>& operator/=(RegularSpectrum<T, N>& lhs, const T rhs);
template <typename T, size_t N> RegularSpectrum<T, N>& operator/=(RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs);

// Component-wise reciprocal.
template <typename T, size_t N> RegularSpectrum<T, N> rcp(const RegularSpectrum<T, N>& s);

// Return whether all components of a spectrum are in [0,1].
template <typename T, size_t N> bool is_saturated(const RegularSpectrum<T, N>& s);

// Clamp the argument to [0,1].
template <typename T, size_t N> RegularSpectrum<T, N> saturate(const RegularSpectrum<T, N>& s);
template <typename T, size_t N> void saturate_in_place(RegularSpectrum<T, N>& s);

// Clamp the argument to [min, max].
template <typename T, size_t N> RegularSpectrum<T, N> clamp(const RegularSpectrum<T, N>& s, const T min, const T max);
template <typename T, size_t N> void clamp_in_place(RegularSpectrum<T, N>& s, const T min, const T max);

// Clamp the argument to [min, +infinity).
template <typename T, size_t N> RegularSpectrum<T, N> clamp_low(const RegularSpectrum<T, N>& s, const T min);
template <typename T, size_t N> void clamp_low_in_place(RegularSpectrum<T, N>& s, const T min);

// Clamp the argument to (-infinity, max].
template <typename T, size_t N> RegularSpectrum<T, N> clamp_high(const RegularSpectrum<T, N>& s, const T max);
template <typename T, size_t N> void clamp_high_in_place(RegularSpectrum<T, N>& s, const T max);

// Return the smallest or largest signed component of a spectrum.
template <typename T, size_t N> T min_value(const RegularSpectrum<T, N>& s);
template <typename T, size_t N> T max_value(const RegularSpectrum<T, N>& s);

// Return the index of the smallest or largest signed component of a spectrum.
template <typename T, size_t N> size_t min_index(const RegularSpectrum<T, N>& s);
template <typename T, size_t N> size_t max_index(const RegularSpectrum<T, N>& s);

// Return the index of the smallest or largest component of a spectrum, in absolute value.
template <typename T, size_t N> size_t min_abs_index(const RegularSpectrum<T, N>& s);
template <typename T, size_t N> size_t max_abs_index(const RegularSpectrum<T, N>& s);

// Component-wise min/max of two spectra.
template <typename T, size_t N> RegularSpectrum<T, N> component_wise_min(const RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs);
template <typename T, size_t N> RegularSpectrum<T, N> component_wise_max(const RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs);

// Return the sum of the components of a spectrum.
template <typename T, size_t N> T sum_value(const RegularSpectrum<T, N>& s);

// Return the average value of a spectrum.
template <typename T, size_t N> T average_value(const RegularSpectrum<T, N>& s);

// Return true if a spectrum contains at least one NaN value.
template <typename T, size_t N> bool has_nan(const RegularSpectrum<T, N>& s);

// Return true if all components of a spectrum are finite (not NaN, not infinite).
template <typename T, size_t N> bool is_finite(const RegularSpectrum<T, N>& s);


//
// Full specializations for spectra of type float and double.
//

typedef RegularSpectrum<float,  31> RegularSpectrum31f;
typedef RegularSpectrum<double, 31> RegularSpectrum31d;


//
// RegularSpectrum class implementation.
//

#ifdef APPLESEED_USE_SSE

template <typename T, size_t N>
inline RegularSpectrum<T, N>::RegularSpectrum()
{
    for (size_t i = N; i < StoredSamples; ++i)
        m_samples[i] = T(0.0);
}

#endif

template <typename T, size_t N>
inline RegularSpectrum<T, N>::RegularSpectrum(const ValueType val)
{
    set(val);

#ifdef APPLESEED_USE_SSE
    for (size_t i = N; i < StoredSamples; ++i)
        m_samples[i] = T(0.0);
#endif
}

template <typename T, size_t N>
template <typename U>
inline RegularSpectrum<T, N>::RegularSpectrum(const RegularSpectrum<U, N>& rhs)
{
    for (size_t i = 0; i < N; ++i)
        m_samples[i] = static_cast<ValueType>(rhs[i]);

#ifdef APPLESEED_USE_SSE
    for (size_t i = N; i < StoredSamples; ++i)
        m_samples[i] = T(0.0);
#endif
}

template <typename T, size_t N>
inline RegularSpectrum<T, N> RegularSpectrum<T, N>::from_array(const ValueType* rhs)
{
    assert(rhs);

    RegularSpectrum result;

    for (size_t i = 0; i < N; ++i)
        result.m_samples[i] = rhs[i];

    return result;
}

template <typename T, size_t N>
inline void RegularSpectrum<T, N>::set(const ValueType val)
{
    for (size_t i = 0; i < N; ++i)
        m_samples[i] = val;
}

#ifdef APPLESEED_USE_SSE

template <>
APPLESEED_FORCE_INLINE void RegularSpectrum<float, 31>::set(const float val)
{
    const __m128 mval = _mm_set1_ps(val);

    _mm_store_ps(&m_samples[ 0], mval);
    _mm_store_ps(&m_samples[ 4], mval);
    _mm_store_ps(&m_samples[ 8], mval);
    _mm_store_ps(&m_samples[12], mval);
    _mm_store_ps(&m_samples[16], mval);
    _mm_store_ps(&m_samples[20], mval);
    _mm_store_ps(&m_samples[24], mval);
    _mm_store_ps(&m_samples[28], mval);
}

#endif  // APPLESEED_USE_SSE

template <typename T, size_t N>
inline T& RegularSpectrum<T, N>::operator[](const size_t i)
{
    assert(i < StoredSamples);
    return m_samples[i];
}

template <typename T, size_t N>
inline const T& RegularSpectrum<T, N>::operator[](const size_t i) const
{
    assert(i < StoredSamples);
    return m_samples[i];
}

template <typename T, size_t N>
inline bool operator!=(const RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (lhs[i] != rhs[i])
            return true;
    }

    return false;
}

template <typename T, size_t N>
inline bool operator==(const RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs)
{
    return !(lhs != rhs);
}

template <typename T, size_t N>
inline bool is_zero(const RegularSpectrum<T, N>& s)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (s[i] != T(0.0))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline bool feq(const RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (!feq(lhs[i], rhs[i]))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline bool feq(const RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs, const T eps)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (!feq(lhs[i], rhs[i], eps))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline bool fz(const RegularSpectrum<T, N>& s)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (!fz(s[i]))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline bool fz(const RegularSpectrum<T, N>& s, const T eps)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (!fz(s[i], eps))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline RegularSpectrum<T, N> operator+(const RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs)
{
    RegularSpectrum<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = lhs[i] + rhs[i];

    return result;
}

template <typename T, size_t N>
inline RegularSpectrum<T, N> operator-(const RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs)
{
    RegularSpectrum<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = lhs[i] - rhs[i];

    return result;
}

template <typename T, size_t N>
inline RegularSpectrum<T, N> operator-(const RegularSpectrum<T, N>& lhs)
{
    RegularSpectrum<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = -lhs[i];

    return result;
}

template <typename T, size_t N>
inline RegularSpectrum<T, N> operator*(const RegularSpectrum<T, N>& lhs, const T rhs)
{
    RegularSpectrum<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = lhs[i] * rhs;

    return result;
}

template <typename T, size_t N>
inline RegularSpectrum<T, N> operator*(const T lhs, const RegularSpectrum<T, N>& rhs)
{
    return rhs * lhs;
}

template <typename T, size_t N>
inline RegularSpectrum<T, N> operator*(const RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs)
{
    RegularSpectrum<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = lhs[i] * rhs[i];

    return result;
}

template <typename T, size_t N>
inline RegularSpectrum<T, N> operator/(const RegularSpectrum<T, N>& lhs, const T rhs)
{
    RegularSpectrum<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = lhs[i] / rhs;

    return result;
}

template <size_t N>
inline RegularSpectrum<float, N> operator/(const RegularSpectrum<float, N>& lhs, const float rhs)
{
    return lhs * (1.0f / rhs);
}

template <size_t N>
inline RegularSpectrum<double, N> operator/(const RegularSpectrum<double, N>& lhs, const double rhs)
{
    return lhs * (1.0 / rhs);
}

template <size_t N>
inline RegularSpectrum<long double, N> operator/(const RegularSpectrum<long double, N>& lhs, const long double rhs)
{
    return lhs * (1.0L / rhs);
}

template <typename T, size_t N>
inline RegularSpectrum<T, N> operator/(const RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs)
{
    RegularSpectrum<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = lhs[i] / rhs[i];

    return result;
}

template <typename T, size_t N>
inline RegularSpectrum<T, N>& operator+=(RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs)
{
    for (size_t i = 0; i < N; ++i)
        lhs[i] += rhs[i];

    return lhs;
}

#ifdef APPLESEED_USE_SSE

template <>
APPLESEED_FORCE_INLINE RegularSpectrum<float, 31>& operator+=(RegularSpectrum<float, 31>& lhs, const RegularSpectrum<float, 31>& rhs)
{
    _mm_store_ps(&lhs[ 0], _mm_add_ps(_mm_load_ps(&lhs[ 0]), _mm_load_ps(&rhs[ 0])));
    _mm_store_ps(&lhs[ 4], _mm_add_ps(_mm_load_ps(&lhs[ 4]), _mm_load_ps(&rhs[ 4])));
    _mm_store_ps(&lhs[ 8], _mm_add_ps(_mm_load_ps(&lhs[ 8]), _mm_load_ps(&rhs[ 8])));
    _mm_store_ps(&lhs[12], _mm_add_ps(_mm_load_ps(&lhs[12]), _mm_load_ps(&rhs[12])));
    _mm_store_ps(&lhs[16], _mm_add_ps(_mm_load_ps(&lhs[16]), _mm_load_ps(&rhs[16])));
    _mm_store_ps(&lhs[20], _mm_add_ps(_mm_load_ps(&lhs[20]), _mm_load_ps(&rhs[20])));
    _mm_store_ps(&lhs[24], _mm_add_ps(_mm_load_ps(&lhs[24]), _mm_load_ps(&rhs[24])));
    _mm_store_ps(&lhs[28], _mm_add_ps(_mm_load_ps(&lhs[28]), _mm_load_ps(&rhs[28])));

    return lhs;
}

#endif  // APPLESEED_USE_SSE

template <typename T, size_t N>
inline RegularSpectrum<T, N>& operator-=(RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs)
{
    for (size_t i = 0; i < N; ++i)
        lhs[i] -= rhs[i];

    return lhs;
}

template <typename T, size_t N>
inline RegularSpectrum<T, N>& operator*=(RegularSpectrum<T, N>& lhs, const T rhs)
{
    for (size_t i = 0; i < N; ++i)
        lhs[i] *= rhs;

    return lhs;
}

#ifdef APPLESEED_USE_SSE

template <>
APPLESEED_FORCE_INLINE RegularSpectrum<float, 31>& operator*=(RegularSpectrum<float, 31>& lhs, const float rhs)
{
    const __m128 mrhs = _mm_set1_ps(rhs);

    _mm_store_ps(&lhs[ 0], _mm_mul_ps(_mm_load_ps(&lhs[ 0]), mrhs));
    _mm_store_ps(&lhs[ 4], _mm_mul_ps(_mm_load_ps(&lhs[ 4]), mrhs));
    _mm_store_ps(&lhs[ 8], _mm_mul_ps(_mm_load_ps(&lhs[ 8]), mrhs));
    _mm_store_ps(&lhs[12], _mm_mul_ps(_mm_load_ps(&lhs[12]), mrhs));
    _mm_store_ps(&lhs[16], _mm_mul_ps(_mm_load_ps(&lhs[16]), mrhs));
    _mm_store_ps(&lhs[20], _mm_mul_ps(_mm_load_ps(&lhs[20]), mrhs));
    _mm_store_ps(&lhs[24], _mm_mul_ps(_mm_load_ps(&lhs[24]), mrhs));
    _mm_store_ps(&lhs[28], _mm_mul_ps(_mm_load_ps(&lhs[28]), mrhs));

    return lhs;
}

#endif  // APPLESEED_USE_SSE

template <typename T, size_t N>
inline RegularSpectrum<T, N>& operator*=(RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs)
{
    for (size_t i = 0; i < N; ++i)
        lhs[i] *= rhs[i];

    return lhs;
}

#ifdef APPLESEED_USE_SSE

template <>
APPLESEED_FORCE_INLINE RegularSpectrum<float, 31>& operator*=(RegularSpectrum<float, 31>& lhs, const RegularSpectrum<float, 31>& rhs)
{
    _mm_store_ps(&lhs[ 0], _mm_mul_ps(_mm_load_ps(&lhs[ 0]), _mm_load_ps(&rhs[ 0])));
    _mm_store_ps(&lhs[ 4], _mm_mul_ps(_mm_load_ps(&lhs[ 4]), _mm_load_ps(&rhs[ 4])));
    _mm_store_ps(&lhs[ 8], _mm_mul_ps(_mm_load_ps(&lhs[ 8]), _mm_load_ps(&rhs[ 8])));
    _mm_store_ps(&lhs[12], _mm_mul_ps(_mm_load_ps(&lhs[12]), _mm_load_ps(&rhs[12])));
    _mm_store_ps(&lhs[16], _mm_mul_ps(_mm_load_ps(&lhs[16]), _mm_load_ps(&rhs[16])));
    _mm_store_ps(&lhs[20], _mm_mul_ps(_mm_load_ps(&lhs[20]), _mm_load_ps(&rhs[20])));
    _mm_store_ps(&lhs[24], _mm_mul_ps(_mm_load_ps(&lhs[24]), _mm_load_ps(&rhs[24])));
    _mm_store_ps(&lhs[28], _mm_mul_ps(_mm_load_ps(&lhs[28]), _mm_load_ps(&rhs[28])));

    return lhs;
}

#endif  // APPLESEED_USE_SSE

template <typename T, size_t N>
inline RegularSpectrum<T, N>& operator/=(RegularSpectrum<T, N>& lhs, const T rhs)
{
    for (size_t i = 0; i < N; ++i)
        lhs[i] /= rhs;

    return lhs;
}

template <size_t N>
inline RegularSpectrum<float, N>& operator/=(RegularSpectrum<float, N>& lhs, const float rhs)
{
    return lhs *= 1.0f / rhs;
}

template <size_t N>
inline RegularSpectrum<double, N>& operator/=(RegularSpectrum<double, N>& lhs, const double rhs)
{
    return lhs *= 1.0 / rhs;
}

template <size_t N>
inline RegularSpectrum<long double, N>& operator/=(RegularSpectrum<long double, N>& lhs, const long double rhs)
{
    return lhs *= 1.0L / rhs;
}

template <typename T, size_t N>
inline RegularSpectrum<T, N>& operator/=(RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs)
{
    for (size_t i = 0; i < N; ++i)
        lhs[i] /= rhs[i];

    return lhs;
}

template <typename T, size_t N>
inline RegularSpectrum<T, N> rcp(const RegularSpectrum<T, N>& s)
{
    RegularSpectrum<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = T(1.0) / s[i];

    return result;
}

template <typename T, size_t N>
inline bool is_saturated(const RegularSpectrum<T, N>& s)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (s[i] < T(0.0) || s[i] > T(1.0))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline RegularSpectrum<T, N> saturate(const RegularSpectrum<T, N>& s)
{
    RegularSpectrum<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = saturate(s[i]);

    return result;
}

template <typename T, size_t N>
inline void saturate_in_place(RegularSpectrum<T, N>& s)
{
    clamp_in_place(s, T(0.0), T(1.0));
}

template <typename T, size_t N>
inline RegularSpectrum<T, N> clamp(const RegularSpectrum<T, N>& s, const T min, const T max)
{
    RegularSpectrum<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = clamp(s[i], min, max);

    return result;
}

template <typename T, size_t N>
inline void clamp_in_place(RegularSpectrum<T, N>& s, const T min, const T max)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (s[i] < min)
            s[i] = min;

        if (s[i] > max)
            s[i] = max;
    }
}

template <typename T, size_t N>
inline RegularSpectrum<T, N> clamp_low(const RegularSpectrum<T, N>& s, const T min)
{
    RegularSpectrum<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = std::max(s[i], min);

    return result;
}

template <typename T, size_t N>
inline void clamp_low_in_place(RegularSpectrum<T, N>& s, const T min)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (s[i] < min)
            s[i] = min;
    }
}

template <typename T, size_t N>
inline RegularSpectrum<T, N> clamp_high(const RegularSpectrum<T, N>& s, const T max)
{
    RegularSpectrum<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = std::min(s[i], max);

    return result;
}

template <typename T, size_t N>
inline void clamp_high_in_place(RegularSpectrum<T, N>& s, const T max)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (s[i] > max)
            s[i] = max;
    }
}

template <typename T, size_t N>
inline T min_value(const RegularSpectrum<T, N>& s)
{
    T value = s[0];

    for (size_t i = 1; i < N; ++i)
    {
        if (value > s[i])
            value = s[i];
    }

    return value;
}

#ifdef APPLESEED_USE_SSE

template <>
inline float min_value(const RegularSpectrum<float, 31>& s)
{
    const __m128 m1 = _mm_min_ps(_mm_load_ps(&s[ 0]), _mm_load_ps(&s[ 4]));
    const __m128 m2 = _mm_min_ps(_mm_load_ps(&s[ 8]), _mm_load_ps(&s[12]));
    const __m128 m3 = _mm_min_ps(_mm_load_ps(&s[16]), _mm_load_ps(&s[20]));
    const __m128 s28 = _mm_load_ps(&s[28]);
    const __m128 m4 = _mm_min_ps(_mm_load_ps(&s[24]), _mm_shuffle_ps(s28, s28, _MM_SHUFFLE(2, 2, 1, 0)));
    const __m128 m5 = _mm_min_ps(m1, m2);
    const __m128 m6 = _mm_min_ps(m3, m4);
          __m128 m  = _mm_min_ps(m5, m6);

    m = _mm_min_ps(m, _mm_shuffle_ps(m, m, _MM_SHUFFLE(2, 3, 0, 1)));
    m = _mm_min_ps(m, _mm_shuffle_ps(m, m, _MM_SHUFFLE(1, 0, 3, 2)));

    return _mm_cvtss_f32(m);
}

#endif  // APPLESEED_USE_SSE

template <typename T, size_t N>
inline T max_value(const RegularSpectrum<T, N>& s)
{
    T value = s[0];

    for (size_t i = 1; i < N; ++i)
    {
        if (value < s[i])
            value = s[i];
    }

    return value;
}

#ifdef APPLESEED_USE_SSE

template <>
inline float max_value(const RegularSpectrum<float, 31>& s)
{
    const __m128 m1 = _mm_max_ps(_mm_load_ps(&s[ 0]), _mm_load_ps(&s[ 4]));
    const __m128 m2 = _mm_max_ps(_mm_load_ps(&s[ 8]), _mm_load_ps(&s[12]));
    const __m128 m3 = _mm_max_ps(_mm_load_ps(&s[16]), _mm_load_ps(&s[20]));
    const __m128 s28 = _mm_load_ps(&s[28]);
    const __m128 m4 = _mm_max_ps(_mm_load_ps(&s[24]), _mm_shuffle_ps(s28, s28, _MM_SHUFFLE(2, 2, 1, 0)));
    const __m128 m5 = _mm_max_ps(m1, m2);
    const __m128 m6 = _mm_max_ps(m3, m4);
          __m128 m  = _mm_max_ps(m5, m6);

    m = _mm_max_ps(m, _mm_shuffle_ps(m, m, _MM_SHUFFLE(2, 3, 0, 1)));
    m = _mm_max_ps(m, _mm_shuffle_ps(m, m, _MM_SHUFFLE(1, 0, 3, 2)));

    return _mm_cvtss_f32(m);
}

#endif  // APPLESEED_USE_SSE

template <typename T, size_t N>
inline size_t min_index(const RegularSpectrum<T, N>& s)
{
    size_t index = 0;
    T value = s[0];

    for (size_t i = 1; i < N; ++i)
    {
        const T x = s[i];

        if (value > x)
        {
            value = x;
            index = i;
        }
    }

    return index;
}

template <typename T, size_t N>
inline size_t max_index(const RegularSpectrum<T, N>& s)
{
    size_t index = 0;
    T value = s[0];

    for (size_t i = 1; i < N; ++i)
    {
        const T x = s[i];

        if (value < x)
        {
            value = x;
            index = i;
        }
    }

    return index;
}

template <typename T, size_t N>
inline size_t min_abs_index(const RegularSpectrum<T, N>& s)
{
    size_t index = 0;
    T value = std::abs(s[0]);

    for (size_t i = 1; i < N; ++i)
    {
        const T x = std::abs(s[i]);

        if (value > x)
        {
            value = x;
            index = i;
        }
    }

    return index;
}

template <typename T, size_t N>
inline size_t max_abs_index(const RegularSpectrum<T, N>& s)
{
    size_t index = 0;
    T value = std::abs(s[0]);

    for (size_t i = 1; i < N; ++i)
    {
        const T x = std::abs(s[i]);

        if (value < x)
        {
            value = x;
            index = i;
        }
    }

    return index;
}

template <typename T, size_t N>
inline RegularSpectrum<T, N> component_wise_min(const RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs)
{
    RegularSpectrum<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = std::min(lhs[i], rhs[i]);

    return result;
}

template <typename T, size_t N>
inline RegularSpectrum<T, N> component_wise_max(const RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs)
{
    RegularSpectrum<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = std::max(lhs[i], rhs[i]);

    return result;
}

template <typename T, size_t N>
inline T sum_value(const RegularSpectrum<T, N>& s)
{
    T sum = s[0];

    for (size_t i = 1; i < N; ++i)
        sum += s[i];

    return sum;
}

template <typename T, size_t N>
inline T average_value(const RegularSpectrum<T, N>& s)
{
    return sum_value(s) / N;
}

template <typename T, size_t N>
inline bool has_nan(const RegularSpectrum<T, N>& s)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (s[i] != s[i])
            return true;
    }

    return false;
}

template <typename T, size_t N>
inline bool is_finite(const RegularSpectrum<T, N>& s)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (!FP<T>::is_finite(s[i]))
            return false;
    }

    return true;
}

}   // namespace foundation
