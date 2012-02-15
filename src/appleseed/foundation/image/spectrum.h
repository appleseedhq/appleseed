
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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

#ifndef APPLESEED_FOUNDATION_IMAGE_SPECTRUM_H
#define APPLESEED_FOUNDATION_IMAGE_SPECTRUM_H

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/platform/compiler.h"
#ifdef APPLESEED_FOUNDATION_USE_SSE
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

#ifdef APPLESEED_FOUNDATION_USE_SSE
    // Number of stored samples such that the size of the sample array is a multiple of 16 bytes.
    static const size_t StoredSamples = (((N * sizeof(T)) + 15) & ~15) / sizeof(T);
#else
    static const size_t StoredSamples = N;
#endif

    // Constructors.
    RegularSpectrum();                                      // leave all components uninitialized
    explicit RegularSpectrum(const ValueType* rhs);         // initialize with array of N scalars
    explicit RegularSpectrum(const ValueType val);          // set all components to 'val'

    // Construct a spectrum from another spectrum of a different type.
    template <typename U>
    explicit RegularSpectrum(const RegularSpectrum<U, N>& rhs);

    // Set all components to a given value.
    void set(const ValueType val);

    // Unchecked array subscripting.
    ValueType& operator[](const size_t i);
    const ValueType& operator[](const size_t i) const;

  private:
    SSE_ALIGN ValueType m_samples[StoredSamples];
};

// Exact inequality and equality tests.
template <typename T, size_t N> bool operator!=(const RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs);
template <typename T, size_t N> bool operator==(const RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs);

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

// Return whether all components of a spectrum are in [0,1].
template <typename T, size_t N> bool is_saturated(const RegularSpectrum<T, N>& s);

// Clamp the argument to [0,1].
template <typename T, size_t N> RegularSpectrum<T, N> saturate(const RegularSpectrum<T, N>& s);

// Return the smallest or largest signed component of a spectrum.
template <typename T, size_t N> T min_value(const RegularSpectrum<T, N>& s);
template <typename T, size_t N> T max_value(const RegularSpectrum<T, N>& s);

// Return the index of the smallest or largest signed component of a spectrum.
template <typename T, size_t N> size_t min_index(const RegularSpectrum<T, N>& s);
template <typename T, size_t N> size_t max_index(const RegularSpectrum<T, N>& s);

// Return the index of the smallest or largest component of a spectrum, in absolute value.
template <typename T, size_t N> size_t min_abs_index(const RegularSpectrum<T, N>& s);
template <typename T, size_t N> size_t max_abs_index(const RegularSpectrum<T, N>& s);

// Return the average value of a spectrum.
template <typename T, size_t N> T average_value(const RegularSpectrum<T, N>& s);


//
// Full specializations for spectra of type float and double.
//

typedef RegularSpectrum<float,  31> Spectrum31f;
typedef RegularSpectrum<double, 31> Spectrum31d;


//
// Regular spectrum implementation.
//

template <typename T, size_t N>
inline RegularSpectrum<T, N>::RegularSpectrum()
{
    for (size_t i = N; i < StoredSamples; ++i)
        m_samples[i] = T(0.0);
}

template <typename T, size_t N>
inline RegularSpectrum<T, N>::RegularSpectrum(const ValueType* rhs)
{
    assert(rhs);

    for (size_t i = 0; i < N; ++i)
        m_samples[i] = rhs[i];

    for (size_t i = N; i < StoredSamples; ++i)
        m_samples[i] = T(0.0);
}

template <typename T, size_t N>
inline RegularSpectrum<T, N>::RegularSpectrum(const ValueType val)
{
    set(val);
}

template <typename T, size_t N>
template <typename U>
inline RegularSpectrum<T, N>::RegularSpectrum(const RegularSpectrum<U, N>& rhs)
{
    for (size_t i = 0; i < StoredSamples; ++i)
        m_samples[i] = static_cast<ValueType>(rhs[i]);
}

template <typename T, size_t N>
inline void RegularSpectrum<T, N>::set(const ValueType val)
{
    for (size_t i = 0; i < StoredSamples; ++i)
        m_samples[i] = val;
}

#ifdef APPLESEED_FOUNDATION_USE_SSE

template <>
FORCE_INLINE void RegularSpectrum<float, 31>::set(const float val)
{
    const sse4f mval = set1ps(val);

    storeps(&m_samples[ 0], mval);
    storeps(&m_samples[ 4], mval);
    storeps(&m_samples[ 8], mval);
    storeps(&m_samples[12], mval);
    storeps(&m_samples[16], mval);
    storeps(&m_samples[20], mval);
    storeps(&m_samples[24], mval);
    storeps(&m_samples[28], mval);
}

#endif  // APPLESEED_FOUNDATION_USE_SSE

template <typename T, size_t N>
inline T& RegularSpectrum<T, N>::operator[](const size_t i)
{
    assert(i < Samples);
    return m_samples[i];
}

template <typename T, size_t N>
inline const T& RegularSpectrum<T, N>::operator[](const size_t i) const
{
    assert(i < Samples);
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

#ifdef APPLESEED_FOUNDATION_USE_SSE

template <>
FORCE_INLINE RegularSpectrum<float, 31>& operator+=(RegularSpectrum<float, 31>& lhs, const RegularSpectrum<float, 31>& rhs)
{
    storeps(&lhs[ 0], addps(loadps(&lhs[ 0]), loadps(&rhs[ 0])));
    storeps(&lhs[ 4], addps(loadps(&lhs[ 4]), loadps(&rhs[ 4])));
    storeps(&lhs[ 8], addps(loadps(&lhs[ 8]), loadps(&rhs[ 8])));
    storeps(&lhs[12], addps(loadps(&lhs[12]), loadps(&rhs[12])));
    storeps(&lhs[16], addps(loadps(&lhs[16]), loadps(&rhs[16])));
    storeps(&lhs[20], addps(loadps(&lhs[20]), loadps(&rhs[20])));
    storeps(&lhs[24], addps(loadps(&lhs[24]), loadps(&rhs[24])));
    storeps(&lhs[28], addps(loadps(&lhs[28]), loadps(&rhs[28])));

    return lhs;
}

#endif  // APPLESEED_FOUNDATION_USE_SSE

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

#ifdef APPLESEED_FOUNDATION_USE_SSE

template <>
FORCE_INLINE RegularSpectrum<float, 31>& operator*=(RegularSpectrum<float, 31>& lhs, const float rhs)
{
    const sse4f mrhs = set1ps(rhs);

    storeps(&lhs[ 0], mulps(loadps(&lhs[ 0]), mrhs));
    storeps(&lhs[ 4], mulps(loadps(&lhs[ 4]), mrhs));
    storeps(&lhs[ 8], mulps(loadps(&lhs[ 8]), mrhs));
    storeps(&lhs[12], mulps(loadps(&lhs[12]), mrhs));
    storeps(&lhs[16], mulps(loadps(&lhs[16]), mrhs));
    storeps(&lhs[20], mulps(loadps(&lhs[20]), mrhs));
    storeps(&lhs[24], mulps(loadps(&lhs[24]), mrhs));
    storeps(&lhs[28], mulps(loadps(&lhs[28]), mrhs));

    return lhs;
}

#endif  // APPLESEED_FOUNDATION_USE_SSE

template <typename T, size_t N>
inline RegularSpectrum<T, N>& operator*=(RegularSpectrum<T, N>& lhs, const RegularSpectrum<T, N>& rhs)
{
    for (size_t i = 0; i < N; ++i)
        lhs[i] *= rhs[i];

    return lhs;
}

#ifdef APPLESEED_FOUNDATION_USE_SSE

template <>
FORCE_INLINE RegularSpectrum<float, 31>& operator*=(RegularSpectrum<float, 31>& lhs, const RegularSpectrum<float, 31>& rhs)
{
    storeps(&lhs[ 0], mulps(loadps(&lhs[ 0]), loadps(&rhs[ 0])));
    storeps(&lhs[ 4], mulps(loadps(&lhs[ 4]), loadps(&rhs[ 4])));
    storeps(&lhs[ 8], mulps(loadps(&lhs[ 8]), loadps(&rhs[ 8])));
    storeps(&lhs[12], mulps(loadps(&lhs[12]), loadps(&rhs[12])));
    storeps(&lhs[16], mulps(loadps(&lhs[16]), loadps(&rhs[16])));
    storeps(&lhs[20], mulps(loadps(&lhs[20]), loadps(&rhs[20])));
    storeps(&lhs[24], mulps(loadps(&lhs[24]), loadps(&rhs[24])));
    storeps(&lhs[28], mulps(loadps(&lhs[28]), loadps(&rhs[28])));

    return lhs;
}

#endif  // APPLESEED_FOUNDATION_USE_SSE

template <typename T, size_t N>
inline RegularSpectrum<T, N>& operator/=(RegularSpectrum<T, N>& lhs, const T rhs)
{
    for (size_t i = 0; i < N; ++i)
        lhs[i] /= rhs;

    return lhs;
}

template <size_t N>
inline RegularSpectrum<float, N>& operator/(RegularSpectrum<float, N>& lhs, const float rhs)
{
    return lhs *= 1.0f / rhs;
}

template <size_t N>
inline RegularSpectrum<double, N>& operator/(RegularSpectrum<double, N>& lhs, const double rhs)
{
    return lhs *= 1.0 / rhs;
}

template <size_t N>
inline RegularSpectrum<long double, N>& operator/(RegularSpectrum<long double, N>& lhs, const long double rhs)
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
inline bool is_saturated(const RegularSpectrum<T, N>& s)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (s[i] < 0.0f || s[i] > 1.0f)
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

template <typename T, size_t N>
inline size_t min_index(const RegularSpectrum<T, N>& s)
{
    size_t index = 0;
    T value = s[0];

    for (size_t i = 1; i < N; ++i)
    {
        const size_t x = s[i];
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
        const size_t x = s[i];
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
        const size_t x = std::abs(s[i]);
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
        const size_t x = std::abs(s[i]);
        if (value < x)
        {
            value = x;
            index = i;
        }
    }

    return index;
}

template <typename T, size_t N>
inline T average_value(const RegularSpectrum<T, N>& s)
{
    T average = s[0];

    for (size_t i = 1; i < N; ++i)
        average += s[i];

    return average / N;
}

}       // namespace foundation


//
// Overload std::min() and std::max() for component-wise min/max operations on spectra.
//

namespace std
{

template <typename T, size_t N>
inline foundation::RegularSpectrum<T, N> min(
    const foundation::RegularSpectrum<T, N>&    lhs,
    const foundation::RegularSpectrum<T, N>&    rhs)
{
    foundation::RegularSpectrum<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = min(lhs[i], rhs[i]);

    return result;
}

template <typename T, size_t N>
inline foundation::RegularSpectrum<T, N> max(
    const foundation::RegularSpectrum<T, N>&    lhs,
    const foundation::RegularSpectrum<T, N>&    rhs)
{
    foundation::RegularSpectrum<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = max(lhs[i], rhs[i]);

    return result;
}

}       // namespace std

#endif  // !APPLESEED_FOUNDATION_IMAGE_SPECTRUM_H
