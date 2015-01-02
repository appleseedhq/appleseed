
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_UTILITY_DYNAMICSPECTRUM_H
#define APPLESEED_RENDERER_UTILITY_DYNAMICSPECTRUM_H

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/regularspectrum.h"
#include "foundation/math/scalar.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/types.h"
#ifdef APPLESEED_USE_SSE
#include "foundation/platform/sse.h"
#endif

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>

namespace renderer
{

//
// A spectrum that can switch between RGB and fully spectral as needed.
//

template <typename T, size_t N>
class DynamicSpectrum
{
  public:
    // Value type and number of samples.
    typedef T ValueType;
    static const size_t Samples = N;

    // Number of stored samples such that the size of the sample array is a multiple of 16 bytes.
    static const size_t StoredSamples = (((N * sizeof(T)) + 15) & ~15) / sizeof(T);

    // Constructors.
    DynamicSpectrum();                                                          // set size to 3, leave all components uninitialized
    explicit DynamicSpectrum(const ValueType* rhs);                             // set size to N, initialize with array of N scalars
    explicit DynamicSpectrum(const ValueType val);                              // set size to 3, set all components to 'val'
    DynamicSpectrum(const foundation::Color<ValueType, 3>& rhs);                // set size to 3
    DynamicSpectrum(const foundation::RegularSpectrum<ValueType, N>& rhs);      // set size to N

    // Construct a spectrum from another spectrum of a different type.
    template <typename U>
    DynamicSpectrum(const DynamicSpectrum<U, N>& rhs);

    // Assignment operators.
    DynamicSpectrum& operator=(const foundation::Color<ValueType, 3>& rhs);
    DynamicSpectrum& operator=(const foundation::RegularSpectrum<ValueType, N>& rhs);

    // Return true if this spectrum currently stores a linear RGB value.
    bool is_rgb() const;

    // Return true if this spectrum currently stores a spectral value.
    bool is_spectral() const;

    // Return the number of active components in the spectrum.
    size_t size() const;

    // Set the number of active components in the spectrum. Allowed values are 3 and N.
    void resize(const size_t size);

    // Set all components to a given value.
    void set(const ValueType val);

    // Access the spectrum as a linear RGB color.
    foundation::Color<ValueType, 3>& rgb();
    const foundation::Color<ValueType, 3>& rgb() const;

    // Convert the spectrum to a linear RGB color.
    foundation::Color<ValueType, 3> convert_to_rgb(
        const foundation::LightingConditions& lighting_conditions) const;

    // Unchecked array subscripting.
    ValueType& operator[](const size_t i);
    const ValueType& operator[](const size_t i) const;

    // Upgrade a spectrum from RGB to spectral.
    // 'source' and 'dest' can reference the same instance.
    static void upgrade(
        const DynamicSpectrum&  source,
        DynamicSpectrum&        dest);

  private:
    SSE_ALIGN ValueType m_samples[StoredSamples];
    foundation::uint32  m_size;
};

// Exact inequality and equality tests.
template <typename T, size_t N> bool operator!=(const DynamicSpectrum<T, N>& lhs, const DynamicSpectrum<T, N>& rhs);
template <typename T, size_t N> bool operator==(const DynamicSpectrum<T, N>& lhs, const DynamicSpectrum<T, N>& rhs);

// Spectrum arithmetic.
template <typename T, size_t N> DynamicSpectrum<T, N>  operator+ (const DynamicSpectrum<T, N>& lhs, const DynamicSpectrum<T, N>& rhs);
template <typename T, size_t N> DynamicSpectrum<T, N>  operator- (const DynamicSpectrum<T, N>& lhs, const DynamicSpectrum<T, N>& rhs);
template <typename T, size_t N> DynamicSpectrum<T, N>  operator- (const DynamicSpectrum<T, N>& lhs);
template <typename T, size_t N> DynamicSpectrum<T, N>  operator* (const DynamicSpectrum<T, N>& lhs, const T rhs);
template <typename T, size_t N> DynamicSpectrum<T, N>  operator* (const T lhs, const DynamicSpectrum<T, N>& rhs);
template <typename T, size_t N> DynamicSpectrum<T, N>  operator* (const DynamicSpectrum<T, N>& lhs, const DynamicSpectrum<T, N>& rhs);
template <typename T, size_t N> DynamicSpectrum<T, N>  operator/ (const DynamicSpectrum<T, N>& lhs, const T rhs);
template <typename T, size_t N> DynamicSpectrum<T, N>  operator/ (const DynamicSpectrum<T, N>& lhs, const DynamicSpectrum<T, N>& rhs);
template <typename T, size_t N> DynamicSpectrum<T, N>& operator+=(DynamicSpectrum<T, N>& lhs, const DynamicSpectrum<T, N>& rhs);
template <typename T, size_t N> DynamicSpectrum<T, N>& operator-=(DynamicSpectrum<T, N>& lhs, const DynamicSpectrum<T, N>& rhs);
template <typename T, size_t N> DynamicSpectrum<T, N>& operator*=(DynamicSpectrum<T, N>& lhs, const T rhs);
template <typename T, size_t N> DynamicSpectrum<T, N>& operator*=(DynamicSpectrum<T, N>& lhs, const DynamicSpectrum<T, N>& rhs);
template <typename T, size_t N> DynamicSpectrum<T, N>& operator/=(DynamicSpectrum<T, N>& lhs, const T rhs);
template <typename T, size_t N> DynamicSpectrum<T, N>& operator/=(DynamicSpectrum<T, N>& lhs, const DynamicSpectrum<T, N>& rhs);


//
// Full specializations for spectra of type float and double.
//

typedef DynamicSpectrum<float,  31> DynamicSpectrum31f;
typedef DynamicSpectrum<double, 31> DynamicSpectrum31d;

}   // namespace renderer

namespace foundation
{

// Return whether all components of a spectrum are exactly zero.
template <typename T, size_t N> bool is_zero(const renderer::DynamicSpectrum<T, N>& s);

// Approximate equality tests.
template <typename T, size_t N> bool feq(const renderer::DynamicSpectrum<T, N>& lhs, const renderer::DynamicSpectrum<T, N>& rhs);
template <typename T, size_t N> bool feq(const renderer::DynamicSpectrum<T, N>& lhs, const renderer::DynamicSpectrum<T, N>& rhs, const T eps);

// Approximate zero tests.
template <typename T, size_t N> bool fz(const renderer::DynamicSpectrum<T, N>& s);
template <typename T, size_t N> bool fz(const renderer::DynamicSpectrum<T, N>& s, const T eps);

// Return whether all components of a spectrum are in [0,1].
template <typename T, size_t N> bool is_saturated(const renderer::DynamicSpectrum<T, N>& s);

// Clamp the argument to [0,1].
template <typename T, size_t N> renderer::DynamicSpectrum<T, N> saturate(const renderer::DynamicSpectrum<T, N>& s);

// Clamp the argument to [min, max].
template <typename T, size_t N> renderer::DynamicSpectrum<T, N> clamp(const renderer::DynamicSpectrum<T, N>& s, const T min, const T max);

// Clamp the argument to [min, +infinity).
template <typename T, size_t N> renderer::DynamicSpectrum<T, N> clamp_low(const renderer::DynamicSpectrum<T, N>& s, const T min);

// Clamp the argument to (-infinity, max].
template <typename T, size_t N> renderer::DynamicSpectrum<T, N> clamp_high(const renderer::DynamicSpectrum<T, N>& s, const T max);

// Return the smallest or largest signed component of a spectrum.
template <typename T, size_t N> T min_value(const renderer::DynamicSpectrum<T, N>& s);
template <typename T, size_t N> T max_value(const renderer::DynamicSpectrum<T, N>& s);

// Return the index of the smallest or largest signed component of a spectrum.
template <typename T, size_t N> size_t min_index(const renderer::DynamicSpectrum<T, N>& s);
template <typename T, size_t N> size_t max_index(const renderer::DynamicSpectrum<T, N>& s);

// Return the index of the smallest or largest component of a spectrum, in absolute value.
template <typename T, size_t N> size_t min_abs_index(const renderer::DynamicSpectrum<T, N>& s);
template <typename T, size_t N> size_t max_abs_index(const renderer::DynamicSpectrum<T, N>& s);

// Return the sum of all values of a spectrum.
template <typename T, size_t N> T sum_value(const renderer::DynamicSpectrum<T, N>& s);

// Return the average value of a spectrum.
template <typename T, size_t N> T average_value(const renderer::DynamicSpectrum<T, N>& s);

// Return true if a spectrum contains at least one NaN value.
template <typename T, size_t N> bool has_nan(const renderer::DynamicSpectrum<T, N>& s);

}   // namespace foundation


//
// DynamicSpectrum class implementation.
//

namespace renderer
{

template <typename T, size_t N>
inline DynamicSpectrum<T, N>::DynamicSpectrum()
  : m_size(3)
{
    for (size_t i = N; i < StoredSamples; ++i)
        m_samples[i] = T(0.0);
}

template <typename T, size_t N>
inline DynamicSpectrum<T, N>::DynamicSpectrum(const ValueType* rhs)
  : m_size(N)
{
    assert(rhs);

    for (size_t i = 0; i < N; ++i)
        m_samples[i] = rhs[i];

    for (size_t i = N; i < StoredSamples; ++i)
        m_samples[i] = T(0.0);
}

template <typename T, size_t N>
inline DynamicSpectrum<T, N>::DynamicSpectrum(const ValueType val)
  : m_size(3)
{
    set(val);

    for (size_t i = N; i < StoredSamples; ++i)
        m_samples[i] = T(0.0);
}

template <typename T, size_t N>
template <typename U>
inline DynamicSpectrum<T, N>::DynamicSpectrum(const DynamicSpectrum<U, N>& rhs)
  : m_size(rhs.m_size)
{
    for (size_t i = 0; i < m_size; ++i)
        m_samples[i] = static_cast<ValueType>(rhs[i]);

    for (size_t i = N; i < StoredSamples; ++i)
        m_samples[i] = T(0.0);
}

template <typename T, size_t N>
inline DynamicSpectrum<T, N>::DynamicSpectrum(const foundation::Color<ValueType, 3>& rhs)
  : m_size(3)
{
    m_samples[0] = rhs[0];
    m_samples[1] = rhs[1];
    m_samples[2] = rhs[2];

    for (size_t i = N; i < StoredSamples; ++i)
        m_samples[i] = T(0.0);
}

template <typename T, size_t N>
inline DynamicSpectrum<T, N>::DynamicSpectrum(const foundation::RegularSpectrum<ValueType, N>& rhs)
  : m_size(N)
{
    for (size_t i = 0; i < N; ++i)
        m_samples[i] = rhs[i];

    for (size_t i = N; i < StoredSamples; ++i)
        m_samples[i] = T(0.0);
}

template <typename T, size_t N>
inline DynamicSpectrum<T, N>& DynamicSpectrum<T, N>::operator=(const foundation::Color<ValueType, 3>& rhs)
{
    m_size = 3;

    m_samples[0] = rhs[0];
    m_samples[1] = rhs[1];
    m_samples[2] = rhs[2];

    return *this;
}

template <typename T, size_t N>
inline DynamicSpectrum<T, N>& DynamicSpectrum<T, N>::operator=(const foundation::RegularSpectrum<ValueType, N>& rhs)
{
    m_size = N;

    for (size_t i = 0; i < N; ++i)
        m_samples[i] = rhs[i];

    return *this;
}

template <typename T, size_t N>
inline bool DynamicSpectrum<T, N>::is_rgb() const
{
    return m_size == 3;
}

template <typename T, size_t N>
inline bool DynamicSpectrum<T, N>::is_spectral() const
{
    return m_size == N;
}

template <typename T, size_t N>
inline size_t DynamicSpectrum<T, N>::size() const
{
    return static_cast<size_t>(m_size);
}

template <typename T, size_t N>
inline void DynamicSpectrum<T, N>::resize(const size_t size)
{
    assert(size == 3 || size == N);
    m_size = static_cast<foundation::uint32>(size);
}

template <typename T, size_t N>
inline void DynamicSpectrum<T, N>::set(const ValueType val)
{
    for (size_t i = 0; i < m_size; ++i)
        m_samples[i] = val;
}

#ifdef APPLESEED_USE_SSE

template <>
FORCE_INLINE void DynamicSpectrum<float, 31>::set(const float val)
{
    const __m128 mval = _mm_set1_ps(val);

    _mm_store_ps(&m_samples[ 0], mval);

    if (m_size > 3)
    {
        _mm_store_ps(&m_samples[ 4], mval);
        _mm_store_ps(&m_samples[ 8], mval);
        _mm_store_ps(&m_samples[12], mval);
        _mm_store_ps(&m_samples[16], mval);
        _mm_store_ps(&m_samples[20], mval);
        _mm_store_ps(&m_samples[24], mval);
        _mm_store_ps(&m_samples[28], mval);
    }
}

#endif  // APPLESEED_USE_SSE

template <typename T, size_t N>
inline foundation::Color<T, 3>& DynamicSpectrum<T, N>::rgb()
{
    return reinterpret_cast<foundation::Color<T, 3>&>(m_samples);
}

template <typename T, size_t N>
inline const foundation::Color<T, 3>& DynamicSpectrum<T, N>::rgb() const
{
    return reinterpret_cast<const foundation::Color<T, 3>&>(m_samples);
}

template <typename T, size_t N>
inline foundation::Color<T, 3> DynamicSpectrum<T, N>::convert_to_rgb(
    const foundation::LightingConditions& lighting_conditions) const
{
    return
        foundation::ciexyz_to_linear_rgb(
            foundation::spectrum_to_ciexyz<float>(lighting_conditions, *this));
}

template <typename T, size_t N>
inline T& DynamicSpectrum<T, N>::operator[](const size_t i)
{
    assert(i < m_size);
    return m_samples[i];
}

template <typename T, size_t N>
inline const T& DynamicSpectrum<T, N>::operator[](const size_t i) const
{
    assert(i < m_size);
    return m_samples[i];
}

template <typename T, size_t N>
inline void DynamicSpectrum<T, N>::upgrade(
    const DynamicSpectrum&  source,
    DynamicSpectrum&        dest)
{
    if (source.is_rgb())
    {
        dest.m_size = N;

        foundation::linear_rgb_illuminance_to_spectrum(
            reinterpret_cast<const foundation::Color<ValueType, 3>&>(source[0]),
            reinterpret_cast<foundation::RegularSpectrum<ValueType, N>&>(dest[0]));
    }
    else
    {
        dest = source;
    }
}

template <typename T, size_t N>
inline bool operator!=(const DynamicSpectrum<T, N>& lhs, const DynamicSpectrum<T, N>& rhs)
{
    if (lhs.size() != rhs.size())
        return true;

    for (size_t i = 0, e = lhs.size(); i < e; ++i)
    {
        if (lhs[i] != rhs[i])
            return true;
    }

    return false;
}

template <typename T, size_t N>
inline bool operator==(const DynamicSpectrum<T, N>& lhs, const DynamicSpectrum<T, N>& rhs)
{
    return !(lhs != rhs);
}

template <typename T, size_t N>
inline DynamicSpectrum<T, N> operator+(const DynamicSpectrum<T, N>& lhs, const DynamicSpectrum<T, N>& rhs)
{
    DynamicSpectrum<T, N> result;

    if (lhs.size() == rhs.size())
    {
        result.resize(lhs.size());

        for (size_t i = 0, e = lhs.size(); i < e; ++i)
            result[i] = lhs[i] + rhs[i];
    }
    else
    {
        result.resize(N);

        DynamicSpectrum<T, N> up_lhs, up_rhs;
        DynamicSpectrum<T, N>::upgrade(lhs, up_lhs);
        DynamicSpectrum<T, N>::upgrade(rhs, up_rhs);

        for (size_t i = 0; i < N; ++i)
            result[i] = up_lhs[i] + up_rhs[i];
    }

    return result;
}

template <typename T, size_t N>
inline DynamicSpectrum<T, N> operator-(const DynamicSpectrum<T, N>& lhs, const DynamicSpectrum<T, N>& rhs)
{
    DynamicSpectrum<T, N> result;

    if (lhs.size() == rhs.size())
    {
        result.resize(lhs.size());

        for (size_t i = 0, e = lhs.size(); i < e; ++i)
            result[i] = lhs[i] - rhs[i];
    }
    else
    {
        result.resize(N);

        DynamicSpectrum<T, N> up_lhs, up_rhs;
        DynamicSpectrum<T, N>::upgrade(lhs, up_lhs);
        DynamicSpectrum<T, N>::upgrade(rhs, up_rhs);

        for (size_t i = 0; i < N; ++i)
            result[i] = up_lhs[i] - up_rhs[i];
    }

    return result;
}

template <typename T, size_t N>
inline DynamicSpectrum<T, N> operator-(const DynamicSpectrum<T, N>& lhs)
{
    DynamicSpectrum<T, N> result;
    result.resize(lhs.size());

    for (size_t i = 0, e = lhs.size(); i < e; ++i)
        result[i] = -lhs[i];

    return result;
}

template <typename T, size_t N>
inline DynamicSpectrum<T, N> operator*(const DynamicSpectrum<T, N>& lhs, const T rhs)
{
    DynamicSpectrum<T, N> result;
    result.resize(lhs.size());

    for (size_t i = 0, e = lhs.size(); i < e; ++i)
        result[i] = lhs[i] * rhs;

    return result;
}

template <typename T, size_t N>
inline DynamicSpectrum<T, N> operator*(const T lhs, const DynamicSpectrum<T, N>& rhs)
{
    return rhs * lhs;
}

template <typename T, size_t N>
inline DynamicSpectrum<T, N> operator*(const DynamicSpectrum<T, N>& lhs, const DynamicSpectrum<T, N>& rhs)
{
    DynamicSpectrum<T, N> result;

    if (lhs.size() == rhs.size())
    {
        result.resize(lhs.size());

        for (size_t i = 0, e = lhs.size(); i < e; ++i)
            result[i] = lhs[i] * rhs[i];
    }
    else
    {
        result.resize(N);

        DynamicSpectrum<T, N> up_lhs, up_rhs;
        DynamicSpectrum<T, N>::upgrade(lhs, up_lhs);
        DynamicSpectrum<T, N>::upgrade(rhs, up_rhs);

        for (size_t i = 0; i < N; ++i)
            result[i] = up_lhs[i] * up_rhs[i];
    }

    return result;
}

template <typename T, size_t N>
inline DynamicSpectrum<T, N> operator/(const DynamicSpectrum<T, N>& lhs, const T rhs)
{
    DynamicSpectrum<T, N> result;
    result.resize(lhs.size());

    for (size_t i = 0, e = lhs.size(); i < e; ++i)
        result[i] = lhs[i] / rhs;

    return result;
}

template <size_t N>
inline DynamicSpectrum<float, N> operator/(const DynamicSpectrum<float, N>& lhs, const float rhs)
{
    return lhs * (1.0f / rhs);
}

template <size_t N>
inline DynamicSpectrum<double, N> operator/(const DynamicSpectrum<double, N>& lhs, const double rhs)
{
    return lhs * (1.0 / rhs);
}

template <size_t N>
inline DynamicSpectrum<long double, N> operator/(const DynamicSpectrum<long double, N>& lhs, const long double rhs)
{
    return lhs * (1.0L / rhs);
}

template <typename T, size_t N>
inline DynamicSpectrum<T, N> operator/(const DynamicSpectrum<T, N>& lhs, const DynamicSpectrum<T, N>& rhs)
{
    DynamicSpectrum<T, N> result;

    if (lhs.size() == rhs.size())
    {
        result.resize(lhs.size());

        for (size_t i = 0, e = lhs.size(); i < e; ++i)
            result[i] = lhs[i] / rhs[i];
    }
    else
    {
        result.resize(N);

        DynamicSpectrum<T, N> up_lhs, up_rhs;
        DynamicSpectrum<T, N>::upgrade(lhs, up_lhs);
        DynamicSpectrum<T, N>::upgrade(rhs, up_rhs);

        for (size_t i = 0; i < N; ++i)
            result[i] = up_lhs[i] / up_rhs[i];
    }

    return result;
}

template <typename T, size_t N>
inline DynamicSpectrum<T, N>& operator+=(DynamicSpectrum<T, N>& lhs, const DynamicSpectrum<T, N>& rhs)
{
    if (lhs.size() <= rhs.size())
    {
        if (lhs.size() < rhs.size())
            DynamicSpectrum<T, N>::upgrade(lhs, lhs);

        for (size_t i = 0, e = lhs.size(); i < e; ++i)
            lhs[i] += rhs[i];
    }
    else
    {
        DynamicSpectrum<T, N> up_rhs;
        DynamicSpectrum<T, N>::upgrade(rhs, up_rhs);

        for (size_t i = 0; i < N; ++i)
            lhs[i] += up_rhs[i];
    }

    return lhs;
}

#ifdef APPLESEED_USE_SSE

template <>
FORCE_INLINE DynamicSpectrum<float, 31>& operator+=(DynamicSpectrum<float, 31>& lhs, const DynamicSpectrum<float, 31>& rhs)
{
    if (lhs.size() <= rhs.size())
    {
        if (lhs.size() < rhs.size())
            DynamicSpectrum<float, 31>::upgrade(lhs, lhs);

        _mm_store_ps(&lhs[ 0], _mm_add_ps(_mm_load_ps(&lhs[ 0]), _mm_load_ps(&rhs[ 0])));

        if (lhs.size() > 3)
        {
            _mm_store_ps(&lhs[ 4], _mm_add_ps(_mm_load_ps(&lhs[ 4]), _mm_load_ps(&rhs[ 4])));
            _mm_store_ps(&lhs[ 8], _mm_add_ps(_mm_load_ps(&lhs[ 8]), _mm_load_ps(&rhs[ 8])));
            _mm_store_ps(&lhs[12], _mm_add_ps(_mm_load_ps(&lhs[12]), _mm_load_ps(&rhs[12])));
            _mm_store_ps(&lhs[16], _mm_add_ps(_mm_load_ps(&lhs[16]), _mm_load_ps(&rhs[16])));
            _mm_store_ps(&lhs[20], _mm_add_ps(_mm_load_ps(&lhs[20]), _mm_load_ps(&rhs[20])));
            _mm_store_ps(&lhs[24], _mm_add_ps(_mm_load_ps(&lhs[24]), _mm_load_ps(&rhs[24])));
            _mm_store_ps(&lhs[28], _mm_add_ps(_mm_load_ps(&lhs[28]), _mm_load_ps(&rhs[28])));
        }
    }
    else
    {
        DynamicSpectrum<float, 31> up_rhs;
        DynamicSpectrum<float, 31>::upgrade(rhs, up_rhs);

        _mm_store_ps(&lhs[ 0], _mm_add_ps(_mm_load_ps(&lhs[ 0]), _mm_load_ps(&up_rhs[ 0])));
        _mm_store_ps(&lhs[ 4], _mm_add_ps(_mm_load_ps(&lhs[ 4]), _mm_load_ps(&up_rhs[ 4])));
        _mm_store_ps(&lhs[ 8], _mm_add_ps(_mm_load_ps(&lhs[ 8]), _mm_load_ps(&up_rhs[ 8])));
        _mm_store_ps(&lhs[12], _mm_add_ps(_mm_load_ps(&lhs[12]), _mm_load_ps(&up_rhs[12])));
        _mm_store_ps(&lhs[16], _mm_add_ps(_mm_load_ps(&lhs[16]), _mm_load_ps(&up_rhs[16])));
        _mm_store_ps(&lhs[20], _mm_add_ps(_mm_load_ps(&lhs[20]), _mm_load_ps(&up_rhs[20])));
        _mm_store_ps(&lhs[24], _mm_add_ps(_mm_load_ps(&lhs[24]), _mm_load_ps(&up_rhs[24])));
        _mm_store_ps(&lhs[28], _mm_add_ps(_mm_load_ps(&lhs[28]), _mm_load_ps(&up_rhs[28])));
    }

    return lhs;
}

#endif  // APPLESEED_USE_SSE

template <typename T, size_t N>
inline DynamicSpectrum<T, N>& operator-=(DynamicSpectrum<T, N>& lhs, const DynamicSpectrum<T, N>& rhs)
{
    if (lhs.size() <= rhs.size())
    {
        if (lhs.size() < rhs.size())
            DynamicSpectrum<T, N>::upgrade(lhs, lhs);

        for (size_t i = 0, e = lhs.size(); i < e; ++i)
            lhs[i] -= rhs[i];
    }
    else
    {
        DynamicSpectrum<T, N> up_rhs;
        DynamicSpectrum<float, 31>::upgrade(rhs, up_rhs);

        for (size_t i = 0; i < N; ++i)
            lhs[i] -= up_rhs[i];
    }

    return lhs;
}

template <typename T, size_t N>
inline DynamicSpectrum<T, N>& operator*=(DynamicSpectrum<T, N>& lhs, const T rhs)
{
    for (size_t i = 0, e = lhs.size(); i < e; ++i)
        lhs[i] *= rhs;

    return lhs;
}

#ifdef APPLESEED_USE_SSE

template <>
FORCE_INLINE DynamicSpectrum<float, 31>& operator*=(DynamicSpectrum<float, 31>& lhs, const float rhs)
{
    const __m128 mrhs = _mm_set1_ps(rhs);

    _mm_store_ps(&lhs[ 0], _mm_mul_ps(_mm_load_ps(&lhs[ 0]), mrhs));

    if (lhs.size() > 3)
    {
        _mm_store_ps(&lhs[ 4], _mm_mul_ps(_mm_load_ps(&lhs[ 4]), mrhs));
        _mm_store_ps(&lhs[ 8], _mm_mul_ps(_mm_load_ps(&lhs[ 8]), mrhs));
        _mm_store_ps(&lhs[12], _mm_mul_ps(_mm_load_ps(&lhs[12]), mrhs));
        _mm_store_ps(&lhs[16], _mm_mul_ps(_mm_load_ps(&lhs[16]), mrhs));
        _mm_store_ps(&lhs[20], _mm_mul_ps(_mm_load_ps(&lhs[20]), mrhs));
        _mm_store_ps(&lhs[24], _mm_mul_ps(_mm_load_ps(&lhs[24]), mrhs));
        _mm_store_ps(&lhs[28], _mm_mul_ps(_mm_load_ps(&lhs[28]), mrhs));
    }

    return lhs;
}

#endif  // APPLESEED_USE_SSE

template <typename T, size_t N>
inline DynamicSpectrum<T, N>& operator*=(DynamicSpectrum<T, N>& lhs, const DynamicSpectrum<T, N>& rhs)
{
    if (lhs.size() <= rhs.size())
    {
        if (lhs.size() < rhs.size())
            DynamicSpectrum<T, N>::upgrade(lhs, lhs);

        for (size_t i = 0, e = lhs.size(); i < e; ++i)
            lhs[i] *= rhs[i];
    }
    else
    {
        DynamicSpectrum<T, N> up_rhs;
        DynamicSpectrum<float, 31>::upgrade(rhs, up_rhs);

        for (size_t i = 0; i < N; ++i)
            lhs[i] *= up_rhs[i];
    }

    return lhs;
}

#ifdef APPLESEED_USE_SSE

template <>
FORCE_INLINE DynamicSpectrum<float, 31>& operator*=(DynamicSpectrum<float, 31>& lhs, const DynamicSpectrum<float, 31>& rhs)
{
    if (lhs.size() <= rhs.size())
    {
        if (lhs.size() < rhs.size())
            DynamicSpectrum<float, 31>::upgrade(lhs, lhs);

        _mm_store_ps(&lhs[ 0], _mm_mul_ps(_mm_load_ps(&lhs[ 0]), _mm_load_ps(&rhs[ 0])));

        if (lhs.size() > 3)
        {
            _mm_store_ps(&lhs[ 4], _mm_mul_ps(_mm_load_ps(&lhs[ 4]), _mm_load_ps(&rhs[ 4])));
            _mm_store_ps(&lhs[ 8], _mm_mul_ps(_mm_load_ps(&lhs[ 8]), _mm_load_ps(&rhs[ 8])));
            _mm_store_ps(&lhs[12], _mm_mul_ps(_mm_load_ps(&lhs[12]), _mm_load_ps(&rhs[12])));
            _mm_store_ps(&lhs[16], _mm_mul_ps(_mm_load_ps(&lhs[16]), _mm_load_ps(&rhs[16])));
            _mm_store_ps(&lhs[20], _mm_mul_ps(_mm_load_ps(&lhs[20]), _mm_load_ps(&rhs[20])));
            _mm_store_ps(&lhs[24], _mm_mul_ps(_mm_load_ps(&lhs[24]), _mm_load_ps(&rhs[24])));
            _mm_store_ps(&lhs[28], _mm_mul_ps(_mm_load_ps(&lhs[28]), _mm_load_ps(&rhs[28])));
        }
    }
    else
    {
        DynamicSpectrum<float, 31> up_rhs;
        DynamicSpectrum<float, 31>::upgrade(rhs, up_rhs);

        _mm_store_ps(&lhs[ 0], _mm_mul_ps(_mm_load_ps(&lhs[ 0]), _mm_load_ps(&up_rhs[ 0])));
        _mm_store_ps(&lhs[ 4], _mm_mul_ps(_mm_load_ps(&lhs[ 4]), _mm_load_ps(&up_rhs[ 4])));
        _mm_store_ps(&lhs[ 8], _mm_mul_ps(_mm_load_ps(&lhs[ 8]), _mm_load_ps(&up_rhs[ 8])));
        _mm_store_ps(&lhs[12], _mm_mul_ps(_mm_load_ps(&lhs[12]), _mm_load_ps(&up_rhs[12])));
        _mm_store_ps(&lhs[16], _mm_mul_ps(_mm_load_ps(&lhs[16]), _mm_load_ps(&up_rhs[16])));
        _mm_store_ps(&lhs[20], _mm_mul_ps(_mm_load_ps(&lhs[20]), _mm_load_ps(&up_rhs[20])));
        _mm_store_ps(&lhs[24], _mm_mul_ps(_mm_load_ps(&lhs[24]), _mm_load_ps(&up_rhs[24])));
        _mm_store_ps(&lhs[28], _mm_mul_ps(_mm_load_ps(&lhs[28]), _mm_load_ps(&up_rhs[28])));
    }

    return lhs;
}

#endif  // APPLESEED_USE_SSE

template <typename T, size_t N>
inline DynamicSpectrum<T, N>& operator/=(DynamicSpectrum<T, N>& lhs, const T rhs)
{
    for (size_t i = 0, e = lhs.size(); i < e; ++i)
        lhs[i] /= rhs;

    return lhs;
}

template <size_t N>
inline DynamicSpectrum<float, N>& operator/=(DynamicSpectrum<float, N>& lhs, const float rhs)
{
    return lhs *= 1.0f / rhs;
}

template <size_t N>
inline DynamicSpectrum<double, N>& operator/=(DynamicSpectrum<double, N>& lhs, const double rhs)
{
    return lhs *= 1.0 / rhs;
}

template <size_t N>
inline DynamicSpectrum<long double, N>& operator/=(DynamicSpectrum<long double, N>& lhs, const long double rhs)
{
    return lhs *= 1.0L / rhs;
}

template <typename T, size_t N>
inline DynamicSpectrum<T, N>& operator/=(DynamicSpectrum<T, N>& lhs, const DynamicSpectrum<T, N>& rhs)
{
    if (lhs.size() <= rhs.size())
    {
        if (lhs.size() < rhs.size())
            DynamicSpectrum<T, N>::upgrade(lhs, lhs);

        for (size_t i = 0, e = lhs.size(); i < e; ++i)
            lhs[i] /= rhs[i];
    }
    else
    {
        DynamicSpectrum<T, N> up_rhs;
        DynamicSpectrum<float, 31>::upgrade(rhs, up_rhs);

        for (size_t i = 0; i < N; ++i)
            lhs[i] /= up_rhs[i];
    }

    return lhs;
}

}   // namespace renderer

namespace foundation
{

template <typename T, size_t N>
inline bool is_zero(const renderer::DynamicSpectrum<T, N>& s)
{
    for (size_t i = 0, e = s.size(); i < e; ++i)
    {
        if (s[i] != T(0.0))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline bool feq(const renderer::DynamicSpectrum<T, N>& lhs, const renderer::DynamicSpectrum<T, N>& rhs)
{
    if (lhs.size() != rhs.size())
        return false;

    for (size_t i = 0, e = lhs.size(); i < e; ++i)
    {
        if (!feq(lhs[i], rhs[i]))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline bool feq(const renderer::DynamicSpectrum<T, N>& lhs, const renderer::DynamicSpectrum<T, N>& rhs, const T eps)
{
    if (lhs.size() != rhs.size())
        return false;

    for (size_t i = 0, e = lhs.size(); i < e; ++i)
    {
        if (!feq(lhs[i], rhs[i], eps))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline bool fz(const renderer::DynamicSpectrum<T, N>& s)
{
    for (size_t i = 0, e = s.size(); i < e; ++i)
    {
        if (!fz(s[i]))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline bool fz(const renderer::DynamicSpectrum<T, N>& s, const T eps)
{
    for (size_t i = 0, e = s.size(); i < e; ++i)
    {
        if (!fz(s[i], eps))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline bool is_saturated(const renderer::DynamicSpectrum<T, N>& s)
{
    for (size_t i = 0, e = s.size(); i < e; ++i)
    {
        if (s[i] < T(0.0) || s[i] > T(1.0))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline renderer::DynamicSpectrum<T, N> saturate(const renderer::DynamicSpectrum<T, N>& s)
{
    renderer::DynamicSpectrum<T, N> result;
    result.resize(s.size());

    for (size_t i = 0, e = s.size(); i < e; ++i)
        result[i] = saturate(s[i]);

    return result;
}

template <typename T, size_t N>
inline renderer::DynamicSpectrum<T, N> clamp(const renderer::DynamicSpectrum<T, N>& s, const T min, const T max)
{
    renderer::DynamicSpectrum<T, N> result;
    result.resize(s.size());

    for (size_t i = 0, e = s.size(); i < e; ++i)
        result[i] = clamp(s[i], min, max);

    return result;
}

template <typename T, size_t N>
inline renderer::DynamicSpectrum<T, N> clamp_low(const renderer::DynamicSpectrum<T, N>& s, const T min)
{
    renderer::DynamicSpectrum<T, N> result;
    result.resize(s.size());

    for (size_t i = 0, e = s.size(); i < e; ++i)
        result[i] = std::max(s[i], min);

    return result;
}

template <typename T, size_t N>
inline renderer::DynamicSpectrum<T, N> clamp_high(const renderer::DynamicSpectrum<T, N>& s, const T max)
{
    renderer::DynamicSpectrum<T, N> result;
    result.resize(s.size());

    for (size_t i = 0, e = s.size(); i < e; ++i)
        result[i] = std::min(s[i], max);

    return result;
}

template <typename T, size_t N>
inline T min_value(const renderer::DynamicSpectrum<T, N>& s)
{
    T value = s[0];

    for (size_t i = 1, e = s.size(); i < e; ++i)
    {
        if (value > s[i])
            value = s[i];
    }

    return value;
}

template <typename T, size_t N>
inline T max_value(const renderer::DynamicSpectrum<T, N>& s)
{
    T value = s[0];

    for (size_t i = 1, e = s.size(); i < e; ++i)
    {
        if (value < s[i])
            value = s[i];
    }

    return value;
}

template <typename T, size_t N>
inline size_t min_index(const renderer::DynamicSpectrum<T, N>& s)
{
    size_t index = 0;
    T value = s[0];

    for (size_t i = 1, e = s.size(); i < e; ++i)
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
inline size_t max_index(const renderer::DynamicSpectrum<T, N>& s)
{
    size_t index = 0;
    T value = s[0];

    for (size_t i = 1, e = s.size(); i < e; ++i)
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
inline size_t min_abs_index(const renderer::DynamicSpectrum<T, N>& s)
{
    size_t index = 0;
    T value = std::abs(s[0]);

    for (size_t i = 1, e = s.size(); i < e; ++i)
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
inline size_t max_abs_index(const renderer::DynamicSpectrum<T, N>& s)
{
    size_t index = 0;
    T value = std::abs(s[0]);

    for (size_t i = 1, e = s.size(); i < e; ++i)
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
inline T sum_value(const renderer::DynamicSpectrum<T, N>& s)
{
    T sum = s[0];

    for (size_t i = 1, e = s.size(); i < e; ++i)
        sum += s[i];

    return sum;
}

template <typename T, size_t N>
inline T average_value(const renderer::DynamicSpectrum<T, N>& s)
{
    return sum_value(s) / s.size();
}

template <typename T, size_t N>
inline bool has_nan(const renderer::DynamicSpectrum<T, N>& s)
{
    for (size_t i = 0, e = s.size(); i < e; ++i)
    {
        if (s[i] != s[i])
            return true;
    }

    return false;
}

}       // namespace foundation

#endif  // !APPLESEED_RENDERER_UTILITY_DYNAMICSPECTRUM_H
