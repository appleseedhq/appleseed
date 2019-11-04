
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/image/regularspectrum.h"
#include "foundation/math/fp.h"
#include "foundation/math/scalar.h"
#include "foundation/platform/compiler.h"
#ifdef APPLESEED_USE_SSE
#include "foundation/platform/sse.h"
#endif
#include "foundation/utility/poison.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>

namespace renderer
{

//
// RGBSpectrum is like renderer::DynamicSpectrum, but RGB-only.
//

template <typename T>
class RGBSpectrum
{
  public:
    // Value type and number of samples.
    typedef T ValueType;
    static const size_t Samples = 3;

    // Number of stored samples such that the size of the sample array is a multiple of 16 bytes.
    static const size_t StoredSamples = (((3 * sizeof(T)) + 15) & ~15) / sizeof(T);

    enum Mode
    {
        RGB = 0,            // RGBSpectrum stores and operates on RGB triplets
    };

    enum Intent
    {
        Reflectance = 0,    // this spectrum represents a reflectance in [0, 1]^N
        Illuminance = 1     // this spectrum represents an illuminance in [0, infinity)^N
    };

    // Change the current thread-local spectrum mode. Return the previous mode.
    static Mode set_mode(const Mode mode);

    // Return the current thread-local spectrum mode.
    static Mode get_mode();

    // Return the number of active color channels for the current spectrum mode.
    static size_t size();

    // Constructors.
#ifdef APPLESEED_USE_SSE
    RGBSpectrum();                                          // leave all components uninitialized
#else
#if !defined(_MSC_VER) || _MSC_VER >= 1800
    RGBSpectrum() = default;                                // leave all components uninitialized
#else
    RGBSpectrum() {}                                        // leave all components uninitialized
#endif
#endif
    explicit RGBSpectrum(const ValueType val);              // set all components to `val`
    RGBSpectrum(
        const foundation::Color<ValueType, 3>&              rgb,
        const foundation::LightingConditions&               lighting_conditions,
        const Intent                                        intent);

    template <size_t N>
    RGBSpectrum(
        const foundation::RegularSpectrum<ValueType, N>&    spectrum,
        const foundation::LightingConditions&               lighting_conditions,
        const Intent                                        intent);

    // Construct a spectrum from another spectrum of a different type.
    template <typename U>
    RGBSpectrum(const RGBSpectrum<U>& rhs);

    // Construct a spectrum from an array of `3` scalars.
    static RGBSpectrum from_array(const ValueType* rhs);

    // Set all components to a given value.
    void set(const ValueType val);

    // Initialize the spectrum from a linear RGB value.
    void set(
        const foundation::Color<ValueType, 3>&              rgb,
        const foundation::LightingConditions&               lighting_conditions,
        const Intent                                        intent);

    // Initialize the spectrum from a spectral value.
    template <size_t N>
    void set(
        const foundation::RegularSpectrum<ValueType, N>&    spectrum,
        const foundation::LightingConditions&               lighting_conditions,
        const Intent                                        intent);

    // Unchecked array subscripting.
    ValueType& operator[](const size_t i);
    const ValueType& operator[](const size_t i) const;

    // Convert the spectrum to a linear RGB color.
    foundation::Color<ValueType, 3> to_rgb(
        const foundation::LightingConditions&   lighting_conditions) const;

    // Convert the spectrum to a CIE XYZ color.
    foundation::Color<ValueType, 3> to_ciexyz(
        const foundation::LightingConditions&   lighting_conditions) const;

  private:
    APPLESEED_SIMD4_ALIGN ValueType m_samples[StoredSamples];
};

// Exact inequality and equality tests.
template <typename T> bool operator!=(const RGBSpectrum<T>& lhs, const RGBSpectrum<T>& rhs);
template <typename T> bool operator==(const RGBSpectrum<T>& lhs, const RGBSpectrum<T>& rhs);

// Spectrum arithmetic.
template <typename T> RGBSpectrum<T>  operator+ (const RGBSpectrum<T>& lhs, const RGBSpectrum<T>& rhs);
template <typename T> RGBSpectrum<T>  operator- (const RGBSpectrum<T>& lhs, const RGBSpectrum<T>& rhs);
template <typename T> RGBSpectrum<T>  operator- (const RGBSpectrum<T>& lhs);
template <typename T> RGBSpectrum<T>  operator* (const RGBSpectrum<T>& lhs, const T rhs);
template <typename T> RGBSpectrum<T>  operator* (const T lhs, const RGBSpectrum<T>& rhs);
template <typename T> RGBSpectrum<T>  operator* (const RGBSpectrum<T>& lhs, const RGBSpectrum<T>& rhs);
template <typename T> RGBSpectrum<T>  operator/ (const RGBSpectrum<T>& lhs, const T rhs);
template <typename T> RGBSpectrum<T>  operator/ (const RGBSpectrum<T>& lhs, const RGBSpectrum<T>& rhs);
template <typename T> RGBSpectrum<T>& operator+=(RGBSpectrum<T>& lhs, const RGBSpectrum<T>& rhs);
template <typename T> RGBSpectrum<T>& operator-=(RGBSpectrum<T>& lhs, const RGBSpectrum<T>& rhs);
template <typename T> RGBSpectrum<T>& operator*=(RGBSpectrum<T>& lhs, const T rhs);
template <typename T> RGBSpectrum<T>& operator*=(RGBSpectrum<T>& lhs, const RGBSpectrum<T>& rhs);
template <typename T> RGBSpectrum<T>& operator/=(RGBSpectrum<T>& lhs, const T rhs);
template <typename T> RGBSpectrum<T>& operator/=(RGBSpectrum<T>& lhs, const RGBSpectrum<T>& rhs);

// Multiply-add: a = a + b * c.
template <typename T> void madd(RGBSpectrum<T>& a, const RGBSpectrum<T>& b, const RGBSpectrum<T>& c);
template <typename T> void madd(RGBSpectrum<T>& a, const RGBSpectrum<T>& b, const T c);


//
// Full specializations for spectra of type float and double.
//

typedef RGBSpectrum<float> RGBSpectrumf;
typedef RGBSpectrum<double> RGBSpectrumd;

}   // namespace renderer

namespace foundation
{

// Return whether all components of a spectrum are exactly zero.
template <typename T> bool is_zero(const renderer::RGBSpectrum<T>& s);

// Approximate equality tests.
template <typename T> bool feq(const renderer::RGBSpectrum<T>& lhs, const renderer::RGBSpectrum<T>& rhs);
template <typename T> bool feq(const renderer::RGBSpectrum<T>& lhs, const renderer::RGBSpectrum<T>& rhs, const T eps);

// Approximate zero tests.
template <typename T> bool fz(const renderer::RGBSpectrum<T>& s);
template <typename T> bool fz(const renderer::RGBSpectrum<T>& s, const T eps);

// Component-wise reciprocal.
template <typename T> renderer::RGBSpectrum<T> rcp(const renderer::RGBSpectrum<T>& s);

// Component-wise square root.
template <typename T> renderer::RGBSpectrum<T> sqrt(const renderer::RGBSpectrum<T>& s);

// Component-wise power.
template <typename T> renderer::RGBSpectrum<T> pow(const renderer::RGBSpectrum<T>& x, const T y);

// Component-wise power.
template <typename T> renderer::RGBSpectrum<T> pow(
    const renderer::RGBSpectrum<T>& x,
    const renderer::RGBSpectrum<T>& y);

// Component-wise logarithm.
template <typename T> renderer::RGBSpectrum<T> log(const renderer::RGBSpectrum<T>& s);

// Component-wise exponent.
template <typename T> renderer::RGBSpectrum<T> exp(const renderer::RGBSpectrum<T>& s);

// Return whether all components of a spectrum are in [0,1].
template <typename T> bool is_saturated(const renderer::RGBSpectrum<T>& s);

// Clamp the argument to [0,1].
template <typename T> renderer::RGBSpectrum<T> saturate(const renderer::RGBSpectrum<T>& s);
template <typename T> void saturate_in_place(renderer::RGBSpectrum<T>& s);

// Clamp the argument to [min, max].
template <typename T> renderer::RGBSpectrum<T> clamp(const renderer::RGBSpectrum<T>& s, const T min, const T max);
template <typename T> void clamp_in_place(renderer::RGBSpectrum<T>& s, const T min, const T max);

// Clamp the argument to [min, +infinity).
template <typename T> renderer::RGBSpectrum<T> clamp_low(const renderer::RGBSpectrum<T>& s, const T min);
template <typename T> void clamp_low_in_place(renderer::RGBSpectrum<T>& s, const T min);

// Clamp the argument to (-infinity, max].
template <typename T> renderer::RGBSpectrum<T> clamp_high(const renderer::RGBSpectrum<T>& s, const T max);
template <typename T> void clamp_high_in_place(renderer::RGBSpectrum<T>& s, const T max);

// Component-wise linear interpolation between a and b.
template <typename T> renderer::RGBSpectrum<T> lerp(
    const renderer::RGBSpectrum<T>& a,
    const renderer::RGBSpectrum<T>& b,
    const renderer::RGBSpectrum<T>& t);

// Return the smallest or largest signed component of a spectrum.
template <typename T> T min_value(const renderer::RGBSpectrum<T>& s);
template <typename T> T max_value(const renderer::RGBSpectrum<T>& s);

// Return the index of the smallest or largest signed component of a spectrum.
template <typename T> size_t min_index(const renderer::RGBSpectrum<T>& s);
template <typename T> size_t max_index(const renderer::RGBSpectrum<T>& s);

// Return the index of the smallest or largest component of a spectrum, in absolute value.
template <typename T> size_t min_abs_index(const renderer::RGBSpectrum<T>& s);
template <typename T> size_t max_abs_index(const renderer::RGBSpectrum<T>& s);

// Return the sum of the components of a spectrum.
template <typename T> T sum_value(const renderer::RGBSpectrum<T>& s);

// Return the average value of a spectrum.
template <typename T> T average_value(const renderer::RGBSpectrum<T>& s);

// Return true if a spectrum contains at least one NaN value.
template <typename T> bool has_nan(const renderer::RGBSpectrum<T>& s);

// Return true if all components of a spectrum are finite (not NaN, not infinite).
template <typename T> bool is_finite(const renderer::RGBSpectrum<T>& s);

// Return true if all components of a spectrum are finite (not NaN, not infinite) and non-negative.
template <typename T> bool is_finite_non_neg(const renderer::RGBSpectrum<T>& s);

}   // namespace foundation


//
// RGBSpectrum class implementation.
//

namespace renderer
{

template <typename T>
typename RGBSpectrum<T>::Mode RGBSpectrum<T>::set_mode(const Mode mode)
{
    return RGB;
}

template <typename T>
inline typename RGBSpectrum<T>::Mode RGBSpectrum<T>::get_mode()
{
    return RGB;
}

template <typename T>
inline size_t RGBSpectrum<T>::size()
{
    return 3;
}

#ifdef APPLESEED_USE_SSE

template <typename T>
inline RGBSpectrum<T>::RGBSpectrum()
{
    m_samples[3] = T(0.0);
}

#endif

template <typename T>
inline RGBSpectrum<T>::RGBSpectrum(const ValueType val)
{
    set(val);

#ifdef APPLESEED_USE_SSE
    m_samples[3] = T(0.0);
#endif
}

template <typename T>
inline RGBSpectrum<T>::RGBSpectrum(
    const foundation::Color<ValueType, 3>&              rgb,
    const foundation::LightingConditions&               lighting_conditions,
    const Intent                                        intent)
{
    set(rgb, lighting_conditions, intent);

#ifdef APPLESEED_USE_SSE
    m_samples[3] = T(0.0);
#endif
}

template <typename T>
template <size_t N>
inline RGBSpectrum<T>::RGBSpectrum(
    const foundation::RegularSpectrum<ValueType, N>&    spectrum,
    const foundation::LightingConditions&               lighting_conditions,
    const Intent                                        intent)
{
    set(spectrum, lighting_conditions, intent);

#ifdef APPLESEED_USE_SSE
    m_samples[3] = T(0.0);
#endif
}

template <typename T>
template <typename U>
inline RGBSpectrum<T>::RGBSpectrum(const RGBSpectrum<U>& rhs)
{
    for (size_t i = 0; i < 3; ++i)
        m_samples[i] = static_cast<ValueType>(rhs[i]);

#ifdef APPLESEED_USE_SSE
    m_samples[3] = T(0.0);
#endif
}

template <typename T>
inline RGBSpectrum<T> RGBSpectrum<T>::from_array(const ValueType* rhs)
{
    assert(rhs);

    RGBSpectrum result;

    for (size_t i = 0; i < 3; ++i)
        result.m_samples[i] = rhs[i];

    return result;
}

template <typename T>
inline void RGBSpectrum<T>::set(const ValueType val)
{
    for (size_t i = 0; i < 3; ++i)
        m_samples[i] = val;
}

template <typename T>
void RGBSpectrum<T>::set(
    const foundation::Color<ValueType, 3>&              rgb,
    const foundation::LightingConditions&               lighting_conditions,
    const Intent                                        intent)
{
    m_samples[0] = rgb[0];
    m_samples[1] = rgb[1];
    m_samples[2] = rgb[2];
}

template <typename T>
template <size_t N>
void RGBSpectrum<T>::set(
    const foundation::RegularSpectrum<ValueType, N>&    spectrum,
    const foundation::LightingConditions&               lighting_conditions,
    const Intent                                        intent)
{
    reinterpret_cast<foundation::Color<T, 3>&>(m_samples[0]) =
        foundation::ciexyz_to_linear_rgb(
            foundation::spectrum_to_ciexyz<T>(lighting_conditions, spectrum));
}

template <typename T>
inline T& RGBSpectrum<T>::operator[](const size_t i)
{
    assert(i < 3);
    return m_samples[i];
}

template <typename T>
inline const T& RGBSpectrum<T>::operator[](const size_t i) const
{
    assert(i < 3);
    return m_samples[i];
}

template <typename T>
inline foundation::Color<T, 3> RGBSpectrum<T>::to_rgb(
    const foundation::LightingConditions& lighting_conditions) const
{
    return foundation::Color<T, 3>(m_samples[0], m_samples[1], m_samples[2]);
}

template <typename T>
inline foundation::Color<T, 3> RGBSpectrum<T>::to_ciexyz(
    const foundation::LightingConditions& lighting_conditions) const
{
    return
        linear_rgb_to_ciexyz(
            foundation::Color<T, 3>(m_samples[0], m_samples[1], m_samples[2]));
}

template <typename T>
inline bool operator!=(const RGBSpectrum<T>& lhs, const RGBSpectrum<T>& rhs)
{
    for (size_t i = 0; i < RGBSpectrum<T>::size(); ++i)
    {
        if (lhs[i] != rhs[i])
            return true;
    }

    return false;
}

template <typename T>
inline bool operator==(const RGBSpectrum<T>& lhs, const RGBSpectrum<T>& rhs)
{
    return !(lhs != rhs);
}

template <typename T>
inline RGBSpectrum<T> operator+(const RGBSpectrum<T>& lhs, const RGBSpectrum<T>& rhs)
{
    RGBSpectrum<T> result;

    for (size_t i = 0; i < RGBSpectrum<T>::size(); ++i)
        result[i] = lhs[i] + rhs[i];

    return result;
}

template <typename T>
inline RGBSpectrum<T> operator-(const RGBSpectrum<T>& lhs, const RGBSpectrum<T>& rhs)
{
    RGBSpectrum<T> result;

    for (size_t i = 0; i < RGBSpectrum<T>::size(); ++i)
        result[i] = lhs[i] - rhs[i];

    return result;
}

template <typename T>
inline RGBSpectrum<T> operator-(const RGBSpectrum<T>& lhs)
{
    RGBSpectrum<T> result;

    for (size_t i = 0; i < RGBSpectrum<T>::size(); ++i)
        result[i] = -lhs[i];

    return result;
}

template <typename T>
inline RGBSpectrum<T> operator*(const RGBSpectrum<T>& lhs, const T rhs)
{
    RGBSpectrum<T> result;

    for (size_t i = 0; i < RGBSpectrum<T>::size(); ++i)
        result[i] = lhs[i] * rhs;

    return result;
}

template <typename T>
inline RGBSpectrum<T> operator*(const T lhs, const RGBSpectrum<T>& rhs)
{
    return rhs * lhs;
}

template <typename T>
inline RGBSpectrum<T> operator*(const RGBSpectrum<T>& lhs, const RGBSpectrum<T>& rhs)
{
    RGBSpectrum<T> result;

    for (size_t i = 0; i < RGBSpectrum<T>::size(); ++i)
        result[i] = lhs[i] * rhs[i];

    return result;
}

template <typename T>
inline RGBSpectrum<T> operator/(const RGBSpectrum<T>& lhs, const T rhs)
{
    RGBSpectrum<T> result;

    for (size_t i = 0; i < RGBSpectrum<T>::size(); ++i)
        result[i] = lhs[i] / rhs;

    return result;
}

template <>
inline RGBSpectrum<float> operator/(const RGBSpectrum<float>& lhs, const float rhs)
{
    return lhs * (1.0f / rhs);
}

template <size_t N>
inline RGBSpectrum<double> operator/(const RGBSpectrum<double>& lhs, const double rhs)
{
    return lhs * (1.0 / rhs);
}

template <size_t N>
inline RGBSpectrum<long double> operator/(const RGBSpectrum<long double>& lhs, const long double rhs)
{
    return lhs * (1.0L / rhs);
}

template <typename T>
inline RGBSpectrum<T> operator/(const RGBSpectrum<T>& lhs, const RGBSpectrum<T>& rhs)
{
    RGBSpectrum<T> result;

    for (size_t i = 0; i < RGBSpectrum<T>::size(); ++i)
        result[i] = lhs[i] / rhs[i];

    return result;
}

template <typename T>
inline RGBSpectrum<T>& operator+=(RGBSpectrum<T>& lhs, const RGBSpectrum<T>& rhs)
{
    for (size_t i = 0; i < RGBSpectrum<T>::size(); ++i)
        lhs[i] += rhs[i];

    return lhs;
}

#ifdef APPLESEED_USE_SSE

template <>
APPLESEED_FORCE_INLINE RGBSpectrum<float>& operator+=(RGBSpectrum<float>& lhs, const RGBSpectrum<float>& rhs)
{
    _mm_store_ps(&lhs[ 0], _mm_add_ps(_mm_load_ps(&lhs[ 0]), _mm_load_ps(&rhs[ 0])));

    if (RGBSpectrum<float>::size() > 3)
    {
        _mm_store_ps(&lhs[ 4], _mm_add_ps(_mm_load_ps(&lhs[ 4]), _mm_load_ps(&rhs[ 4])));
        _mm_store_ps(&lhs[ 8], _mm_add_ps(_mm_load_ps(&lhs[ 8]), _mm_load_ps(&rhs[ 8])));
        _mm_store_ps(&lhs[12], _mm_add_ps(_mm_load_ps(&lhs[12]), _mm_load_ps(&rhs[12])));
        _mm_store_ps(&lhs[16], _mm_add_ps(_mm_load_ps(&lhs[16]), _mm_load_ps(&rhs[16])));
        _mm_store_ps(&lhs[20], _mm_add_ps(_mm_load_ps(&lhs[20]), _mm_load_ps(&rhs[20])));
        _mm_store_ps(&lhs[24], _mm_add_ps(_mm_load_ps(&lhs[24]), _mm_load_ps(&rhs[24])));
        _mm_store_ps(&lhs[28], _mm_add_ps(_mm_load_ps(&lhs[28]), _mm_load_ps(&rhs[28])));
    }

    return lhs;
}

#endif  // APPLESEED_USE_SSE

template <typename T>
inline RGBSpectrum<T>& operator-=(RGBSpectrum<T>& lhs, const RGBSpectrum<T>& rhs)
{
    for (size_t i = 0; i < RGBSpectrum<T>::size(); ++i)
        lhs[i] -= rhs[i];

    return lhs;
}

template <typename T>
inline RGBSpectrum<T>& operator*=(RGBSpectrum<T>& lhs, const T rhs)
{
    for (size_t i = 0; i < RGBSpectrum<T>::size(); ++i)
        lhs[i] *= rhs;

    return lhs;
}

#ifdef APPLESEED_USE_SSE

template <>
APPLESEED_FORCE_INLINE RGBSpectrum<float>& operator*=(RGBSpectrum<float>& lhs, const float rhs)
{
    const __m128 mrhs = _mm_set1_ps(rhs);

    _mm_store_ps(&lhs[ 0], _mm_mul_ps(_mm_load_ps(&lhs[ 0]), mrhs));

    if (RGBSpectrum<float>::size() > 3)
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

template <typename T>
inline RGBSpectrum<T>& operator*=(RGBSpectrum<T>& lhs, const RGBSpectrum<T>& rhs)
{
    for (size_t i = 0; i < RGBSpectrum<T>::size(); ++i)
        lhs[i] *= rhs[i];

    return lhs;
}

#ifdef APPLESEED_USE_SSE

template <>
APPLESEED_FORCE_INLINE RGBSpectrum<float>& operator*=(RGBSpectrum<float>& lhs, const RGBSpectrum<float>& rhs)
{
    _mm_store_ps(&lhs[ 0], _mm_mul_ps(_mm_load_ps(&lhs[ 0]), _mm_load_ps(&rhs[ 0])));

    if (RGBSpectrum<float>::size() > 3)
    {
        _mm_store_ps(&lhs[ 4], _mm_mul_ps(_mm_load_ps(&lhs[ 4]), _mm_load_ps(&rhs[ 4])));
        _mm_store_ps(&lhs[ 8], _mm_mul_ps(_mm_load_ps(&lhs[ 8]), _mm_load_ps(&rhs[ 8])));
        _mm_store_ps(&lhs[12], _mm_mul_ps(_mm_load_ps(&lhs[12]), _mm_load_ps(&rhs[12])));
        _mm_store_ps(&lhs[16], _mm_mul_ps(_mm_load_ps(&lhs[16]), _mm_load_ps(&rhs[16])));
        _mm_store_ps(&lhs[20], _mm_mul_ps(_mm_load_ps(&lhs[20]), _mm_load_ps(&rhs[20])));
        _mm_store_ps(&lhs[24], _mm_mul_ps(_mm_load_ps(&lhs[24]), _mm_load_ps(&rhs[24])));
        _mm_store_ps(&lhs[28], _mm_mul_ps(_mm_load_ps(&lhs[28]), _mm_load_ps(&rhs[28])));
    }

    return lhs;
}

#endif  // APPLESEED_USE_SSE

template <typename T>
inline RGBSpectrum<T>& operator/=(RGBSpectrum<T>& lhs, const T rhs)
{
    for (size_t i = 0; i < RGBSpectrum<T>::size(); ++i)
        lhs[i] /= rhs;

    return lhs;
}

inline RGBSpectrum<float>& operator/=(RGBSpectrum<float>& lhs, const float rhs)
{
    return lhs *= 1.0f / rhs;
}

inline RGBSpectrum<double>& operator/=(RGBSpectrum<double>& lhs, const double rhs)
{
    return lhs *= 1.0 / rhs;
}

inline RGBSpectrum<long double>& operator/=(RGBSpectrum<long double>& lhs, const long double rhs)
{
    return lhs *= 1.0L / rhs;
}

template <typename T>
inline RGBSpectrum<T>& operator/=(RGBSpectrum<T>& lhs, const RGBSpectrum<T>& rhs)
{
    for (size_t i = 0; i < RGBSpectrum<T>::size(); ++i)
        lhs[i] /= rhs[i];

    return lhs;
}

template <typename T>
inline void madd(
    RGBSpectrum<T>&                  a,
    const RGBSpectrum<T>&            b,
    const RGBSpectrum<T>&            c)
{
    for (size_t i = 0; i < RGBSpectrum<T>::size(); ++i)
        a[i] += b[i] * c[i];
}

template <typename T>
inline void madd(
    RGBSpectrum<T>&                  a,
    const RGBSpectrum<T>&            b,
    const T                          c)
{
    for (size_t i = 0; i < RGBSpectrum<T>::size(); ++i)
        a[i] += b[i] * c;
}

#ifdef APPLESEED_USE_SSE

template <>
APPLESEED_FORCE_INLINE void madd(
    RGBSpectrum<float>&             a,
    const RGBSpectrum<float>&       b,
    const RGBSpectrum<float>&       c)
{
    _mm_store_ps(&a[0], _mm_add_ps(_mm_load_ps(&a[0]), _mm_mul_ps(_mm_load_ps(&b[0]), _mm_load_ps(&c[0]))));
}

template <>
APPLESEED_FORCE_INLINE void madd(
    RGBSpectrum<float>&             a,
    const RGBSpectrum<float>&       b,
    const float                             c)
{
    const __m128 k = _mm_set_ps1(c);
    _mm_store_ps(&a[0], _mm_add_ps(_mm_load_ps(&a[0]), _mm_mul_ps(_mm_load_ps(&b[0]), k)));
}

#endif  // APPLESEED_USE_SSE

}       // namespace renderer

namespace foundation
{

template <typename T>
inline bool is_zero(const renderer::RGBSpectrum<T>& s)
{
    for (size_t i = 0, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
    {
        if (s[i] != T(0.0))
            return false;
    }

    return true;
}

template <typename T>
inline bool feq(const renderer::RGBSpectrum<T>& lhs, const renderer::RGBSpectrum<T>& rhs)
{
    for (size_t i = 0, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
    {
        if (!feq(lhs[i], rhs[i]))
            return false;
    }

    return true;
}

template <typename T>
inline bool feq(const renderer::RGBSpectrum<T>& lhs, const renderer::RGBSpectrum<T>& rhs, const T eps)
{
    for (size_t i = 0, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
    {
        if (!feq(lhs[i], rhs[i], eps))
            return false;
    }

    return true;
}

template <typename T>
inline bool fz(const renderer::RGBSpectrum<T>& s)
{
    for (size_t i = 0, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
    {
        if (!fz(s[i]))
            return false;
    }

    return true;
}

template <typename T>
inline bool fz(const renderer::RGBSpectrum<T>& s, const T eps)
{
    for (size_t i = 0, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
    {
        if (!fz(s[i], eps))
            return false;
    }

    return true;
}

template <typename T>
inline renderer::RGBSpectrum<T> rcp(const renderer::RGBSpectrum<T>& s)
{
    renderer::RGBSpectrum<T> result;

    for (size_t i = 0, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
        result[i] = T(1.0) / s[i];

    return result;
}

template <typename T>
inline renderer::RGBSpectrum<T> sqrt(const renderer::RGBSpectrum<T>& s)
{
    renderer::RGBSpectrum<T> result;

    for (size_t i = 0, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
        result[i] = std::sqrt(s[i]);

    return result;
}

#ifdef APPLESEED_USE_SSE

APPLESEED_FORCE_INLINE renderer::RGBSpectrum<float> sqrt(const renderer::RGBSpectrum<float>& s)
{
    renderer::RGBSpectrum<float> result;
    _mm_store_ps(&result[ 0], _mm_sqrt_ps(_mm_load_ps(&s[ 0])));
    return result;
}

#endif  // APPLESEED_USE_SSE

template <typename T>
inline renderer::RGBSpectrum<T> pow(const renderer::RGBSpectrum<T>& x, const T y)
{
    renderer::RGBSpectrum<T> result;

    for (size_t i = 0, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
        result[i] = std::pow(x[i], y);

    return result;
}

template <typename T>
inline renderer::RGBSpectrum<T> pow(
    const renderer::RGBSpectrum<T>& x,
    const renderer::RGBSpectrum<T>& y)
{
    renderer::RGBSpectrum<T> result;

    for (size_t i = 0, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
        result[i] = std::pow(x[i], y[i]);

    return result;
}

template <typename T>
inline renderer::RGBSpectrum<T> log(const renderer::RGBSpectrum<T>& s)
{
    renderer::RGBSpectrum<T> result;

    for (size_t i = 0, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
        result[i] = std::log(s[i]);

    return result;
}

template <typename T>
inline renderer::RGBSpectrum<T> exp(const renderer::RGBSpectrum<T>& s)
{
    renderer::RGBSpectrum<T> result;

    for (size_t i = 0, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
        result[i] = std::exp(s[i]);

    return result;
}


template <typename T>
inline bool is_saturated(const renderer::RGBSpectrum<T>& s)
{
    for (size_t i = 0, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
    {
        if (s[i] < T(0.0) || s[i] > T(1.0))
            return false;
    }

    return true;
}

template <typename T>
inline renderer::RGBSpectrum<T> saturate(const renderer::RGBSpectrum<T>& s)
{
    renderer::RGBSpectrum<T> result;

    for (size_t i = 0, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
        result[i] = saturate(s[i]);

    return result;
}

template <typename T>
inline void saturate_in_place(renderer::RGBSpectrum<T>& s)
{
    clamp_in_place(s, T(0.0), T(1.0));
}

template <typename T>
inline renderer::RGBSpectrum<T> clamp(const renderer::RGBSpectrum<T>& s, const T min, const T max)
{
    renderer::RGBSpectrum<T> result;

    for (size_t i = 0, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
        result[i] = clamp(s[i], min, max);

    return result;
}

template <typename T>
inline void clamp_in_place(renderer::RGBSpectrum<T>& s, const T min, const T max)
{
    for (size_t i = 0, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
    {
        if (s[i] < min)
            s[i] = min;

        if (s[i] > max)
            s[i] = max;
    }
}

template <typename T>
inline renderer::RGBSpectrum<T> clamp_low(const renderer::RGBSpectrum<T>& s, const T min)
{
    renderer::RGBSpectrum<T> result;

    for (size_t i = 0, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
        result[i] = std::max(s[i], min);

    return result;
}

template <typename T>
inline void clamp_low_in_place(renderer::RGBSpectrum<T>& s, const T min)
{
    for (size_t i = 0, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
    {
        if (s[i] < min)
            s[i] = min;
    }
}

template <typename T>
inline renderer::RGBSpectrum<T> clamp_high(const renderer::RGBSpectrum<T>& s, const T max)
{
    renderer::RGBSpectrum<T> result;

    for (size_t i = 0, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
        result[i] = std::min(s[i], max);

    return result;
}

template <typename T>
inline void clamp_high_in_place(renderer::RGBSpectrum<T>& s, const T max)
{
    for (size_t i = 0, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
    {
        if (s[i] > max)
            s[i] = max;
    }
}

template <typename T>
inline renderer::RGBSpectrum<T> lerp(
    const renderer::RGBSpectrum<T>& a,
    const renderer::RGBSpectrum<T>& b,
    const renderer::RGBSpectrum<T>& t)
{
    renderer::RGBSpectrum<T> result;

    for (size_t i = 0, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
        result[i] = foundation::lerp(a[i], b[i], t[i]);

    return result;
}

#ifdef APPLESEED_USE_SSE

template <>
APPLESEED_FORCE_INLINE renderer::RGBSpectrum<float> lerp(
    const renderer::RGBSpectrum<float>& a,
    const renderer::RGBSpectrum<float>& b,
    const renderer::RGBSpectrum<float>& t)
{
    renderer::RGBSpectrum<float> result;

    __m128 one4 = _mm_set1_ps(1.0f);
    __m128 t4 = _mm_load_ps(&t[0]);
    __m128 one_minus_t4 = _mm_sub_ps(one4, t4);
    __m128 x = _mm_mul_ps(_mm_load_ps(&a[0]), one_minus_t4);
    __m128 y = _mm_mul_ps(_mm_load_ps(&b[0]), t4);
    _mm_store_ps(&result[0], _mm_add_ps(x, y));

    return result;
}

#endif  // APPLESEED_USE_SSE

template <typename T>
inline T min_value(const renderer::RGBSpectrum<T>& s)
{
    return std::min(std::min(s[0], s[1]), s[2]);
}

template <typename T>
inline T max_value(const renderer::RGBSpectrum<T>& s)
{
    return std::max(std::max(s[0], s[1]), s[2]);
}

template <typename T>
inline size_t min_index(const renderer::RGBSpectrum<T>& s)
{
    size_t index = 0;
    T value = s[0];

    for (size_t i = 1, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
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

template <typename T>
inline size_t max_index(const renderer::RGBSpectrum<T>& s)
{
    size_t index = 0;
    T value = s[0];

    for (size_t i = 1, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
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

template <typename T>
inline size_t min_abs_index(const renderer::RGBSpectrum<T>& s)
{
    size_t index = 0;
    T value = std::abs(s[0]);

    for (size_t i = 1, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
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

template <typename T>
inline size_t max_abs_index(const renderer::RGBSpectrum<T>& s)
{
    size_t index = 0;
    T value = std::abs(s[0]);

    for (size_t i = 1, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
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

template <typename T>
inline T sum_value(const renderer::RGBSpectrum<T>& s)
{
    T sum = s[0];

    for (size_t i = 1, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
        sum += s[i];

    return sum;
}

template <typename T>
inline T average_value(const renderer::RGBSpectrum<T>& s)
{
    return sum_value(s) / renderer::RGBSpectrum<T>::size();
}

template <typename T>
inline bool has_nan(const renderer::RGBSpectrum<T>& s)
{
    for (size_t i = 0, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
    {
        if (s[i] != s[i])
            return true;
    }

    return false;
}

template <typename T>
inline bool is_finite(const renderer::RGBSpectrum<T>& s)
{
    for (size_t i = 0, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
    {
        if (!FP<T>::is_finite(s[i]))
            return false;
    }

    return true;
}

template <typename T>
inline bool is_finite_non_neg(const renderer::RGBSpectrum<T>& s)
{
    for (size_t i = 0, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
    {
        if (!FP<T>::is_finite_non_neg(s[i]))
            return false;
    }

    return true;
}

template <typename T>
class PoisonImpl<renderer::RGBSpectrum<T>>
{
  public:
    static void do_poison(renderer::RGBSpectrum<T>& s)
    {
        for (size_t i = 0, e = renderer::RGBSpectrum<T>::size(); i < e; ++i)
            debug_poison(s[i]);
    }
};

}   // namespace foundation
