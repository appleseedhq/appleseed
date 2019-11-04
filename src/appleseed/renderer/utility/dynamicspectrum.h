
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
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
// Internal working spectrum type, either RGB or spectral depending on the thread-local spectrum mode.
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

    enum Mode
    {
        RGB = 0,            // DynamicSpectrum stores and operates on RGB triplets
        Spectral = 1        // DynamicSpectrum stores and operates on spectra
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
    DynamicSpectrum();                                      // leave all components uninitialized
#else
#if !defined(_MSC_VER) || _MSC_VER >= 1800
    DynamicSpectrum() = default;                            // leave all components uninitialized
#else
    DynamicSpectrum() {}                                    // leave all components uninitialized
#endif
#endif
    explicit DynamicSpectrum(const ValueType val);          // set all components to `val`
    DynamicSpectrum(
        const foundation::Color<ValueType, 3>&              rgb,
        const foundation::LightingConditions&               lighting_conditions,
        const Intent                                        intent);
    DynamicSpectrum(
        const foundation::RegularSpectrum<ValueType, N>&    spectrum,
        const foundation::LightingConditions&               lighting_conditions,
        const Intent                                        intent);

    // Construct a spectrum from another spectrum of a different type.
    template <typename U>
    DynamicSpectrum(const DynamicSpectrum<U, N>& rhs);

    // Construct a spectrum from an array of `s_size` scalars.
    static DynamicSpectrum from_array(const ValueType* rhs);

    // Set all components to a given value.
    void set(const ValueType val);

    // Initialize the spectrum from a linear RGB value.
    void set(
        const foundation::Color<ValueType, 3>&              rgb,
        const foundation::LightingConditions&               lighting_conditions,
        const Intent                                        intent);

    // Initialize the spectrum from a spectral value.
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
    static APPLESEED_TLS Mode       s_mode;
    static APPLESEED_TLS size_t     s_size;

    APPLESEED_SIMD4_ALIGN ValueType m_samples[StoredSamples];
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

// Multiply-add: a = a + b * c.
template <typename T, size_t N> void madd(DynamicSpectrum<T, N>& a, const DynamicSpectrum<T, N>& b, const DynamicSpectrum<T, N>& c);
template <typename T, size_t N> void madd(DynamicSpectrum<T, N>& a, const DynamicSpectrum<T, N>& b, const T c);


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

// Component-wise reciprocal.
template <typename T, size_t N> renderer::DynamicSpectrum<T, N> rcp(const renderer::DynamicSpectrum<T, N>& s);

// Component-wise square root.
template <typename T, size_t N> renderer::DynamicSpectrum<T, N> sqrt(const renderer::DynamicSpectrum<T, N>& s);

// Component-wise power.
template <typename T, size_t N> renderer::DynamicSpectrum<T, N> pow(const renderer::DynamicSpectrum<T, N>& x, const T y);

// Component-wise power.
template <typename T, size_t N> renderer::DynamicSpectrum<T, N> pow(
    const renderer::DynamicSpectrum<T, N>& x,
    const renderer::DynamicSpectrum<T, N>& y);

// Component-wise logarithm.
template <typename T, size_t N> renderer::DynamicSpectrum<T, N> log(const renderer::DynamicSpectrum<T, N>& s);

// Component-wise exponent.
template <typename T, size_t N> renderer::DynamicSpectrum<T, N> exp(const renderer::DynamicSpectrum<T, N>& s);

// Return whether all components of a spectrum are in [0,1].
template <typename T, size_t N> bool is_saturated(const renderer::DynamicSpectrum<T, N>& s);

// Clamp the argument to [0,1].
template <typename T, size_t N> renderer::DynamicSpectrum<T, N> saturate(const renderer::DynamicSpectrum<T, N>& s);
template <typename T, size_t N> void saturate_in_place(renderer::DynamicSpectrum<T, N>& s);

// Clamp the argument to [min, max].
template <typename T, size_t N> renderer::DynamicSpectrum<T, N> clamp(const renderer::DynamicSpectrum<T, N>& s, const T min, const T max);
template <typename T, size_t N> void clamp_in_place(renderer::DynamicSpectrum<T, N>& s, const T min, const T max);

// Clamp the argument to [min, +infinity).
template <typename T, size_t N> renderer::DynamicSpectrum<T, N> clamp_low(const renderer::DynamicSpectrum<T, N>& s, const T min);
template <typename T, size_t N> void clamp_low_in_place(renderer::DynamicSpectrum<T, N>& s, const T min);

// Clamp the argument to (-infinity, max].
template <typename T, size_t N> renderer::DynamicSpectrum<T, N> clamp_high(const renderer::DynamicSpectrum<T, N>& s, const T max);
template <typename T, size_t N> void clamp_high_in_place(renderer::DynamicSpectrum<T, N>& s, const T max);

// Component-wise linear interpolation between a and b.
template <typename T, size_t N> renderer::DynamicSpectrum<T, N> lerp(
    const renderer::DynamicSpectrum<T, N>& a,
    const renderer::DynamicSpectrum<T, N>& b,
    const renderer::DynamicSpectrum<T, N>& t);

// Return the smallest or largest signed component of a spectrum.
template <typename T, size_t N> T min_value(const renderer::DynamicSpectrum<T, N>& s);
template <typename T, size_t N> T max_value(const renderer::DynamicSpectrum<T, N>& s);

// Return the index of the smallest or largest signed component of a spectrum.
template <typename T, size_t N> size_t min_index(const renderer::DynamicSpectrum<T, N>& s);
template <typename T, size_t N> size_t max_index(const renderer::DynamicSpectrum<T, N>& s);

// Return the index of the smallest or largest component of a spectrum, in absolute value.
template <typename T, size_t N> size_t min_abs_index(const renderer::DynamicSpectrum<T, N>& s);
template <typename T, size_t N> size_t max_abs_index(const renderer::DynamicSpectrum<T, N>& s);

// Return the sum of the components of a spectrum.
template <typename T, size_t N> T sum_value(const renderer::DynamicSpectrum<T, N>& s);

// Return the average value of a spectrum.
template <typename T, size_t N> T average_value(const renderer::DynamicSpectrum<T, N>& s);

// Return true if a spectrum contains at least one NaN value.
template <typename T, size_t N> bool has_nan(const renderer::DynamicSpectrum<T, N>& s);

// Return true if all components of a spectrum are finite (not NaN, not infinite).
template <typename T, size_t N> bool is_finite(const renderer::DynamicSpectrum<T, N>& s);

// Return true if all components of a spectrum are finite (not NaN, not infinite) and non-negative.
template <typename T, size_t N> bool is_finite_non_neg(const renderer::DynamicSpectrum<T, N>& s);

}   // namespace foundation


//
// DynamicSpectrum class implementation.
//

namespace renderer
{

// Full specialization is required in the value for Apple LLVM version 7.0.0 (clang-700.1.76).
template <typename T, size_t N>
APPLESEED_TLS typename DynamicSpectrum<T, N>::Mode DynamicSpectrum<T, N>::s_mode = DynamicSpectrum<float, 31>::RGB;

template <typename T, size_t N>
APPLESEED_TLS size_t DynamicSpectrum<T, N>::s_size = 3;

template <typename T, size_t N>
typename DynamicSpectrum<T, N>::Mode DynamicSpectrum<T, N>::set_mode(const Mode mode)
{
    const Mode old_mode = s_mode;

    s_mode = mode;
    s_size = mode == RGB ? 3 : N;

    return old_mode;
}

template <typename T, size_t N>
inline typename DynamicSpectrum<T, N>::Mode DynamicSpectrum<T, N>::get_mode()
{
    return s_mode;
}

template <typename T, size_t N>
inline size_t DynamicSpectrum<T, N>::size()
{
    return s_size;
}

#ifdef APPLESEED_USE_SSE

template <typename T, size_t N>
inline DynamicSpectrum<T, N>::DynamicSpectrum()
{
    m_samples[s_size] = T(0.0);
}

#endif

template <typename T, size_t N>
inline DynamicSpectrum<T, N>::DynamicSpectrum(const ValueType val)
{
    set(val);

#ifdef APPLESEED_USE_SSE
    m_samples[s_size] = T(0.0);
#endif
}

template <typename T, size_t N>
inline DynamicSpectrum<T, N>::DynamicSpectrum(
    const foundation::Color<ValueType, 3>&              rgb,
    const foundation::LightingConditions&               lighting_conditions,
    const Intent                                        intent)
{
    set(rgb, lighting_conditions, intent);

#ifdef APPLESEED_USE_SSE
    m_samples[s_size] = T(0.0);
#endif
}

template <typename T, size_t N>
inline DynamicSpectrum<T, N>::DynamicSpectrum(
    const foundation::RegularSpectrum<ValueType, N>&    spectrum,
    const foundation::LightingConditions&               lighting_conditions,
    const Intent                                        intent)
{
    set(spectrum, lighting_conditions, intent);

#ifdef APPLESEED_USE_SSE
    m_samples[s_size] = T(0.0);
#endif
}

template <typename T, size_t N>
template <typename U>
inline DynamicSpectrum<T, N>::DynamicSpectrum(const DynamicSpectrum<U, N>& rhs)
{
    for (size_t i = 0; i < s_size; ++i)
        m_samples[i] = static_cast<ValueType>(rhs[i]);

#ifdef APPLESEED_USE_SSE
    m_samples[s_size] = T(0.0);
#endif
}

template <typename T, size_t N>
inline DynamicSpectrum<T, N> DynamicSpectrum<T, N>::from_array(const ValueType* rhs)
{
    assert(rhs);

    DynamicSpectrum result;

    for (size_t i = 0; i < s_size; ++i)
        result.m_samples[i] = rhs[i];

    return result;
}

template <typename T, size_t N>
inline void DynamicSpectrum<T, N>::set(const ValueType val)
{
    for (size_t i = 0; i < s_size; ++i)
        m_samples[i] = val;
}

#ifdef APPLESEED_USE_SSE

template <>
APPLESEED_FORCE_INLINE void DynamicSpectrum<float, 31>::set(const float val)
{
    const __m128 mval = _mm_set1_ps(val);

    _mm_store_ps(&m_samples[ 0], mval);

    if (s_size > 3)
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
void DynamicSpectrum<T, N>::set(
    const foundation::Color<ValueType, 3>&              rgb,
    const foundation::LightingConditions&               lighting_conditions,
    const Intent                                        intent)
{
    if (s_mode == RGB)
    {
        m_samples[0] = rgb[0];
        m_samples[1] = rgb[1];
        m_samples[2] = rgb[2];
    }
    else
    {
        if (intent == Reflectance)
        {
            foundation::linear_rgb_reflectance_to_spectrum(
                rgb,
                reinterpret_cast<foundation::RegularSpectrum<T, N>&>(m_samples[0]));
        }
        else
        {
            foundation::linear_rgb_illuminance_to_spectrum(
                rgb,
                reinterpret_cast<foundation::RegularSpectrum<T, N>&>(m_samples[0]));
        }
    }
}

template <typename T, size_t N>
void DynamicSpectrum<T, N>::set(
    const foundation::RegularSpectrum<ValueType, N>&    spectrum,
    const foundation::LightingConditions&               lighting_conditions,
    const Intent                                        intent)
{
    if (s_mode == Spectral)
    {
        for (size_t i = 0; i < N; ++i)
            m_samples[i] = spectrum[i];
    }
    else
    {
        reinterpret_cast<foundation::Color<T, 3>&>(m_samples[0]) =
            foundation::ciexyz_to_linear_rgb(
                foundation::spectrum_to_ciexyz<T>(lighting_conditions, spectrum));
    }
}

template <typename T, size_t N>
inline T& DynamicSpectrum<T, N>::operator[](const size_t i)
{
    assert(i < s_size);
    return m_samples[i];
}

template <typename T, size_t N>
inline const T& DynamicSpectrum<T, N>::operator[](const size_t i) const
{
    assert(i < s_size);
    return m_samples[i];
}

template <typename T, size_t N>
inline foundation::Color<T, 3> DynamicSpectrum<T, N>::to_rgb(
    const foundation::LightingConditions& lighting_conditions) const
{
    return
        s_mode == RGB
            ? foundation::Color<T, 3>(m_samples[0], m_samples[1], m_samples[2])
            : foundation::ciexyz_to_linear_rgb(
                  foundation::spectrum_to_ciexyz<T>(lighting_conditions, *this));
}

template <typename T, size_t N>
inline foundation::Color<T, 3> DynamicSpectrum<T, N>::to_ciexyz(
    const foundation::LightingConditions& lighting_conditions) const
{
    return
        s_mode == RGB
            ? linear_rgb_to_ciexyz(
                  foundation::Color<T, 3>(m_samples[0], m_samples[1], m_samples[2]))
            : foundation::spectrum_to_ciexyz<T>(lighting_conditions, *this);
}

template <typename T, size_t N>
inline bool operator!=(const DynamicSpectrum<T, N>& lhs, const DynamicSpectrum<T, N>& rhs)
{
    for (size_t i = 0; i < DynamicSpectrum<T, N>::size(); ++i)
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

    for (size_t i = 0; i < DynamicSpectrum<T, N>::size(); ++i)
        result[i] = lhs[i] + rhs[i];

    return result;
}

template <typename T, size_t N>
inline DynamicSpectrum<T, N> operator-(const DynamicSpectrum<T, N>& lhs, const DynamicSpectrum<T, N>& rhs)
{
    DynamicSpectrum<T, N> result;

    for (size_t i = 0; i < DynamicSpectrum<T, N>::size(); ++i)
        result[i] = lhs[i] - rhs[i];

    return result;
}

template <typename T, size_t N>
inline DynamicSpectrum<T, N> operator-(const DynamicSpectrum<T, N>& lhs)
{
    DynamicSpectrum<T, N> result;

    for (size_t i = 0; i < DynamicSpectrum<T, N>::size(); ++i)
        result[i] = -lhs[i];

    return result;
}

template <typename T, size_t N>
inline DynamicSpectrum<T, N> operator*(const DynamicSpectrum<T, N>& lhs, const T rhs)
{
    DynamicSpectrum<T, N> result;

    for (size_t i = 0; i < DynamicSpectrum<T, N>::size(); ++i)
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

    for (size_t i = 0; i < DynamicSpectrum<T, N>::size(); ++i)
        result[i] = lhs[i] * rhs[i];

    return result;
}

template <typename T, size_t N>
inline DynamicSpectrum<T, N> operator/(const DynamicSpectrum<T, N>& lhs, const T rhs)
{
    DynamicSpectrum<T, N> result;

    for (size_t i = 0; i < DynamicSpectrum<T, N>::size(); ++i)
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

    for (size_t i = 0; i < DynamicSpectrum<T, N>::size(); ++i)
        result[i] = lhs[i] / rhs[i];

    return result;
}

template <typename T, size_t N>
inline DynamicSpectrum<T, N>& operator+=(DynamicSpectrum<T, N>& lhs, const DynamicSpectrum<T, N>& rhs)
{
    for (size_t i = 0; i < DynamicSpectrum<T, N>::size(); ++i)
        lhs[i] += rhs[i];

    return lhs;
}

#ifdef APPLESEED_USE_SSE

template <>
APPLESEED_FORCE_INLINE DynamicSpectrum<float, 31>& operator+=(DynamicSpectrum<float, 31>& lhs, const DynamicSpectrum<float, 31>& rhs)
{
    _mm_store_ps(&lhs[ 0], _mm_add_ps(_mm_load_ps(&lhs[ 0]), _mm_load_ps(&rhs[ 0])));

    if (DynamicSpectrum<float, 31>::size() > 3)
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

template <typename T, size_t N>
inline DynamicSpectrum<T, N>& operator-=(DynamicSpectrum<T, N>& lhs, const DynamicSpectrum<T, N>& rhs)
{
    for (size_t i = 0; i < DynamicSpectrum<T, N>::size(); ++i)
        lhs[i] -= rhs[i];

    return lhs;
}

template <typename T, size_t N>
inline DynamicSpectrum<T, N>& operator*=(DynamicSpectrum<T, N>& lhs, const T rhs)
{
    for (size_t i = 0; i < DynamicSpectrum<T, N>::size(); ++i)
        lhs[i] *= rhs;

    return lhs;
}

#ifdef APPLESEED_USE_SSE

template <>
APPLESEED_FORCE_INLINE DynamicSpectrum<float, 31>& operator*=(DynamicSpectrum<float, 31>& lhs, const float rhs)
{
    const __m128 mrhs = _mm_set1_ps(rhs);

    _mm_store_ps(&lhs[ 0], _mm_mul_ps(_mm_load_ps(&lhs[ 0]), mrhs));

    if (DynamicSpectrum<float, 31>::size() > 3)
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
    for (size_t i = 0; i < DynamicSpectrum<T, N>::size(); ++i)
        lhs[i] *= rhs[i];

    return lhs;
}

#ifdef APPLESEED_USE_SSE

template <>
APPLESEED_FORCE_INLINE DynamicSpectrum<float, 31>& operator*=(DynamicSpectrum<float, 31>& lhs, const DynamicSpectrum<float, 31>& rhs)
{
    _mm_store_ps(&lhs[ 0], _mm_mul_ps(_mm_load_ps(&lhs[ 0]), _mm_load_ps(&rhs[ 0])));

    if (DynamicSpectrum<float, 31>::size() > 3)
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

template <typename T, size_t N>
inline DynamicSpectrum<T, N>& operator/=(DynamicSpectrum<T, N>& lhs, const T rhs)
{
    for (size_t i = 0; i < DynamicSpectrum<T, N>::size(); ++i)
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
    for (size_t i = 0; i < DynamicSpectrum<T, N>::size(); ++i)
        lhs[i] /= rhs[i];

    return lhs;
}

template <typename T, size_t N>
inline void madd(
    DynamicSpectrum<T, N>&                  a,
    const DynamicSpectrum<T, N>&            b,
    const DynamicSpectrum<T, N>&            c)
{
    for (size_t i = 0; i < DynamicSpectrum<T, N>::size(); ++i)
        a[i] += b[i] * c[i];
}

template <typename T, size_t N>
inline void madd(
    DynamicSpectrum<T, N>&                  a,
    const DynamicSpectrum<T, N>&            b,
    const T                                 c)
{
    for (size_t i = 0; i < DynamicSpectrum<T, N>::size(); ++i)
        a[i] += b[i] * c;
}

#ifdef APPLESEED_USE_SSE

template <>
APPLESEED_FORCE_INLINE void madd(
    DynamicSpectrum<float, 31>&             a,
    const DynamicSpectrum<float, 31>&       b,
    const DynamicSpectrum<float, 31>&       c)
{
    _mm_store_ps(&a[0], _mm_add_ps(_mm_load_ps(&a[0]), _mm_mul_ps(_mm_load_ps(&b[0]), _mm_load_ps(&c[0]))));

    if (DynamicSpectrum<float, 31>::size() > 3)
    {
        _mm_store_ps(&a[ 4], _mm_add_ps(_mm_load_ps(&a[ 4]), _mm_mul_ps(_mm_load_ps(&b[ 4]), _mm_load_ps(&c[ 4]))));
        _mm_store_ps(&a[ 8], _mm_add_ps(_mm_load_ps(&a[ 8]), _mm_mul_ps(_mm_load_ps(&b[ 8]), _mm_load_ps(&c[ 8]))));
        _mm_store_ps(&a[12], _mm_add_ps(_mm_load_ps(&a[12]), _mm_mul_ps(_mm_load_ps(&b[12]), _mm_load_ps(&c[12]))));
        _mm_store_ps(&a[16], _mm_add_ps(_mm_load_ps(&a[16]), _mm_mul_ps(_mm_load_ps(&b[16]), _mm_load_ps(&c[16]))));
        _mm_store_ps(&a[20], _mm_add_ps(_mm_load_ps(&a[20]), _mm_mul_ps(_mm_load_ps(&b[20]), _mm_load_ps(&c[20]))));
        _mm_store_ps(&a[24], _mm_add_ps(_mm_load_ps(&a[24]), _mm_mul_ps(_mm_load_ps(&b[24]), _mm_load_ps(&c[24]))));
        _mm_store_ps(&a[28], _mm_add_ps(_mm_load_ps(&a[28]), _mm_mul_ps(_mm_load_ps(&b[28]), _mm_load_ps(&c[28]))));
    }
}

template <>
APPLESEED_FORCE_INLINE void madd(
    DynamicSpectrum<float, 31>&             a,
    const DynamicSpectrum<float, 31>&       b,
    const float                             c)
{
    const __m128 k = _mm_set_ps1(c);

    _mm_store_ps(&a[0], _mm_add_ps(_mm_load_ps(&a[0]), _mm_mul_ps(_mm_load_ps(&b[0]), k)));

    if (DynamicSpectrum<float, 31>::size() > 3)
    {
        _mm_store_ps(&a[ 4], _mm_add_ps(_mm_load_ps(&a[ 4]), _mm_mul_ps(_mm_load_ps(&b[ 4]), k)));
        _mm_store_ps(&a[ 8], _mm_add_ps(_mm_load_ps(&a[ 8]), _mm_mul_ps(_mm_load_ps(&b[ 8]), k)));
        _mm_store_ps(&a[12], _mm_add_ps(_mm_load_ps(&a[12]), _mm_mul_ps(_mm_load_ps(&b[12]), k)));
        _mm_store_ps(&a[16], _mm_add_ps(_mm_load_ps(&a[16]), _mm_mul_ps(_mm_load_ps(&b[16]), k)));
        _mm_store_ps(&a[20], _mm_add_ps(_mm_load_ps(&a[20]), _mm_mul_ps(_mm_load_ps(&b[20]), k)));
        _mm_store_ps(&a[24], _mm_add_ps(_mm_load_ps(&a[24]), _mm_mul_ps(_mm_load_ps(&b[24]), k)));
        _mm_store_ps(&a[28], _mm_add_ps(_mm_load_ps(&a[28]), _mm_mul_ps(_mm_load_ps(&b[28]), k)));
    }
}

#endif  // APPLESEED_USE_SSE

}       // namespace renderer

namespace foundation
{

template <typename T, size_t N>
inline bool is_zero(const renderer::DynamicSpectrum<T, N>& s)
{
    for (size_t i = 0, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
    {
        if (s[i] != T(0.0))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline bool feq(const renderer::DynamicSpectrum<T, N>& lhs, const renderer::DynamicSpectrum<T, N>& rhs)
{
    for (size_t i = 0, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
    {
        if (!feq(lhs[i], rhs[i]))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline bool feq(const renderer::DynamicSpectrum<T, N>& lhs, const renderer::DynamicSpectrum<T, N>& rhs, const T eps)
{
    for (size_t i = 0, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
    {
        if (!feq(lhs[i], rhs[i], eps))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline bool fz(const renderer::DynamicSpectrum<T, N>& s)
{
    for (size_t i = 0, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
    {
        if (!fz(s[i]))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline bool fz(const renderer::DynamicSpectrum<T, N>& s, const T eps)
{
    for (size_t i = 0, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
    {
        if (!fz(s[i], eps))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline renderer::DynamicSpectrum<T, N> rcp(const renderer::DynamicSpectrum<T, N>& s)
{
    renderer::DynamicSpectrum<T, N> result;

    for (size_t i = 0, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
        result[i] = T(1.0) / s[i];

    return result;
}


template <typename T, size_t N>
inline renderer::DynamicSpectrum<T, N> sqrt(const renderer::DynamicSpectrum<T, N>& s)
{
    renderer::DynamicSpectrum<T, N> result;

    for (size_t i = 0, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
        result[i] = std::sqrt(s[i]);

    return result;
}

#ifdef APPLESEED_USE_SSE

template <>
APPLESEED_FORCE_INLINE renderer::DynamicSpectrum<float, 31> sqrt(const renderer::DynamicSpectrum<float, 31>& s)
{
    renderer::DynamicSpectrum<float, 31> result;

    _mm_store_ps(&result[ 0], _mm_sqrt_ps(_mm_load_ps(&s[ 0])));

    if (renderer::DynamicSpectrum<float, 31>::size() > 3)
    {
        _mm_store_ps(&result[ 4], _mm_sqrt_ps(_mm_load_ps(&s[ 4])));
        _mm_store_ps(&result[ 8], _mm_sqrt_ps(_mm_load_ps(&s[ 8])));
        _mm_store_ps(&result[12], _mm_sqrt_ps(_mm_load_ps(&s[12])));
        _mm_store_ps(&result[16], _mm_sqrt_ps(_mm_load_ps(&s[16])));
        _mm_store_ps(&result[20], _mm_sqrt_ps(_mm_load_ps(&s[20])));
        _mm_store_ps(&result[24], _mm_sqrt_ps(_mm_load_ps(&s[24])));
        _mm_store_ps(&result[28], _mm_sqrt_ps(_mm_load_ps(&s[28])));
    }

    return result;
}

#endif  // APPLESEED_USE_SSE

template <typename T, size_t N>
inline renderer::DynamicSpectrum<T, N> pow(const renderer::DynamicSpectrum<T, N>& x, const T y)
{
    renderer::DynamicSpectrum<T, N> result;

    for (size_t i = 0, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
        result[i] = std::pow(x[i], y);

    return result;
}

template <typename T, size_t N>
inline renderer::DynamicSpectrum<T, N> pow(
    const renderer::DynamicSpectrum<T, N>& x,
    const renderer::DynamicSpectrum<T, N>& y)
{
    renderer::DynamicSpectrum<T, N> result;

    for (size_t i = 0, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
        result[i] = std::pow(x[i], y[i]);

    return result;
}

template <typename T, size_t N>
inline renderer::DynamicSpectrum<T, N> log(const renderer::DynamicSpectrum<T, N>& s)
{
    renderer::DynamicSpectrum<T, N> result;

    for (size_t i = 0, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
        result[i] = std::log(s[i]);

    return result;
}

template <typename T, size_t N>
inline renderer::DynamicSpectrum<T, N> exp(const renderer::DynamicSpectrum<T, N>& s)
{
    renderer::DynamicSpectrum<T, N> result;

    for (size_t i = 0, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
        result[i] = std::exp(s[i]);

    return result;
}

template <typename T, size_t N>
inline bool is_saturated(const renderer::DynamicSpectrum<T, N>& s)
{
    for (size_t i = 0, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
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

    for (size_t i = 0, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
        result[i] = saturate(s[i]);

    return result;
}

template <typename T, size_t N>
inline void saturate_in_place(renderer::DynamicSpectrum<T, N>& s)
{
    clamp_in_place(s, T(0.0), T(1.0));
}

template <typename T, size_t N>
inline renderer::DynamicSpectrum<T, N> clamp(const renderer::DynamicSpectrum<T, N>& s, const T min, const T max)
{
    renderer::DynamicSpectrum<T, N> result;

    for (size_t i = 0, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
        result[i] = clamp(s[i], min, max);

    return result;
}

template <typename T, size_t N>
inline void clamp_in_place(renderer::DynamicSpectrum<T, N>& s, const T min, const T max)
{
    for (size_t i = 0, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
    {
        if (s[i] < min)
            s[i] = min;

        if (s[i] > max)
            s[i] = max;
    }
}

template <typename T, size_t N>
inline renderer::DynamicSpectrum<T, N> clamp_low(const renderer::DynamicSpectrum<T, N>& s, const T min)
{
    renderer::DynamicSpectrum<T, N> result;

    for (size_t i = 0, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
        result[i] = std::max(s[i], min);

    return result;
}

template <typename T, size_t N>
inline void clamp_low_in_place(renderer::DynamicSpectrum<T, N>& s, const T min)
{
    for (size_t i = 0, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
    {
        if (s[i] < min)
            s[i] = min;
    }
}

template <typename T, size_t N>
inline renderer::DynamicSpectrum<T, N> clamp_high(const renderer::DynamicSpectrum<T, N>& s, const T max)
{
    renderer::DynamicSpectrum<T, N> result;

    for (size_t i = 0, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
        result[i] = std::min(s[i], max);

    return result;
}

template <typename T, size_t N>
inline void clamp_high_in_place(renderer::DynamicSpectrum<T, N>& s, const T max)
{
    for (size_t i = 0, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
    {
        if (s[i] > max)
            s[i] = max;
    }
}

template <typename T, size_t N>
inline renderer::DynamicSpectrum<T, N> lerp(
    const renderer::DynamicSpectrum<T, N>& a,
    const renderer::DynamicSpectrum<T, N>& b,
    const renderer::DynamicSpectrum<T, N>& t)
{
    renderer::DynamicSpectrum<T, N> result;

    for (size_t i = 0, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
        result[i] = foundation::lerp(a[i], b[i], t[i]);

    return result;
}

#ifdef APPLESEED_USE_SSE

template <>
APPLESEED_FORCE_INLINE renderer::DynamicSpectrum<float, 31> lerp(
    const renderer::DynamicSpectrum<float, 31>& a,
    const renderer::DynamicSpectrum<float, 31>& b,
    const renderer::DynamicSpectrum<float, 31>& t)
{
    renderer::DynamicSpectrum<float, 31> result;

    __m128 one4 = _mm_set1_ps(1.0f);
    __m128 t4 = _mm_load_ps(&t[0]);
    __m128 one_minus_t4 = _mm_sub_ps(one4, t4);
    __m128 x = _mm_mul_ps(_mm_load_ps(&a[0]), one_minus_t4);
    __m128 y = _mm_mul_ps(_mm_load_ps(&b[0]), t4);
    _mm_store_ps(&result[0], _mm_add_ps(x, y));

    if (renderer::DynamicSpectrum<float, 31>::size() > 3)
    {
        for (size_t i = 4; i < a.StoredSamples; i += 4)
        {
            t4 = _mm_load_ps(&t[i]);
            one_minus_t4 = _mm_sub_ps(one4, t4);
            x = _mm_mul_ps(_mm_load_ps(&a[i]), one_minus_t4);
            y = _mm_mul_ps(_mm_load_ps(&b[i]), t4);
            _mm_store_ps(&result[i], _mm_add_ps(x, y));
        }
    }

    return result;
}

#endif  // APPLESEED_USE_SSE

template <typename T, size_t N>
inline T min_value(const renderer::DynamicSpectrum<T, N>& s)
{
    T value = s[0];

    for (size_t i = 1, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
    {
        if (value > s[i])
            value = s[i];
    }

    return value;
}

#ifdef APPLESEED_USE_SSE

template <>
inline float min_value(const renderer::DynamicSpectrum<float, 31>& s)
{
    if (renderer::DynamicSpectrum<float, 31>::size() == 3)
        return std::min(std::min(s[0], s[1]), s[2]);

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
inline T max_value(const renderer::DynamicSpectrum<T, N>& s)
{
    T value = s[0];

    for (size_t i = 1, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
    {
        if (value < s[i])
            value = s[i];
    }

    return value;
}

#ifdef APPLESEED_USE_SSE

template <>
inline float max_value(const renderer::DynamicSpectrum<float, 31>& s)
{
    if (renderer::DynamicSpectrum<float, 31>::size() == 3)
        return std::max(std::max(s[0], s[1]), s[2]);

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
inline size_t min_index(const renderer::DynamicSpectrum<T, N>& s)
{
    size_t index = 0;
    T value = s[0];

    for (size_t i = 1, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
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
inline size_t max_index(const renderer::DynamicSpectrum<T, N>& s)
{
    size_t index = 0;
    T value = s[0];

    for (size_t i = 1, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
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
inline size_t min_abs_index(const renderer::DynamicSpectrum<T, N>& s)
{
    size_t index = 0;
    T value = std::abs(s[0]);

    for (size_t i = 1, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
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
inline size_t max_abs_index(const renderer::DynamicSpectrum<T, N>& s)
{
    size_t index = 0;
    T value = std::abs(s[0]);

    for (size_t i = 1, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
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
inline T sum_value(const renderer::DynamicSpectrum<T, N>& s)
{
    T sum = s[0];

    for (size_t i = 1, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
        sum += s[i];

    return sum;
}

template <typename T, size_t N>
inline T average_value(const renderer::DynamicSpectrum<T, N>& s)
{
    return sum_value(s) / renderer::DynamicSpectrum<T, N>::size();
}

template <typename T, size_t N>
inline bool has_nan(const renderer::DynamicSpectrum<T, N>& s)
{
    for (size_t i = 0, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
    {
        if (s[i] != s[i])
            return true;
    }

    return false;
}

template <typename T, size_t N>
inline bool is_finite(const renderer::DynamicSpectrum<T, N>& s)
{
    for (size_t i = 0, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
    {
        if (!FP<T>::is_finite(s[i]))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline bool is_finite_non_neg(const renderer::DynamicSpectrum<T, N>& s)
{
    for (size_t i = 0, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
    {
        if (!FP<T>::is_finite_non_neg(s[i]))
            return false;
    }

    return true;
}

template <typename T, size_t N>
class PoisonImpl<renderer::DynamicSpectrum<T, N>>
{
  public:
    static void do_poison(renderer::DynamicSpectrum<T, N>& s)
    {
        for (size_t i = 0, e = renderer::DynamicSpectrum<T, N>::size(); i < e; ++i)
            debug_poison(s[i]);
    }
};

}   // namespace foundation
