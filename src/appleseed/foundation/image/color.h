
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_FOUNDATION_IMAGE_COLOR_H
#define APPLESEED_FOUNDATION_IMAGE_COLOR_H

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/platform/types.h"

// Imath headers.
#ifdef APPLESEED_ENABLE_IMATH_INTEROP
#include "foundation/platform/exrheaderguards.h"
BEGIN_EXR_INCLUDES
#include "OpenEXR/ImathColor.h"
END_EXR_INCLUDES
#endif

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>

namespace foundation
{

//
// N-dimensional color class and operations.
//

template <typename T, size_t N>
class Color
{
  public:
    // Value type and number of components.
    typedef T ValueType;
    static const size_t Components = N;

    // Constructors.
    Color();                                    // leave all components uninitialized
    explicit Color(const ValueType* rhs);       // initialize with array of N scalars
    explicit Color(const ValueType val);        // set all components to 'val'

    // Construct a color from another color of a different type.
    template <typename U>
    Color(const Color<U, N>& rhs);

    // Set all components to a given value.
    void set(const ValueType val);

    // Unchecked array subscripting.
    ValueType& operator[](const size_t i);
    const ValueType& operator[](const size_t i) const;

  private:
    // Color components.
    ValueType m_comp[N];
};

// Exact inequality and equality tests.
template <typename T, size_t N> bool operator!=(const Color<T, N>& lhs, const Color<T, N>& rhs);
template <typename T, size_t N> bool operator==(const Color<T, N>& lhs, const Color<T, N>& rhs);

// Approximate equality tests.
template <typename T, size_t N> bool feq(const Color<T, N>& lhs, const Color<T, N>& rhs);
template <typename T, size_t N> bool feq(const Color<T, N>& lhs, const Color<T, N>& rhs, const T eps);

// Approximate zero tests.
template <typename T, size_t N> bool fz(const Color<T, N>& c);
template <typename T, size_t N> bool fz(const Color<T, N>& c, const T eps);

// Color arithmetic.
template <typename T, size_t N> Color<T, N>  operator+ (const Color<T, N>& lhs, const Color<T, N>& rhs);
template <typename T, size_t N> Color<T, N>  operator- (const Color<T, N>& lhs, const Color<T, N>& rhs);
template <typename T, size_t N> Color<T, N>  operator- (const Color<T, N>& lhs);
template <typename T, size_t N> Color<T, N>  operator* (const Color<T, N>& lhs, const T rhs);
template <typename T, size_t N> Color<T, N>  operator* (const T lhs, const Color<T, N>& rhs);
template <typename T, size_t N> Color<T, N>  operator* (const Color<T, N>& lhs, const Color<T, N>& rhs);
template <typename T, size_t N> Color<T, N>  operator/ (const Color<T, N>& lhs, const T rhs);
template <typename T, size_t N> Color<T, N>  operator/ (const Color<T, N>& lhs, const Color<T, N>& rhs);
template <typename T, size_t N> Color<T, N>& operator+=(Color<T, N>& lhs, const Color<T, N>& rhs);
template <typename T, size_t N> Color<T, N>& operator-=(Color<T, N>& lhs, const Color<T, N>& rhs);
template <typename T, size_t N> Color<T, N>& operator*=(Color<T, N>& lhs, const T rhs);
template <typename T, size_t N> Color<T, N>& operator*=(Color<T, N>& lhs, const Color<T, N>& rhs);
template <typename T, size_t N> Color<T, N>& operator/=(Color<T, N>& lhs, const T rhs);
template <typename T, size_t N> Color<T, N>& operator/=(Color<T, N>& lhs, const Color<T, N>& rhs);

// Return whether all components of a color are in [0,1].
template <typename T, size_t N> bool is_saturated(const Color<T, N>& c);

// Clamp the argument to [0,1].
template <typename T, size_t N> Color<T, N> saturate(const Color<T, N>& c);

// Clamp the argument to [min, max].
template <typename T, size_t N> Color<T, N> clamp(const Color<T, N>& c, const T min, const T max);

// Clamp the argument to [min, +infinity).
template <typename T, size_t N> Color<T, N> clamp_low(const Color<T, N>& c, const T min);

// Clamp the argument to (-infinity, max].
template <typename T, size_t N> Color<T, N> clamp_high(const Color<T, N>& c, const T max);

// Return the smallest or largest signed component of a color.
template <typename T, size_t N> T min_value(const Color<T, N>& c);
template <typename T, size_t N> T max_value(const Color<T, N>& c);

// Return the index of the smallest or largest signed component of a color.
template <typename T, size_t N> size_t min_index(const Color<T, N>& c);
template <typename T, size_t N> size_t max_index(const Color<T, N>& c);

// Return the index of the smallest or largest component of a color, in absolute value.
template <typename T, size_t N> size_t min_abs_index(const Color<T, N>& c);
template <typename T, size_t N> size_t max_abs_index(const Color<T, N>& c);

// Component-wise min/max of two colors.
template <typename T, size_t N> Color<T, N> component_wise_min(const Color<T, N>& lhs, const Color<T, N>& rhs);
template <typename T, size_t N> Color<T, N> component_wise_max(const Color<T, N>& lhs, const Color<T, N>& rhs);

// Return the average value of a color.
template <typename T, size_t N> T average_value(const Color<T, N>& c);

// Return true if a color contains at least one NaN value.
template <typename T, size_t N> bool has_nan(const Color<T, N>& c);


//
// RGB color class of arbitrary type.
//

template <typename T>
class Color<T, 3>
{
  public:
    // Value type and number of components.
    typedef T ValueType;
    static const size_t Components = 3;

    // Color components.
    ValueType r, g, b;

    // Constructors.
    Color();                                    // leave all components uninitialized
    explicit Color(const ValueType* rhs);       // initialize with array of 3 scalars
    explicit Color(const ValueType val);        // set all components to 'val'
    Color(                                      // set individual components
        const ValueType r,
        const ValueType g,
        const ValueType b);

    // Construct a color from another color of a different type.
    template <typename U>
    Color(const Color<U, 3>& rhs);

#ifdef APPLESEED_ENABLE_IMATH_INTEROP

    // Implicit construction from an Imath::Color3.
    Color(const Imath::Color3<T>& rhs);

    // Reinterpret this color as an Imath::Color3.
    operator Imath::Color3<T>&();
    operator const Imath::Color3<T>&() const;

#endif

    // Set all components to a given value.
    void set(const ValueType val);

    // Unchecked array subscripting.
    ValueType& operator[](const size_t i);
    const ValueType& operator[](const size_t i) const;
};


//
// RGBA color class of arbitrary type.
//

template <typename T>
class Color<T, 4>
{
  public:
    // Value type and number of components.
    typedef T ValueType;
    static const size_t Components = 4;

    // Color components.
    ValueType r, g, b, a;

    // Constructors.
    Color();                                    // leave all components uninitialized
    explicit Color(const ValueType* rhs);       // initialize with array of 4 scalars
    explicit Color(const ValueType val);        // set all components to 'val'
    Color(
        const Color<T, 3>&  rgb,
        const ValueType     a);
    Color(                                      // set individual components
        const ValueType     r,
        const ValueType     g,
        const ValueType     b,
        const ValueType     a);

    // Construct a color from another color of a different type.
    template <typename U>
    Color(const Color<U, 4>& rhs);

#ifdef APPLESEED_ENABLE_IMATH_INTEROP

    // Implicit construction from an Imath::Color4.
    Color(const Imath::Color4<T>& rhs);

    // Reinterpret this color as an Imath::Color4.
    operator Imath::Color4<T>&();
    operator const Imath::Color4<T>&() const;

#endif

    // Set all components to a given value.
    void set(const ValueType val);

    // Access the color as a 3-component color.
    Color<ValueType, 3>& rgb();
    const Color<ValueType, 3>& rgb() const;

    // Unchecked array subscripting.
    ValueType& operator[](const size_t i);
    const ValueType& operator[](const size_t i) const;
};


//
// Full specializations for colors of type int, float and double.
//

typedef Color<uint8,    3> Color3b;
typedef Color<float,    3> Color3f;
typedef Color<double,   3> Color3d;

typedef Color<uint8,    4> Color4b;
typedef Color<float,    4> Color4f;
typedef Color<double,   4> Color4d;


//
// N-dimensional color implementation.
//

template <typename T, size_t N>
inline Color<T, N>::Color()
{
}

template <typename T, size_t N>
inline Color<T, N>::Color(const ValueType* rhs)
{
    assert(rhs);

    for (size_t i = 0; i < N; ++i)
        m_comp[i] = rhs[i];
}

template <typename T, size_t N>
inline Color<T, N>::Color(const ValueType val)
{
    set(val);
}

template <typename T, size_t N>
template <typename U>
inline Color<T, N>::Color(const Color<U, N>& rhs)
{
    for (size_t i = 0; i < N; ++i)
        m_comp[i] = static_cast<ValueType>(rhs.m_comp[i]);
}

template <typename T, size_t N>
inline void Color<T, N>::set(const ValueType val)
{
    for (size_t i = 0; i < N; ++i)
        m_comp[i] = val;
}

template <typename T, size_t N>
inline T& Color<T, N>::operator[](const size_t i)
{
    assert(i < Components);
    return m_comp[i];
}

template <typename T, size_t N>
inline const T& Color<T, N>::operator[](const size_t i) const
{
    assert(i < Components);
    return m_comp[i];
}

template <typename T, size_t N>
inline bool operator!=(const Color<T, N>& lhs, const Color<T, N>& rhs)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (lhs[i] != rhs[i])
            return true;
    }

    return false;
}

template <typename T, size_t N>
inline bool operator==(const Color<T, N>& lhs, const Color<T, N>& rhs)
{
    return !(lhs != rhs);
}

template <typename T, size_t N>
inline bool feq(const Color<T, N>& lhs, const Color<T, N>& rhs)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (!feq(lhs[i], rhs[i]))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline bool feq(const Color<T, N>& lhs, const Color<T, N>& rhs, const T eps)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (!feq(lhs[i], rhs[i], eps))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline bool fz(const Color<T, N>& v)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (!fz(v[i]))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline bool fz(const Color<T, N>& v, const T eps)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (!fz(v[i], eps))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline Color<T, N> operator+(const Color<T, N>& lhs, const Color<T, N>& rhs)
{
    Color<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = lhs[i] + rhs[i];

    return result;
}

template <typename T, size_t N>
inline Color<T, N> operator-(const Color<T, N>& lhs, const Color<T, N>& rhs)
{
    Color<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = lhs[i] - rhs[i];

    return result;
}

template <typename T, size_t N>
inline Color<T, N> operator-(const Color<T, N>& lhs)
{
    Color<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = -lhs[i];

    return result;
}

template <typename T, size_t N>
inline Color<T, N> operator*(const Color<T, N>& lhs, const T rhs)
{
    Color<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = lhs[i] * rhs;

    return result;
}

template <typename T, size_t N>
inline Color<T, N> operator*(const T lhs, const Color<T, N>& rhs)
{
    return rhs * lhs;
}

template <typename T, size_t N>
inline Color<T, N> operator*(const Color<T, N>& lhs, const Color<T, N>& rhs)
{
    Color<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = lhs[i] * rhs[i];

    return result;
}

template <typename T, size_t N>
inline Color<T, N> operator/(const Color<T, N>& lhs, const T rhs)
{
    Color<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = lhs[i] / rhs;

    return result;
}

template <size_t N>
inline Color<float, N> operator/(const Color<float, N>& lhs, const float rhs)
{
    return lhs * (1.0f / rhs);
}

template <size_t N>
inline Color<double, N> operator/(const Color<double, N>& lhs, const double rhs)
{
    return lhs * (1.0 / rhs);
}

template <size_t N>
inline Color<long double, N> operator/(const Color<long double, N>& lhs, const long double rhs)
{
    return lhs * (1.0L / rhs);
}

template <typename T, size_t N>
inline Color<T, N> operator/(const Color<T, N>& lhs, const Color<T, N>& rhs)
{
    Color<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = lhs[i] / rhs[i];

    return result;
}

template <typename T, size_t N>
inline Color<T, N>& operator+=(Color<T, N>& lhs, const Color<T, N>& rhs)
{
    for (size_t i = 0; i < N; ++i)
        lhs[i] += rhs[i];

    return lhs;
}

template <typename T, size_t N>
inline Color<T, N>& operator-=(Color<T, N>& lhs, const Color<T, N>& rhs)
{
    for (size_t i = 0; i < N; ++i)
        lhs[i] -= rhs[i];

    return lhs;
}

template <typename T, size_t N>
inline Color<T, N>& operator*=(Color<T, N>& lhs, const T rhs)
{
    for (size_t i = 0; i < N; ++i)
        lhs[i] *= rhs;

    return lhs;
}

template <typename T, size_t N>
inline Color<T, N>& operator*=(Color<T, N>& lhs, const Color<T, N>& rhs)
{
    for (size_t i = 0; i < N; ++i)
        lhs[i] *= rhs[i];

    return lhs;
}

template <typename T, size_t N>
inline Color<T, N>& operator/=(Color<T, N>& lhs, const T rhs)
{
    for (size_t i = 0; i < N; ++i)
        lhs[i] /= rhs;

    return lhs;
}

template <size_t N>
inline Color<float, N>& operator/=(Color<float, N>& lhs, const float rhs)
{
    return lhs *= 1.0f / rhs;
}

template <size_t N>
inline Color<double, N>& operator/=(Color<double, N>& lhs, const double rhs)
{
    return lhs *= 1.0 / rhs;
}

template <size_t N>
inline Color<long double, N>& operator/=(Color<long double, N>& lhs, const long double rhs)
{
    return lhs *= 1.0L / rhs;
}

template <typename T, size_t N>
inline Color<T, N>& operator/=(Color<T, N>& lhs, const Color<T, N>& rhs)
{
    for (size_t i = 0; i < N; ++i)
        lhs[i] /= rhs[i];

    return lhs;
}

template <typename T, size_t N>
inline bool is_saturated(const Color<T, N>& c)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (c[i] < T(0.0) || c[i] > T(1.0))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline Color<T, N> saturate(const Color<T, N>& c)
{
    Color<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = saturate(c[i]);

    return result;
}

template <typename T, size_t N>
inline Color<T, N> clamp(const Color<T, N>& c, const T min, const T max)
{
    Color<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = clamp(c[i], min, max);

    return result;
}

template <typename T, size_t N>
inline Color<T, N> clamp_low(const Color<T, N>& c, const T min)
{
    Color<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = std::max(c[i], min);

    return result;
}

template <typename T, size_t N>
inline Color<T, N> clamp_high(const Color<T, N>& c, const T max)
{
    Color<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = std::min(c[i], max);

    return result;
}

template <typename T, size_t N>
inline T min_value(const Color<T, N>& c)
{
    T value = c[0];

    for (size_t i = 1; i < N; ++i)
    {
        if (value > c[i])
            value = c[i];
    }

    return value;
}

template <typename T, size_t N>
inline T max_value(const Color<T, N>& c)
{
    T value = c[0];

    for (size_t i = 1; i < N; ++i)
    {
        if (value < c[i])
            value = c[i];
    }

    return value;
}

template <typename T, size_t N>
inline size_t min_index(const Color<T, N>& c)
{
    size_t index = 0;
    T value = c[0];

    for (size_t i = 1; i < N; ++i)
    {
        const T x = c[i];

        if (value > x)
        {
            value = x;
            index = i;
        }
    }

    return index;
}

template <typename T, size_t N>
inline size_t max_index(const Color<T, N>& c)
{
    size_t index = 0;
    T value = c[0];

    for (size_t i = 1; i < N; ++i)
    {
        const T x = c[i];

        if (value < x)
        {
            value = x;
            index = i;
        }
    }

    return index;
}

template <typename T, size_t N>
inline size_t min_abs_index(const Color<T, N>& c)
{
    size_t index = 0;
    T value = std::abs(c[0]);

    for (size_t i = 1; i < N; ++i)
    {
        const T x = std::abs(c[i]);

        if (value > x)
        {
            value = x;
            index = i;
        }
    }

    return index;
}

template <typename T, size_t N>
inline size_t max_abs_index(const Color<T, N>& c)
{
    size_t index = 0;
    T value = std::abs(c[0]);

    for (size_t i = 1; i < N; ++i)
    {
        const T x = std::abs(c[i]);

        if (value < x)
        {
            value = x;
            index = i;
        }
    }

    return index;
}

template <typename T, size_t N>
inline Color<T, N> component_wise_min(const Color<T, N>& lhs, const Color<T, N>& rhs)
{
    Color<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = std::min(lhs[i], rhs[i]);

    return result;
}

template <typename T, size_t N>
inline Color<T, N> component_wise_max(const Color<T, N>& lhs, const Color<T, N>& rhs)
{
    Color<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = std::max(lhs[i], rhs[i]);

    return result;
}

template <typename T, size_t N>
inline T average_value(const Color<T, N>& c)
{
    T average = c[0];

    for (size_t i = 1; i < N; ++i)
        average += c[i];

    return average * (T(1.0) / N);
}

template <typename T, size_t N>
inline bool has_nan(const Color<T, N>& c)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (c[i] != c[i])
            return true;
    }

    return false;
}


//
// RGB color implementation.
//

template <typename T>
inline Color<T, 3>::Color()
{
}

template <typename T>
inline Color<T, 3>::Color(const ValueType* rhs)
{
    assert(rhs);
    r = rhs[0];
    g = rhs[1];
    b = rhs[2];
}

template <typename T>
inline Color<T, 3>::Color(const ValueType val)
  : r(val)
  , g(val)
  , b(val)
{
}

template <typename T>
inline Color<T, 3>::Color(
    const ValueType r_,
    const ValueType g_,
    const ValueType b_)
  : r(r_)
  , g(g_)
  , b(b_)
{
}

template <typename T>
template <typename U>
inline Color<T, 3>::Color(const Color<U, 3>& rhs)
  : r(static_cast<ValueType>(rhs.r))
  , g(static_cast<ValueType>(rhs.g))
  , b(static_cast<ValueType>(rhs.b))
{
}

#ifdef APPLESEED_ENABLE_IMATH_INTEROP

template <typename T>
inline Color<T, 3>::Color(const Imath::Color3<T>& rhs)
  : r(rhs.x)
  , g(rhs.y)
  , b(rhs.z)
{
}

template <typename T>
inline Color<T, 3>::operator Imath::Color3<T>&()
{
    return reinterpret_cast<Imath::Color3<T>&>(*this);
}

template <typename T>
inline Color<T, 3>::operator const Imath::Color3<T>&() const
{
    return reinterpret_cast<const Imath::Color3<T>&>(*this);
}

#endif

template <typename T>
inline void Color<T, 3>::set(const ValueType val)
{
    r = g = b = val;
}

template <typename T>
inline T& Color<T, 3>::operator[](const size_t i)
{
    assert(i < Components);
    return (&r)[i];
}

template <typename T>
inline const T& Color<T, 3>::operator[](const size_t i) const
{
    assert(i < Components);
    return (&r)[i];
}


//
// RGBA color implementation.
//

template <typename T>
inline Color<T, 4>::Color()
{
}

template <typename T>
inline Color<T, 4>::Color(const ValueType* rhs)
{
    assert(rhs);
    r = rhs[0];
    g = rhs[1];
    b = rhs[2];
    a = rhs[3];
}

template <typename T>
inline Color<T, 4>::Color(const ValueType val)
  : r(val)
  , g(val)
  , b(val)
  , a(val)
{
}

template <typename T>
inline Color<T, 4>::Color(
    const Color<T, 3>&  rgb,
    const ValueType     a_)
  : r(rgb.r)
  , g(rgb.g)
  , b(rgb.b)
  , a(a_)
{
}

template <typename T>
inline Color<T, 4>::Color(
    const ValueType     r_,
    const ValueType     g_,
    const ValueType     b_,
    const ValueType     a_)
  : r(r_)
  , g(g_)
  , b(b_)
  , a(a_)
{
}

template <typename T>
template <typename U>
inline Color<T, 4>::Color(const Color<U, 4>& rhs)
  : r(static_cast<ValueType>(rhs.r))
  , g(static_cast<ValueType>(rhs.g))
  , b(static_cast<ValueType>(rhs.b))
  , a(static_cast<ValueType>(rhs.a))
{
}

#ifdef APPLESEED_ENABLE_IMATH_INTEROP

template <typename T>
inline Color<T, 4>::Color(const Imath::Color4<T>& rhs)
  : r(rhs.r)
  , g(rhs.g)
  , b(rhs.b)
  , a(rhs.a)
{
}

template <typename T>
inline Color<T, 4>::operator Imath::Color4<T>&()
{
    return reinterpret_cast<Imath::Color4<T>&>(*this);
}

template <typename T>
inline Color<T, 4>::operator const Imath::Color4<T>&() const
{
    return reinterpret_cast<const Imath::Color4<T>&>(*this);
}

#endif

template <typename T>
inline void Color<T, 4>::set(const ValueType val)
{
    r = g = b = a = val;
}

template <typename T>
inline Color<T, 3>& Color<T, 4>::rgb()
{
    return *reinterpret_cast<Color<T, 3>*>(&r);
}

template <typename T>
inline const Color<T, 3>& Color<T, 4>::rgb() const
{
    return *reinterpret_cast<const Color<T, 3>*>(&r);
}

template <typename T>
inline T& Color<T, 4>::operator[](const size_t i)
{
    assert(i < Components);
    return (&r)[i];
}

template <typename T>
inline const T& Color<T, 4>::operator[](const size_t i) const
{
    assert(i < Components);
    return (&r)[i];
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_IMAGE_COLOR_H
