
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
#include "foundation/hash/hash.h"
#include "foundation/math/fp.h"
#include "foundation/math/matrix.h"
#include "foundation/math/scalar.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/poison.h"

// Imath headers.
#ifdef APPLESEED_ENABLE_IMATH_INTEROP
#include "foundation/platform/_beginexrheaders.h"
#include "Imath/ImathColor.h"
#include "foundation/platform/_endexrheaders.h"
#endif

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>
#include <cstdint>

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
#if APPLESEED_COMPILER_CXX_DEFAULTED_FUNCTIONS
    Color() = default;                          // leave all components uninitialized
#else
    Color() {}                                  // leave all components uninitialized
#endif
    explicit Color(const ValueType val);        // set all components to `val`

    // Construct a color from another color of a different type.
    template <typename U>
    explicit Color(const Color<U, N>& rhs);

    // Construct a color from an array of N scalars.
    static Color from_array(const ValueType* rhs);

    // Set all components to a given value.
    void set(const ValueType val);

    // Unchecked array subscripting.
    ValueType& operator[](const size_t i);
    const ValueType& operator[](const size_t i) const;

  private:
    // Color components.
    ValueType m_comp[N];
};

// Poisoning.
template <typename T, size_t N>
class PoisonImpl<Color<T, N>>
{
  public:
    static void do_poison(Color<T, N>& c);
};

// Exact inequality and equality tests.
template <typename T, size_t N> bool operator!=(const Color<T, N>& lhs, const Color<T, N>& rhs);
template <typename T, size_t N> bool operator==(const Color<T, N>& lhs, const Color<T, N>& rhs);

// Return whether all components of a color are exactly zero.
template <typename T, size_t N> bool is_zero(const Color<T, N>& c);

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

// Component-wise reciprocal.
template <typename T, size_t N> Color<T, N> rcp(const Color<T, N>& c);

// Return whether all components of a color are in [0,1].
template <typename T, size_t N> bool is_saturated(const Color<T, N>& c);

// Clamp the argument to [0,1].
template <typename T, size_t N> APPLESEED_NODISCARD Color<T, N> saturate(const Color<T, N>& c);

// Clamp the argument to [min, max].
template <typename T, size_t N> APPLESEED_NODISCARD Color<T, N> clamp(const Color<T, N>& c, const T min, const T max);

// Clamp the argument to [min, +infinity).
template <typename T, size_t N> APPLESEED_NODISCARD Color<T, N> clamp_low(const Color<T, N>& c, const T min);

// Clamp the argument to (-infinity, max].
template <typename T, size_t N> APPLESEED_NODISCARD Color<T, N> clamp_high(const Color<T, N>& c, const T max);

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

// Return the sum of the components of a color.
template <typename T, size_t N> T sum_value(const Color<T, N>& c);

// Return the average value of a color.
template <typename T, size_t N> T average_value(const Color<T, N>& c);

// Return true if a color contains at least one NaN value.
template <typename T, size_t N> bool has_nan(const Color<T, N>& c);

// Return true if all components of a color are finite (not NaN, not infinite).
template <typename T, size_t N> bool is_finite(const Color<T, N>& c);

// Return true if all components of a color are finite (not NaN, not infinite) and non-negative.
template <typename T, size_t N> bool is_finite_non_neg(const Color<T, N>& c);


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
#if APPLESEED_COMPILER_CXX_DEFAULTED_FUNCTIONS
    Color() = default;                          // leave all components uninitialized
#else
    Color() {}                                  // leave all components uninitialized
#endif
    explicit Color(const ValueType val);        // set all components to `val`
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

    // Construct a color from an array of 3 scalars.
    static Color from_array(const ValueType* rhs);

    // Set all components to a given value.
    void set(const ValueType val);

    // Unchecked array subscripting.
    ValueType& operator[](const size_t i);
    const ValueType& operator[](const size_t i) const;
};

// Matrix-color multiplication.
template <typename T>
Color<T, 3> operator*(
    const Matrix<T, 3, 3>&  m,
    const Color<T, 3>&      c);

template <typename T>
Color<T, 3> operator*(
    const Color<T, 3>&      c,
    const Matrix<T, 3, 3>&  m);


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
#if APPLESEED_COMPILER_CXX_DEFAULTED_FUNCTIONS
    Color() = default;                          // leave all components uninitialized
#else
    Color() {}                                  // leave all components uninitialized
#endif
    explicit Color(const ValueType val);        // set all components to `val`
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

    // Construct a color from an array of 4 scalars.
    static Color from_array(const ValueType* rhs);

    // Set all components to a given value.
    void set(const ValueType val);

    // Access the color as a 3-component color.
    Color<ValueType, 3>& rgb();
    const Color<ValueType, 3>& rgb() const;

    // Unchecked array subscripting.
    ValueType& operator[](const size_t i);
    const ValueType& operator[](const size_t i) const;

    //
    // References and interesting resources on alpha compositing:
    //
    //   http://keithp.com/~keithp/porterduff/p253-porter.pdf
    //   http://en.wikipedia.org/wiki/Alpha_compositing
    //   http://dvd-hq.info/alpha_matting.php
    //   http://my.opera.com/emoller/blog/2012/08/28/alpha-blending
    //

    // Apply/undo alpha premultiplication in place.
    void premultiply_in_place();
    void unpremultiply_in_place();

    // Retrieve premultiplied/unpremultiplied copies of this color.
    Color premultiplied() const;
    Color unpremultiplied() const;
};


//
// Full specializations for colors of type int, float and double.
//

typedef Color<std::uint8_t, 3> Color3b;
typedef Color<float,        3> Color3f;
typedef Color<double,       3> Color3d;

typedef Color<std::uint8_t, 4> Color4b;
typedef Color<float,        4> Color4f;
typedef Color<double,       4> Color4d;


//
// Functions based on colors.
//

// Compute a color from a given integer.
template <typename T, typename Int>
Color<T, 3> integer_to_color3(const Int i);

// Compute the square L2 distance between two linear RGB colors.
template <typename T>
T square_distance(
    const Color<T, 3>& c1,
    const Color<T, 3>& c2);


//
// N-dimensional color implementation.
//

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
inline Color<T, N> Color<T, N>::from_array(const ValueType* rhs)
{
    assert(rhs);

    Color result;

    for (size_t i = 0; i < N; ++i)
        result.m_comp[i] = rhs[i];

    return result;
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
void PoisonImpl<Color<T, N>>::do_poison(Color<T, N>& c)
{
    for (size_t i = 0; i < N; ++i)
        always_poison(c[i]);
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
inline bool is_zero(const Color<T, N>& c)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (c[i] != T(0.0))
            return false;
    }

    return true;
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
inline Color<T, N> rcp(const Color<T, N>& c)
{
    Color<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = T(1.0) / c[i];

    return result;
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
inline T sum_value(const Color<T, N>& c)
{
    T sum = c[0];

    for (size_t i = 1; i < N; ++i)
        sum += c[i];

    return sum;
}

template <typename T, size_t N>
inline T average_value(const Color<T, N>& c)
{
    return sum_value(c) / N;
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

template <typename T, size_t N>
inline bool is_finite(const Color<T, N>& c)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (!FP<T>::is_finite(c[i]))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline bool is_finite_non_neg(const Color<T, N>& c)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (!FP<T>::is_finite_non_neg(c[i]))
            return false;
    }

    return true;
}


//
// RGB color implementation.
//

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
inline Color<T, 3> Color<T, 3>::from_array(const ValueType* rhs)
{
    assert(rhs);
    return Color(rhs[0], rhs[1], rhs[2]);
}

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

template <typename T>
inline Color<T, 3> operator*(
    const Matrix<T, 3, 3>&  m,
    const Color<T, 3>&      c)
{
    Color<T, 3> res;

    res[0] = m[0] * c[0] + m[1] * c[1] + m[2] * c[2];
    res[1] = m[3] * c[0] + m[4] * c[1] + m[5] * c[2];
    res[2] = m[6] * c[0] + m[7] * c[1] + m[8] * c[2];

    return res;
}

template <typename T>
inline Color<T, 3> operator*(
    const Color<T, 3>&      c,
    const Matrix<T, 3, 3>&  m)
{
    Color<T, 3> res;

    res[0] = c[0] * m[0] + c[1] * m[3] + c[2] * m[6];
    res[1] = c[0] * m[1] + c[1] * m[4] + c[2] * m[7];
    res[2] = c[0] * m[2] + c[1] * m[5] + c[2] * m[8];

    return res;
}


//
// RGBA color implementation.
//

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
inline Color<T, 4> Color<T, 4>::from_array(const ValueType* rhs)
{
    assert(rhs);
    return Color(rhs[0], rhs[1], rhs[2], rhs[3]);
}

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

template <typename T>
inline void Color<T, 4>::premultiply_in_place()
{
    r *= a;
    g *= a;
    b *= a;
}

template <typename T>
inline void Color<T, 4>::unpremultiply_in_place()
{
    if (a != T(0.0))
    {
        const T rcp_a = T(1.0) / a;
        r *= rcp_a;
        g *= rcp_a;
        b *= rcp_a;
    }
}

template <typename T>
inline Color<T, 4> Color<T, 4>:: premultiplied() const
{
    return
        Color(
            r * a,
            g * a,
            b * a,
            a);
}

template <typename T>
inline Color<T, 4> Color<T, 4>::unpremultiplied() const
{
    if (a != T(0.0))
    {
        const T rcp_a = T(1.0) / a;
        return
            Color(
                r * rcp_a,
                g * rcp_a,
                b * rcp_a,
                a);
    }
    else return *this;
}


//
// Color functions implementation.
//

template <typename T, typename Int>
Color<T, 3> integer_to_color3(const Int i)
{
    const std::uint32_t u = static_cast<std::uint32_t>(i);    // keep the low 32 bits

    const std::uint32_t x = hash_uint32(u);
    const std::uint32_t y = hash_uint32(u + 1);
    const std::uint32_t z = hash_uint32(u + 2);

    return Color<T, 3>(
        static_cast<T>(x) * (1.0f / 4294967295.0f),
        static_cast<T>(y) * (1.0f / 4294967295.0f),
        static_cast<T>(z) * (1.0f / 4294967295.0f));
}

template <typename T>
inline T square_distance(
    const Color<T, 3>& c1,
    const Color<T, 3>& c2)
{
    return
        square(c1.r - c2.r) +
        square(c1.g - c2.g) +
        square(c1.b - c2.b);
}

}   // namespace foundation
