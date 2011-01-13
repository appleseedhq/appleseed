
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

#ifndef APPLESEED_FOUNDATION_MATH_VECTOR_H
#define APPLESEED_FOUNDATION_MATH_VECTOR_H

// appleseed.foundation headers.
#include "foundation/math/scalar.h"

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>

namespace foundation
{

//
// N-dimensional vector class of arbitrary type.
//

template <typename T, size_t N>
class Vector
{
  public:
    // Types.
    typedef T ValueType;
    typedef Vector<T, N> VectorType;

    // Dimension.
    static const size_t Dimension = N;

    // Constructors.
    Vector();                                   // leave all components uninitialized
    explicit Vector(const ValueType* rhs);      // initialize with array of N scalars
    explicit Vector(const ValueType val);       // set all components to 'val'

    // Construct a vector from another vector of a different type.
    template <typename U>
    explicit Vector(const Vector<U, N>& rhs);

    // Unchecked array subscripting.
    ValueType& operator[](const size_t i);
    const ValueType& operator[](const size_t i) const;

  private:
    // Vector components.
    ValueType m_comp[N];
};

// Exact inequality and equality tests.
template <typename T, size_t N> bool operator!=(const Vector<T, N>& lhs, const Vector<T, N>& rhs);
template <typename T, size_t N> bool operator==(const Vector<T, N>& lhs, const Vector<T, N>& rhs);

// Approximate equality tests.
template <typename T, size_t N> bool feq(const Vector<T, N>& lhs, const Vector<T, N>& rhs);
template <typename T, size_t N> bool feq(const Vector<T, N>& lhs, const Vector<T, N>& rhs, const T eps);

// Approximate zero tests.
template <typename T, size_t N> bool fz(const Vector<T, N>& v);
template <typename T, size_t N> bool fz(const Vector<T, N>& v, const T eps);

// Vector arithmetic.
template <typename T, size_t N> Vector<T, N>  operator+ (const Vector<T, N>& lhs, const Vector<T, N>& rhs);
template <typename T, size_t N> Vector<T, N>  operator- (const Vector<T, N>& lhs, const Vector<T, N>& rhs);
template <typename T, size_t N> Vector<T, N>  operator- (const Vector<T, N>& lhs);
template <typename T, size_t N> Vector<T, N>  operator* (const Vector<T, N>& lhs, const T rhs);
template <typename T, size_t N> Vector<T, N>  operator* (const T lhs, const Vector<T, N>& rhs);
template <typename T, size_t N> Vector<T, N>  operator/ (const Vector<T, N>& lhs, const T rhs);
template <typename T, size_t N> Vector<T, N>  operator* (const Vector<T, N>& lhs, const Vector<T, N>& rhs);
template <typename T, size_t N> Vector<T, N>  operator/ (const Vector<T, N>& lhs, const Vector<T, N>& rhs);
template <typename T, size_t N> Vector<T, N>& operator+=(Vector<T, N>& lhs, const Vector<T, N>& rhs);
template <typename T, size_t N> Vector<T, N>& operator-=(Vector<T, N>& lhs, const Vector<T, N>& rhs);
template <typename T, size_t N> Vector<T, N>& operator*=(Vector<T, N>& lhs, const T rhs);
template <typename T, size_t N> Vector<T, N>& operator/=(Vector<T, N>& lhs, const T rhs);
template <typename T, size_t N> Vector<T, N>& operator*=(Vector<T, N>& lhs, const Vector<T, N>& rhs);
template <typename T, size_t N> Vector<T, N>& operator/=(Vector<T, N>& lhs, const Vector<T, N>& rhs);

// Dot product.
template <typename T, size_t N> T dot(const Vector<T, N>& lhs, const Vector<T, N>& rhs);

// Vector square norm, norm and normalization.
template <typename T, size_t N> T square_norm(const Vector<T, N>& v);
template <typename T, size_t N> T norm(const Vector<T, N>& v);
template <typename T, size_t N> Vector<T, N> normalize(const Vector<T, N>& v);

// Return true if a vector is normalized (unit-length), false otherwise.
template <typename T, size_t N> bool is_normalized(const Vector<T, N>& v);
template <typename T, size_t N> bool is_normalized(const Vector<T, N>& v, const T eps);

// Return n if dot(n, i) < 0, -n otherwise.
template <typename T, size_t N>
Vector<T, N> faceforward(
    const Vector<T, N>& n,
    const Vector<T, N>& i);

// Return the reflection vector given an incoming vector i and a unit-length normal vector n.
template <typename T, size_t N>
Vector<T, N> reflect(
    const Vector<T, N>& i,
    const Vector<T, N>& n);

// Return the refracted vector given a unit-length incoming vector i, a unit-length normal
// vector n and a relative index of refraction eta.  eta is the ratio of the index of
// refraction in the volume containing the incoming vector to that of the volume being entered.
template <typename T, size_t N>
Vector<T, N> refract(
    const Vector<T, N>& i,
    const Vector<T, N>& n,
    const T             eta);

// Clamp the argument to [min, max].
template <typename T, size_t N>
Vector<T, N> clamp(
    const Vector<T, N>& v,
    const T             min,
    const T             max);

// Clamp the argument to [0,1].
template <typename T, size_t N> Vector<T, N> saturate(const Vector<T, N>& v);

// Return the smallest or largest signed component of a vector.
template <typename T, size_t N> T min_value(const Vector<T, N>& v);
template <typename T, size_t N> T max_value(const Vector<T, N>& v);

// Return the index of the smallest or largest signed component of a vector.
template <typename T, size_t N> size_t min_index(const Vector<T, N>& v);
template <typename T, size_t N> size_t max_index(const Vector<T, N>& v);

// Return the index of the smallest or largest component of a vector, in absolute value.
template <typename T, size_t N> size_t min_abs_index(const Vector<T, N>& v);
template <typename T, size_t N> size_t max_abs_index(const Vector<T, N>& v);


//
// 2-dimensional vector class of arbitrary type.
//

template <typename T>
class Vector<T, 2>
{
  public:
    // Types.
    typedef T ValueType;
    typedef Vector<T, 2> VectorType;

    // Dimension.
    static const size_t Dimension = 2;

    // Vector components.
    ValueType x, y;

    // Constructors.
    Vector();                                   // leave all components uninitialized
    explicit Vector(const ValueType* rhs);      // initialize with array of 2 scalars
    explicit Vector(const ValueType val);       // set all components to 'val'
    Vector(                                     // set individual components
        const ValueType x,
        const ValueType y);

    // Construct a vector from another vector of a different type.
    template <typename U>
    explicit Vector(const Vector<U, 2>& rhs);

    // Unchecked array subscripting.
    ValueType& operator[](const size_t i);
    const ValueType& operator[](const size_t i) const;
};

// Determinant of the 2D matrix whose first column is lhs and second column is rhs.
template <typename T> T det(const Vector<T, 2>& lhs, const Vector<T, 2>& rhs);


//
// 3-dimensional vector class of arbitrary type.
//

template <typename T>
class Vector<T, 3>
{
  public:
    // Types.
    typedef T ValueType;
    typedef Vector<T, 3> VectorType;

    // Dimension.
    static const size_t Dimension = 3;

    // Vector components.
    ValueType x, y, z;

    // Constructors.
    Vector();                                   // leave all components uninitialized
    explicit Vector(const ValueType* rhs);      // initialize with array of 3 scalars
    explicit Vector(const ValueType val);       // set all components to 'val'
    Vector(                                     // set individual components
        const ValueType x,
        const ValueType y,
        const ValueType z);

    // Construct a vector from another vector of a different type.
    template <typename U>
    explicit Vector(const Vector<U, 3>& rhs);

    // Build a unit vector from two angles.
    static VectorType unit_vector(
        const ValueType theta,                  // angle with Y basis vector, in radians
        const ValueType phi);                   // angle with X basis vector, in radians
    static VectorType unit_vector(
        const ValueType cos_theta,              // cosine of angle with Y basis vector
        const ValueType sin_theta,              // sine of angle with Y basis vector
        const ValueType cos_phi,                // cosine of angle with X basis vector
        const ValueType sin_phi);               // sine of angle with X basis vector

    // Unchecked array subscripting.
    ValueType& operator[](const size_t i);
    const ValueType& operator[](const size_t i) const;
};

// Cross product.
template <typename T> Vector<T, 3> cross(const Vector<T, 3>& lhs, const Vector<T, 3>& rhs);


//
// 4-dimensional vector class of arbitrary type.
//

template <typename T>
class Vector<T, 4>
{
  public:
    // Types.
    typedef T ValueType;
    typedef Vector<T, 4> VectorType;

    // Dimension.
    static const size_t Dimension = 4;

    // Vector components.
    ValueType x, y, z, w;

    // Constructors.
    Vector();                                   // leave all components uninitialized
    explicit Vector(const ValueType* rhs);      // initialize with array of 4 scalars
    explicit Vector(const ValueType val);       // set all components to 'val'
    Vector(                                     // set individual components
        const ValueType x,
        const ValueType y,
        const ValueType z,
        const ValueType w);

    // Construct a vector from another vector of a different type.
    template <typename U>
    explicit Vector(const Vector<U, 4>& rhs);

    // Unchecked array subscripting.
    ValueType& operator[](const size_t i);
    const ValueType& operator[](const size_t i) const;
};


//
// Full specializations for 1D, 2D, 3D and 4D vectors of type int, float and double.
//

typedef Vector<int,    1> Vector1i;
typedef Vector<float,  1> Vector1f;
typedef Vector<double, 1> Vector1d;

typedef Vector<int,    2> Vector2i;
typedef Vector<float,  2> Vector2f;
typedef Vector<double, 2> Vector2d;

typedef Vector<int,    3> Vector3i;
typedef Vector<float,  3> Vector3f;
typedef Vector<double, 3> Vector3d;

typedef Vector<int,    4> Vector4i;
typedef Vector<float,  4> Vector4f;
typedef Vector<double, 4> Vector4d;


//
// N-dimensional vector implementation.
//

template <typename T, size_t N>
inline Vector<T, N>::Vector()
{
}

template <typename T, size_t N>
inline Vector<T, N>::Vector(const ValueType* rhs)
{
    assert(rhs);

    for (size_t i = 0; i < N; ++i)
        m_comp[i] = rhs[i];
}

template <typename T, size_t N>
inline Vector<T, N>::Vector(const ValueType val)
{
    for (size_t i = 0; i < N; ++i)
        m_comp[i] = val;
}

template <typename T, size_t N>
template <typename U>
inline Vector<T, N>::Vector(const Vector<U, N>& rhs)
{
    for (size_t i = 0; i < N; ++i)
        m_comp[i] = static_cast<ValueType>(rhs[i]);
}

template <typename T, size_t N>
inline T& Vector<T, N>::operator[](const size_t i)
{
    assert(i < Dimension);
    return m_comp[i];
}

template <typename T, size_t N>
inline const T& Vector<T, N>::operator[](const size_t i) const
{
    assert(i < Dimension);
    return m_comp[i];
}

template <typename T, size_t N>
inline bool operator!=(const Vector<T, N>& lhs, const Vector<T, N>& rhs)
{
    for (size_t i = 0; i < N; ++i)
        if (lhs[i] != rhs[i])
            return true;

    return false;
}

template <typename T, size_t N>
inline bool operator==(const Vector<T, N>& lhs, const Vector<T, N>& rhs)
{
    return !(lhs != rhs);
}

template <typename T, size_t N>
inline bool feq(const Vector<T, N>& lhs, const Vector<T, N>& rhs)
{
    for (size_t i = 0; i < N; ++i)
        if (!feq(lhs[i], rhs[i]))
            return false;

    return true;
}

template <typename T, size_t N>
inline bool feq(const Vector<T, N>& lhs, const Vector<T, N>& rhs, const T eps)
{
    for (size_t i = 0; i < N; ++i)
        if (!feq(lhs[i], rhs[i], eps))
            return false;

    return true;
}

template <typename T, size_t N>
inline bool fz(const Vector<T, N>& v)
{
    for (size_t i = 0; i < N; ++i)
        if (!fz(v[i]))
            return false;

    return true;
}

template <typename T, size_t N>
inline bool fz(const Vector<T, N>& v, const T eps)
{
    for (size_t i = 0; i < N; ++i)
        if (!fz(v[i], eps))
            return false;

    return true;
}

template <typename T, size_t N>
inline Vector<T, N> operator+(const Vector<T, N>& lhs, const Vector<T, N>& rhs)
{
    Vector<T, N> vec;

    for (size_t i = 0; i < N; ++i)
        vec[i] = lhs[i] + rhs[i];

    return vec;
}

template <typename T, size_t N>
inline Vector<T, N> operator-(const Vector<T, N>& lhs, const Vector<T, N>& rhs)
{
    Vector<T, N> vec;

    for (size_t i = 0; i < N; ++i)
        vec[i] = lhs[i] - rhs[i];

    return vec;
}

template <typename T, size_t N>
inline Vector<T, N> operator-(const Vector<T, N>& lhs)
{
    Vector<T, N> vec;

    for (size_t i = 0; i < N; ++i)
        vec[i] = -lhs[i];

    return vec;
}

template <typename T, size_t N>
inline Vector<T, N> operator*(const Vector<T, N>& lhs, const T rhs)
{
    Vector<T, N> vec;

    for (size_t i = 0; i < N; ++i)
        vec[i] = lhs[i] * rhs;

    return vec;
}

template <typename T, size_t N>
inline Vector<T, N> operator*(const T lhs, const Vector<T, N>& rhs)
{
    return rhs * lhs;
}

template <typename T, size_t N>
inline Vector<T, N> operator/(const Vector<T, N>& lhs, const T rhs)
{
    return lhs * (T(1.0) / rhs);
}

template <typename T, size_t N>
inline Vector<T, N> operator*(const Vector<T, N>& lhs, const Vector<T, N>& rhs)
{
    Vector<T, N> vec;

    for (size_t i = 0; i < N; ++i)
        vec[i] = lhs[i] * rhs[i];

    return vec;
}

template <typename T, size_t N>
inline Vector<T, N> operator/(const Vector<T, N>& lhs, const Vector<T, N>& rhs)
{
    Vector<T, N> vec;

    for (size_t i = 0; i < N; ++i)
        vec[i] = lhs[i] / rhs[i];

    return vec;
}

template <typename T, size_t N>
inline Vector<T, N>& operator+=(Vector<T, N>& lhs, const Vector<T, N>& rhs)
{
    for (size_t i = 0; i < N; ++i)
        lhs[i] += rhs[i];

    return lhs;
}

template <typename T, size_t N>
inline Vector<T, N>& operator-=(Vector<T, N>& lhs, const Vector<T, N>& rhs)
{
    for (size_t i = 0; i < N; ++i)
        lhs[i] -= rhs[i];

    return lhs;
}

template <typename T, size_t N>
inline Vector<T, N>& operator*=(Vector<T, N>& lhs, const T rhs)
{
    for (size_t i = 0; i < N; ++i)
        lhs[i] *= rhs;

    return lhs;
}

template <typename T, size_t N>
inline Vector<T, N>& operator/=(Vector<T, N>& lhs, const T rhs)
{
    return lhs *= (T(1.0) / rhs);
}

template <typename T, size_t N>
inline Vector<T, N>& operator*=(Vector<T, N>& lhs, const Vector<T, N>& rhs)
{
    for (size_t i = 0; i < N; ++i)
        lhs[i] *= rhs[i];

    return lhs;
}

template <typename T, size_t N>
inline Vector<T, N>& operator/=(Vector<T, N>& lhs, const Vector<T, N>& rhs)
{
    for (size_t i = 0; i < N; ++i)
        lhs[i] /= rhs[i];

    return lhs;
}

template <typename T, size_t N>
inline T dot(const Vector<T, N>& lhs, const Vector<T, N>& rhs)
{
    T result = T(0.0);

    for (size_t i = 0; i < N; ++i)
        result += lhs[i] * rhs[i];

    return result;
}

template <typename T, size_t N>
inline T square_norm(const Vector<T, N>& v)
{
    return dot(v, v);
}

template <typename T, size_t N>
inline T norm(const Vector<T, N>& v)
{
    return std::sqrt(square_norm(v));
}

template <typename T, size_t N>
inline Vector<T, N> normalize(const Vector<T, N>& v)
{
    return v / norm(v);
}

template <typename T, size_t N>
inline bool is_normalized(const Vector<T, N>& v)
{
    return feq(square_norm(v), T(1.0));
}

template <typename T, size_t N>
inline bool is_normalized(const Vector<T, N>& v, const T eps)
{
    return feq(square_norm(v), T(1.0), eps);
}

template <typename T, size_t N>
inline Vector<T, N> faceforward(
    const Vector<T, N>& n,
    const Vector<T, N>& i)
{
    return dot(n, i) < T(0.0) ? n : -n;
}

template <typename T, size_t N>
inline Vector<T, N> reflect(
    const Vector<T, N>& i,
    const Vector<T, N>& n)
{
    assert(is_normalized(n));

    const T dot_in = dot(i, n);
    return (dot_in + dot_in) * n - i;
}

template <typename T, size_t N>
inline Vector<T, N> refract(
    const Vector<T, N>& i,
    const Vector<T, N>& n,
    const T             eta)
{
    //
    // Reference: http://en.wikipedia.org/wiki/Snell's_law
    //

    assert(is_normalized(i));
    assert(is_normalized(n));

    const T cos_theta_i = dot(i, n);
    const T sin_theta_i2 = T(1.0) - cos_theta_i * cos_theta_i;
    const T cos_theta_t2 = T(1.0) - eta * eta * sin_theta_i2;

    // Handle total internal reflection.
    if (cos_theta_t2 < T(0.0))
        return Vector<T, N>(T(0.0));

    const T cos_theta_t = std::sqrt(cos_theta_t2);

    return
        cos_theta_i > T(0.0)
            ? (eta * cos_theta_i - cos_theta_t) * n - eta * i
            : (eta * cos_theta_i + cos_theta_t) * n - eta * i;
}

template <typename T, size_t N>
inline Vector<T, N> clamp(
    const Vector<T, N>& v,
    const T             min,
    const T             max)
{
    Vector<T, N> vec;

    for (size_t i = 0; i < N; ++i)
        vec[i] = clamp(v[i], min, max);

    return vec;
}

template <typename T, size_t N>
inline Vector<T, N> saturate(const Vector<T, N>& v)
{
    Vector<T, N> vec;

    for (size_t i = 0; i < N; ++i)
        vec[i] = saturate(v[i]);

    return vec;
}

template <typename T, size_t N>
inline T min_value(const Vector<T, N>& v)
{
    T value = v[0];

    for (size_t i = 1; i < N; ++i)
    {
        if (value > v[i])
            value = v[i];
    }

    return value;
}

template <typename T, size_t N>
inline T max_value(const Vector<T, N>& v)
{
    T value = v[0];

    for (size_t i = 1; i < N; ++i)
    {
        if (value < v[i])
            value = v[i];
    }

    return value;
}

template <typename T, size_t N>
inline size_t min_index(const Vector<T, N>& v)
{
    size_t index = 0;
    T value = v[0];

    for (size_t i = 1; i < N; ++i)
    {
        const T x = v[i];
        if (value > x)
        {
            value = x;
            index = i;
        }
    }

    return index;
}

template <typename T, size_t N>
inline size_t max_index(const Vector<T, N>& v)
{
    size_t index = 0;
    T value = v[0];

    for (size_t i = 1; i < N; ++i)
    {
        const T x = v[i];
        if (value < x)
        {
            value = x;
            index = i;
        }
    }

    return index;
}

template <typename T, size_t N>
inline size_t min_abs_index(const Vector<T, N>& v)
{
    size_t index = 0;
    T value = std::abs(v[0]);

    for (size_t i = 1; i < N; ++i)
    {
        const T x = std::abs(v[i]);
        if (value > x)
        {
            value = x;
            index = i;
        }
    }

    return index;
}

template <typename T, size_t N>
inline size_t max_abs_index(const Vector<T, N>& v)
{
    size_t index = 0;
    T value = std::abs(v[0]);

    for (size_t i = 1; i < N; ++i)
    {
        const T x = std::abs(v[i]);
        if (value < x)
        {
            value = x;
            index = i;
        }
    }

    return index;
}


//
// 2D vector implementation.
//

template <typename T>
inline Vector<T, 2>::Vector()
{
}

template <typename T>
inline Vector<T, 2>::Vector(const ValueType* rhs)
{
    assert(rhs);
    x = rhs[0];
    y = rhs[1];
}

template <typename T>
inline Vector<T, 2>::Vector(const ValueType val)
  : x(val)
  , y(val)
{
}

template <typename T>
inline Vector<T, 2>::Vector(
    const ValueType x_,
    const ValueType y_)
  : x(x_)
  , y(y_)
{
}

template <typename T>
template <typename U>
inline Vector<T, 2>::Vector(const Vector<U, 2>& rhs)
  : x(static_cast<ValueType>(rhs.x))
  , y(static_cast<ValueType>(rhs.y))
{
}

template <typename T>
inline T& Vector<T, 2>::operator[](const size_t i)
{
    assert(i < Dimension);
    return (&x)[i];
}

template <typename T>
inline const T& Vector<T, 2>::operator[](const size_t i) const
{
    assert(i < Dimension);
    return (&x)[i];
}

template <typename T>
inline T det(const Vector<T, 2>& lhs, const Vector<T, 2>& rhs)
{
    return lhs.x * rhs.y - lhs.y * rhs.x;
}


//
// 3D vector implementation.
//

template <typename T>
inline Vector<T, 3>::Vector()
{
}

template <typename T>
inline Vector<T, 3>::Vector(const ValueType* rhs)
{
    assert(rhs);
    x = rhs[0];
    y = rhs[1];
    z = rhs[2];
}

template <typename T>
inline Vector<T, 3>::Vector(const ValueType val)
  : x(val)
  , y(val)
  , z(val)
{
}

template <typename T>
inline Vector<T, 3>::Vector(
    const ValueType x_,
    const ValueType y_,
    const ValueType z_)
  : x(x_)
  , y(y_)
  , z(z_)
{
}

template <typename T>
template <typename U>
inline Vector<T, 3>::Vector(const Vector<U, 3>& rhs)
  : x(static_cast<ValueType>(rhs.x))
  , y(static_cast<ValueType>(rhs.y))
  , z(static_cast<ValueType>(rhs.z))
{
}

template <typename T>
inline Vector<T, 3> Vector<T, 3>::unit_vector(
    const ValueType theta,
    const ValueType phi)
{
    const ValueType sin_theta = std::sin(theta);
    return Vector<T, 3>(
        std::cos(phi) * sin_theta,
        std::cos(theta),
        std::sin(phi) * sin_theta);
}

template <typename T>
inline Vector<T, 3> Vector<T, 3>::unit_vector(
    const ValueType cos_theta,
    const ValueType sin_theta,
    const ValueType cos_phi,
    const ValueType sin_phi)
{
    return Vector<T, 3>(
        cos_phi * sin_theta,
        cos_theta,
        sin_phi * sin_theta);
}

template <typename T>
inline T& Vector<T, 3>::operator[](const size_t i)
{
    assert(i < Dimension);
    return (&x)[i];
}

template <typename T>
inline const T& Vector<T, 3>::operator[](const size_t i) const
{
    assert(i < Dimension);
    return (&x)[i];
}

template <typename T>
inline Vector<T, 3> cross(const Vector<T, 3>& lhs, const Vector<T, 3>& rhs)
{
    Vector<T, 3> vec;
    vec.x = lhs.y * rhs.z - rhs.y * lhs.z;
    vec.y = lhs.z * rhs.x - rhs.z * lhs.x;
    vec.z = lhs.x * rhs.y - rhs.x * lhs.y;
    return vec;
}


//
// 4D vector implementation.
//

template <typename T>
inline Vector<T, 4>::Vector()
{
}

template <typename T>
inline Vector<T, 4>::Vector(const ValueType* rhs)
{
    assert(rhs);
    x = rhs[0];
    y = rhs[1];
    z = rhs[2];
    w = rhs[3];
}

template <typename T>
inline Vector<T, 4>::Vector(const ValueType val)
  : x(val)
  , y(val)
  , z(val)
  , w(val)
{
}

template <typename T>
inline Vector<T, 4>::Vector(
    const ValueType x_,
    const ValueType y_,
    const ValueType z_,
    const ValueType w_)
  : x(x_)
  , y(y_)
  , z(z_)
  , w(w_)
{
}

template <typename T>
template <typename U>
inline Vector<T, 4>::Vector(const Vector<U, 4>& rhs)
  : x(static_cast<ValueType>(rhs.x))
  , y(static_cast<ValueType>(rhs.y))
  , z(static_cast<ValueType>(rhs.z))
  , w(static_cast<ValueType>(rhs.w))
{
}

template <typename T>
inline T& Vector<T, 4>::operator[](const size_t i)
{
    assert(i < Dimension);
    return (&x)[i];
}

template <typename T>
inline const T& Vector<T, 4>::operator[](const size_t i) const
{
    assert(i < Dimension);
    return (&x)[i];
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_VECTOR_H
