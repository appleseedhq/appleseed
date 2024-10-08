
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
#include "foundation/utility/poison.h"

// Imath headers.
#ifdef APPLESEED_ENABLE_IMATH_INTEROP
#include "foundation/platform/_beginexrheaders.h"
#include "Imath/ImathVec.h"
#include "foundation/platform/_endexrheaders.h"
#endif

// Standard headers.
#include <algorithm>
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
#if APPLESEED_COMPILER_CXX_DEFAULTED_FUNCTIONS
    Vector() = default;                         // leave all components uninitialized
#else
    Vector() {}                                 // leave all components uninitialized
#endif
    explicit Vector(const ValueType val);       // set all components to `val`

    // Construct a vector from another vector of a different type.
    template <typename U>
    explicit Vector(const Vector<U, N>& rhs);

    // Construct a vector from an array of N scalars.
    static VectorType from_array(const ValueType* rhs);

    // Unchecked array subscripting.
    ValueType& operator[](const size_t i);
    const ValueType& operator[](const size_t i) const;

  private:
    // Vector components.
    ValueType m_comp[N];
};

// Poisoning.
template <typename T, size_t N>
class PoisonImpl<Vector<T, N>>
{
  public:
    static void do_poison(Vector<T, N>& v);
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
template <typename T, size_t N> Vector<T, N> safe_normalize(const Vector<T, N>& v, const Vector<T, N>& fallback);
template <typename T, size_t N> Vector<T, N> safe_normalize(const Vector<T, N>& v);

// Bring the norm of a nearly-unit vector closer to 1 by performing a single Newton-Raphson step.
template <typename T, size_t N>
Vector<T, N> improve_normalization(const Vector<T, N>& v);

// Bring the norm of a nearly-unit vector closer to 1 by performing a set number of Newton-Raphson steps.
template <size_t Steps, typename T, size_t N>
Vector<T, N> improve_normalization(const Vector<T, N>& v);

// Return true if a vector is normalized (unit-length), false otherwise.
template <typename T, size_t N> bool is_normalized(const Vector<T, N>& v);
template <typename T, size_t N> bool is_normalized(const Vector<T, N>& v, const T eps);

// Return n if dot(n, i) < 0, -n otherwise.
template <typename T, size_t N>
Vector<T, N> faceforward(
    const Vector<T, N>& n,
    const Vector<T, N>& i);

// Return v if it is in the same hemisphere as ref, -v otherwise.
template <typename T, size_t N>
Vector<T, N> flip_to_same_hemisphere(
    const Vector<T, N>& v,
    const Vector<T, N>& ref);

// Return v with the component along n zeroed.
template <typename T, size_t N>
Vector<T, N> project(
    const Vector<T, N>& v,
    const Vector<T, N>& n);

// Return the reflection vector given an incoming vector i and a unit-length normal vector n.
template <typename T, size_t N>
Vector<T, N> reflect(
    const Vector<T, N>& i,
    const Vector<T, N>& n);

//
// Sets t to the refracted vector given a unit-length incoming vector i, a unit-length normal
// vector n and a relative index of refraction eta.  eta is the ratio of the index of
// refraction in the volume containing the incoming vector to that of the volume being entered.
// Returns false in the case of total internal reflection.
//
// Geometry:
//
//           i      n                      eta_i
//                                   eta = -----
//           ^      ^                      eta_t
//            \     |
//             \    |
//              \   |
//               \  |                IOR = eta_i
//                \ |
//                 \|
//   ---------------+---------------  interface
//                  |
//                  |
//                   |               IOR = eta_t
//                   |
//                    |
//                    |
//                    v
//
//      t = refract(i, n, eta_i / eta_t)
//

template <typename T, size_t N>
bool refract(
    const Vector<T, N>& i,
    const Vector<T, N>& n,
    const T             eta,
    Vector<T, N>&       t);

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

// Component-wise min/max of two vectors.
template <typename T, size_t N> Vector<T, N> component_wise_min(const Vector<T, N>& lhs, const Vector<T, N>& rhs);
template <typename T, size_t N> Vector<T, N> component_wise_max(const Vector<T, N>& lhs, const Vector<T, N>& rhs);


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
#if APPLESEED_COMPILER_CXX_DEFAULTED_FUNCTIONS
    Vector() = default;                         // leave all components uninitialized
#else
    Vector() {}                                 // leave all components uninitialized
#endif
    explicit Vector(const ValueType val);       // set all components to `val`
    Vector(                                     // set individual components
        const ValueType x,
        const ValueType y);

    // Construct a vector from another vector of a different type.
    template <typename U>
    explicit Vector(const Vector<U, 2>& rhs);

#ifdef APPLESEED_ENABLE_IMATH_INTEROP

    // Implicit construction from an Imath::Vec2.
    Vector(const Imath::Vec2<T>& rhs);

    // Reinterpret this vector as an Imath::Vec2.
    operator Imath::Vec2<T>&();
    operator const Imath::Vec2<T>&() const;

#endif

    // Construct a vector from an array of 2 scalars.
    static VectorType from_array(const ValueType* rhs);

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
#if APPLESEED_COMPILER_CXX_DEFAULTED_FUNCTIONS
    Vector() = default;                         // leave all components uninitialized
#else
    Vector() {}                                 // leave all components uninitialized
#endif
    explicit Vector(const ValueType val);       // set all components to `val`
    Vector(                                     // set individual components
        const ValueType x,
        const ValueType y,
        const ValueType z);

    // Construct a vector from another vector of a different type.
    template <typename U>
    explicit Vector(const Vector<U, 3>& rhs);

#ifdef APPLESEED_ENABLE_IMATH_INTEROP

    // Implicit construction from an Imath::Vec3.
    Vector(const Imath::Vec3<T>& rhs);

    // Reinterpret this vector as an Imath::Vec3.
    operator Imath::Vec3<T>&();
    operator const Imath::Vec3<T>&() const;

#endif

    // Construct a vector from an array of 3 scalars.
    static VectorType from_array(const ValueType* rhs);

    // Build a unit vector from two angles.
    static VectorType make_unit_vector(
        const ValueType theta,                  // angle with Y basis vector, in radians
        const ValueType phi);                   // angle with X basis vector, in radians
    static VectorType make_unit_vector(
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
#if APPLESEED_COMPILER_CXX_DEFAULTED_FUNCTIONS
    Vector() = default;                         // leave all components uninitialized
#else
    Vector() {}                                 // leave all components uninitialized
#endif
    explicit Vector(const ValueType val);       // set all components to `val`
    Vector(                                     // set individual components
        const ValueType x,
        const ValueType y,
        const ValueType z,
        const ValueType w);

    // Construct a vector from another vector of a different type.
    template <typename U>
    explicit Vector(const Vector<U, 4>& rhs);

    // Construct a vector from an array of 4 scalars.
    static VectorType from_array(const ValueType* rhs);

    // Unchecked array subscripting.
    ValueType& operator[](const size_t i);
    const ValueType& operator[](const size_t i) const;
};


//
// Full specializations for 1D, 2D, 3D and 4D vectors of type int, float and double.
//

typedef Vector<int,    1> Vector1i;
typedef Vector<size_t, 1> Vector1u;
typedef Vector<float,  1> Vector1f;
typedef Vector<double, 1> Vector1d;

typedef Vector<int,    2> Vector2i;
typedef Vector<size_t, 2> Vector2u;
typedef Vector<float,  2> Vector2f;
typedef Vector<double, 2> Vector2d;

typedef Vector<int,    3> Vector3i;
typedef Vector<size_t, 3> Vector3u;
typedef Vector<float,  3> Vector3f;
typedef Vector<double, 3> Vector3d;

typedef Vector<int,    4> Vector4i;
typedef Vector<size_t, 4> Vector4u;
typedef Vector<float,  4> Vector4f;
typedef Vector<double, 4> Vector4d;


//
// Utility class to find the Imath equivalent of a given foundation::Vector type.
//

#ifdef APPLESEED_ENABLE_IMATH_INTEROP

template <typename T, size_t N>
struct ImathVecEquivalent;          // no equivalent in the general case

template <typename T>
struct ImathVecEquivalent<T, 2>
{
    typedef Imath::Vec2<T> Type;
};

template <typename T>
struct ImathVecEquivalent<T, 3>
{
    typedef Imath::Vec3<T> Type;
};

#endif


//
// N-dimensional vector implementation.
//

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
inline Vector<T, N> Vector<T, N>::from_array(const ValueType* rhs)
{
    assert(rhs);

    Vector<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result.m_comp[i] = rhs[i];

    return result;
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
void PoisonImpl<Vector<T, N>>::do_poison(Vector<T, N>& v)
{
    for (size_t i = 0; i < N; ++i)
        always_poison(v[i]);
}

template <typename T, size_t N>
inline bool operator!=(const Vector<T, N>& lhs, const Vector<T, N>& rhs)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (lhs[i] != rhs[i])
            return true;
    }

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
    {
        if (!feq(lhs[i], rhs[i]))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline bool feq(const Vector<T, N>& lhs, const Vector<T, N>& rhs, const T eps)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (!feq(lhs[i], rhs[i], eps))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline bool fz(const Vector<T, N>& v)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (!fz(v[i]))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline bool fz(const Vector<T, N>& v, const T eps)
{
    for (size_t i = 0; i < N; ++i)
    {
        if (!fz(v[i], eps))
            return false;
    }

    return true;
}

template <typename T, size_t N>
inline Vector<T, N> operator+(const Vector<T, N>& lhs, const Vector<T, N>& rhs)
{
    Vector<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = lhs[i] + rhs[i];

    return result;
}

template <typename T, size_t N>
inline Vector<T, N> operator-(const Vector<T, N>& lhs, const Vector<T, N>& rhs)
{
    Vector<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = lhs[i] - rhs[i];

    return result;
}

template <typename T, size_t N>
inline Vector<T, N> operator-(const Vector<T, N>& lhs)
{
    Vector<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = -lhs[i];

    return result;
}

template <typename T, size_t N>
inline Vector<T, N> operator*(const Vector<T, N>& lhs, const T rhs)
{
    Vector<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = lhs[i] * rhs;

    return result;
}

template <typename T, size_t N>
inline Vector<T, N> operator*(const T lhs, const Vector<T, N>& rhs)
{
    return rhs * lhs;
}

template <typename T, size_t N>
inline Vector<T, N> operator/(const Vector<T, N>& lhs, const T rhs)
{
    Vector<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = lhs[i] / rhs;

    return result;
}

template <size_t N>
inline Vector<float, N> operator/(const Vector<float, N>& lhs, const float rhs)
{
    return lhs * (1.0f / rhs);
}

template <size_t N>
inline Vector<double, N> operator/(const Vector<double, N>& lhs, const double rhs)
{
    return lhs * (1.0 / rhs);
}

template <size_t N>
inline Vector<long double, N> operator/(const Vector<long double, N>& lhs, const long double rhs)
{
    return lhs * (1.0L / rhs);
}

template <typename T, size_t N>
inline Vector<T, N> operator*(const Vector<T, N>& lhs, const Vector<T, N>& rhs)
{
    Vector<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = lhs[i] * rhs[i];

    return result;
}

template <typename T, size_t N>
inline Vector<T, N> operator/(const Vector<T, N>& lhs, const Vector<T, N>& rhs)
{
    Vector<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = lhs[i] / rhs[i];

    return result;
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
    for (size_t i = 0; i < N; ++i)
        lhs[i] /= rhs;

    return lhs;
}

template <size_t N>
inline Vector<float, N>& operator/=(Vector<float, N>& lhs, const float rhs)
{
    return lhs *= 1.0f / rhs;
}

template <size_t N>
inline Vector<double, N>& operator/=(Vector<double, N>& lhs, const double rhs)
{
    return lhs *= 1.0 / rhs;
}

template <size_t N>
inline Vector<long double, N>& operator/=(Vector<long double, N>& lhs, const long double rhs)
{
    return lhs *= 1.0L / rhs;
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
    const T n = norm(v);
    assert(n > T(0.0));
    return v / n;
}

template <typename T, size_t N>
inline Vector<T, N> safe_normalize(const Vector<T, N>& v, const Vector<T, N>& fallback)
{
    assert(is_normalized(fallback));
    const T n = norm(v);
    return n > 0.0 ? v / n : fallback;
}

template <typename T, size_t N>
inline Vector<T, N> safe_normalize(const Vector<T, N>& v)
{
    Vector<T, N> result = v;

    const T n = norm(result);

    if (n > 0.0)
        result /= n;
    else result[0] = T(1.0);

    assert(is_normalized(result));

    return result;
}

template <typename T, size_t N>
inline Vector<T, N> improve_normalization(const Vector<T, N>& v)
{
    return improve_normalization<1, T, N>(v);
}

template <size_t Steps, typename T, size_t N>
inline Vector<T, N> improve_normalization(const Vector<T, N>& v)
{
    Vector<T, N> result = v;

    for (size_t i = 0; i < Steps; ++i)
        result *= (T(3.0) - square_norm(result)) * T(0.5);

    return result;
}

template <typename T, size_t N>
inline bool is_normalized(const Vector<T, N>& v)
{
    return feq(square_norm(v), T(1.0), make_eps<T>(1.0e-4f, 1.0e-5));
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
inline Vector<T, N> flip_to_same_hemisphere(
    const Vector<T, N>& v,
    const Vector<T, N>& ref)
{
    return dot(v, ref) < T(0.0) ? -v : v;
}

template <typename T, size_t N>
inline Vector<T, N> project(
    const Vector<T, N>& v,
    const Vector<T, N>& n)
{
    return v - dot(v, n) * n;
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
inline bool refract(
    const Vector<T, N>& i,
    const Vector<T, N>& n,
    const T             eta,
    Vector<T, N>&       t)
{
    //
    // Reference: http://en.wikipedia.org/wiki/Snell's_law
    //

    assert(is_normalized(i));
    assert(is_normalized(n));

    const T cos_theta_i = dot(i, n);
    assert(cos_theta_i >= T(0.0));

    const T sin_theta_i2 = T(1.0) - square(cos_theta_i);
    const T sin_theta_t2 = sin_theta_i2 * square(eta);
    const T cos_theta_t2 = T(1.0) - sin_theta_t2;

    if (cos_theta_t2 < T(0.0))
    {
        // Total internal reflection.
        return false;
    }

    const T cos_theta_t = std::sqrt(cos_theta_t2);

    t = (eta * cos_theta_i - cos_theta_t) * n - eta * i;
    assert(is_normalized(t));

    return true;
}

template <typename T, size_t N>
inline Vector<T, N> clamp(
    const Vector<T, N>& v,
    const T             min,
    const T             max)
{
    Vector<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = clamp(v[i], min, max);

    return result;
}

template <typename T, size_t N>
inline Vector<T, N> saturate(const Vector<T, N>& v)
{
    Vector<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = saturate(v[i]);

    return result;
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

template <typename T, size_t N>
inline Vector<T, N> component_wise_min(const Vector<T, N>& lhs, const Vector<T, N>& rhs)
{
    Vector<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = std::min(lhs[i], rhs[i]);

    return result;
}

template <typename T, size_t N>
inline Vector<T, N> component_wise_max(const Vector<T, N>& lhs, const Vector<T, N>& rhs)
{
    Vector<T, N> result;

    for (size_t i = 0; i < N; ++i)
        result[i] = std::max(lhs[i], rhs[i]);

    return result;
}


//
// 2D vector implementation.
//

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

#ifdef APPLESEED_ENABLE_IMATH_INTEROP

template <typename T>
inline Vector<T, 2>::Vector(const Imath::Vec2<T>& rhs)
  : x(rhs.x)
  , y(rhs.y)
{
}

template <typename T>
inline Vector<T, 2>::operator Imath::Vec2<T>&()
{
    return reinterpret_cast<Imath::Vec2<T>&>(*this);
}

template <typename T>
inline Vector<T, 2>::operator const Imath::Vec2<T>&() const
{
    return reinterpret_cast<const Imath::Vec2<T>&>(*this);
}

#endif

template <typename T>
inline Vector<T, 2> Vector<T, 2>::from_array(const ValueType* rhs)
{
    assert(rhs);
    return Vector(rhs[0], rhs[1]);
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

#ifdef APPLESEED_ENABLE_IMATH_INTEROP

template <typename T>
inline Vector<T, 3>::Vector(const Imath::Vec3<T>& rhs)
  : x(rhs.x)
  , y(rhs.y)
  , z(rhs.z)
{
}

template <typename T>
inline Vector<T, 3>::operator Imath::Vec3<T>&()
{
    return reinterpret_cast<Imath::Vec3<T>&>(*this);
}

template <typename T>
inline Vector<T, 3>::operator const Imath::Vec3<T>&() const
{
    return reinterpret_cast<const Imath::Vec3<T>&>(*this);
}

#endif

template <typename T>
inline Vector<T, 3> Vector<T, 3>::from_array(const ValueType* rhs)
{
    assert(rhs);
    return Vector(rhs[0], rhs[1], rhs[2]);
}

template <typename T>
inline Vector<T, 3> Vector<T, 3>::make_unit_vector(
    const ValueType theta,
    const ValueType phi)
{
    const ValueType sin_theta = std::sin(theta);

    return
        Vector<T, 3>(
            std::cos(phi) * sin_theta,
            std::cos(theta),
            std::sin(phi) * sin_theta);
}

template <typename T>
inline Vector<T, 3> Vector<T, 3>::make_unit_vector(
    const ValueType cos_theta,
    const ValueType sin_theta,
    const ValueType cos_phi,
    const ValueType sin_phi)
{
    return
        Vector<T, 3>(
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
    Vector<T, 3> result;
    result.x = lhs.y * rhs.z - rhs.y * lhs.z;
    result.y = lhs.z * rhs.x - rhs.z * lhs.x;
    result.z = lhs.x * rhs.y - rhs.x * lhs.y;
    return result;
}


//
// 4D vector implementation.
//

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
inline Vector<T, 4> Vector<T, 4>::from_array(const ValueType* rhs)
{
    assert(rhs);
    return Vector(rhs[0], rhs[1], rhs[2], rhs[3]);
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

}   // namespace foundation
