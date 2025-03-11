
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
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"

// Imath headers.
#ifdef APPLESEED_ENABLE_IMATH_INTEROP
#include "foundation/platform/_beginexrheaders.h"
#include "Imath/ImathQuat.h"
#include "foundation/platform/_endexrheaders.h"
#endif

// Standard headers.
#include <cassert>
#include <cmath>

namespace foundation
{

//
// Quaternion of arbitrary type.
//

template <typename T>
class Quaternion
{
  public:
    // Types.
    typedef T ValueType;
    typedef Vector<T, 3> VectorType;
    typedef Quaternion<T> QuaternionType;

    // Public members.
    ValueType   s;                                          // scalar part
    VectorType  v;                                          // vector part

    // Constructors.
#if APPLESEED_COMPILER_CXX_DEFAULTED_FUNCTIONS
    Quaternion() = default;                                 // leave all components uninitialized
#else
    Quaternion() {}                                         // leave all components uninitialized
#endif
    Quaternion(const ValueType s, const VectorType& v);     // initializing constructor

    // Construct a quaternion from another quaternion of a different type.
    template <typename U>
    Quaternion(const Quaternion<U>& rhs);

#ifdef APPLESEED_ENABLE_IMATH_INTEROP

    // Implicit construction from an Imath::Quat.
    Quaternion(const Imath::Quat<T>& rhs);

    // Reinterpret this quaternion as an Imath::Quat.
    operator Imath::Quat<T>&();
    operator const Imath::Quat<T>&() const;

#endif

    // Construct and return the identity quaternion.
    static QuaternionType make_identity();

    // Build a rotation quaternion from an axis and an angle.
    static QuaternionType make_rotation(
        const VectorType&   axis,                           // rotation axis, unit-length
        const ValueType     angle);                         // rotation angle, in radians

    // Build a rotation quaternion from two point on the unit sphere.
    static QuaternionType make_rotation(
        const VectorType&   from,
        const VectorType&   to);

    // Convert a unit quaternion to the axis-angle representation.
    // The returned axis is always unit-length.
    void extract_axis_angle(
        VectorType&         axis,
        ValueType&          angle) const;
};

// Exact inequality and equality tests.
template <typename T> bool operator!=(const Quaternion<T>& lhs, const Quaternion<T>& rhs);
template <typename T> bool operator==(const Quaternion<T>& lhs, const Quaternion<T>& rhs);

// Approximate equality tests.
template <typename T> bool feq(const Quaternion<T>& lhs, const Quaternion<T>& rhs);
template <typename T> bool feq(const Quaternion<T>& lhs, const Quaternion<T>& rhs, const T eps);

// Approximate zero tests.
template <typename T> bool fz(const Quaternion<T>& q);
template <typename T> bool fz(const Quaternion<T>& q, const T eps);

// Quaternion arithmetic.
template <typename T> Quaternion<T>  operator+ (const Quaternion<T>& lhs, const Quaternion<T>& rhs);
template <typename T> Quaternion<T>  operator- (const Quaternion<T>& lhs, const Quaternion<T>& rhs);
template <typename T> Quaternion<T>  operator- (const Quaternion<T>& lhs);
template <typename T> Quaternion<T>  operator* (const Quaternion<T>& lhs, const T rhs);
template <typename T> Quaternion<T>  operator* (const T lhs, const Quaternion<T>& rhs);
template <typename T> Quaternion<T>  operator/ (const Quaternion<T>& lhs, const T rhs);
template <typename T> Quaternion<T>& operator+=(Quaternion<T>& lhs, const Quaternion<T>& rhs);
template <typename T> Quaternion<T>& operator-=(Quaternion<T>& lhs, const Quaternion<T>& rhs);
template <typename T> Quaternion<T>& operator*=(Quaternion<T>& lhs, const T rhs);
template <typename T> Quaternion<T>& operator/=(Quaternion<T>& lhs, const T rhs);

// Dot product.
template <typename T> T dot(const Quaternion<T>& lhs, const Quaternion<T>& rhs);

// Quaternion products.
template <typename T> Quaternion<T>  operator* (const Quaternion<T>& lhs, const Quaternion<T>& rhs);
template <typename T> Quaternion<T>& operator*=(Quaternion<T>& lhs, const Quaternion<T>& rhs);

// Quaternion conjugate.
template <typename T> Quaternion<T> conjugate(const Quaternion<T>& q);

// Quaternion inverse.
template <typename T> Quaternion<T> inverse(const Quaternion<T>& q);

// Quaternion square norm, norm and normalization.
template <typename T> T square_norm(const Quaternion<T>& q);
template <typename T> T norm(const Quaternion<T>& q);
template <typename T> Quaternion<T> normalize(const Quaternion<T>& q);

// Return true if a quaternion is normalized (unit-length), false otherwise.
template <typename T> bool is_normalized(const Quaternion<T>& q);
template <typename T> bool is_normalized(const Quaternion<T>& q, const T eps);

// Spherical linear interpolation between two unit-length quaternions.
template <typename T> Quaternion<T> slerp(const Quaternion<T>& p, const Quaternion<T>& q, const T t);

//
// Approximate but faster spherical linear interpolation between two unit-length quaternions.
// See http://zeuxcg.org/2015/07/23/approximating-slerp/ for derivation and error analysis.
//
// Another interesting reference:
//
//   Slerping Clock Cycles
//   J.M.P. van Waveren
//   http://fabiensanglard.net/doom3_documentation/37725-293747_293747.pdf
//

template <typename T> Quaternion<T> fast_slerp(const Quaternion<T>& p, const Quaternion<T>& q, const T t);

// Rotation of a vector by a quaternion.
template <typename T> Vector<T, 3> rotate(const Quaternion<T>& q, const Vector<T, 3>& v);


//
// Full specializations for quaternions of type float and double.
//

typedef Quaternion<float> Quaternionf;
typedef Quaternion<double> Quaterniond;


//
// Quaternion class implementation.
//

template <typename T>
inline Quaternion<T>::Quaternion(const ValueType s_, const VectorType& v_)
  : s(s_)
  , v(v_)
{
}

template <typename T>
template <typename U>
inline Quaternion<T>::Quaternion(const Quaternion<U>& rhs)
  : s(static_cast<ValueType>(rhs.s))
  , v(rhs.v)
{
}

#ifdef APPLESEED_ENABLE_IMATH_INTEROP

template <typename T>
inline Quaternion<T>::Quaternion(const Imath::Quat<T>& rhs)
  : s(rhs.r)
  , v(rhs.v)
{
}

template <typename T>
inline Quaternion<T>::operator Imath::Quat<T>&()
{
    return reinterpret_cast<Imath::Quat<T>&>(*this);
}

template <typename T>
inline Quaternion<T>::operator const Imath::Quat<T>&() const
{
    return reinterpret_cast<const Imath::Quat<T>&>(*this);
}

#endif

template <typename T>
inline Quaternion<T> Quaternion<T>::make_identity()
{
    return QuaternionType(ValueType(1.0), VectorType(0.0));
}

template <typename T>
inline Quaternion<T> Quaternion<T>::make_rotation(
    const VectorType&   axis,
    const ValueType     angle)
{
    assert(is_normalized(axis));

    const ValueType half_angle = ValueType(0.5) * angle;

    return QuaternionType(
        std::cos(half_angle),
        std::sin(half_angle) * axis);
}

template <typename T>
inline Quaternion<T> Quaternion<T>::make_rotation(
    const VectorType&   from,
    const VectorType&   to)
{
    assert(is_normalized(from));
    assert(is_normalized(to));

    return normalize(Quaternion(dot(from, to) + T(1.0), cross(from, to)));
}

template <typename T>
inline void Quaternion<T>::extract_axis_angle(
    VectorType&         axis,
    ValueType&          angle) const
{
    assert(is_normalized(*this, make_eps<T>(1.0e-4f, 1.0e-6)));

    if (s < T(-1.0) || s > T(1.0))
    {
        angle = T(0.0);
        axis = VectorType(T(1.0), T(0.0), T(0.0));
    }
    else
    {
        angle = T(2.0) * std::acos(s);
        axis = safe_normalize(v);
    }
}

template <typename T>
inline bool operator!=(const Quaternion<T>& lhs, const Quaternion<T>& rhs)
{
    return lhs.s != rhs.s || lhs.v != rhs.v;
}

template <typename T>
inline bool operator==(const Quaternion<T>& lhs, const Quaternion<T>& rhs)
{
    return !(lhs != rhs);
}

template <typename T>
inline bool feq(const Quaternion<T>& lhs, const Quaternion<T>& rhs)
{
    return feq(lhs.s, rhs.s) && feq(lhs.v, rhs.v);
}

template <typename T>
inline bool feq(const Quaternion<T>& lhs, const Quaternion<T>& rhs, const T eps)
{
    return feq(lhs.s, rhs.s, eps) && feq(lhs.v, rhs.v, eps);
}

template <typename T>
inline bool fz(const Quaternion<T>& q)
{
    return fz(q.s) && fz(q.v);
}

template <typename T>
inline bool fz(const Quaternion<T>& q, const T eps)
{
    return fz(q.s, eps) && fz(q.v, eps);
}

template <typename T>
inline Quaternion<T> operator+(const Quaternion<T>& lhs, const Quaternion<T>& rhs)
{
    return Quaternion<T>(lhs.s + rhs.s, lhs.v + rhs.v);
}

template <typename T>
inline Quaternion<T> operator-(const Quaternion<T>& lhs, const Quaternion<T>& rhs)
{
    return Quaternion<T>(lhs.s - rhs.s, lhs.v - rhs.v);
}

template <typename T>
inline Quaternion<T> operator-(const Quaternion<T>& lhs)
{
    return Quaternion<T>(-lhs.s, -lhs.v);
}

template <typename T>
inline Quaternion<T> operator*(const Quaternion<T>& lhs, const T rhs)
{
    return Quaternion<T>(lhs.s * rhs, lhs.v * rhs);
}

template <typename T>
inline Quaternion<T> operator*(const T lhs, const Quaternion<T>& rhs)
{
    return rhs * lhs;
}

template <typename T>
inline Quaternion<T> operator/(const Quaternion<T>& lhs, const T rhs)
{
    return Quaternion<T>(lhs.s / rhs, lhs.v / rhs);
}

template <>
inline Quaternion<float> operator/(const Quaternion<float>& lhs, const float rhs)
{
    return lhs * (1.0f / rhs);
}

template <>
inline Quaternion<double> operator/(const Quaternion<double>& lhs, const double rhs)
{
    return lhs * (1.0 / rhs);
}

template <>
inline Quaternion<long double> operator/(const Quaternion<long double>& lhs, const long double rhs)
{
    return lhs * (1.0L / rhs);
}

template <typename T>
inline Quaternion<T>& operator+=(Quaternion<T>& lhs, const Quaternion<T>& rhs)
{
    lhs.s += rhs.s;
    lhs.v += rhs.v;
    return lhs;
}

template <typename T>
inline Quaternion<T>& operator-=(Quaternion<T>& lhs, const Quaternion<T>& rhs)
{
    lhs.s -= rhs.s;
    lhs.v -= rhs.v;
    return lhs;
}

template <typename T>
inline Quaternion<T>& operator*=(Quaternion<T>& lhs, const T rhs)
{
    lhs.s *= rhs;
    lhs.v *= rhs;
    return lhs;
}

template <typename T>
inline Quaternion<T>& operator/=(Quaternion<T>& lhs, const T rhs)
{
    lhs.s /= rhs;
    lhs.v /= rhs;
    return lhs;
}

template <>
inline Quaternion<float>& operator/=(Quaternion<float>& lhs, const float rhs)
{
    return lhs *= 1.0f / rhs;
}

template <>
inline Quaternion<double>& operator/=(Quaternion<double>& lhs, const double rhs)
{
    return lhs *= 1.0 / rhs;
}

template <>
inline Quaternion<long double>& operator/=(Quaternion<long double>& lhs, const long double rhs)
{
    return lhs *= 1.0L / rhs;
}

template <typename T>
inline T dot(const Quaternion<T>& lhs, const Quaternion<T>& rhs)
{
    return lhs.s * rhs.s + dot(lhs.v, rhs.v);
}

template <typename T>
inline Quaternion<T> operator*(const Quaternion<T>& lhs, const Quaternion<T>& rhs)
{
    return Quaternion<T>(
        lhs.s * rhs.s - dot(lhs.v, rhs.v),
        lhs.s * rhs.v + rhs.s * lhs.v + cross(lhs.v, rhs.v));
}

template <typename T>
inline Quaternion<T>& operator*=(Quaternion<T>& lhs, const Quaternion<T>& rhs)
{
    const T new_s = lhs.s * rhs.s - dot(lhs.v, rhs.v);
    lhs.v = lhs.s * rhs.v + rhs.s * lhs.v + cross(lhs.v, rhs.v);
    lhs.s = new_s;
    return lhs;
}

template <typename T>
inline Quaternion<T> conjugate(const Quaternion<T>& q)
{
    return Quaternion<T>(q.s, -q.v);
}

template <typename T>
inline Quaternion<T> inverse(const Quaternion<T>& q)
{
    return conjugate(q) / square_norm(q);
}

template <typename T>
inline T square_norm(const Quaternion<T>& q)
{
    return dot(q, q);
}

template <typename T>
inline T norm(const Quaternion<T>& q)
{
    return std::sqrt(square_norm(q));
}

template <typename T>
inline Quaternion<T> normalize(const Quaternion<T>& q)
{
    return q / norm(q);
}

template <typename T>
inline bool is_normalized(const Quaternion<T>& q)
{
    return feq(square_norm(q), T(1.0));
}

template <typename T>
inline bool is_normalized(const Quaternion<T>& q, const T eps)
{
    return feq(square_norm(q), T(1.0), eps);
}

template <typename T>
APPLESEED_FORCE_INLINE Quaternion<T> slerp(const Quaternion<T>& p, const Quaternion<T>& q, const T t)
{
    const T Eps = make_eps<T>(1.0e-4f, 1.0e-6);

    assert(is_normalized(p, Eps));
    assert(is_normalized(q, Eps));

    const T cos_theta = clamp(dot(p, q), T(-1.0), T(1.0));
    const T theta = std::acos(cos_theta);
    const T sin_theta = std::sqrt(T(1.0) - cos_theta * cos_theta);

    return
        sin_theta < Eps
            ? lerp(p, q, t)
            : (std::sin((T(1.0) - t) * theta) * p + std::sin(t * theta) * q) / sin_theta;
}

template <typename T>
inline Quaternion<T> fast_slerp(const Quaternion<T>& p, const Quaternion<T>& q, const T t)
{
    const T d = dot(p, q);
    const T a = T(1.0904) + d * (T(-3.2452) + d * (T(3.55645) + d * T(-1.43519)));
    const T b = T(0.848013) + d * (T(-1.06021) + d * T(0.215638));
    const T u = t - T(1.0);
    const T v = t - T(0.5);
    const T k = a * v * v + b;
    const T w = k * u * v * t + t;
    return normalize(lerp(p, q, w));
}

template <typename T>
inline Vector<T, 3> rotate(const Quaternion<T>& q, const Vector<T, 3>& v)
{
    //
    // Unoptimized implementation:
    //
    //     return (q * Quaternion<T>(T(0.0), v) * conjugate(q)).v;
    //
    // Reference for the optimized implementation:
    //
    //   https://fgiesen.wordpress.com/2019/02/09/rotating-a-single-vector-using-a-quaternion/
    //

    const Vector<T, 3> t = T(2.0) * cross(q.v, v);
    return v + q.s * t + cross(q.v, t);
}

}   // namespace foundation
