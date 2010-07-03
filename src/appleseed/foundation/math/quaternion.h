
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

#ifndef APPLESEED_FOUNDATION_MATH_QUATERNION_H
#define APPLESEED_FOUNDATION_MATH_QUATERNION_H

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"

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
    Quaternion();                                           // leave all components uninitialized
    Quaternion(const ValueType s, const VectorType& v);     // initializing constructor

    // Construct a quaternion from another quaternion of a different type.
    template <typename U>
    explicit Quaternion(const Quaternion<U>& rhs);

    // Return the identity quaternion.
    static QuaternionType identity();

    // Build a rotation quaternion from an axis and an angle.
    static QuaternionType rotation(
        const VectorType&   axis,                           // rotation axis, unit-length
        const ValueType     angle);                         // rotation angle, in radians

    // Build a rotation quaternion from two point on the unit sphere.
    static QuaternionType rotation(
        const VectorType&   from,
        const VectorType&   to);
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


//
// Full specializations for quaternions of type float and double.
//

typedef Quaternion<float> Quaternionf;
typedef Quaternion<double> Quaterniond;


//
// Quaternion class implementation.
//

// Constructors.
template <typename T>
inline Quaternion<T>::Quaternion()
{
}
template <typename T>
inline Quaternion<T>::Quaternion(const ValueType s_, const VectorType& v_)
  : s(s_)
  , v(v_)
{
}

// Construct a quaternion from another quaternion of a different type.
template <typename T>
template <typename U>
inline Quaternion<T>::Quaternion(const Quaternion<U>& rhs)
  : s(static_cast<ValueType>(rhs.s))
  , v(rhs.v)
{
}

// Return the identity quaternion.
template <typename T>
inline Quaternion<T> Quaternion<T>::identity()
{
    return QuaternionType(ValueType(1.0), VectorType(0.0));
}

// Build a rotation quaternion from an axis and an angle.
template <typename T>
inline Quaternion<T> Quaternion<T>::rotation(
    const VectorType&   axis,
    const ValueType     angle)
{
    assert(is_normalized(axis));
    const ValueType half_angle = ValueType(0.5) * angle;
    return QuaternionType(
        std::cos(half_angle),
        std::sin(half_angle) * axis);
}

// Build a rotation quaternion from two point on the unit sphere.
template <typename T>
inline Quaternion<T> Quaternion<T>::rotation(
    const VectorType&   from,
    const VectorType&   to)
{
    return Quaternion(dot(from, to), cross(from, to));
}

// Exact inequality and equality tests.
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

// Approximate equality tests.
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

// Approximate zero tests.
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

// Quaternion arithmetic.
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
    return lhs * (T(1.0) / rhs);
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
    return lhs *= (T(1.0) / rhs);
}

// Quaternion products.
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

// Quaternion conjugate.
template <typename T>
inline Quaternion<T> conjugate(const Quaternion<T>& q)
{
    return Quaternion<T>(q.s, -q.v);
}

// Quaternion inverse.
template <typename T>
inline Quaternion<T> inverse(const Quaternion<T>& q)
{
    return conjugate(q) / square_norm(q);
}

// Quaternion square norm, norm and normalization.
template <typename T>
inline T square_norm(const Quaternion<T>& q)
{
    return q.s * q.s + dot(q.v, q.v);
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

// Return true if a quaternion is normalized (unit-length), false otherwise.
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

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_QUATERNION_H
