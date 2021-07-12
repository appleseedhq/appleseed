
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
#include "foundation/core/exceptions/exception.h"
#include "foundation/core/exceptions/exceptionnotimplemented.h"
#include "foundation/math/quaternion.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#ifdef APPLESEED_USE_SSE
#include "foundation/platform/sse.h"
#endif
#include "foundation/utility/poison.h"

// Imath headers.
#ifdef APPLESEED_ENABLE_IMATH_INTEROP
#include "foundation/platform/_beginexrheaders.h"
#include "Imath/ImathMatrix.h"
#include "foundation/platform/_endexrheaders.h"
#endif

// Standard headers.
#include <cassert>
#include <cmath>
#include <cstddef>

namespace foundation
{

//
// MxN matrix class (M rows, N columns) of arbitrary type.
//
// Matrices are stored in row-major order.
//
// References:
//
//   Rotation matrix
//   http://en.wikipedia.org/wiki/Rotation_matrix
//
//   Real-Time Rendering, Second Edition, A. K. Peters
//
//   A function for creating a rotation matrix that rotates a vector called "from" into another vector called "to"
//   https://github.com/erich666/jgt-code/blob/master/Volume_04/Number_4/Moller1999/fromtorot.c
//

template <typename T, size_t M, size_t N>
class Matrix
{
  public:
    // Types.
    typedef T ValueType;
    typedef Matrix<T, M, N> MatrixType;

    // Dimensions and number of components.
    static const size_t Rows = M;
    static const size_t Columns = N;
    static const size_t Components = M * N;

    // Constructors.
#if APPLESEED_COMPILER_CXX_DEFAULTED_FUNCTIONS
    Matrix() = default;                                     // leave all components uninitialized
#else
    Matrix() {}                                             // leave all components uninitialized
#endif
    explicit Matrix(const ValueType val);                   // set all components to `val`

    // Construct a matrix from another matrix of a different type.
    template <typename U>
    explicit Matrix(const Matrix<U, M, N>& rhs);

    // Construct a matrix from an array of M * N scalars.
    static MatrixType from_array(const ValueType* rhs);

    // Unchecked array subscripting.
    ValueType& operator[](const size_t i);
    const ValueType& operator[](const size_t i) const;

    // Unchecked Fortran-style subscripting (0-based).
    ValueType& operator()(const size_t row, const size_t col);
    const ValueType& operator()(const size_t row, const size_t col) const;

  private:
    ValueType m_comp[Components];
};

// Poisoning.
template <typename T, size_t M, size_t N>
class PoisonImpl<Matrix<T, M, N>>
{
  public:
    static void do_poison(Matrix<T, M, N>& m);
};

// Exact inequality and equality tests.
template <typename T, size_t M, size_t N> bool operator!=(const Matrix<T, M, N>& lhs, const Matrix<T, M, N>& rhs);
template <typename T, size_t M, size_t N> bool operator==(const Matrix<T, M, N>& lhs, const Matrix<T, M, N>& rhs);

// Approximate equality tests.
template <typename T, size_t M, size_t N> bool feq(const Matrix<T, M, N>& lhs, const Matrix<T, M, N>& rhs);
template <typename T, size_t M, size_t N> bool feq(const Matrix<T, M, N>& lhs, const Matrix<T, M, N>& rhs, const T eps);

// Approximate zero tests.
template <typename T, size_t M, size_t N> bool fz(const Matrix<T, M, N>& m);
template <typename T, size_t M, size_t N> bool fz(const Matrix<T, M, N>& m, const T eps);

// Matrix arithmetic.
template <typename T, size_t M, size_t N> Matrix<T, M, N>  operator+ (const Matrix<T, M, N>& lhs, const Matrix<T, M, N>& rhs);
template <typename T, size_t M, size_t N> Matrix<T, M, N>  operator- (const Matrix<T, M, N>& lhs, const Matrix<T, M, N>& rhs);
template <typename T, size_t M, size_t N> Matrix<T, M, N>  operator- (const Matrix<T, M, N>& lhs);
template <typename T, size_t M, size_t N> Matrix<T, M, N>  operator* (const Matrix<T, M, N>& lhs, const T rhs);
template <typename T, size_t M, size_t N> Matrix<T, M, N>  operator* (const T lhs, const Matrix<T, M, N>& rhs);
template <typename T, size_t M, size_t N> Matrix<T, M, N>  operator/ (const Matrix<T, M, N>& lhs, const T rhs);
template <typename T, size_t M, size_t N> Matrix<T, M, N>& operator+=(Matrix<T, M, N>& lhs, const Matrix<T, M, N>& rhs);
template <typename T, size_t M, size_t N> Matrix<T, M, N>& operator-=(Matrix<T, M, N>& lhs, const Matrix<T, M, N>& rhs);
template <typename T, size_t M, size_t N> Matrix<T, M, N>& operator*=(Matrix<T, M, N>& lhs, const T rhs);
template <typename T, size_t M, size_t N> Matrix<T, M, N>& operator/=(Matrix<T, M, N>& lhs, const T rhs);

// Matrix-matrix multiplication (MxN * NxK = MxK).
template <typename T, size_t M, size_t N, size_t K>
Matrix<T, M, K> operator*(
    const Matrix<T, M, N>&  lhs,
    const Matrix<T, N, K>&  rhs);

// Matrix-vector multiplication (MxN * Nx1 = Mx1).
template <typename T, size_t M, size_t N>
Vector<T, M> operator*(
    const Matrix<T, M, N>&  m,
    const Vector<T, N>&     v);

// Vector-matrix multiplication (1xM * MxN = 1xN).
template <typename T, size_t M, size_t N>
Vector<T, N> operator*(
    const Vector<T, M>&     v,
    const Matrix<T, M, N>&  m);

// Matrix transposition.
template <typename T, size_t M, size_t N>
Matrix<T, N, M> transpose(const Matrix<T, M, N>& mat);


//
// NxN matrix class of arbitrary type.
//

template <typename T, size_t N>
class Matrix<T, N, N>
{
  public:
    // Types.
    typedef T ValueType;
    typedef Matrix<T, N, N> MatrixType;

    // Dimensions and number of components.
    static const size_t Rows = N;
    static const size_t Columns = N;
    static const size_t Components = N * N;

    // Constructors.
#if APPLESEED_COMPILER_CXX_DEFAULTED_FUNCTIONS
    Matrix() = default;                                     // leave all components uninitialized
#else
    Matrix() {}                                             // leave all components uninitialized
#endif
    explicit Matrix(const ValueType val);                   // set all components to `val`

    // Construct a matrix from another matrix of a different type.
    template <typename U>
    Matrix(const Matrix<U, N, N>& rhs);

    // Construct a matrix from an array of N * N scalars.
    static MatrixType from_array(const ValueType* rhs);

    // Construct and return the NxN identity matrix.
    static MatrixType make_identity();

    // Return the NxN identity matrix.
    static const MatrixType& identity();

    // Unchecked array subscripting.
    ValueType& operator[](const size_t i);
    const ValueType& operator[](const size_t i) const;

    // Unchecked Fortran-style subscripting (0-based).
    ValueType& operator()(const size_t row, const size_t col);
    const ValueType& operator()(const size_t row, const size_t col) const;

  private:
    ValueType m_comp[Components];

    // The identity matrix returned by identity().
    static const MatrixType m_identity;
};

// Matrix trace.
template <typename T, size_t N>
T trace(const Matrix<T, N, N>& mat);

// Matrix determinant.
template <typename T, size_t N>
T det(const Matrix<T, N, N>& mat);

// Exception thrown when attempting to invert a singular matrix.
struct ExceptionSingularMatrix : public Exception {};

// Matrix inversion using Gauss-Jordan elimination with partial pivoting.
// Throws a foundation::ExceptionSingularMatrix exception if the matrix is singular.
template <typename T, size_t N>
Matrix<T, N, N> inverse(
    const Matrix<T, N, N>&  mat,
    const T                 eps = default_eps<T>());


//
// 3x3 matrix class of arbitrary type.
//

template <typename T>
class Matrix<T, 3, 3>
{
  public:
    // Types.
    typedef T ValueType;
    typedef Matrix<T, 3, 3> MatrixType;

    // Dimensions and number of components.
    static const size_t Rows = 3;
    static const size_t Columns = 3;
    static const size_t Components = 3 * 3;

    // Constructors.
#if APPLESEED_COMPILER_CXX_DEFAULTED_FUNCTIONS
    Matrix() = default;                                     // leave all components uninitialized
#else
    Matrix() {}                                             // leave all components uninitialized
#endif
    explicit Matrix(const ValueType val);                   // set all components to `val`

    // Construct a matrix from another matrix of a different type.
    template <typename U>
    Matrix(const Matrix<U, 3, 3>& rhs);

#ifdef APPLESEED_ENABLE_IMATH_INTEROP

    // Implicit construction from an Imath::Matrix33.
    Matrix(const Imath::Matrix33<T>& rhs);

    // Convert this matrix to an Imath::Matrix33.
    operator Imath::Matrix33<T>() const;

#endif

    // Construct a matrix from an array of 9 scalars.
    static MatrixType from_array(const ValueType* rhs);

    // Construct and return the 3x3 identity matrix.
    static MatrixType make_identity();

    // Return the 3x3 identity matrix.
    static const MatrixType& identity();

    // Build canonical transformation matrices.
    static MatrixType make_scaling(const Vector<T, 3>& s);
    static MatrixType make_rotation_x(
        const ValueType         angle);                     // rotation angle, in radians
    static MatrixType make_rotation_y(
        const ValueType         angle);                     // rotation angle, in radians
    static MatrixType make_rotation_z(
        const ValueType         angle);                     // rotation angle, in radians
    static MatrixType make_rotation_x(
        const ValueType         cos_angle,                  // cosine of the rotation angle
        const ValueType         sin_angle);                 // sine of the rotation angle
    static MatrixType make_rotation_y(
        const ValueType         cos_angle,                  // cosine of the rotation angle
        const ValueType         sin_angle);                 // sine of the rotation angle
    static MatrixType make_rotation_z(
        const ValueType         cos_angle,                  // cosine of the rotation angle
        const ValueType         sin_angle);                 // sine of the rotation angle

    // Build a rotation matrix from Euler angles.
    static MatrixType make_rotation(
        const ValueType         yaw,                        // rotation angle, in radians
        const ValueType         pitch,                      // rotation angle, in radians
        const ValueType         roll);                      // rotation angle, in radians

    // Build a rotation matrix from an axis and an angle.
    static MatrixType make_rotation(
        const Vector<T, 3>&     axis,                       // rotation axis, unit-length
        const ValueType         angle);                     // rotation angle, in radians
    static MatrixType make_rotation(
        const Vector<T, 3>&     axis,                       // rotation axis, unit-length
        const ValueType         cos_angle,                  // cosine of the rotation angle
        const ValueType         sin_angle);                 // sine of the rotation angle

    // Build a rotation matrix from a unit quaternion.
    static MatrixType make_rotation(
        const Quaternion<T>&    q);                         // unit quaternion

    // Unchecked array subscripting.
    ValueType& operator[](const size_t i);
    const ValueType& operator[](const size_t i) const;

    // Unchecked Fortran-style subscripting (0-based).
    ValueType& operator()(const size_t row, const size_t col);
    const ValueType& operator()(const size_t row, const size_t col) const;

    // Extract the scaling factors from a transformation matrix.
    Vector<T, 3> extract_scaling() const;

    // Extract Euler angles from a rotation matrix.
    void extract_euler_angles(
        ValueType&              yaw,
        ValueType&              pitch,
        ValueType&              roll) const;

    // Extract a unit quaternion from a rotation matrix.
    Quaternion<T> extract_unit_quaternion() const;

    // Decompose a matrix into scaling -> rotation.
    void decompose(
        Vector<T, 3>&           scaling,
        Quaternion<T>&          rotation) const;

  private:
    ValueType m_comp[Components];

    // The identity matrix returned by identity().
    static const MatrixType m_identity;
};

// Rotate a given vector by a given angle around a given axis.
template <typename T>
Vector<T, 3> rotate(
    const Vector<T, 3>&     v,
    const Vector<T, 3>&     axis,
    const T                 angle);


//
// 4x4 matrix class of arbitrary type.
//

template <typename T>
class Matrix<T, 4, 4>
{
  public:
    // Types.
    typedef T ValueType;
    typedef Matrix<T, 4, 4> MatrixType;

    // Dimensions and number of components.
    static const size_t Rows = 4;
    static const size_t Columns = 4;
    static const size_t Components = 4 * 4;

    // Constructors.
#if APPLESEED_COMPILER_CXX_DEFAULTED_FUNCTIONS
    Matrix() = default;                                     // leave all components uninitialized
#else
    Matrix() {}                                             // leave all components uninitialized
#endif
    explicit Matrix(const ValueType val);                   // set all components to `val`

    // Construct a matrix from another matrix of a different type.
    template <typename U>
    Matrix(const Matrix<U, 4, 4>& rhs);

#ifdef APPLESEED_ENABLE_IMATH_INTEROP

    // Implicit construction from an Imath::Matrix44.
    Matrix(const Imath::Matrix44<T>& rhs);

    // Convert this matrix to an Imath::Matrix44.
    operator Imath::Matrix44<T>() const;

#endif

    // Construct a matrix from an array of 16 scalars.
    static MatrixType from_array(const ValueType* rhs);

    // Construct and return the 4x4 identity matrix.
    static MatrixType make_identity();

    // Return the 4x4 identity matrix.
    static const MatrixType& identity();

    // Build canonical transformation matrices.
    static MatrixType make_translation(const Vector<T, 3>& v);
    static MatrixType make_scaling(const Vector<T, 3>& s);
    static MatrixType make_rotation_x(
        const ValueType         angle);                     // rotation angle, in radians
    static MatrixType make_rotation_y(
        const ValueType         angle);                     // rotation angle, in radians
    static MatrixType make_rotation_z(
        const ValueType         angle);                     // rotation angle, in radians
    static MatrixType make_rotation_x(
        const ValueType         cos_angle,                  // cosine of the rotation angle
        const ValueType         sin_angle);                 // sine of the rotation angle
    static MatrixType make_rotation_y(
        const ValueType         cos_angle,                  // cosine of the rotation angle
        const ValueType         sin_angle);                 // sine of the rotation angle
    static MatrixType make_rotation_z(
        const ValueType         cos_angle,                  // cosine of the rotation angle
        const ValueType         sin_angle);                 // sine of the rotation angle

    // Build a rotation matrix from Euler angles.
    static MatrixType make_rotation(
        const ValueType         yaw,                        // rotation angle, in radians
        const ValueType         pitch,                      // rotation angle, in radians
        const ValueType         roll);                      // rotation angle, in radians

    // Build a rotation matrix from an axis and an angle.
    static MatrixType make_rotation(
        const Vector<T, 3>&     axis,                       // rotation axis, unit-length
        const ValueType         angle);                     // rotation angle, in radians
    static MatrixType make_rotation(
        const Vector<T, 3>&     axis,                       // rotation axis, unit-length
        const ValueType         cos_angle,                  // cosine of the rotation angle
        const ValueType         sin_angle);                 // sine of the rotation angle

    // Build a rotation matrix from a unit quaternion.
    static MatrixType make_rotation(
        const Quaternion<T>&    q);                         // unit quaternion

    // Build a look-at transformation matrix. The returned matrix is orthonormal,
    // and such that the Z- axis is pointing toward the target point.
    static MatrixType make_lookat(
        const Vector<T, 3>&     origin,                     // camera origin
        const Vector<T, 3>&     target,                     // target point
        const Vector<T, 3>&     up);                        // up vector, unit-length

    // Build a matrix that maps a frustum defined by the
    // top, botom, left, and right points on the near plane and
    // extending from z_near to z_far in the +Z axis to the axis aligned
    // cube in the area (-1, -1, -1) to (1, 1, 1).
    static MatrixType make_frustum(
        const ValueType bottom,
        const ValueType top,
        const ValueType left,
        const ValueType right,
        const ValueType z_near,
        const ValueType z_far);

    // Unchecked array subscripting.
    ValueType& operator[](const size_t i);
    const ValueType& operator[](const size_t i) const;

    // Unchecked Fortran-style subscripting (0-based).
    ValueType& operator()(const size_t row, const size_t col);
    const ValueType& operator()(const size_t row, const size_t col) const;

    // Extract the upper-left 3x3 matrix.
    Matrix<T, 3, 3> extract_matrix3() const;

    // Extract the translation from a transformation matrix.
    Vector<T, 3> extract_translation() const;

    // Decompose a matrix into scaling -> rotation -> translation.
    void decompose(
        Vector<T, 3>&           scaling,
        Quaternion<T>&          rotation,
        Vector<T, 3>&           translation) const;

  private:
    APPLESEED_SIMD4_ALIGN ValueType m_comp[Components];

    // The identity matrix returned by identity().
    static const MatrixType m_identity;
};

// Return true if the matrix swaps the handedness.
template <typename T>
bool swaps_handedness(const Matrix<T, 4, 4>& mat);


//
// Full specializations for 2x2, 3x3 and 4x4 matrices of type int, float and double.
//

typedef Matrix<int,    2, 2> Matrix2i;
typedef Matrix<float,  2, 2> Matrix2f;
typedef Matrix<double, 2, 2> Matrix2d;
typedef Matrix<int,    3, 3> Matrix3i;
typedef Matrix<float,  3, 3> Matrix3f;
typedef Matrix<double, 3, 3> Matrix3d;
typedef Matrix<int,    4, 4> Matrix4i;
typedef Matrix<float,  4, 4> Matrix4f;
typedef Matrix<double, 4, 4> Matrix4d;


//
// MxN matrix class implementation.
//

template <typename T, size_t M, size_t N>
inline Matrix<T, M, N>::Matrix(const ValueType val)
{
    for (size_t i = 0; i < Components; ++i)
        m_comp[i] = val;
}

template <typename T, size_t M, size_t N>
template <typename U>
inline Matrix<T, M, N>::Matrix(const Matrix<U, M, N>& rhs)
{
    for (size_t i = 0; i < Components; ++i)
        m_comp[i] = static_cast<ValueType>(rhs[i]);
}

template <typename T, size_t M, size_t N>
inline Matrix<T, M, N> Matrix<T, M, N>::from_array(const ValueType* rhs)
{
    assert(rhs);

    Matrix result;

    for (size_t i = 0; i < Components; ++i)
        result.m_comp[i] = rhs[i];

    return result;
}

template <typename T, size_t M, size_t N>
inline T& Matrix<T, M, N>::operator[](const size_t i)
{
    assert(i < Components);
    return m_comp[i];
}

template <typename T, size_t M, size_t N>
inline const T& Matrix<T, M, N>::operator[](const size_t i) const
{
    assert(i < Components);
    return m_comp[i];
}

template <typename T, size_t M, size_t N>
inline T& Matrix<T, M, N>::operator()(const size_t row, const size_t col)
{
    assert(row < Rows);
    assert(col < Columns);
    return m_comp[row * Columns + col];
}

template <typename T, size_t M, size_t N>
inline const T& Matrix<T, M, N>::operator()(const size_t row, const size_t col) const
{
    assert(row < Rows);
    assert(col < Columns);
    return m_comp[row * Columns + col];
}

template <typename T, size_t M, size_t N>
void PoisonImpl<Matrix<T, M, N>>::do_poison(Matrix<T, M, N>& m)
{
    for (size_t i = 0; i < m.Components; ++i)
        always_poison(m[i]);
}

template <typename T, size_t M, size_t N>
inline bool operator!=(const Matrix<T, M, N>& lhs, const Matrix<T, M, N>& rhs)
{
    for (size_t i = 0; i < lhs.Components; ++i)
    {
        if (lhs[i] != rhs[i])
            return true;
    }

    return false;
}

template <typename T, size_t M, size_t N>
inline bool operator==(const Matrix<T, M, N>& lhs, const Matrix<T, M, N>& rhs)
{
    return !(lhs != rhs);
}

template <typename T, size_t M, size_t N>
inline bool feq(const Matrix<T, M, N>& lhs, const Matrix<T, M, N>& rhs)
{
    for (size_t i = 0; i < lhs.Components; ++i)
    {
        if (!feq(lhs[i], rhs[i]))
            return false;
    }

    return true;
}

template <typename T, size_t M, size_t N>
inline bool feq(const Matrix<T, M, N>& lhs, const Matrix<T, M, N>& rhs, const T eps)
{
    for (size_t i = 0; i < lhs.Components; ++i)
    {
        if (!feq(lhs[i], rhs[i], eps))
            return false;
    }

    return true;
}

template <typename T, size_t M, size_t N>
inline bool fz(const Matrix<T, M, N>& m)
{
    for (size_t i = 0; i < m.Components; ++i)
    {
        if (!fz(m[i]))
            return false;
    }

    return true;
}

template <typename T, size_t M, size_t N>
inline bool fz(const Matrix<T, M, N>& m, const T eps)
{
    for (size_t i = 0; i < m.Components; ++i)
    {
        if (!fz(m[i], eps))
            return false;
    }

    return true;
}

template <typename T, size_t M, size_t N>
inline Matrix<T, M, N> operator+(const Matrix<T, M, N>& lhs, const Matrix<T, M, N>& rhs)
{
    Matrix<T, M, N> mat;

    for (size_t i = 0; i < lhs.Components; ++i)
        mat[i] = lhs[i] + rhs[i];

    return mat;
}

template <typename T, size_t M, size_t N>
inline Matrix<T, M, N> operator-(const Matrix<T, M, N>& lhs, const Matrix<T, M, N>& rhs)
{
    Matrix<T, M, N> mat;

    for (size_t i = 0; i < lhs.Components; ++i)
        mat[i] = lhs[i] - rhs[i];

    return mat;
}

template <typename T, size_t M, size_t N>
inline Matrix<T, M, N> operator-(const Matrix<T, M, N>& lhs)
{
    Matrix<T, M, N> mat;

    for (size_t i = 0; i < lhs.Components; ++i)
        mat[i] = -lhs[i];

    return mat;
}

template <typename T, size_t M, size_t N>
inline Matrix<T, M, N> operator*(const Matrix<T, M, N>& lhs, const T rhs)
{
    Matrix<T, M, N> mat;

    for (size_t i = 0; i < lhs.Components; ++i)
        mat[i] = lhs[i] * rhs;

    return mat;
}

template <typename T, size_t M, size_t N>
inline Matrix<T, M, N> operator*(const T lhs, const Matrix<T, M, N>& rhs)
{
    return rhs * lhs;
}

template <typename T, size_t M, size_t N>
inline Matrix<T, M, N> operator/(const Matrix<T, M, N>& lhs, const T rhs)
{
    Matrix<T, M, N> result;

    for (size_t i = 0; i < lhs.Components; ++i)
        result[i] = lhs[i] / rhs;

    return result;
}

template <size_t M, size_t N>
inline Matrix<float, M, N> operator/(const Matrix<float, M, N>& lhs, const float rhs)
{
    return lhs * (1.0f / rhs);
}

template <size_t M, size_t N>
inline Matrix<double, M, N> operator/(const Matrix<double, M, N>& lhs, const double rhs)
{
    return lhs * (1.0 / rhs);
}

template <size_t M, size_t N>
inline Matrix<long double, M, N> operator/(const Matrix<long double, M, N>& lhs, const long double rhs)
{
    return lhs * (1.0L / rhs);
}

template <typename T, size_t M, size_t N>
inline Matrix<T, M, N>& operator+=(Matrix<T, M, N>& lhs, const Matrix<T, M, N>& rhs)
{
    for (size_t i = 0; i < lhs.Components; ++i)
        lhs[i] += rhs[i];

    return lhs;
}

template <typename T, size_t M, size_t N>
inline Matrix<T, M, N>& operator-=(Matrix<T, M, N>& lhs, const Matrix<T, M, N>& rhs)
{
    for (size_t i = 0; i < lhs.Components; ++i)
        lhs[i] -= rhs[i];

    return lhs;
}

template <typename T, size_t M, size_t N>
inline Matrix<T, M, N>& operator*=(Matrix<T, M, N>& lhs, const T rhs)
{
    for (size_t i = 0; i < lhs.Components; ++i)
        lhs[i] *= rhs;

    return lhs;
}

template <typename T, size_t M, size_t N>
inline Matrix<T, M, N>& operator/=(Matrix<T, M, N>& lhs, const T rhs)
{
    for (size_t i = 0; i < lhs.Components; ++i)
        lhs[i] /= rhs;

    return lhs;
}

template <size_t M, size_t N>
inline Matrix<float, M, N>& operator/=(Matrix<float, M, N>& lhs, const float rhs)
{
    return lhs *= 1.0f / rhs;
}

template <size_t M, size_t N>
inline Matrix<double, M, N>& operator/=(Matrix<double, M, N>& lhs, const double rhs)
{
    return lhs *= 1.0 / rhs;
}

template <size_t M, size_t N>
inline Matrix<long double, M, N>& operator/=(Matrix<long double, M, N>& lhs, const long double rhs)
{
    return lhs *= 1.0L / rhs;
}

template <typename T, size_t M, size_t N, size_t K>
inline Matrix<T, M, K> operator*(
    const Matrix<T, M, N>&  lhs,
    const Matrix<T, N, K>&  rhs)
{
    Matrix<T, M, K> res;
    size_t res_index = 0;

    for (size_t r = 0; r < M; ++r)
    {
        for (size_t c = 0; c < K; ++c)
        {
            res[res_index] = T(0.0);

            for (size_t i = 0; i < N; ++i)
            {
                res[res_index] += lhs[r * N + i] * rhs[i * K + c];
            }

            ++res_index;
        }
    }

    return res;
}

template <typename T, size_t M, size_t N>
inline Vector<T, M> operator*(
    const Matrix<T, M, N>&  m,
    const Vector<T, N>&     v)
{
    Vector<T, M> res;

    for (size_t r = 0; r < M; ++r)
    {
        res[r] = T(0.0);

        for (size_t c = 0; c < N; ++c)
            res[r] += m(r, c) * v[c];
    }

    return res;
}

template <typename T, size_t M, size_t N>
inline Vector<T, N> operator*(
    const Vector<T, M>&     v,
    const Matrix<T, M, N>&  m)
{
    Vector<T, N> res;

    for (size_t c = 0; c < N; ++c)
    {
        res[c] = T(0.0);

        for (size_t r = 0; r < M; ++r)
            res[c] += v[r] * m(r, c);
    }

    return res;
}

template <typename T, size_t M, size_t N>
inline Matrix<T, N, M> transpose(const Matrix<T, M, N>& mat)
{
    Matrix<T, N, M> res;
    T* p = &res(0, 0);

    for (size_t c = 0; c < N; ++c)
    {
        for (size_t r = 0; r < M; ++r)
            *p++ = mat(r, c);
    }

    return res;
}


//
// NxN matrix class implementation.
//

template <typename T, size_t N>
const Matrix<T, N, N> Matrix<T, N, N>::m_identity(Matrix<T, N, N>::make_identity());

template <typename T, size_t N>
inline Matrix<T, N, N>::Matrix(const ValueType val)
{
    for (size_t i = 0; i < Components; ++i)
        m_comp[i] = val;
}

template <typename T, size_t N>
template <typename U>
inline Matrix<T, N, N>::Matrix(const Matrix<U, N, N>& rhs)
{
    for (size_t i = 0; i < Components; ++i)
        m_comp[i] = static_cast<ValueType>(rhs[i]);
}

template <typename T, size_t N>
inline Matrix<T, N, N> Matrix<T, N, N>::from_array(const ValueType* rhs)
{
    assert(rhs);

    Matrix result;

    for (size_t i = 0; i < Components; ++i)
        result.m_comp[i] = rhs[i];

    return result;
}

template <typename T, size_t N>
Matrix<T, N, N> Matrix<T, N, N>::make_identity()
{
    MatrixType mat(T(0.0));

    for (size_t i = 0; i < N; ++i)
        mat(i, i) = T(1.0);

    return mat;
}

template <typename T, size_t N>
inline const Matrix<T, N, N>& Matrix<T, N, N>::identity()
{
    return m_identity;
}

template <typename T, size_t N>
inline T& Matrix<T, N, N>::operator[](const size_t i)
{
    assert(i < Components);
    return m_comp[i];
}

template <typename T, size_t N>
inline const T& Matrix<T, N, N>::operator[](const size_t i) const
{
    assert(i < Components);
    return m_comp[i];
}

template <typename T, size_t N>
inline T& Matrix<T, N, N>::operator()(const size_t row, const size_t col)
{
    assert(row < Rows);
    assert(col < Columns);
    return m_comp[row * Columns + col];
}

template <typename T, size_t N>
inline const T& Matrix<T, N, N>::operator()(const size_t row, const size_t col) const
{
    assert(row < Rows);
    assert(col < Columns);
    return m_comp[row * Columns + col];
}

template <typename T, size_t N>
inline T trace(const Matrix<T, N, N>& mat)
{
    T sum = T(0.0);

    for (size_t i = 0; i < N; ++i)
        sum += mat(i, i);

    return sum;
}

template <typename T, size_t N>
inline T det(const Matrix<T, N, N>& mat)
{
    throw ExceptionNotImplemented();
    return T(0.0);
}

template <typename T, size_t N>
Matrix<T, N, N> inverse(
    const Matrix<T, N, N>&  mat,
    const T                 eps)
{
    assert(eps >= T(0.0));

    // m is a Nx2N block-matrix of the form [mat, identity].
    Matrix<T, N, 2 * N> m(T(0.0));

    // Set the left block of m to mat.
    for (size_t r = 0; r < N; ++r)
    {
        for (size_t c = 0; c < N; ++c)
            m(r, c) = mat(r, c);
    }

    // Set the right block of m to the NxN identity matrix.
    for (size_t i = 0; i < N; ++i)
        m(i, N + i) = T(1.0);

    // Gauss-Jordan elimination loop.
    for (size_t i = 0; i < N; ++i)
    {
        // Find, in the i'th column, starting from the i'th row,
        // the element with the largest absolute value (pivot).
        T max_value = std::abs(m(i, i));
        size_t max_index = i;
        for (size_t r = i + 1; r < N; ++r)
        {
            const T x = std::abs(m(r, i));
            if (x > max_value)
            {
                max_value = x;
                max_index = r;
            }
        }

        // If the pivot is zero, the matrix is singular.
        if (max_value < eps)
            throw ExceptionSingularMatrix();

        // Swap the i'th row with the max_index'th row.
        if (i != max_index)
        {
            for (size_t c = 0; c < 2 * N; ++c)
            {
                const T tmp = m(i, c);
                m(i, c) = m(max_index, c);
                m(max_index, c) = tmp;
            }
        }

        // Divide the i'th row by the value of the pivot.
        const T rcp_value = T(1.0) / m(i, i);
        for (size_t c = 0; c < 2 * N; ++c)
            m(i, c) *= rcp_value;

        // At that point, the pivot must be equal to 1.0.
        assert(feq(m(i, i), T(1.0)));

        // Subtract m(r, i) * i'th row from all r'th rows, r != i.
        for (size_t r = 0; r < N; ++r)
        {
            if (r != i)
            {
                const T x = m(r, i);
                for (size_t c = 0; c < 2 * N; ++c)
                    m(r, c) -= x * m(i, c);
//              assert(feq(m(r, i), T(0.0)));   // subject to numerical instability
            }
        }
    }

#ifndef NDEBUG
    // Check that the left block of m is now equal to the identity.
    for (size_t r = 0; r < N; ++r)
    {
        for (size_t c = 0; c < N; ++c)
            assert(feq(m(r, c), r == c ? T(1.0) : T(0.0)));
    }
#endif

    // Extract the inverse matrix from the right block of m.
    Matrix<T, N, N> res;
    for (size_t r = 0; r < N; ++r)
    {
        for (size_t c = 0; c < N; ++c)
            res(r, c) = m(r, N + c);
    }

    return res;
}


//
// 3x3 matrix class implementation.
//

template <typename T>
const Matrix<T, 3, 3> Matrix<T, 3, 3>::m_identity(Matrix<T, 3, 3>::make_identity());

template <typename T>
inline Matrix<T, 3, 3>::Matrix(const ValueType val)
{
    for (size_t i = 0; i < Components; ++i)
        m_comp[i] = val;
}

template <typename T>
template <typename U>
inline Matrix<T, 3, 3>::Matrix(const Matrix<U, 3, 3>& rhs)
{
    for (size_t i = 0; i < Components; ++i)
        m_comp[i] = static_cast<ValueType>(rhs[i]);
}

#ifdef APPLESEED_ENABLE_IMATH_INTEROP

template <typename T>
inline Matrix<T, 3, 3>::Matrix(const Imath::Matrix33<T>& rhs)
{
    T* p = m_comp;

    for (int i = 0; i < 3; ++i)
    {
        for (int j = 0; j < 3; ++j)
            *p++ = rhs[j][i];
    }
}

template <typename T>
inline Matrix<T, 3, 3>::operator Imath::Matrix33<T>() const
{
    Imath::Matrix33<T> result;
    T* p = &result[0][0];

    for (size_t i = 0; i < 3; ++i)
    {
        for (size_t j = 0; j < 3; ++j)
            *p++ = (*this)(j, i);
    }

    return result;
}

#endif

template <typename T>
inline Matrix<T, 3, 3> Matrix<T, 3, 3>::from_array(const ValueType* rhs)
{
    assert(rhs);

    Matrix result;

    for (size_t i = 0; i < Components; ++i)
        result.m_comp[i] = rhs[i];

    return result;
}

template <typename T>
Matrix<T, 3, 3> Matrix<T, 3, 3>::make_identity()
{
    MatrixType mat;

    mat[0] = T(1.0);
    mat[1] = T(0.0);
    mat[2] = T(0.0);

    mat[3] = T(0.0);
    mat[4] = T(1.0);
    mat[5] = T(0.0);

    mat[6] = T(0.0);
    mat[7] = T(0.0);
    mat[8] = T(1.0);

    return mat;
}

template <typename T>
inline const Matrix<T, 3, 3>& Matrix<T, 3, 3>::identity()
{
    return m_identity;
}

template <typename T>
inline Matrix<T, 3, 3> Matrix<T, 3, 3>::make_scaling(const Vector<T, 3>& s)
{
    MatrixType mat;

    mat[0] = s.x;
    mat[1] = T(0.0);
    mat[2] = T(0.0);

    mat[3] = T(0.0);
    mat[4] = s.y;
    mat[5] = T(0.0);

    mat[6] = T(0.0);
    mat[7] = T(0.0);
    mat[8] = s.z;

    return mat;
}

template <typename T>
inline Matrix<T, 3, 3> Matrix<T, 3, 3>::make_rotation_x(const ValueType angle)
{
    return make_rotation_x(std::cos(angle), std::sin(angle));
}

template <typename T>
inline Matrix<T, 3, 3> Matrix<T, 3, 3>::make_rotation_y(const ValueType angle)
{
    return make_rotation_y(std::cos(angle), std::sin(angle));
}

template <typename T>
inline Matrix<T, 3, 3> Matrix<T, 3, 3>::make_rotation_z(const ValueType angle)
{
    return make_rotation_z(std::cos(angle), std::sin(angle));
}

template <typename T>
inline Matrix<T, 3, 3> Matrix<T, 3, 3>::make_rotation_x(
    const ValueType         cos_angle,
    const ValueType         sin_angle)
{
    MatrixType mat;

    mat[0] = T(1.0);
    mat[1] = T(0.0);
    mat[2] = T(0.0);

    mat[3] = T(0.0);
    mat[4] = cos_angle;
    mat[5] = -sin_angle;

    mat[6] = T(0.0);
    mat[7] = sin_angle;
    mat[8] = cos_angle;

    return mat;
}

template <typename T>
inline Matrix<T, 3, 3> Matrix<T, 3, 3>::make_rotation_y(
    const ValueType         cos_angle,
    const ValueType         sin_angle)
{
    MatrixType mat;

    mat[0] = cos_angle;
    mat[1] = T(0.0);
    mat[2] = sin_angle;

    mat[3] = T(0.0);
    mat[4] = T(1.0);
    mat[5] = T(0.0);

    mat[6] = -sin_angle;
    mat[7] = T(0.0);
    mat[8] = cos_angle;

    return mat;
}

template <typename T>
inline Matrix<T, 3, 3> Matrix<T, 3, 3>::make_rotation_z(
    const ValueType         cos_angle,
    const ValueType         sin_angle)
{
    MatrixType mat;

    mat[0] = cos_angle;
    mat[1] = -sin_angle;
    mat[2] = T(0.0);

    mat[3] = sin_angle;
    mat[4] = cos_angle;
    mat[5] = T(0.0);

    mat[6] = T(0.0);
    mat[7] = T(0.0);
    mat[8] = T(1.0);

    return mat;
}

template <typename T>
inline Matrix<T, 3, 3> Matrix<T, 3, 3>::make_rotation(
    const ValueType         yaw,
    const ValueType         pitch,
    const ValueType         roll)
{
    const T cos_yaw   = std::cos(yaw);
    const T sin_yaw   = std::sin(yaw);
    const T cos_pitch = std::cos(pitch);
    const T sin_pitch = std::sin(pitch);
    const T cos_roll  = std::cos(roll);
    const T sin_roll  = std::sin(roll);

    MatrixType mat;

    mat[0] = cos_yaw * cos_roll - sin_yaw * sin_pitch * sin_roll;
    mat[1] = -cos_pitch * sin_roll;
    mat[2] = sin_yaw * cos_roll + cos_yaw * sin_pitch * sin_roll;

    mat[3] = cos_yaw * sin_roll + sin_yaw * sin_pitch * cos_roll;
    mat[4] = cos_pitch * cos_roll;
    mat[5] = sin_yaw * sin_roll - cos_yaw * sin_pitch * cos_roll;

    mat[6] = -sin_yaw * cos_pitch;
    mat[7] = sin_pitch;
    mat[8] = cos_yaw * cos_pitch;

    return mat;
}

template <typename T>
inline Matrix<T, 3, 3> Matrix<T, 3, 3>::make_rotation(
    const Vector<T, 3>&     axis,
    const ValueType         angle)
{
    return make_rotation(axis, std::cos(angle), std::sin(angle));
}

template <typename T>
inline Matrix<T, 3, 3> Matrix<T, 3, 3>::make_rotation(
    const Vector<T, 3>&     axis,
    const ValueType         cos_angle,
    const ValueType         sin_angle)
{
    assert(is_normalized(axis));

    const T one_min_cos_angle = T(1.0) - cos_angle;

    MatrixType mat;

    mat[0] = one_min_cos_angle * axis.x * axis.x + cos_angle;
    mat[1] = one_min_cos_angle * axis.x * axis.y - sin_angle * axis.z;
    mat[2] = one_min_cos_angle * axis.x * axis.z + sin_angle * axis.y;

    mat[3] = one_min_cos_angle * axis.y * axis.x + sin_angle * axis.z;
    mat[4] = one_min_cos_angle * axis.y * axis.y + cos_angle;
    mat[5] = one_min_cos_angle * axis.y * axis.z - sin_angle * axis.x;

    mat[6] = one_min_cos_angle * axis.z * axis.x - sin_angle * axis.y;
    mat[7] = one_min_cos_angle * axis.z * axis.y + sin_angle * axis.x;
    mat[8] = one_min_cos_angle * axis.z * axis.z + cos_angle;

    return mat;
}

template <typename T>
inline Matrix<T, 3, 3> Matrix<T, 3, 3>::make_rotation(
    const Quaternion<T>&    q)
{
    //
    // Implementation from Wild Magic Source Code, David Eberly
    // http://www.geometrictools.com
    //
    // Another interesting reference if this code needs to be optimized:
    //
    // From Quaternion to Matrix and Back
    // http://fabiensanglard.net/doom3_documentation/37726-293748.pdf
    //

    assert(is_normalized(q));

    const ValueType tx  = q.v[0] + q.v[0];
    const ValueType ty  = q.v[1] + q.v[1];
    const ValueType tz  = q.v[2] + q.v[2];
    const ValueType twx = tx * q.s;
    const ValueType twy = ty * q.s;
    const ValueType twz = tz * q.s;
    const ValueType txx = tx * q.v[0];
    const ValueType txy = ty * q.v[0];
    const ValueType txz = tz * q.v[0];
    const ValueType tyy = ty * q.v[1];
    const ValueType tyz = tz * q.v[1];
    const ValueType tzz = tz * q.v[2];

    MatrixType mat;

    mat[0] = ValueType(1.0) - (tyy + tzz);
    mat[1] = txy - twz;
    mat[2] = txz + twy;

    mat[3] = txy + twz;
    mat[4] = ValueType(1.0) - (txx + tzz);
    mat[5] = tyz - twx;

    mat[6] = txz - twy;
    mat[7] = tyz + twx;
    mat[8] = ValueType(1.0) - (txx + tyy);

    return mat;
}

template <typename T>
inline T& Matrix<T, 3, 3>::operator[](const size_t i)
{
    assert(i < Components);
    return m_comp[i];
}

template <typename T>
inline const T& Matrix<T, 3, 3>::operator[](const size_t i) const
{
    assert(i < Components);
    return m_comp[i];
}

template <typename T>
inline T& Matrix<T, 3, 3>::operator()(const size_t row, const size_t col)
{
    assert(row < Rows);
    assert(col < Columns);
    return m_comp[row * Columns + col];
}

template <typename T>
inline const T& Matrix<T, 3, 3>::operator()(const size_t row, const size_t col) const
{
    assert(row < Rows);
    assert(col < Columns);
    return m_comp[row * Columns + col];
}

template <typename T>
inline Vector<T, 3> Matrix<T, 3, 3>::extract_scaling() const
{
    Vector<T, 3> scaling;

    scaling[0] = norm(Vector<T, 3>(m_comp[0], m_comp[3], m_comp[6]));
    scaling[1] = norm(Vector<T, 3>(m_comp[1], m_comp[4], m_comp[7]));
    scaling[2] = norm(Vector<T, 3>(m_comp[2], m_comp[5], m_comp[8]));

    return scaling;
}

template <typename T>
inline void Matrix<T, 3, 3>::extract_euler_angles(
    ValueType&              yaw,
    ValueType&              pitch,
    ValueType&              roll) const
{
    // todo: in debug, make sure the matrix is orthogonal.

    if (m_comp[8] != ValueType(0.0))
    {
        yaw   = std::atan2(-m_comp[6], m_comp[8]);
        pitch = std::asin(m_comp[7]);
        roll  = std::atan2(-m_comp[1], m_comp[4]);
    }
    else
    {
        yaw   = ValueType(0.0);
        pitch = HalfPi<ValueType>();
        roll  = std::atan2(m_comp[3], m_comp[0]);
    }
}

template <typename T>
inline Quaternion<T> Matrix<T, 3, 3>::extract_unit_quaternion() const
{
    //
    // Implementation from Wild Magic Source Code, David Eberly
    // http://www.geometrictools.com
    //
    // Algorithm in Ken Shoemake's article in 1987 SIGGRAPH course notes
    // article "Quaternion Calculus and Fast Animation".
    //

    // todo: in debug, make sure the matrix is orthogonal.

    const ValueType t = m_comp[0] + m_comp[4] + m_comp[8];      // rotation matrix trace

    if (t > ValueType(0.0))
    {
        // |w| > 1/2, may as well choose w > 1/2.

        ValueType root = std::sqrt(t + ValueType(1.0));         // 2w

        Quaternion<T> q;
        q.s   = ValueType(0.5) * root;
        root  = ValueType(0.5) / root;                          // 1/(4w)
        q.v.x = (m_comp[7] - m_comp[5]) * root;
        q.v.y = (m_comp[2] - m_comp[6]) * root;
        q.v.z = (m_comp[3] - m_comp[1]) * root;
        return q;
    }
    else
    {
        // |w| <= 1/2.

        size_t i = 0;
        if (m_comp[4] > m_comp[0]) i = 1;
        if (m_comp[8] > m_comp[i*3+i]) i = 2;

        // Fast modulo 3.
        // See http://www.codercorner.com/Modulo3.htm.
        const size_t j = (1UL << i) & 3;
        const size_t k = (1UL << j) & 3;

        ValueType root =
            std::sqrt(m_comp[i*3+i] - m_comp[j*3+j] - m_comp[k*3+k] + ValueType(1.0));

        Quaternion<T> q;
        q.v[i] = ValueType(0.5) * root;
        root   = ValueType(0.5) / root;
        q.s    = (m_comp[k * 3 + j] - m_comp[j * 3 + k]) * root;
        q.v[j] = (m_comp[j * 3 + i] + m_comp[i * 3 + j]) * root;
        q.v[k] = (m_comp[k * 3 + i] + m_comp[i * 3 + k]) * root;
        return q;
    }
}

template <typename T>
inline void Matrix<T, 3, 3>::decompose(
    Vector<T, 3>&           scaling,
    Quaternion<T>&          rotation) const
{
    scaling = extract_scaling();

    if (det(*this) < T(0.0))
        scaling[0] = -scaling[0];

    const Vector<T, 3> rcp_scaling(
        T(1.0) / scaling[0],
        T(1.0) / scaling[1],
        T(1.0) / scaling[2]);

    MatrixType unit_matrix(*this);

    unit_matrix[0] *= rcp_scaling[0];
    unit_matrix[3] *= rcp_scaling[0];
    unit_matrix[6] *= rcp_scaling[0];

    unit_matrix[1] *= rcp_scaling[1];
    unit_matrix[4] *= rcp_scaling[1];
    unit_matrix[7] *= rcp_scaling[1];

    unit_matrix[2] *= rcp_scaling[2];
    unit_matrix[5] *= rcp_scaling[2];
    unit_matrix[8] *= rcp_scaling[2];

    rotation = unit_matrix.extract_unit_quaternion();
}

template <typename T>
inline T det(const Matrix<T, 3, 3>& mat)
{
    return
        mat[0] * mat[4] * mat[8] +
        mat[1] * mat[5] * mat[6] +
        mat[2] * mat[3] * mat[7] -
        mat[2] * mat[4] * mat[6] -
        mat[1] * mat[3] * mat[8] -
        mat[0] * mat[5] * mat[7];
}

template <typename T>
inline Vector<T, 3> rotate(
    const Vector<T, 3>&     v,
    const Vector<T, 3>&     axis,
    const T                 angle)
{
    return Matrix<T, 3, 3>::make_rotation(axis, angle) * v;
}

template <typename T>
inline Matrix<T, 3, 3> operator*(
    const Matrix<T, 3, 3>&  lhs,
    const Matrix<T, 3, 3>&  rhs)
{
    Matrix<T, 3, 3> res;

    res[0] = lhs[0] * rhs[0] + lhs[1] * rhs[3] + lhs[2] * rhs[6];
    res[1] = lhs[0] * rhs[1] + lhs[1] * rhs[4] + lhs[2] * rhs[7];
    res[2] = lhs[0] * rhs[2] + lhs[1] * rhs[5] + lhs[2] * rhs[8];

    res[3] = lhs[3] * rhs[0] + lhs[4] * rhs[3] + lhs[5] * rhs[6];
    res[4] = lhs[3] * rhs[1] + lhs[4] * rhs[4] + lhs[5] * rhs[7];
    res[5] = lhs[3] * rhs[2] + lhs[4] * rhs[5] + lhs[5] * rhs[8];

    res[6] = lhs[6] * rhs[0] + lhs[7] * rhs[3] + lhs[8] * rhs[6];
    res[7] = lhs[6] * rhs[1] + lhs[7] * rhs[4] + lhs[8] * rhs[7];
    res[8] = lhs[6] * rhs[2] + lhs[7] * rhs[5] + lhs[8] * rhs[8];

    return res;
}

template <typename T>
inline Vector<T, 3> operator*(
    const Matrix<T, 3, 3>&  m,
    const Vector<T, 3>&     v)
{
    Vector<T, 3> res;

    res[0] = m[0] * v[0] + m[1] * v[1] + m[2] * v[2];
    res[1] = m[3] * v[0] + m[4] * v[1] + m[5] * v[2];
    res[2] = m[6] * v[0] + m[7] * v[1] + m[8] * v[2];

    return res;
}

template <typename T>
inline Vector<T, 3> operator*(
    const Vector<T, 3>&     v,
    const Matrix<T, 3, 3>&  m)
{
    Vector<T, 3> res;

    res[0] = v[0] * m[0] + v[1] * m[3] + v[2] * m[6];
    res[1] = v[0] * m[1] + v[1] * m[4] + v[2] * m[7];
    res[2] = v[0] * m[2] + v[1] * m[5] + v[2] * m[8];

    return res;
}


//
// 4x4 matrix class implementation.
//

template <typename T>
const Matrix<T, 4, 4> Matrix<T, 4, 4>::m_identity(Matrix<T, 4, 4>::make_identity());

template <typename T>
inline Matrix<T, 4, 4>::Matrix(const ValueType val)
{
    for (size_t i = 0; i < Components; ++i)
        m_comp[i] = val;
}

template <typename T>
template <typename U>
inline Matrix<T, 4, 4>::Matrix(const Matrix<U, 4, 4>& rhs)
{
    for (size_t i = 0; i < Components; ++i)
        m_comp[i] = static_cast<ValueType>(rhs[i]);
}

#ifdef APPLESEED_ENABLE_IMATH_INTEROP

template <typename T>
inline Matrix<T, 4, 4>::Matrix(const Imath::Matrix44<T>& rhs)
{
    T* p = m_comp;

    for (int i = 0; i < 4; ++i)
    {
        for (int j = 0; j < 4; ++j)
            *p++ = rhs[j][i];
    }
}

template <typename T>
inline Matrix<T, 4, 4>::operator Imath::Matrix44<T>() const
{
    Imath::Matrix44<T> result;
    T* p = &result[0][0];

    for (size_t i = 0; i < 4; ++i)
    {
        for (size_t j = 0; j < 4; ++j)
            *p++ = (*this)(j, i);
    }

    return result;
}

#endif

template <typename T>
inline Matrix<T, 4, 4> Matrix<T, 4, 4>::from_array(const ValueType* rhs)
{
    assert(rhs);

    Matrix result;

    for (size_t i = 0; i < Components; ++i)
        result.m_comp[i] = rhs[i];

    return result;
}

template <typename T>
Matrix<T, 4, 4> Matrix<T, 4, 4>::make_identity()
{
    MatrixType mat;

    mat[ 0] = T(1.0);
    mat[ 1] = T(0.0);
    mat[ 2] = T(0.0);
    mat[ 3] = T(0.0);

    mat[ 4] = T(0.0);
    mat[ 5] = T(1.0);
    mat[ 6] = T(0.0);
    mat[ 7] = T(0.0);

    mat[ 8] = T(0.0);
    mat[ 9] = T(0.0);
    mat[10] = T(1.0);
    mat[11] = T(0.0);

    mat[12] = T(0.0);
    mat[13] = T(0.0);
    mat[14] = T(0.0);
    mat[15] = T(1.0);

    return mat;
}

template <typename T>
inline const Matrix<T, 4, 4>& Matrix<T, 4, 4>::identity()
{
    return m_identity;
}

template <typename T>
inline Matrix<T, 4, 4> Matrix<T, 4, 4>::make_translation(const Vector<T, 3>& v)
{
    MatrixType mat;

    mat[ 0] = T(1.0);
    mat[ 1] = T(0.0);
    mat[ 2] = T(0.0);
    mat[ 3] = v.x;

    mat[ 4] = T(0.0);
    mat[ 5] = T(1.0);
    mat[ 6] = T(0.0);
    mat[ 7] = v.y;

    mat[ 8] = T(0.0);
    mat[ 9] = T(0.0);
    mat[10] = T(1.0);
    mat[11] = v.z;

    mat[12] = T(0.0);
    mat[13] = T(0.0);
    mat[14] = T(0.0);
    mat[15] = T(1.0);

    return mat;
}

template <typename T>
inline Matrix<T, 4, 4> Matrix<T, 4, 4>::make_scaling(const Vector<T, 3>& s)
{
    MatrixType mat;

    mat[ 0] = s.x;
    mat[ 1] = T(0.0);
    mat[ 2] = T(0.0);
    mat[ 3] = T(0.0);

    mat[ 4] = T(0.0);
    mat[ 5] = s.y;
    mat[ 6] = T(0.0);
    mat[ 7] = T(0.0);

    mat[ 8] = T(0.0);
    mat[ 9] = T(0.0);
    mat[10] = s.z;
    mat[11] = T(0.0);

    mat[12] = T(0.0);
    mat[13] = T(0.0);
    mat[14] = T(0.0);
    mat[15] = T(1.0);

    return mat;
}

template <typename T>
inline Matrix<T, 4, 4> Matrix<T, 4, 4>::make_rotation_x(const ValueType angle)
{
    return make_rotation_x(std::cos(angle), std::sin(angle));
}

template <typename T>
inline Matrix<T, 4, 4> Matrix<T, 4, 4>::make_rotation_y(const ValueType angle)
{
    return make_rotation_y(std::cos(angle), std::sin(angle));
}

template <typename T>
inline Matrix<T, 4, 4> Matrix<T, 4, 4>::make_rotation_z(const ValueType angle)
{
    return make_rotation_z(std::cos(angle), std::sin(angle));
}

template <typename T>
inline Matrix<T, 4, 4> Matrix<T, 4, 4>::make_rotation_x(
    const ValueType         cos_angle,
    const ValueType         sin_angle)
{
    MatrixType mat;

    mat[ 0] = T(1.0);
    mat[ 1] = T(0.0);
    mat[ 2] = T(0.0);
    mat[ 3] = T(0.0);

    mat[ 4] = T(0.0);
    mat[ 5] = cos_angle;
    mat[ 6] = -sin_angle;
    mat[ 7] = T(0.0);

    mat[ 8] = T(0.0);
    mat[ 9] = sin_angle;
    mat[10] = cos_angle;
    mat[11] = T(0.0);

    mat[12] = T(0.0);
    mat[13] = T(0.0);
    mat[14] = T(0.0);
    mat[15] = T(1.0);

    return mat;
}

template <typename T>
inline Matrix<T, 4, 4> Matrix<T, 4, 4>::make_rotation_y(
    const ValueType         cos_angle,
    const ValueType         sin_angle)
{
    MatrixType mat;

    mat[ 0] = cos_angle;
    mat[ 1] = T(0.0);
    mat[ 2] = sin_angle;
    mat[ 3] = T(0.0);

    mat[ 4] = T(0.0);
    mat[ 5] = T(1.0);
    mat[ 6] = T(0.0);
    mat[ 7] = T(0.0);

    mat[ 8] = -sin_angle;
    mat[ 9] = T(0.0);
    mat[10] = cos_angle;
    mat[11] = T(0.0);

    mat[12] = T(0.0);
    mat[13] = T(0.0);
    mat[14] = T(0.0);
    mat[15] = T(1.0);

    return mat;
}

template <typename T>
inline Matrix<T, 4, 4> Matrix<T, 4, 4>::make_rotation_z(
    const ValueType         cos_angle,
    const ValueType         sin_angle)
{
    MatrixType mat;

    mat[ 0] = cos_angle;
    mat[ 1] = -sin_angle;
    mat[ 2] = T(0.0);
    mat[ 3] = T(0.0);

    mat[ 4] = sin_angle;
    mat[ 5] = cos_angle;
    mat[ 6] = T(0.0);
    mat[ 7] = T(0.0);

    mat[ 8] = T(0.0);
    mat[ 9] = T(0.0);
    mat[10] = T(1.0);
    mat[11] = T(0.0);

    mat[12] = T(0.0);
    mat[13] = T(0.0);
    mat[14] = T(0.0);
    mat[15] = T(1.0);

    return mat;
}

template <typename T>
inline Matrix<T, 4, 4> Matrix<T, 4, 4>::make_rotation(
    const ValueType         yaw,
    const ValueType         pitch,
    const ValueType         roll)
{
    const T cos_yaw   = std::cos(yaw);
    const T sin_yaw   = std::sin(yaw);
    const T cos_pitch = std::cos(pitch);
    const T sin_pitch = std::sin(pitch);
    const T cos_roll  = std::cos(roll);
    const T sin_roll  = std::sin(roll);

    MatrixType mat;

    mat[ 0] = cos_yaw * cos_roll - sin_yaw * sin_pitch * sin_roll;
    mat[ 1] = -cos_pitch * sin_roll;
    mat[ 2] = sin_yaw * cos_roll + cos_yaw * sin_pitch * sin_roll;
    mat[ 3] = T(0.0);

    mat[ 4] = cos_yaw * sin_roll + sin_yaw * sin_pitch * cos_roll;
    mat[ 5] = cos_pitch * cos_roll;
    mat[ 6] = sin_yaw * sin_roll - cos_yaw * sin_pitch * cos_roll;
    mat[ 7] = T(0.0);

    mat[ 8] = -sin_yaw * cos_pitch;
    mat[ 9] = sin_pitch;
    mat[10] = cos_yaw * cos_pitch;
    mat[11] = T(0.0);

    mat[12] = T(0.0);
    mat[13] = T(0.0);
    mat[14] = T(0.0);
    mat[15] = T(1.0);

    return mat;
}

template <typename T>
inline Matrix<T, 4, 4> Matrix<T, 4, 4>::make_rotation(
    const Vector<T, 3>&     axis,
    const ValueType         angle)
{
    return make_rotation(axis, std::cos(angle), std::sin(angle));
}

template <typename T>
inline Matrix<T, 4, 4> Matrix<T, 4, 4>::make_rotation(
    const Vector<T, 3>&     axis,
    const ValueType         cos_angle,
    const ValueType         sin_angle)
{
    assert(is_normalized(axis));

    const T one_min_cos_angle = T(1.0) - cos_angle;

    MatrixType mat;

    mat[ 0] = one_min_cos_angle * axis.x * axis.x + cos_angle;
    mat[ 1] = one_min_cos_angle * axis.x * axis.y - sin_angle * axis.z;
    mat[ 2] = one_min_cos_angle * axis.x * axis.z + sin_angle * axis.y;
    mat[ 3] = T(0.0);

    mat[ 4] = one_min_cos_angle * axis.y * axis.x + sin_angle * axis.z;
    mat[ 5] = one_min_cos_angle * axis.y * axis.y + cos_angle;
    mat[ 6] = one_min_cos_angle * axis.y * axis.z - sin_angle * axis.x;
    mat[ 7] = T(0.0);

    mat[ 8] = one_min_cos_angle * axis.z * axis.x - sin_angle * axis.y;
    mat[ 9] = one_min_cos_angle * axis.z * axis.y + sin_angle * axis.x;
    mat[10] = one_min_cos_angle * axis.z * axis.z + cos_angle;
    mat[11] = T(0.0);

    mat[12] = T(0.0);
    mat[13] = T(0.0);
    mat[14] = T(0.0);
    mat[15] = T(1.0);

    return mat;
}

template <typename T>
inline Matrix<T, 4, 4> Matrix<T, 4, 4>::make_rotation(
    const Quaternion<T>&    q)
{
    //
    // Implementation from Wild Magic Source Code, David Eberly
    // http://www.geometrictools.com
    //

    assert(is_normalized(q));

    const ValueType tx  = q.v[0] + q.v[0];
    const ValueType ty  = q.v[1] + q.v[1];
    const ValueType tz  = q.v[2] + q.v[2];
    const ValueType twx = tx * q.s;
    const ValueType twy = ty * q.s;
    const ValueType twz = tz * q.s;
    const ValueType txx = tx * q.v[0];
    const ValueType txy = ty * q.v[0];
    const ValueType txz = tz * q.v[0];
    const ValueType tyy = ty * q.v[1];
    const ValueType tyz = tz * q.v[1];
    const ValueType tzz = tz * q.v[2];

    MatrixType mat;

    mat[ 0] = ValueType(1.0) - (tyy + tzz);
    mat[ 1] = txy - twz;
    mat[ 2] = txz + twy;
    mat[ 3] = T(0.0);

    mat[ 4] = txy + twz;
    mat[ 5] = ValueType(1.0) - (txx + tzz);
    mat[ 6] = tyz - twx;
    mat[ 7] = T(0.0);

    mat[ 8] = txz - twy;
    mat[ 9] = tyz + twx;
    mat[10] = ValueType(1.0) - (txx + tyy);
    mat[11] = T(0.0);

    mat[12] = T(0.0);
    mat[13] = T(0.0);
    mat[14] = T(0.0);
    mat[15] = T(1.0);

    return mat;
}

template <typename T>
inline Matrix<T, 4, 4> Matrix<T, 4, 4>::make_lookat(
    const Vector<T, 3>&     origin,
    const Vector<T, 3>&     target,
    const Vector<T, 3>&     up)
{
    assert(is_normalized(up));

    const Vector<T, 3> z = normalize(origin - target);
    const Vector<T, 3> x = normalize(cross(up, z));
    const Vector<T, 3> y = cross(z,  x);

    // Make sure (x, y, z) forms an orthonormal basis.
    assert(is_normalized(x));
    assert(is_normalized(y));
    assert(is_normalized(z));
    assert(fz(dot(x, y)));
    assert(fz(dot(x, z)));
    assert(fz(dot(y, z)));

    MatrixType mat;

    mat[ 0] = x.x;
    mat[ 1] = y.x;
    mat[ 2] = z.x;
    mat[ 3] = origin.x;

    mat[ 4] = x.y;
    mat[ 5] = y.y;
    mat[ 6] = z.y;
    mat[ 7] = origin.y;

    mat[ 8] = x.z;
    mat[ 9] = y.z;
    mat[10] = z.z;
    mat[11] = origin.z;

    mat[12] = T(0.0);
    mat[13] = T(0.0);
    mat[14] = T(0.0);
    mat[15] = T(1.0);

    return mat;
}

template <typename T>
Matrix<T, 4, 4> Matrix<T, 4, 4>::make_frustum(
    const ValueType bottom,
    const ValueType top,
    const ValueType left,
    const ValueType right,
    const ValueType z_near,
    const ValueType z_far)
{
    assert(left   != right);
    assert(top    != bottom);
    assert(z_near != z_far);

    const T a = (right + left) / (right - left);
    const T b = (top + bottom) / (top - bottom);
    const T c = -(z_far + z_near) / (z_far - z_near);
    const T d = T(-2.0) * (z_far * z_near) / (z_far - z_near);

    MatrixType mat;

    mat[ 0] = T(2.0) * z_near / (right - left);
    mat[ 1] = T(0.0);
    mat[ 2] = a;
    mat[ 3] = T(0.0);

    mat[ 4] = T(0.0);
    mat[ 5] = T(2.0) * z_near / (top - bottom);
    mat[ 6] = b;
    mat[ 7] = T(0.0);

    mat[ 8] = T(0.0);
    mat[ 9] = T(0.0);
    mat[10] = c;
    mat[11] = d;

    mat[12] = T(0.0);
    mat[13] = T(0.0);
    mat[14] = T(-1.0);
    mat[15] = T(0.0);

    return mat;
}

template <typename T>
inline T& Matrix<T, 4, 4>::operator[](const size_t i)
{
    assert(i < Components);
    return m_comp[i];
}

template <typename T>
inline const T& Matrix<T, 4, 4>::operator[](const size_t i) const
{
    assert(i < Components);
    return m_comp[i];
}

template <typename T>
inline T& Matrix<T, 4, 4>::operator()(const size_t row, const size_t col)
{
    assert(row < Rows);
    assert(col < Columns);
    return m_comp[row * Columns + col];
}

template <typename T>
inline const T& Matrix<T, 4, 4>::operator()(const size_t row, const size_t col) const
{
    assert(row < Rows);
    assert(col < Columns);
    return m_comp[row * Columns + col];
}

template <typename T>
inline Matrix<T, 3, 3> Matrix<T, 4, 4>::extract_matrix3() const
{
    Matrix<T, 3, 3> mat;

    mat[0] = m_comp[ 0];
    mat[1] = m_comp[ 1];
    mat[2] = m_comp[ 2];

    mat[3] = m_comp[ 4];
    mat[4] = m_comp[ 5];
    mat[5] = m_comp[ 6];

    mat[6] = m_comp[ 8];
    mat[7] = m_comp[ 9];
    mat[8] = m_comp[10];

    return mat;
}

template <typename T>
inline Vector<T, 3> Matrix<T, 4, 4>::extract_translation() const
{
    return Vector<T, 3>(m_comp[3], m_comp[7], m_comp[11]);
}

template <typename T>
inline void Matrix<T, 4, 4>::decompose(
    Vector<T, 3>&           scaling,
    Quaternion<T>&          rotation,
    Vector<T, 3>&           translation) const
{
    Matrix<T, 3, 3> matrix3(extract_matrix3());
    matrix3.decompose(scaling, rotation);
    translation = extract_translation();
}

template <typename T>
inline bool swaps_handedness(const Matrix<T, 4, 4>& mat)
{
    const T d =
        mat[0] * mat[5] * mat[10] +
        mat[1] * mat[6] * mat[ 8] +
        mat[2] * mat[4] * mat[ 9] -
        mat[2] * mat[5] * mat[ 8] -
        mat[1] * mat[4] * mat[10] -
        mat[0] * mat[6] * mat[ 9];

    return d < T(0.0);
}

template <typename T>
inline Matrix<T, 4, 4> operator*(
    const Matrix<T, 4, 4>&  lhs,
    const Matrix<T, 4, 4>&  rhs)
{
    Matrix<T, 4, 4> res;

    res[ 0] = lhs[ 0] * rhs[0] + lhs[ 1] * rhs[4] + lhs[ 2] * rhs[ 8] + lhs[ 3] * rhs[12];
    res[ 1] = lhs[ 0] * rhs[1] + lhs[ 1] * rhs[5] + lhs[ 2] * rhs[ 9] + lhs[ 3] * rhs[13];
    res[ 2] = lhs[ 0] * rhs[2] + lhs[ 1] * rhs[6] + lhs[ 2] * rhs[10] + lhs[ 3] * rhs[14];
    res[ 3] = lhs[ 0] * rhs[3] + lhs[ 1] * rhs[7] + lhs[ 2] * rhs[11] + lhs[ 3] * rhs[15];

    res[ 4] = lhs[ 4] * rhs[0] + lhs[ 5] * rhs[4] + lhs[ 6] * rhs[ 8] + lhs[ 7] * rhs[12];
    res[ 5] = lhs[ 4] * rhs[1] + lhs[ 5] * rhs[5] + lhs[ 6] * rhs[ 9] + lhs[ 7] * rhs[13];
    res[ 6] = lhs[ 4] * rhs[2] + lhs[ 5] * rhs[6] + lhs[ 6] * rhs[10] + lhs[ 7] * rhs[14];
    res[ 7] = lhs[ 4] * rhs[3] + lhs[ 5] * rhs[7] + lhs[ 6] * rhs[11] + lhs[ 7] * rhs[15];

    res[ 8] = lhs[ 8] * rhs[0] + lhs[ 9] * rhs[4] + lhs[10] * rhs[ 8] + lhs[11] * rhs[12];
    res[ 9] = lhs[ 8] * rhs[1] + lhs[ 9] * rhs[5] + lhs[10] * rhs[ 9] + lhs[11] * rhs[13];
    res[10] = lhs[ 8] * rhs[2] + lhs[ 9] * rhs[6] + lhs[10] * rhs[10] + lhs[11] * rhs[14];
    res[11] = lhs[ 8] * rhs[3] + lhs[ 9] * rhs[7] + lhs[10] * rhs[11] + lhs[11] * rhs[15];

    res[12] = lhs[12] * rhs[0] + lhs[13] * rhs[4] + lhs[14] * rhs[ 8] + lhs[15] * rhs[12];
    res[13] = lhs[12] * rhs[1] + lhs[13] * rhs[5] + lhs[14] * rhs[ 9] + lhs[15] * rhs[13];
    res[14] = lhs[12] * rhs[2] + lhs[13] * rhs[6] + lhs[14] * rhs[10] + lhs[15] * rhs[14];
    res[15] = lhs[12] * rhs[3] + lhs[13] * rhs[7] + lhs[14] * rhs[11] + lhs[15] * rhs[15];

    return res;
}

#ifdef APPLESEED_USE_SSE

// SSE2-optimized double precision 4x4 matrix multiplication.
template <>
inline Matrix<double, 4, 4> operator*(
    const Matrix<double, 4, 4>& lhs,
    const Matrix<double, 4, 4>& rhs)
{
    Matrix<double, 4, 4> res;

    const __m128d rhs0  = _mm_load_pd(&rhs[0]);
    const __m128d rhs2  = _mm_load_pd(&rhs[2]);
    const __m128d rhs4  = _mm_load_pd(&rhs[4]);
    const __m128d rhs6  = _mm_load_pd(&rhs[6]);
    const __m128d rhs8  = _mm_load_pd(&rhs[8]);
    const __m128d rhs10 = _mm_load_pd(&rhs[10]);
    const __m128d rhs12 = _mm_load_pd(&rhs[12]);
    const __m128d rhs14 = _mm_load_pd(&rhs[14]);

    __m128d res0, res2;

    const __m128d lhs0 = _mm_set1_pd(lhs[0]);
    const __m128d lhs1 = _mm_set1_pd(lhs[1]);
    const __m128d lhs2 = _mm_set1_pd(lhs[2]);
    const __m128d lhs3 = _mm_set1_pd(lhs[3]);

    res0 = _mm_mul_pd(lhs0, rhs0);
    res2 = _mm_mul_pd(lhs0, rhs2);
    res0 = _mm_add_pd(res0, _mm_mul_pd(lhs1, rhs4));
    res2 = _mm_add_pd(res2, _mm_mul_pd(lhs1, rhs6));
    res0 = _mm_add_pd(res0, _mm_mul_pd(lhs2, rhs8));
    res2 = _mm_add_pd(res2, _mm_mul_pd(lhs2, rhs10));
    res0 = _mm_add_pd(res0, _mm_mul_pd(lhs3, rhs12));
    res2 = _mm_add_pd(res2, _mm_mul_pd(lhs3, rhs14));

    _mm_store_pd(&res[0], res0);
    _mm_store_pd(&res[2], res2);

    const __m128d lhs4 = _mm_set1_pd(lhs[4]);
    const __m128d lhs5 = _mm_set1_pd(lhs[5]);
    const __m128d lhs6 = _mm_set1_pd(lhs[6]);
    const __m128d lhs7 = _mm_set1_pd(lhs[7]);

    res0 = _mm_mul_pd(lhs4, rhs0);
    res2 = _mm_mul_pd(lhs4, rhs2);
    res0 = _mm_add_pd(res0, _mm_mul_pd(lhs5, rhs4));
    res2 = _mm_add_pd(res2, _mm_mul_pd(lhs5, rhs6));
    res0 = _mm_add_pd(res0, _mm_mul_pd(lhs6, rhs8));
    res2 = _mm_add_pd(res2, _mm_mul_pd(lhs6, rhs10));
    res0 = _mm_add_pd(res0, _mm_mul_pd(lhs7, rhs12));
    res2 = _mm_add_pd(res2, _mm_mul_pd(lhs7, rhs14));

    _mm_store_pd(&res[4], res0);
    _mm_store_pd(&res[6], res2);

    const __m128d lhs8  = _mm_set1_pd(lhs[8]);
    const __m128d lhs9  = _mm_set1_pd(lhs[9]);
    const __m128d lhs10 = _mm_set1_pd(lhs[10]);
    const __m128d lhs11 = _mm_set1_pd(lhs[11]);

    res0 = _mm_mul_pd(lhs8, rhs0);
    res2 = _mm_mul_pd(lhs8, rhs2);
    res0 = _mm_add_pd(res0, _mm_mul_pd(lhs9, rhs4));
    res2 = _mm_add_pd(res2, _mm_mul_pd(lhs9, rhs6));
    res0 = _mm_add_pd(res0, _mm_mul_pd(lhs10, rhs8));
    res2 = _mm_add_pd(res2, _mm_mul_pd(lhs10, rhs10));
    res0 = _mm_add_pd(res0, _mm_mul_pd(lhs11, rhs12));
    res2 = _mm_add_pd(res2, _mm_mul_pd(lhs11, rhs14));

    _mm_store_pd(&res[8],  res0);
    _mm_store_pd(&res[10], res2);

    const __m128d lhs12 = _mm_set1_pd(lhs[12]);
    const __m128d lhs13 = _mm_set1_pd(lhs[13]);
    const __m128d lhs14 = _mm_set1_pd(lhs[14]);
    const __m128d lhs15 = _mm_set1_pd(lhs[15]);

    res0 = _mm_mul_pd(lhs12, rhs0);
    res2 = _mm_mul_pd(lhs12, rhs2);
    res0 = _mm_add_pd(res0, _mm_mul_pd(lhs13, rhs4));
    res2 = _mm_add_pd(res2, _mm_mul_pd(lhs13, rhs6));
    res0 = _mm_add_pd(res0, _mm_mul_pd(lhs14, rhs8));
    res2 = _mm_add_pd(res2, _mm_mul_pd(lhs14, rhs10));
    res0 = _mm_add_pd(res0, _mm_mul_pd(lhs15, rhs12));
    res2 = _mm_add_pd(res2, _mm_mul_pd(lhs15, rhs14));

    _mm_store_pd(&res[12], res0);
    _mm_store_pd(&res[14], res2);

    return res;
}

#endif  // APPLESEED_USE_SSE

template <typename T>
inline Vector<T, 4> operator*(
    const Matrix<T, 4, 4>&  m,
    const Vector<T, 4>&     v)
{
    Vector<T, 4> res;

    res[0] = m[ 0] * v[0] + m[ 1] * v[1] + m[ 2] * v[2] + m[ 3] * v[3];
    res[1] = m[ 4] * v[0] + m[ 5] * v[1] + m[ 6] * v[2] + m[ 7] * v[3];
    res[2] = m[ 8] * v[0] + m[ 9] * v[1] + m[10] * v[2] + m[11] * v[3];
    res[3] = m[12] * v[0] + m[13] * v[1] + m[14] * v[2] + m[15] * v[3];

    return res;
}

#ifdef APPLESEED_USE_SSE

// SSE2-optimized double precision 4x4 matrix-vector multiplication.
template <>
inline Vector<double, 4> operator*(
    const Matrix<double, 4, 4>& m,
    const Vector<double, 4>&    v)
{
    Vector<double, 4> res;

    const __m128d v0 = _mm_loadu_pd(&v[0]);
    const __m128d v2 = _mm_loadu_pd(&v[2]);

    const __m128d res0 = _mm_add_pd(_mm_mul_pd(_mm_load_pd(&m[ 0]), v0), _mm_mul_pd(_mm_load_pd(&m[ 2]), v2));
    const __m128d res1 = _mm_add_pd(_mm_mul_pd(_mm_load_pd(&m[ 4]), v0), _mm_mul_pd(_mm_load_pd(&m[ 6]), v2));
    const __m128d res2 = _mm_add_pd(_mm_mul_pd(_mm_load_pd(&m[ 8]), v0), _mm_mul_pd(_mm_load_pd(&m[10]), v2));
    const __m128d res3 = _mm_add_pd(_mm_mul_pd(_mm_load_pd(&m[12]), v0), _mm_mul_pd(_mm_load_pd(&m[14]), v2));

    _mm_storeu_pd(
        &res[0],
        _mm_add_pd(
            _mm_shuffle_pd(res0, res1, _MM_SHUFFLE2(0, 0)),
            _mm_shuffle_pd(res0, res1, _MM_SHUFFLE2(1, 1))));

    _mm_storeu_pd(
        &res[2],
        _mm_add_pd(
            _mm_shuffle_pd(res2, res3, _MM_SHUFFLE2(0, 0)),
            _mm_shuffle_pd(res2, res3, _MM_SHUFFLE2(1, 1))));

    return res;
}

#endif  // APPLESEED_USE_SSE

template <typename T>
inline Vector<T, 4> operator*(
    const Vector<T, 4>&     v,
    const Matrix<T, 4, 4>&  m)
{
    Vector<T, 4> res;

    res[0] = v[0] * m[ 0] + v[1] * m[ 4] + v[2] * m[ 8] + v[3] * m[12];
    res[1] = v[0] * m[ 1] + v[1] * m[ 5] + v[2] * m[ 9] + v[3] * m[13];
    res[2] = v[0] * m[ 2] + v[1] * m[ 6] + v[2] * m[10] + v[3] * m[14];
    res[3] = v[0] * m[ 3] + v[1] * m[ 7] + v[2] * m[11] + v[3] * m[15];

    return res;
}

}   // namespace foundation
