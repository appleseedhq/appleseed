
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

#ifndef APPLESEED_FOUNDATION_MATH_MATRIX_H
#define APPLESEED_FOUNDATION_MATH_MATRIX_H

// appleseed.foundation headers.
#include "foundation/core/exception.h"
#include "foundation/core/exceptionnotimplemented.h"
#include "foundation/math/quaternion.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#ifdef APPLESEED_FOUNDATION_USE_SSE
#include "foundation/platform/sse.h"
#endif

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>

namespace foundation
{

//
// References:
//
//   http://en.wikipedia.org/wiki/Rotation_matrix
//
//   Real-Time Rendering, Second Edition, A. K. Peters
//


//
// MxN matrix class (M rows, N columns) of arbitrary type.
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
    Matrix();                                               // leave all components uninitialized
    explicit Matrix(const ValueType* rhs);                  // initialize with array of M * N scalars
    explicit Matrix(const ValueType val);                   // set all components to 'val'

    // Construct a matrix from another matrix of a different type.
    template <typename U>
    explicit Matrix(const Matrix<U, M, N>& rhs);

    // Unchecked array subscripting.
    ValueType& operator[](const size_t i);
    const ValueType& operator[](const size_t i) const;

    // Unchecked Fortran-style subscripting (0-based).
    ValueType& operator()(const size_t row, const size_t col);
    const ValueType& operator()(const size_t row, const size_t col) const;

  private:
    ValueType m_comp[Components];
};

// Exact inequality and equality tests.
template <typename T, size_t M, size_t N> bool operator!=(const Matrix<T, M, N>& lhs, const Matrix<T, M, N>& rhs);
template <typename T, size_t M, size_t N> bool operator==(const Matrix<T, M, N>& lhs, const Matrix<T, M, N>& rhs);

// Approximate equality tests.
template <typename T, size_t M, size_t N> bool feq(const Matrix<T, M, N>& lhs, const Matrix<T, M, N>& rhs);
template <typename T, size_t M, size_t N> bool feq(const Matrix<T, M, N>& lhs, const Matrix<T, M, N>& rhs, const T eps);

// Approximate zero tests.
template <typename T, size_t M, size_t N> bool fz(const Matrix<T, M, N>& v);
template <typename T, size_t M, size_t N> bool fz(const Matrix<T, M, N>& v, const T eps);

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
    const Matrix<T, M, N>&  lhs,
    const Vector<T, N>&     rhs);

// Vector-matrix multiplication (1xM * MxN = 1xN).
template <typename T, size_t M, size_t N>
Vector<T, N> operator*(
    const Vector<T, M>&     lhs,
    const Matrix<T, M, N>&  rhs);

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
    Matrix();                                               // leave all components uninitialized
    explicit Matrix(const ValueType* rhs);                  // initialize with array of N * N scalars
    explicit Matrix(const ValueType val);                   // set all components to 'val'

    // Construct a matrix from another matrix of a different type.
    template <typename U>
    explicit Matrix(const Matrix<U, N, N>& rhs);

    // Return the NxN identity matrix.
    static MatrixType identity();

    // Unchecked array subscripting.
    ValueType& operator[](const size_t i);
    const ValueType& operator[](const size_t i) const;

    // Unchecked Fortran-style subscripting (0-based).
    ValueType& operator()(const size_t row, const size_t col);
    const ValueType& operator()(const size_t row, const size_t col) const;

  private:
    ValueType m_comp[Components];
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
    Matrix();                                               // leave all components uninitialized
    explicit Matrix(const ValueType* rhs);                  // initialize with array of 9 scalars
    explicit Matrix(const ValueType val);                   // set all components to 'val'

    // Construct a matrix from another matrix of a different type.
    template <typename U>
    explicit Matrix(const Matrix<U, 3, 3>& rhs);

    // Return the 3x3 identity matrix.
    static MatrixType identity();

    // Build canonical transformation matrices.
    static MatrixType scaling(const Vector<T, 3>& s);
    static MatrixType rotation_x(const ValueType angle);    // rotation angle, in radians
    static MatrixType rotation_y(const ValueType angle);    // rotation angle, in radians
    static MatrixType rotation_z(const ValueType angle);    // rotation angle, in radians

    // Build a rotation matrix from Euler angles.
    static MatrixType rotation(
        const ValueType         yaw,                        // rotation angle, in radians
        const ValueType         pitch,                      // rotation angle, in radians
        const ValueType         roll);                      // rotation angle, in radians

    // Build a rotation matrix from an axis and an angle.
    static MatrixType rotation(
        const Vector<T, 3>&     axis,                       // rotation axis, unit-length
        const ValueType         angle);                     // rotation angle, in radians

    // Build a rotation matrix from a unit quaternion.
    static MatrixType rotation(
        const Quaternion<T>&    q);                         // unit quaternion

    // Extract Euler angles from a rotation matrix.
    void extract_euler_angles(
        ValueType&              yaw,
        ValueType&              pitch,
        ValueType&              roll) const;

    // Extract a unit quaternion from a rotation matrix.
    Quaternion<T> extract_unit_quaternion() const;

    // Unchecked array subscripting.
    ValueType& operator[](const size_t i);
    const ValueType& operator[](const size_t i) const;

    // Unchecked Fortran-style subscripting (0-based).
    ValueType& operator()(const size_t row, const size_t col);
    const ValueType& operator()(const size_t row, const size_t col) const;

  private:
    ValueType m_comp[Components];
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
    Matrix();                                               // leave all components uninitialized
    explicit Matrix(const ValueType* rhs);                  // initialize with array of 16 scalars
    explicit Matrix(const ValueType val);                   // set all components to 'val'

    // Construct a matrix from another matrix of a different type.
    template <typename U>
    explicit Matrix(const Matrix<U, 4, 4>& rhs);

    // Return the 4x4 identity matrix.
    static MatrixType identity();

    // Build canonical transformation matrices.
    static MatrixType translation(const Vector<T, 3>& v);
    static MatrixType scaling(const Vector<T, 3>& s);
    static MatrixType rotation_x(const ValueType angle);    // rotation angle, in radians
    static MatrixType rotation_y(const ValueType angle);    // rotation angle, in radians
    static MatrixType rotation_z(const ValueType angle);    // rotation angle, in radians

    // Build a rotation matrix from Euler angles.
    static MatrixType rotation(
        const ValueType         yaw,                        // rotation angle, in radians
        const ValueType         pitch,                      // rotation angle, in radians
        const ValueType         roll);                      // rotation angle, in radians

    // Build a rotation matrix from an axis and an angle.
    static MatrixType rotation(                
        const Vector<T, 3>&     axis,                       // rotation axis, unit-length
        const ValueType         angle);                     // rotation angle, in radians

    // Build a rotation matrix from a unit quaternion.
    static MatrixType rotation(
        const Quaternion<T>&    q);                         // unit quaternion

    // Build a look-at transformation matrix. The returned matrix is orthonormal,
    // and such that the Z- axis is pointing toward the target point.
    static MatrixType lookat(
        const Vector<T, 3>&     origin,                     // camera origin
        const Vector<T, 3>&     target,                     // target point
        const Vector<T, 3>&     up);                        // up vector, unit-length

    // Extract Euler angles from a rotation matrix.
    void extract_euler_angles(
        ValueType&              yaw,
        ValueType&              pitch,
        ValueType&              roll) const;

    // Extract a unit quaternion from a rotation matrix.
    Quaternion<T> extract_unit_quaternion() const;

    // Unchecked array subscripting.
    ValueType& operator[](const size_t i);
    const ValueType& operator[](const size_t i) const;

    // Unchecked Fortran-style subscripting (0-based).
    ValueType& operator()(const size_t row, const size_t col);
    const ValueType& operator()(const size_t row, const size_t col) const;

  private:
    FOUNDATION_ALIGN_SSE_VARIABLE
    ValueType m_comp[Components];
};


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

// Constructors.
template <typename T, size_t M, size_t N>
inline Matrix<T, M, N>::Matrix()
{
}
template <typename T, size_t M, size_t N>
inline Matrix<T, M, N>::Matrix(const ValueType* rhs)
{
    assert(rhs);
    for (size_t i = 0; i < Components; ++i)
        m_comp[i] = rhs[i];
}
template <typename T, size_t M, size_t N>
inline Matrix<T, M, N>::Matrix(const ValueType val)
{
    for (size_t i = 0; i < Components; ++i)
        m_comp[i] = val;
}

// Construct a matrix from another matrix of a different type.
template <typename T, size_t M, size_t N>
template <typename U>
inline Matrix<T, M, N>::Matrix(const Matrix<U, M, N>& rhs)
{
    for (size_t i = 0; i < Components; ++i)
        m_comp[i] = static_cast<ValueType>(rhs[i]);
}

// Unchecked array subscripting.
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

// Unchecked Fortran-style subscripting (0-based).
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

// Exact inequality and equality tests.
template <typename T, size_t M, size_t N>
inline bool operator!=(const Matrix<T, M, N>& lhs, const Matrix<T, M, N>& rhs)
{
    for (size_t i = 0; i < lhs.Components; ++i)
        if (lhs[i] != rhs[i])
            return true;
    return false;
}
template <typename T, size_t M, size_t N>
inline bool operator==(const Matrix<T, M, N>& lhs, const Matrix<T, M, N>& rhs)
{
    return !(lhs != rhs);
}

// Approximate equality tests.
template <typename T, size_t M, size_t N>
inline bool feq(const Matrix<T, M, N>& lhs, const Matrix<T, M, N>& rhs)
{
    for (size_t i = 0; i < lhs.Components; ++i)
        if (!feq(lhs[i], rhs[i]))
            return false;
    return true;
}
template <typename T, size_t M, size_t N>
inline bool feq(const Matrix<T, M, N>& lhs, const Matrix<T, M, N>& rhs, const T eps)
{
    for (size_t i = 0; i < lhs.Components; ++i)
        if (!feq(lhs[i], rhs[i], eps))
            return false;
    return true;
}

// Approximate zero tests.
template <typename T, size_t M, size_t N>
inline bool fz(const Matrix<T, M, N>& v)
{
    for (size_t i = 0; i < v.Components; ++i)
        if (!fz(v[i]))
            return false;
    return true;
}
template <typename T, size_t M, size_t N>
inline bool fz(const Matrix<T, M, N>& v, const T eps)
{
    for (size_t i = 0; i < v.Components; ++i)
        if (!fz(v[i], eps))
            return false;
    return true;
}

// Matrix arithmetic.
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
    return lhs * (T(1.0) / rhs);
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
    return lhs *= (T(1.0) / rhs);
}

// Matrix-matrix multiplication (MxN * NxK = MxK).
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
                res[res_index] += lhs[r * K + i] * rhs[c + i * M];
            }
            ++res_index;
        }
    }
    return res;
}

// 3x3 matrix multiplication.
template <typename T>
inline Matrix<T, 3, 3> operator*(
    const Matrix<T, 3, 3>&  lhs,
    const Matrix<T, 3, 3>&  rhs)
{
    Matrix<T, 3, 3> res;

    // Compute first row.
    res[0] = lhs[0] * rhs[0] + lhs[1] * rhs[3] + lhs[2] * rhs[6];
    res[1] = lhs[0] * rhs[1] + lhs[1] * rhs[4] + lhs[2] * rhs[7];
    res[2] = lhs[0] * rhs[2] + lhs[1] * rhs[5] + lhs[2] * rhs[8];

    // Compute second row.
    res[3] = lhs[3] * rhs[0] + lhs[4] * rhs[3] + lhs[5] * rhs[6];
    res[4] = lhs[3] * rhs[1] + lhs[4] * rhs[4] + lhs[5] * rhs[7];
    res[5] = lhs[3] * rhs[2] + lhs[4] * rhs[5] + lhs[5] * rhs[8];

    // Compute third row.
    res[6] = lhs[6] * rhs[0] + lhs[7] * rhs[3] + lhs[8] * rhs[6];
    res[7] = lhs[6] * rhs[1] + lhs[7] * rhs[4] + lhs[8] * rhs[7];
    res[8] = lhs[6] * rhs[2] + lhs[7] * rhs[5] + lhs[8] * rhs[8];

    return res;
}

// 4x4 matrix multiplication.
template <typename T>
inline Matrix<T, 4, 4> operator*(
    const Matrix<T, 4, 4>&  lhs,
    const Matrix<T, 4, 4>&  rhs)
{
    Matrix<T, 4, 4> res;

    // Compute first row.
    res[ 0] = lhs[ 0] * rhs[0] + lhs[ 1] * rhs[4] + lhs[ 2] * rhs[ 8] + lhs[ 3] * rhs[12];
    res[ 1] = lhs[ 0] * rhs[1] + lhs[ 1] * rhs[5] + lhs[ 2] * rhs[ 9] + lhs[ 3] * rhs[13];
    res[ 2] = lhs[ 0] * rhs[2] + lhs[ 1] * rhs[6] + lhs[ 2] * rhs[10] + lhs[ 3] * rhs[14];
    res[ 3] = lhs[ 0] * rhs[3] + lhs[ 1] * rhs[7] + lhs[ 2] * rhs[11] + lhs[ 3] * rhs[15];

    // Compute second row.
    res[ 4] = lhs[ 4] * rhs[0] + lhs[ 5] * rhs[4] + lhs[ 6] * rhs[ 8] + lhs[ 7] * rhs[12];
    res[ 5] = lhs[ 4] * rhs[1] + lhs[ 5] * rhs[5] + lhs[ 6] * rhs[ 9] + lhs[ 7] * rhs[13];
    res[ 6] = lhs[ 4] * rhs[2] + lhs[ 5] * rhs[6] + lhs[ 6] * rhs[10] + lhs[ 7] * rhs[14];
    res[ 7] = lhs[ 4] * rhs[3] + lhs[ 5] * rhs[7] + lhs[ 6] * rhs[11] + lhs[ 7] * rhs[15];

    // Compute third row.
    res[ 8] = lhs[ 8] * rhs[0] + lhs[ 9] * rhs[4] + lhs[10] * rhs[ 8] + lhs[11] * rhs[12];
    res[ 9] = lhs[ 8] * rhs[1] + lhs[ 9] * rhs[5] + lhs[10] * rhs[ 9] + lhs[11] * rhs[13];
    res[10] = lhs[ 8] * rhs[2] + lhs[ 9] * rhs[6] + lhs[10] * rhs[10] + lhs[11] * rhs[14];
    res[11] = lhs[ 8] * rhs[3] + lhs[ 9] * rhs[7] + lhs[10] * rhs[11] + lhs[11] * rhs[15];

    // Compute fourth row.
    res[12] = lhs[12] * rhs[0] + lhs[13] * rhs[4] + lhs[14] * rhs[ 8] + lhs[15] * rhs[12];
    res[13] = lhs[12] * rhs[1] + lhs[13] * rhs[5] + lhs[14] * rhs[ 9] + lhs[15] * rhs[13];
    res[14] = lhs[12] * rhs[2] + lhs[13] * rhs[6] + lhs[14] * rhs[10] + lhs[15] * rhs[14];
    res[15] = lhs[12] * rhs[3] + lhs[13] * rhs[7] + lhs[14] * rhs[11] + lhs[15] * rhs[15];

    return res;
}

#ifdef APPLESEED_FOUNDATION_USE_SSE

// SSE2-optimized double precision 4x4 matrix multiplication.
template <>
inline Matrix<double, 4, 4> operator*(
    const Matrix<double, 4, 4>& lhs,
    const Matrix<double, 4, 4>& rhs)
{
    Matrix<double, 4, 4> res;

    const sse2d rhs0  = loadpd(&rhs[0]);
    const sse2d rhs2  = loadpd(&rhs[2]);
    const sse2d rhs4  = loadpd(&rhs[4]);
    const sse2d rhs6  = loadpd(&rhs[6]);
    const sse2d rhs8  = loadpd(&rhs[8]);
    const sse2d rhs10 = loadpd(&rhs[10]);
    const sse2d rhs12 = loadpd(&rhs[12]);
    const sse2d rhs14 = loadpd(&rhs[14]);

    sse2d res0, res2;

    const sse2d lhs0 = set1pd(lhs[0]);
    const sse2d lhs1 = set1pd(lhs[1]);
    const sse2d lhs2 = set1pd(lhs[2]);
    const sse2d lhs3 = set1pd(lhs[3]);

    res0 = mulpd(lhs0, rhs0);
    res2 = mulpd(lhs0, rhs2);
    res0 = addpd(res0, mulpd(lhs1, rhs4));
    res2 = addpd(res2, mulpd(lhs1, rhs6));
    res0 = addpd(res0, mulpd(lhs2, rhs8));
    res2 = addpd(res2, mulpd(lhs2, rhs10));
    res0 = addpd(res0, mulpd(lhs3, rhs12));
    res2 = addpd(res2, mulpd(lhs3, rhs14));

    storepd(&res[0], res0);
    storepd(&res[2], res2);

    const sse2d lhs4 = set1pd(lhs[4]);
    const sse2d lhs5 = set1pd(lhs[5]);
    const sse2d lhs6 = set1pd(lhs[6]);
    const sse2d lhs7 = set1pd(lhs[7]);

    res0 = mulpd(lhs4, rhs0);
    res2 = mulpd(lhs4, rhs2);
    res0 = addpd(res0, mulpd(lhs5, rhs4));
    res2 = addpd(res2, mulpd(lhs5, rhs6));
    res0 = addpd(res0, mulpd(lhs6, rhs8));
    res2 = addpd(res2, mulpd(lhs6, rhs10));
    res0 = addpd(res0, mulpd(lhs7, rhs12));
    res2 = addpd(res2, mulpd(lhs7, rhs14));

    storepd(&res[4], res0);
    storepd(&res[6], res2);

    const sse2d lhs8  = set1pd(lhs[8]);
    const sse2d lhs9  = set1pd(lhs[9]);
    const sse2d lhs10 = set1pd(lhs[10]);
    const sse2d lhs11 = set1pd(lhs[11]);

    res0 = mulpd(lhs8, rhs0);
    res2 = mulpd(lhs8, rhs2);
    res0 = addpd(res0, mulpd(lhs9, rhs4));
    res2 = addpd(res2, mulpd(lhs9, rhs6));
    res0 = addpd(res0, mulpd(lhs10, rhs8));
    res2 = addpd(res2, mulpd(lhs10, rhs10));
    res0 = addpd(res0, mulpd(lhs11, rhs12));
    res2 = addpd(res2, mulpd(lhs11, rhs14));

    storepd(&res[8],  res0);
    storepd(&res[10], res2);

    const sse2d lhs12 = set1pd(lhs[12]);
    const sse2d lhs13 = set1pd(lhs[13]);
    const sse2d lhs14 = set1pd(lhs[14]);
    const sse2d lhs15 = set1pd(lhs[15]);

    res0 = mulpd(lhs12, rhs0);
    res2 = mulpd(lhs12, rhs2);
    res0 = addpd(res0, mulpd(lhs13, rhs4));
    res2 = addpd(res2, mulpd(lhs13, rhs6));
    res0 = addpd(res0, mulpd(lhs14, rhs8));
    res2 = addpd(res2, mulpd(lhs14, rhs10));
    res0 = addpd(res0, mulpd(lhs15, rhs12));
    res2 = addpd(res2, mulpd(lhs15, rhs14));

    storepd(&res[12], res0);
    storepd(&res[14], res2);

    return res;
}

#endif  // APPLESEED_FOUNDATION_USE_SSE

// Matrix-vector multiplication (MxN * Nx1 = Mx1).
template <typename T, size_t M, size_t N>
inline Vector<T, M> operator*(
    const Matrix<T, M, N>&  lhs,
    const Vector<T, N>&     rhs)
{
    Vector<T, M> res;
    for (size_t r = 0; r < M; ++r)
    {
        res[r] = T(0.0);
        for (size_t c = 0; c < N; ++c)
            res[r] += lhs(r, c) * rhs[c];
    }
    return res;
}

// 4x4 matrix-vector multiplication.
template <typename T>
inline Vector<T, 4> operator*(
    const Matrix<T, 4, 4>&  lhs,
    const Vector<T, 4>&     rhs)
{
    Vector<T, 4> res;
    res[0] = lhs[ 0] * rhs[0] + lhs[ 1] * rhs[1] + lhs[ 2] * rhs[2] + lhs[ 3] * rhs[3];
    res[1] = lhs[ 4] * rhs[0] + lhs[ 5] * rhs[1] + lhs[ 6] * rhs[2] + lhs[ 7] * rhs[3];
    res[2] = lhs[ 8] * rhs[0] + lhs[ 9] * rhs[1] + lhs[10] * rhs[2] + lhs[11] * rhs[3];
    res[3] = lhs[12] * rhs[0] + lhs[13] * rhs[1] + lhs[14] * rhs[2] + lhs[15] * rhs[3];
    return res;
}

#ifdef APPLESEED_FOUNDATION_USE_SSE_DISABLED

// SSE2-optimized double precision 4x4 matrix-vector multiplication.
template <>
inline Vector<double, 4> operator*(
    const Matrix<double, 4, 4>& lhs,
    const Vector<double, 4>&    rhs)
{
    Vector<double, 4> res;

    const sse2d rhs0 = loadupd(&rhs[0]);
    const sse2d rhs2 = loadupd(&rhs[2]);

    const sse2d res0 = addpd(mulpd(loadpd(&lhs[0]), rhs0), mulpd(loadpd(&lhs[2]), rhs2));
    const sse2d res1 = addpd(mulpd(loadpd(&lhs[4]), rhs0), mulpd(loadpd(&lhs[6]), rhs2));
    const sse2d res2 = addpd(mulpd(loadpd(&lhs[8]), rhs0), mulpd(loadpd(&lhs[10]), rhs2));
    const sse2d res3 = addpd(mulpd(loadpd(&lhs[12]), rhs0), mulpd(loadpd(&lhs[14]), rhs2));

    storeupd(
        &res[0],
        addpd(
            shufflepd(res0, res1, _MM_SHUFFLE2(0, 0)),
            shufflepd(res0, res1, _MM_SHUFFLE2(1, 1))));

    storeupd(
        &res[2],
        addpd(
            shufflepd(res2, res3, _MM_SHUFFLE2(0, 0)),
            shufflepd(res2, res3, _MM_SHUFFLE2(1, 1))));

    return res;
}

#endif  // APPLESEED_FOUNDATION_USE_SSE

// Vector-matrix multiplication (1xM * MxN = 1xN).
template <typename T, size_t M, size_t N>
inline Vector<T, N> operator*(
    const Vector<T, M>&     lhs,
    const Matrix<T, M, N>&  rhs)
{
    Vector<T, N> res;
    for (size_t c = 0; c < N; ++c)
    {
        res[c] = T(0.0);
        for (size_t r = 0; r < M; ++r)
            res[c] += lhs[r] * rhs(r, c);
    }
    return res;
}

// 4x4 vector-matrix multiplication.
template <typename T>
inline Vector<T, 4> operator*(
    const Vector<T, 4>&     lhs,
    const Matrix<T, 4, 4>&  rhs)
{
    // todo: it might be possible to do this more efficiently.
    // todo: is this correct at all?
    return rhs * lhs;
}

// Matrix transposition.
template <typename T, size_t M, size_t N>
inline Matrix<T, N, M> transpose(const Matrix<T, M, N>& mat)
{
    // todo: reimplement for better performances.
    Matrix<T, N, M> res;
    for (size_t r = 0; r < M; ++r)
    {
        for (size_t c = 0; c < N; ++c)
            res(c, r) = mat(r, c);
    }
    return res;
}


//
// NxN matrix class implementation.
//

// Constructors.
template <typename T, size_t N>
inline Matrix<T, N, N>::Matrix()
{
}
template <typename T, size_t N>
inline Matrix<T, N, N>::Matrix(const ValueType* rhs)
{
    assert(rhs);
    for (size_t i = 0; i < Components; ++i)
        m_comp[i] = rhs[i];
}
template <typename T, size_t N>
inline Matrix<T, N, N>::Matrix(const ValueType val)
{
    for (size_t i = 0; i < Components; ++i)
        m_comp[i] = val;
}

// Construct a matrix from another matrix of a different type.
template <typename T, size_t N>
template <typename U>
inline Matrix<T, N, N>::Matrix(const Matrix<U, N, N>& rhs)
{
    for (size_t i = 0; i < Components; ++i)
        m_comp[i] = static_cast<ValueType>(rhs[i]);
}

// Return the NxN identity matrix.
template <typename T, size_t N>
inline Matrix<T, N, N> Matrix<T, N, N>::identity()
{
    MatrixType mat(T(0.0));
    for (size_t i = 0; i < N; ++i)
        mat(i, i) = T(1.0);
    return mat;
}

// Unchecked array subscripting.
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

// Unchecked Fortran-style subscripting (0-based).
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

// Matrix trace.
template <typename T, size_t N>
inline T trace(const Matrix<T, N, N>& mat)
{
    T sum = T(0.0);
    for (size_t i = 0; i < N; ++i)
        sum += mat(i, i);
    return sum;
}

// Matrix determinant.
template <typename T, size_t N>
inline T det(const Matrix<T, N, N>& mat)
{
    throw ExceptionNotImplemented();
    return T(0.0);
}

// Matrix inversion using Gauss-Jordan elimination with partial pivoting.
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
//              assert(feq(m(r, i), T(0.0)));   // often subject to numerical inaccuracies
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

// Constructors.
template <typename T>
inline Matrix<T, 3, 3>::Matrix()
{
}
template <typename T>
inline Matrix<T, 3, 3>::Matrix(const ValueType* rhs)
{
    assert(rhs);
    for (size_t i = 0; i < Components; ++i)
        m_comp[i] = rhs[i];
}
template <typename T>
inline Matrix<T, 3, 3>::Matrix(const ValueType val)
{
    for (size_t i = 0; i < Components; ++i)
        m_comp[i] = val;
}

// Construct a matrix from another matrix of a different type.
template <typename T>
template <typename U>
inline Matrix<T, 3, 3>::Matrix(const Matrix<U, 3, 3>& rhs)
{
    for (size_t i = 0; i < Components; ++i)
        m_comp[i] = static_cast<ValueType>(rhs[i]);
}

// Return the 3x3 identity matrix.
template <typename T>
inline Matrix<T, 3, 3> Matrix<T, 3, 3>::identity()
{
    MatrixType mat(T(0.0));
    mat[0] = T(1.0);
    mat[4] = T(1.0);
    mat[8] = T(1.0);
    return mat;
}

// Build canonical transformation matrices.
template <typename T>
inline Matrix<T, 3, 3> Matrix<T, 3, 3>::scaling(const Vector<T, 3>& s)
{
    MatrixType mat(T(0.0));
    mat[0] = s.x;
    mat[4] = s.y;
    mat[8] = s.z;
    return mat;
}
template <typename T>
inline Matrix<T, 3, 3> Matrix<T, 3, 3>::rotation_x(const ValueType angle)
{
    const T cos_angle = T(std::cos(angle));
    const T sin_angle = T(std::sin(angle));
    MatrixType mat(T(0.0));
    mat[0] =  T(1.0);
    mat[4] =  cos_angle;
    mat[5] = -sin_angle;
    mat[7] =  sin_angle;
    mat[8] =  cos_angle;
    return mat;
}
template <typename T>
inline Matrix<T, 3, 3> Matrix<T, 3, 3>::rotation_y(const ValueType angle)
{
    const T cos_angle = T(std::cos(angle));
    const T sin_angle = T(std::sin(angle));
    MatrixType mat(T(0.0));
    mat[0] =  cos_angle;
    mat[2] =  sin_angle;
    mat[4] =  T(1.0);
    mat[6] = -sin_angle;
    mat[8] =  cos_angle;
    return mat;
}
template <typename T>
inline Matrix<T, 3, 3> Matrix<T, 3, 3>::rotation_z(const ValueType angle)
{
    const T cos_angle = T(std::cos(angle));
    const T sin_angle = T(std::sin(angle));
    MatrixType mat(T(0.0));
    mat[0] =  cos_angle;
    mat[1] = -sin_angle;
    mat[3] =  sin_angle;
    mat[4] =  cos_angle;
    mat[8] =  T(1.0);
    return mat;
}

// Build a rotation matrix from Euler angles.
template <typename T>
inline Matrix<T, 3, 3> Matrix<T, 3, 3>::rotation(
    const ValueType         yaw,
    const ValueType         pitch,
    const ValueType         roll)
{
    const T cos_yaw   = T(std::cos(yaw));
    const T sin_yaw   = T(std::sin(yaw));
    const T cos_pitch = T(std::cos(pitch));
    const T sin_pitch = T(std::sin(pitch));
    const T cos_roll  = T(std::cos(roll));
    const T sin_roll  = T(std::sin(roll));

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

// Build a rotation matrix from an axis and an angle.
template <typename T>
inline Matrix<T, 3, 3> Matrix<T, 3, 3>::rotation(
    const Vector<T, 3>&     axis,
    const ValueType         angle)
{
    assert(is_normalized(axis));

    const T cos_angle = T(std::cos(angle));
    const T sin_angle = T(std::sin(angle));
    const T one_min_cos_angle = T(1.0) - cos_angle;

    MatrixType mat;

    // First row.
    mat[0] = one_min_cos_angle * axis.x * axis.x + cos_angle;
    mat[1] = one_min_cos_angle * axis.x * axis.y - sin_angle * axis.z;
    mat[2] = one_min_cos_angle * axis.x * axis.z + sin_angle * axis.y;

    // Second row.
    mat[3] = one_min_cos_angle * axis.y * axis.x + sin_angle * axis.z;
    mat[4] = one_min_cos_angle * axis.y * axis.y + cos_angle;
    mat[5] = one_min_cos_angle * axis.y * axis.z - sin_angle * axis.x;

    // Third row.
    mat[6] = one_min_cos_angle * axis.z * axis.x - sin_angle * axis.y;
    mat[7] = one_min_cos_angle * axis.z * axis.y + sin_angle * axis.x;
    mat[8] = one_min_cos_angle * axis.z * axis.z + cos_angle;

    return mat;
}

// Build a rotation matrix from a unit quaternion.
template <typename T>
inline Matrix<T, 3, 3> Matrix<T, 3, 3>::rotation(
    const Quaternion<T>&    q)
{
    //
    // Implementation from Wild Magic Source Code, David Eberly
    // http://www.geometrictools.com
    //

    assert(is_normalized(q));

    const ValueType tx  = ValueType(2.0) * q.v[0];
    const ValueType ty  = ValueType(2.0) * q.v[1];
    const ValueType tz  = ValueType(2.0) * q.v[2];
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

    // First row.
    mat[0] = ValueType(1.0) - (tyy + tzz);
    mat[1] = txy - twz;
    mat[2] = txz + twy;

    // Second row.
    mat[3] = txy + twz;
    mat[4] = ValueType(1.0) - (txx + tzz);
    mat[5] = tyz - twx;

    // Third row.
    mat[6] = txz - twy;
    mat[7] = tyz + twx;
    mat[8] = ValueType(1.0) - (txx + tyy);

    return mat;
}

// Extract Euler angles from a rotation matrix.
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
        pitch = HalfPi;
        roll  = std::atan2(m_comp[3], m_comp[0]);
    }
}

// Extract a unit quaternion from a rotation matrix.
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
        const size_t j = (1 << i) & 3;
        const size_t k = (1 << j) & 3;

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

// Unchecked array subscripting.
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

// Unchecked Fortran-style subscripting (0-based).
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

// Rotate a given vector by a given angle around a given axis.
template <typename T>
inline Vector<T, 3> rotate(
    const Vector<T, 3>&     v,
    const Vector<T, 3>&     axis,
    const T                 angle)
{
    return Matrix<T, 3, 3>::rotation(axis, angle) * v;
}


//
// 4x4 matrix class implementation.
//

// Constructors.
template <typename T>
inline Matrix<T, 4, 4>::Matrix()
{
}
template <typename T>
inline Matrix<T, 4, 4>::Matrix(const ValueType* rhs)
{
    assert(rhs);
    for (size_t i = 0; i < Components; ++i)
        m_comp[i] = rhs[i];
}
template <typename T>
inline Matrix<T, 4, 4>::Matrix(const ValueType val)
{
    for (size_t i = 0; i < Components; ++i)
        m_comp[i] = val;
}

// Construct a matrix from another matrix of a different type.
template <typename T>
template <typename U>
inline Matrix<T, 4, 4>::Matrix(const Matrix<U, 4, 4>& rhs)
{
    for (size_t i = 0; i < Components; ++i)
        m_comp[i] = static_cast<ValueType>(rhs[i]);
}

// Return the 4x4 identity matrix.
template <typename T>
inline Matrix<T, 4, 4> Matrix<T, 4, 4>::identity()
{
    MatrixType mat(T(0.0));
    mat[ 0] = T(1.0);
    mat[ 5] = T(1.0);
    mat[10] = T(1.0);
    mat[15] = T(1.0);
    return mat;
}

// Build canonical transformation matrices.
template <typename T>
inline Matrix<T, 4, 4> Matrix<T, 4, 4>::translation(const Vector<T, 3>& v)
{
    MatrixType mat = identity();
    mat[ 3] = v.x;
    mat[ 7] = v.y;
    mat[11] = v.z;
    return mat;
}
template <typename T>
inline Matrix<T, 4, 4> Matrix<T, 4, 4>::scaling(const Vector<T, 3>& s)
{
    MatrixType mat(T(0.0));
    mat[ 0] = s.x;
    mat[ 5] = s.y;
    mat[10] = s.z;
    mat[15] = T(1.0);
    return mat;
}
template <typename T>
inline Matrix<T, 4, 4> Matrix<T, 4, 4>::rotation_x(const ValueType angle)
{
    const T cos_angle = T(std::cos(angle));
    const T sin_angle = T(std::sin(angle));
    MatrixType mat(T(0.0));
    mat[ 0] =  T(1.0);
    mat[ 5] =  cos_angle;
    mat[ 6] = -sin_angle;
    mat[ 9] =  sin_angle;
    mat[10] =  cos_angle;
    mat[15] =  T(1.0);
    return mat;
}
template <typename T>
inline Matrix<T, 4, 4> Matrix<T, 4, 4>::rotation_y(const ValueType angle)
{
    const T cos_angle = T(std::cos(angle));
    const T sin_angle = T(std::sin(angle));
    MatrixType mat(T(0.0));
    mat[ 0] =  cos_angle;
    mat[ 2] =  sin_angle;
    mat[ 5] =  T(1.0);
    mat[ 8] = -sin_angle;
    mat[10] =  cos_angle;
    mat[15] =  T(1.0);
    return mat;
}
template <typename T>
inline Matrix<T, 4, 4> Matrix<T, 4, 4>::rotation_z(const ValueType angle)
{
    const T cos_angle = T(std::cos(angle));
    const T sin_angle = T(std::sin(angle));
    MatrixType mat(T(0.0));
    mat[ 0] =  cos_angle;
    mat[ 1] = -sin_angle;
    mat[ 4] =  sin_angle;
    mat[ 5] =  cos_angle;
    mat[10] =  T(1.0);
    mat[15] =  T(1.0);
    return mat;
}

// Build a rotation matrix from Euler angles.
template <typename T>
inline Matrix<T, 4, 4> Matrix<T, 4, 4>::rotation(
    const ValueType         yaw,
    const ValueType         pitch,
    const ValueType         roll)
{
    const T cos_yaw   = T(std::cos(yaw));
    const T sin_yaw   = T(std::sin(yaw));
    const T cos_pitch = T(std::cos(pitch));
    const T sin_pitch = T(std::sin(pitch));
    const T cos_roll  = T(std::cos(roll));
    const T sin_roll  = T(std::sin(roll));

    MatrixType mat;

    // First row.
    mat[ 0] = cos_yaw * cos_roll - sin_yaw * sin_pitch * sin_roll;
    mat[ 1] = -cos_pitch * sin_roll;
    mat[ 2] = sin_yaw * cos_roll + cos_yaw * sin_pitch * sin_roll;
    mat[ 3] = T(0.0);

    // Second row.
    mat[ 4] = cos_yaw * sin_roll + sin_yaw * sin_pitch * cos_roll;
    mat[ 5] = cos_pitch * cos_roll;
    mat[ 6] = sin_yaw * sin_roll - cos_yaw * sin_pitch * cos_roll;
    mat[ 7] = T(0.0);

    // Third row.
    mat[ 8] = -sin_yaw * cos_pitch;
    mat[ 9] = sin_pitch;
    mat[10] = cos_yaw * cos_pitch;
    mat[11] = T(0.0);

    // Fourth row.
    mat[12] = T(0.0);
    mat[13] = T(0.0);
    mat[14] = T(0.0);
    mat[15] = T(1.0);

    return mat;
}

// Build a rotation matrix from an axis and an angle.
template <typename T>
inline Matrix<T, 4, 4> Matrix<T, 4, 4>::rotation(
    const Vector<T, 3>&     axis,
    const ValueType         angle)
{
    assert(is_normalized(axis));

    const T cos_angle = T(std::cos(angle));
    const T sin_angle = T(std::sin(angle));
    const T one_min_cos_angle = T(1.0) - cos_angle;

    MatrixType mat;

    // First row.
    mat[ 0] = one_min_cos_angle * axis.x * axis.x + cos_angle;
    mat[ 1] = one_min_cos_angle * axis.x * axis.y - sin_angle * axis.z;
    mat[ 2] = one_min_cos_angle * axis.x * axis.z + sin_angle * axis.y;
    mat[ 3] = T(0.0);

    // Second row.
    mat[ 4] = one_min_cos_angle * axis.y * axis.x + sin_angle * axis.z;
    mat[ 5] = one_min_cos_angle * axis.y * axis.y + cos_angle;
    mat[ 6] = one_min_cos_angle * axis.y * axis.z - sin_angle * axis.x;
    mat[ 7] = T(0.0);

    // Third row.
    mat[ 8] = one_min_cos_angle * axis.z * axis.x - sin_angle * axis.y;
    mat[ 9] = one_min_cos_angle * axis.z * axis.y + sin_angle * axis.x;
    mat[10] = one_min_cos_angle * axis.z * axis.z + cos_angle;
    mat[11] = T(0.0);

    // Fourth row.
    mat[12] = T(0.0);
    mat[13] = T(0.0);
    mat[14] = T(0.0);
    mat[15] = T(1.0);

    return mat;
}

// Build a rotation matrix from a unit quaternion.
template <typename T>
inline Matrix<T, 4, 4> Matrix<T, 4, 4>::rotation(
    const Quaternion<T>&    q)
{
    //
    // Implementation from Wild Magic Source Code, David Eberly
    // http://www.geometrictools.com
    //

    assert(is_normalized(q));

    const ValueType tx  = ValueType(2.0) * q.v[0];
    const ValueType ty  = ValueType(2.0) * q.v[1];
    const ValueType tz  = ValueType(2.0) * q.v[2];
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

    // First row.
    mat[ 0] = ValueType(1.0) - (tyy + tzz);
    mat[ 1] = txy - twz;
    mat[ 2] = txz + twy;
    mat[ 3] = T(0.0);

    // Second row.
    mat[ 4] = txy + twz;
    mat[ 5] = ValueType(1.0) - (txx + tzz);
    mat[ 6] = tyz - twx;
    mat[ 7] = T(0.0);

    // Third row.
    mat[ 8] = txz - twy;
    mat[ 9] = tyz + twx;
    mat[10] = ValueType(1.0) - (txx + tyy);
    mat[11] = T(0.0);

    // Fourth row.
    mat[12] = T(0.0);
    mat[13] = T(0.0);
    mat[14] = T(0.0);
    mat[15] = T(1.0);

    return mat;
}

// Build a look-at transformation matrix.
template <typename T>
inline Matrix<T, 4, 4> Matrix<T, 4, 4>::lookat(
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

    // First column.
    mat[ 0] = x.x;
    mat[ 4] = x.y;
    mat[ 8] = x.z;
    mat[12] = T(0.0);

    // Second column.
    mat[ 1] = y.x;
    mat[ 5] = y.y;
    mat[ 9] = y.z;
    mat[13] = T(0.0);

    // Third column.
    mat[ 2] = z.x;
    mat[ 6] = z.y;
    mat[10] = z.z;
    mat[14] = T(0.0);

    // Fourth column.
    mat[ 3] = origin.x;
    mat[ 7] = origin.y;
    mat[11] = origin.z;
    mat[15] = T(1.0);

    return mat;
}

// Extract Euler angles from a rotation matrix.
template <typename T>
inline void Matrix<T, 4, 4>::extract_euler_angles(
    ValueType&              yaw,
    ValueType&              pitch,
    ValueType&              roll) const
{
    // todo: in debug, make sure the matrix is orthogonal.

    if (m_comp[8] != ValueType(0.0))
    {
        yaw   = std::atan2(-m_comp[8], m_comp[10]);
        pitch = std::asin(m_comp[9]);
        roll  = std::atan2(-m_comp[1], m_comp[5]);
    }
    else
    {
        yaw   = ValueType(0.0);
        pitch = HalfPi;
        roll  = std::atan2(m_comp[4], m_comp[0]);
    }
}

// Extract a unit quaternion from a rotation matrix.
template <typename T>
inline Quaternion<T> Matrix<T, 4, 4>::extract_unit_quaternion() const
{
    //
    // Implementation from Wild Magic Source Code, David Eberly
    // http://www.geometrictools.com
    //
    // Algorithm in Ken Shoemake's article in 1987 SIGGRAPH course notes
    // article "Quaternion Calculus and Fast Animation".
    //

    // todo: in debug, make sure the matrix is orthogonal.

    const ValueType t = m_comp[0] + m_comp[5] + m_comp[10];     // rotation matrix trace

    if (t > ValueType(0.0))
    {
        // |w| > 1/2, may as well choose w > 1/2.

        ValueType root = std::sqrt(t + ValueType(1.0));         // 2w

        Quaternion<T> q;
        q.s   = ValueType(0.5) * root;
        root  = ValueType(0.5) / root;                          // 1/(4w)
        q.v.x = (m_comp[9] - m_comp[6]) * root;
        q.v.y = (m_comp[2] - m_comp[8]) * root;
        q.v.z = (m_comp[4] - m_comp[1]) * root;
        return q;
    }
    else
    {
        // |w| <= 1/2.

        size_t i = 0;
        if (m_comp[5] > m_comp[0]) i = 1;
        if (m_comp[10] > m_comp[i*4+i]) i = 2;

        const size_t j = (1 << i) & 3;
        const size_t k = (1 << j) & 3;

        ValueType root =
            std::sqrt(m_comp[i*4+i] - m_comp[j*4+j] - m_comp[k*4+k] + ValueType(1.0));

        Quaternion<T> q;
        q.v[i] = ValueType(0.5) * root;
        root   = ValueType(0.5) / root;
        q.s    = (m_comp[k*4+j] - m_comp[j*4+k]) * root;
        q.v[j] = (m_comp[j*4+i] + m_comp[i*4+j]) * root;
        q.v[k] = (m_comp[k*4+i] + m_comp[i*4+k]) * root;
        return q;
    }
}

// Unchecked array subscripting.
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

// Unchecked Fortran-style subscripting (0-based).
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

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_MATRIX_H
