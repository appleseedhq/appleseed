
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2012-2013 Esteban Tovagliari, Jupiter Jazz Limited
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

#ifndef APPLESEED_PYTHON_UNALIGNED_MATRIX44_H
#define APPLESEED_PYTHON_UNALIGNED_MATRIX44_H

// Has to be first, to avoid redefinition warnings.
#include "boost/python/detail/wrap_python.hpp"

// appleseed.foundation headers.
#include "foundation/math/matrix.h"
#include "foundation/platform/python.h"
#include "foundation/utility/iostreamop.h"

namespace foundation
{

template <class T>
class UnalignedMatrix44
{
  public:
    static UnalignedMatrix44<T> identity()
    {
        return UnalignedMatrix44<T>(Matrix<T, 4, 4>::identity());
    }

    static UnalignedMatrix44<T> translation(const Vector<T, 3>& v)
    {
        return UnalignedMatrix44<T>(Matrix<T, 4, 4>::translation(v));
    }

    static UnalignedMatrix44<T> scaling(const Vector<T, 3>& s)
    {
        return UnalignedMatrix44<T>(Matrix<T, 4, 4>::scaling(s));
    }

    static UnalignedMatrix44<T> rotation_x(const T angle)
    {
        return UnalignedMatrix44<T>(Matrix<T, 4, 4>::rotation_x(angle));
    }

    static UnalignedMatrix44<T> rotation_y(const T angle)
    {
        return UnalignedMatrix44<T>(Matrix<T, 4, 4>::rotation_y(angle));
    }

    static UnalignedMatrix44<T> rotation_z(const T angle)
    {
        return UnalignedMatrix44<T>(Matrix<T, 4, 4>::rotation_z(angle));
    }

    static UnalignedMatrix44<T> rotation(const T yaw, const T pitch, const T roll)
    {
        return UnalignedMatrix44<T>(Matrix<T, 4, 4>::rotation(yaw, pitch, roll));
    }

    static UnalignedMatrix44<T> rotation(const Vector<T, 3>& axis, const T angle)
    {
        return UnalignedMatrix44<T>(Matrix<T, 4, 4>::rotation(axis, angle));
    }

    static UnalignedMatrix44<T> rotation(const Quaternion<T>& q)
    {
        return UnalignedMatrix44<T>(Matrix<T, 4, 4>::rotation(q));
    }

    static UnalignedMatrix44<T> lookat(const Vector<T, 3>& origin, const Vector<T, 3>& target, const Vector<T, 3>& up)
    {
        return UnalignedMatrix44<T>(Matrix<T, 4, 4>::lookat(origin, target, up));
    }

    UnalignedMatrix44() {}

    explicit UnalignedMatrix44(const T x)
    {
        for (int i = 0; i < 16; ++i)
            m_data[i] = x;
    }

    template <class U>
    explicit UnalignedMatrix44(const Matrix<U, 4, 4>& m)
    {
        for (int i = 0; i < 16; ++i)
            m_data[i] = static_cast<T>(m[i]);
    }

    template <class U>
    explicit UnalignedMatrix44(const UnalignedMatrix44<U>& m)
    {
        for (int i = 0; i < 16; ++i)
            m_data[i] = static_cast<T>(m[i]);
    }

    template <class U>
    UnalignedMatrix44<T>& operator=(const UnalignedMatrix44<U>& m)
    {
        for (int i = 0; i < 16; ++i)
            m_data[i] = static_cast<T>(m[i]);

        return *this;
    }

    Matrix<T, 4, 4> as_foundation_matrix() const
    {
        return Matrix<T, 4, 4>(m_data);
    }

    T operator[](const int index) const
    {
        return m_data[index];
    }

    T& operator[](const int index)
    {
        return m_data[index];
    }

    T operator()(const int row, const int col) const
    {
        return m_data[row * 4 + col];
    }

    T& operator()(const int row, const int col)
    {
        return m_data[row * 4 + col];
    }

    Matrix<T, 3, 3> extract_matrix3() const
    {
        return as_foundation_matrix().extract_matrix3();
    }

    Vector<T, 3> extract_translation() const
    {
        return as_foundation_matrix().extract_translation();
    }

    Vector<T, 3> transform_vector(const Vector<T, 3>& v) const
    {
        Vector<T, 3> res;

        res[0] = m_data[0] * v[0] + m_data[1] * v[1] + m_data[ 2] * v[2];
        res[1] = m_data[4] * v[0] + m_data[5] * v[1] + m_data[ 6] * v[2];
        res[2] = m_data[8] * v[0] + m_data[9] * v[1] + m_data[10] * v[2];

        return res;
    }

    Vector<T, 3> transform_point(const Vector<T, 3>& p) const
    {
        Vector<T, 3> res;

        res.x = m_data[ 0] * p.x +
                m_data[ 1] * p.y +
                m_data[ 2] * p.z +
                m_data[ 3];

        res.y = m_data[ 4] * p.x +
                m_data[ 5] * p.y +
                m_data[ 6] * p.z +
                m_data[ 7];

        res.z = m_data[ 8] * p.x +
                m_data[ 9] * p.y +
                m_data[10] * p.z +
                m_data[11];

        const T w = m_data[12] * p.x +
                    m_data[13] * p.y +
                    m_data[14] * p.z +
                    m_data[15];

        if (w == T(0.0))
        {
            PyErr_SetString(PyExc_RuntimeError, "Zero homogeneous coordinate in appleseed.Matrix44.transform_point");
            boost::python::throw_error_already_set();
            return res;
        }

        res /= w;

        return res;
    }

  private:
    T m_data[16];
};

template <class T>
UnalignedMatrix44<T> operator*(const UnalignedMatrix44<T>& a, const UnalignedMatrix44<T>& b)
{
    return UnalignedMatrix44<T>(a.as_foundation_matrix() * b.as_foundation_matrix());
}

template <class T>
Vector<T,4> operator*(const UnalignedMatrix44<T>& a, const Vector<T,4>& v)
{
    return a.as_foundation_matrix() * v;
}

template <class T>
UnalignedMatrix44<T> invert_matrix(const UnalignedMatrix44<T>& mat)
{
    try
    {
        return UnalignedMatrix44<T>(inverse(mat.as_foundation_matrix()));
    }
    catch (ExceptionSingularMatrix&)
    {
        PyErr_SetString(PyExc_RuntimeError, "Singular matrix in appleseed.Matrix.inverse");
        boost::python::throw_error_already_set();
    }

    return UnalignedMatrix44<T>();
}

template <class T>
std::ostream& operator<<(std::ostream& s, const UnalignedMatrix44<T>& matrix)
{
    return s << matrix.as_foundation_matrix();
}

}       // namespace foundation

#endif  // !APPLESEED_PYTHON_UNALIGNED_MATRIX44_H
