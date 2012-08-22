//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2012 Esteban Tovagliari.
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

#include "foundation/math/matrix.h"
#include "foundation/utility/iostreamop.h"

namespace foundation
{

template<class T>
class UnalignedMatrix44
{
  public:

    static UnalignedMatrix44<T> identity()
    {
        return UnalignedMatrix44<T>( Matrix<T,4,4>::identity());
    }

    static UnalignedMatrix44<T> translation(const Vector<T,3>& v)
    {
        return UnalignedMatrix44<T>( Matrix<T,4,4>::translation( v));
    }

    static UnalignedMatrix44<T> scaling(const Vector<T,3>& s)
    {
        return UnalignedMatrix44<T>( Matrix<T,4,4>::scaling( s));
    }

    static UnalignedMatrix44<T> rotation_x( T angle)
    {
        return UnalignedMatrix44<T>( Matrix<T,4,4>::rotation_x( angle));
    }

    static UnalignedMatrix44<T> rotation_y( T angle)
    {
        return UnalignedMatrix44<T>( Matrix<T,4,4>::rotation_y( angle));
    }

    static UnalignedMatrix44<T> rotation_z( T angle)
    {
        return UnalignedMatrix44<T>( Matrix<T,4,4>::rotation_z( angle));
    }

    static UnalignedMatrix44<T> rotation( T yaw, T pitch, T roll)
    {
        return UnalignedMatrix44<T>( Matrix<T,4,4>::rotation( yaw, pitch, roll));
    }

    static UnalignedMatrix44<T> rotation( const Vector<T,3>& axis, T angle)
    {
        return UnalignedMatrix44<T>( Matrix<T,4,4>::rotation( axis, angle));
    }

    static UnalignedMatrix44<T> rotation( const Quaternion<T>& q)
    {
        return UnalignedMatrix44<T>( Matrix<T,4,4>::rotation( q));
    }

    static UnalignedMatrix44<T> lookat( const Vector<T,3>& origin, const Vector<T,3>& target, const Vector<T,3>& up)
    {
        return UnalignedMatrix44<T>( Matrix<T,4,4>::lookat( origin, target, up));
    }

    UnalignedMatrix44() {}

    explicit UnalignedMatrix44( T x)
    {
        for( int i = 0; i < 16; ++i)
            data_[i] = x;
    }

    explicit UnalignedMatrix44( const Matrix<T,4,4>& m)
    {
        for( int i = 0; i < 16; ++i)
            data_[i] = m[i];
    }

    Matrix<T,4,4> as_foundation_matrix() const
    {
        return Matrix<T,4,4>( data_);
    }

    T operator[]( int index) const
    {
        return data_[index];
    }

    T& operator[]( int index)
    {
        return data_[index];
    }

    T operator()( int row, int col) const
    {
        return data_[row * 4 + col];
    }

    T& operator()( int row, int col)
    {
        return data_[row * 4 + col];
    }

    Vector<T,3> extract_translation() const
    {
        return as_foundation_matrix().extract_translation();
    }

    Quaternion<T> extract_unit_quaternion() const
    {
        return as_foundation_matrix().extract_unit_quaternion();
    }

  private:

    T data_[16];
};

template<class T>
UnalignedMatrix44<T> operator*( const UnalignedMatrix44<T>& a, const UnalignedMatrix44<T>& b)
{
    return UnalignedMatrix44<T>( a.as_foundation_matrix() * b.as_foundation_matrix());
}

template<class T>
Vector<T,4> operator*( const UnalignedMatrix44<T>& a, const Vector<T,4>& v)
{
    return a.as_foundation_matrix() * v;
}

template<class T>
Vector<T,3> operator*( const UnalignedMatrix44<T>& a, const Vector<T,3>& v)
{
    Vector<T,4> v4( v.x, v.y, v.z, T( 1.0));
    v4 = a.as_foundation_matrix() * v4;

    if( feq( v4.w, T(0.0), make_eps<T>(1.0e-6f, 1.0e-9)))
    {
        // TODO: do something here. (est.)
    }

    return Vector<T,3>( v4.x / v4.w, v4.y / v4.w, v4.z / v4.w);
}

template <class T>
std::ostream& operator<<( std::ostream& s, const UnalignedMatrix44<T>& matrix)
{
    return s << matrix.as_foundation_matrix();
}

} // foundation

#endif
