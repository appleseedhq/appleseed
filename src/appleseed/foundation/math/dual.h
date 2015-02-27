
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_MATH_DUAL_H
#define APPLESEED_FOUNDATION_MATH_DUAL_H

// appleseed.foundation headers.
#include "foundation/math/vector.h"


namespace foundation
{

//
// Dual holds a quantity and its partial derivatives wrt. x and y.
//

template <typename T>
class Dual
{
  public:
    // Types.
    typedef T ValueType;

    // Constructors.
    Dual();
    explicit Dual(const T& value);
    Dual(const T& value, const T& dx, const T& dy);

    T m_value;
    T m_dx;
    T m_dy;
};

//
// Dual class implementation.
//

template <typename T>
inline Dual<T>::Dual()
{
}

template <typename T>
inline Dual<T>::Dual(const T& value)
  : m_value(value)
  , m_dx(T(0.0))
  , m_dy(T(0.0))
{
}

template <typename T>
inline Dual<T>::Dual(const T& value, const T& dx, const T& dy)
  : m_value(value)
  , m_dx(dx)
  , m_dy(dy)
{
}


//
// Full specializations for scalars and vectors of type float and double.
//

typedef Dual<float>     Dual1f;
typedef Dual<double>    Dual1d;

typedef Dual<Vector2f>  Dual2f;
typedef Dual<Vector2d>  Dual2d;

typedef Dual<Vector3f>  Dual3f;
typedef Dual<Vector3d>  Dual3d;

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_DUAL_H
