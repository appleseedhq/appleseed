
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/math/vector.h"

// Standard headers.
#include <cassert>

namespace foundation
{

//
// Dual holds a quantity and optionally its partial derivatives wrt. X and Y.
//

template <typename T>
class Dual
{
  public:
    // Types.
    typedef T ValueType;

    // Constructors.
#if APPLESEED_COMPILER_CXX_DEFAULTED_FUNCTIONS
    Dual() = default;                         // leave all components uninitialized
#else
    Dual() {}                                 // leave all components uninitialized
#endif

    explicit Dual(const T& value);
    Dual(const T& value, const T& dx, const T& dy);

    // Construct a dual from another dual of a different type.
    template <typename U>
    explicit Dual(const Dual<U>& rhs);

    // Value.
    const T& get_value() const;

    // Derivatives.
    bool has_derivatives() const;
    const T& get_dx() const;
    const T& get_dy() const;

    void set_derivatives(const T& dx, const T& dy);

  private:
    template <typename U>
    friend class Dual;

    T       m_value;
    T       m_dx;
    T       m_dy;
    bool    m_has_derivatives;
};


//
// Full specializations for scalars and vectors of type float and double.
//

typedef Dual<float>     Dual1f;
typedef Dual<double>    Dual1d;

typedef Dual<Vector2f>  Dual2f;
typedef Dual<Vector2d>  Dual2d;

typedef Dual<Vector3f>  Dual3f;
typedef Dual<Vector3d>  Dual3d;


//
// Dual class implementation.
//

template <typename T>
inline Dual<T>::Dual(const T& value)
  : m_value(value)
  , m_has_derivatives(false)
{
}

template <typename T>
inline Dual<T>::Dual(const T& value, const T& dx, const T& dy)
  : m_value(value)
  , m_dx(dx)
  , m_dy(dy)
  , m_has_derivatives(true)
{
}

template <typename T>
template <typename U>
inline Dual<T>::Dual(const Dual<U>& rhs)
  : m_value(rhs.m_value)
  , m_dx(rhs.m_dx)
  , m_dy(rhs.m_dy)
  , m_has_derivatives(rhs.m_has_derivatives)
{
}

template <typename T>
inline const T& Dual<T>::get_value() const
{
    return m_value;
}

template <typename T>
inline bool Dual<T>::has_derivatives() const
{
    return m_has_derivatives;
}

template <typename T>
inline const T& Dual<T>::get_dx() const
{
    assert(m_has_derivatives);
    return m_dx;
}

template <typename T>
inline const T& Dual<T>::get_dy() const
{
    assert(m_has_derivatives);
    return m_dy;
}

template <typename T>
inline void Dual<T>::set_derivatives(const T& dx, const T& dy)
{
    m_dx = dx;
    m_dy = dy;
    m_has_derivatives = true;
}

}   // namespace foundation
