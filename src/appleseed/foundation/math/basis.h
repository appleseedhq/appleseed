
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_MATH_BASIS_H
#define APPLESEED_FOUNDATION_MATH_BASIS_H

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/poison.h"

// Standard headers.
#include <cassert>
#include <cmath>

namespace foundation
{

//
// The Basis3 class represents a 3-dimensional orthonormal basis, and allows
// transformation of vectors from parent space to local space and from local
// space to parent space.
//

template <typename T>
class Basis3
{
  public:
    // Vector and basis types.
    typedef Vector<T, 3> VectorType;
    typedef Basis3<T> BasisType;

    // Constructors.
    Basis3();                                       // leave all components uninitialized
    explicit Basis3(const VectorType& normal);      // normal must be unit-length
    Basis3(
        const VectorType&   normal,                 // must be unit-length
        const VectorType&   u);                     // does not need to be unit-length
    Basis3(
        const VectorType&   normal,                 // must be unit-length
        const VectorType&   u,                      // must be unit-length
        const VectorType&   v);                     // must be unit-length

    // Construct a basis from another basis of a different type.
    template <typename U>
    explicit Basis3(const Basis3<U>& rhs);

    // Rebuild the basis for a given unit-length normal vector.
    void build(const VectorType& normal);           // normal must be unit-length

    // Rebuild the basis for a given unit-length normal and U tangent vector.
    void build(
        const VectorType&   normal,                 // must be unit-length
        const VectorType&   u);                     // does not need to be unit-length

    // Rebuild the basis for a given unit-length normal and tangent vectors.
    void build(
        const VectorType&   normal,                 // must be unit-length
        const VectorType&   u,                      // must be unit-length
        const VectorType&   v);                     // must be unit-length

    // Transform a 3D vector. The vector is not required to be unit-length,
    // and is not normalized after transformation. However the length of the
    // vector is preserved by the transformation.
    VectorType transform_to_local(const VectorType& v) const;
    VectorType transform_to_parent(const VectorType& v) const;

    // Transform a 3D vector of a different type.
    template <typename U>
    Vector<U, 3> transform_to_local(const Vector<U, 3>& v) const;
    template <typename U>
    Vector<U, 3> transform_to_parent(const Vector<U, 3>& v) const;

    // Retrieve the individual basis vectors.
    const VectorType& get_normal() const;
    const VectorType& get_tangent_u() const;
    const VectorType& get_tangent_v() const;

  private:
    template <typename> friend class Basis3;
    template <typename> friend class PoisonImpl;

    VectorType m_n, m_u, m_v;

#ifndef NDEBUG
    // Run a bunch of assertions on the basis vectors.
    void checks();
#endif
};

// Poisoning.
template <typename T>
class PoisonImpl<Basis3<T> >
{
  public:
    static void do_poison(Basis3<T>& basis);
};


//
// Specializations for single and double precision orthonormal basis.
//

typedef Basis3<float>  Basis3f;
typedef Basis3<double> Basis3d;


//
// Basis3 class implementation.
//

template <typename T>
inline Basis3<T>::Basis3()
{
}

template <typename T>
inline Basis3<T>::Basis3(const VectorType& normal)
{
    build(normal);
}

template <typename T>
inline Basis3<T>::Basis3(
    const VectorType&   normal,
    const VectorType&   u)
{
    build(normal, u);
}

template <typename T>
inline Basis3<T>::Basis3(
    const VectorType&   normal,
    const VectorType&   u,
    const VectorType&   v)
{
    build(normal, u, v);
}

template <typename T>
template <typename U>
inline Basis3<T>::Basis3(const Basis3<U>& rhs)
  : m_n(rhs.m_n)
  , m_u(rhs.m_u)
  , m_v(rhs.m_v)
{
}

template <typename T>
inline void Basis3<T>::build(const VectorType& normal)
{
    //
    // Reference:
    //
    //   Tom Duff, James Burgess, Per Christensen, Christophe Hery, Andrew Kensler, 
    //   Max Liani, and Ryusuke Villemin, Building an Orthonormal Basis, Revisited, 
    //   Journal of Computer Graphics Techniques (JCGT), vol. 6, no. 1, 1-8, 2017
    //   http://jcgt.org/published/0006/01/01/paper-lowres.pdf
    //

    assert(is_normalized(normal));

    m_n = normal;

    const T sign = m_n[2] < T(0.0) ? T(-1.0) : T(1.0);

    const T a = T(-1.0) / (sign + m_n[2]);
    const T b = m_n[0] * m_n[1] * a;

    m_u = VectorType(b, sign + m_n[1] * m_n[1] * a, -m_n[1]);
    m_v = VectorType(T(1.0) + sign * m_n[0] * m_n[0] * a, sign * b, -sign * m_n[0]);

#ifndef NDEBUG
    checks();
#endif
}

template <typename T>
inline void Basis3<T>::build(
    const VectorType&   normal,
    const VectorType&   u)
{
    assert(is_normalized(normal));

    m_n = normal;
    m_v = normalize(cross(u, m_n));
    m_u = cross(m_n, m_v);

#ifndef NDEBUG
    checks();
#endif
}

template <typename T>
inline void Basis3<T>::build(
    const VectorType&   normal,
    const VectorType&   u,
    const VectorType&   v)
{
    assert(is_normalized(normal));
    assert(is_normalized(u));
    assert(is_normalized(v));

    m_n = normal;
    m_u = u;
    m_v = v;

#ifndef NDEBUG
    checks();
#endif
}

template <typename T>
inline Vector<T, 3> Basis3<T>::transform_to_local(const VectorType& v) const
{
    return
        Vector<T, 3>(
            dot(v, m_u),
            dot(v, m_n),
            dot(v, m_v));
}

template <typename T>
inline Vector<T, 3> Basis3<T>::transform_to_parent(const VectorType& v) const
{
    return
        v[0] * m_u +
        v[1] * m_n +
        v[2] * m_v;
}

template <typename T>
template <typename U>
inline Vector<U, 3> Basis3<T>::transform_to_local(const Vector<U, 3>& v) const
{
    return
        Vector<U, 3>(
            dot(v, Vector<U, 3>(m_u)),
            dot(v, Vector<U, 3>(m_n)),
            dot(v, Vector<U, 3>(m_v)));
}

template <typename T>
template <typename U>
inline Vector<U, 3> Basis3<T>::transform_to_parent(const Vector<U, 3>& v) const
{
    return
        v[0] * Vector<U, 3>(m_u) +
        v[1] * Vector<U, 3>(m_n) +
        v[2] * Vector<U, 3>(m_v);
}

template <typename T>
inline const Vector<T, 3>& Basis3<T>::get_normal() const
{
    return m_n;
}

template <typename T>
inline const Vector<T, 3>& Basis3<T>::get_tangent_u() const
{
    return m_u;
}

template <typename T>
inline const Vector<T, 3>& Basis3<T>::get_tangent_v() const
{
    return m_v;
}

template <typename T>
void PoisonImpl<Basis3<T> >::do_poison(Basis3<T>& basis)
{
    poison(basis.m_n);
    poison(basis.m_u);
    poison(basis.m_v);
}

#ifndef NDEBUG

template <typename T>
void Basis3<T>::checks()
{
    // Make sure (m_u, m_n, m_v) forms an orthonormal basis.
    assert(is_normalized(m_u));
    assert(is_normalized(m_n));
    assert(is_normalized(m_v));
    assert(fz(dot(m_u, m_n), make_eps<T>(1.0e-4f, 1.0e-6)));
    assert(fz(dot(m_u, m_v), make_eps<T>(1.0e-4f, 1.0e-6)));
    assert(fz(dot(m_n, m_v), make_eps<T>(1.0e-4f, 1.0e-6)));

    // Make sure (m_u, m_n, m_v) is right-handed.
    assert(feq(dot(cross(m_u, m_n), m_v), T(1.0), make_eps<T>(1.0e-4f, 1.0e-5)));
}

#endif

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_BASIS_H
