
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
#include "foundation/math/ray.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/poison.h"

namespace foundation
{

//
// Havel-Herout ray-triangle intersection test.
//
// Reference:
//
//   http://igad.nhtv.nl/~bikker/files/faster.pdf
//

template <typename T>
struct TriangleHH
{
    // Types.
    typedef T ValueType;
    typedef Vector<T, 3> VectorType;
    typedef Ray<T, 3> RayType;

    // Triangle plane.
    VectorType  m_n;
    ValueType   m_d;

    // First additional plane.
    VectorType  m_n1;
    ValueType   m_d1;

    // Second additional plane.
    VectorType  m_n2;
    ValueType   m_d2;

    // Constructors.
    TriangleHH();
    TriangleHH(
        const VectorType&   v0,
        const VectorType&   v1,
        const VectorType&   v2);

    // Construct a triangle from another triangle of a different type.
    template <typename U>
    TriangleHH(const TriangleHH<U>& rhs);

    bool intersect(
        const RayType&      ray,
        ValueType&          t,
        ValueType&          u,
        ValueType&          v) const;

    bool intersect(const RayType& ray) const;
};

template <typename T>
struct TriangleHHSupportPlane
{
    // Types.
    typedef T ValueType;
    typedef Vector<T, 3> VectorType;
    typedef Ray<T, 3> RayType;

    // Triangle plane.
    VectorType  m_n;
    ValueType   m_d;

    // Constructors.
    TriangleHHSupportPlane();
    explicit TriangleHHSupportPlane(const TriangleHH<T>& triangle);

    void initialize(const TriangleHH<T>& triangle);

    ValueType intersect(
        const VectorType&   org,
        const VectorType&   dir) const;
};

// Poisoning.
template <typename T>
class PoisonImpl<TriangleHHSupportPlane<T>>
{
  public:
    static void do_poison(TriangleHHSupportPlane<T>& plane);
};


//
// TriangleHH class implementation.
//

template <typename T>
inline TriangleHH<T>::TriangleHH()
{
}

template <typename T>
inline TriangleHH<T>::TriangleHH(
    const VectorType&       v0,
    const VectorType&       v1,
    const VectorType&       v2)
{
    // Compute triangle edges.
    const VectorType e0 = v1 - v0;
    const VectorType e1 = v2 - v0;

    // Compute triangle plane.
    m_n = cross(e0, e1);
    m_d = dot(m_n, v0);

    const ValueType square_norm_n = square_norm(m_n);

    // Compute first additional plane.
    m_n1 = cross(e1, m_n) / square_norm_n;
    m_d1 = -dot(m_n1, v0);

    // Compute second additional plane.
    m_n2 = cross(m_n, e0) / square_norm_n;
    m_d2 = -dot(m_n2, v0);
}

template <typename T>
template <typename U>
APPLESEED_FORCE_INLINE TriangleHH<T>::TriangleHH(const TriangleHH<U>& rhs)
  : m_n(VectorType(rhs.m_n))
  , m_d(static_cast<ValueType>(rhs.m_d))
  , m_n1(VectorType(rhs.m_n1))
  , m_d1(static_cast<ValueType>(rhs.m_d1))
  , m_n2(VectorType(rhs.m_n2))
  , m_d2(static_cast<ValueType>(rhs.m_d2))
{
}

template <typename T>
APPLESEED_FORCE_INLINE bool TriangleHH<T>::intersect(
    const RayType&          ray,
    ValueType&              t,
    ValueType&              u,
    ValueType&              v) const
{
    const ValueType det = dot(ray.m_dir, m_n);
    const ValueType tprime = m_d - dot(ray.m_org, m_n);
    const VectorType pprime = det * ray.m_org + tprime * ray.m_dir;

    // Check bounds on u parameter.
    const ValueType uprime = dot(pprime, m_n1) + det * m_d1;
    if (uprime * (det - uprime) < ValueType(0.0))
        return false;

    // Check bounds on v parameter.
    const ValueType vprime = dot(pprime, m_n2) + det * m_d2;
    if (vprime * (det - uprime - vprime) < ValueType(0.0))
        return false;

    const ValueType rdet = ValueType(1.0) / det;

    // Calculate t parameter and test bounds.
    t = tprime * rdet;
    if (t >= ray.m_tmax || t < ray.m_tmin)
        return false;

    // Calculate u and v parameters.
    u = uprime * rdet;
    v = vprime * rdet;
    return true;
}

template <typename T>
APPLESEED_FORCE_INLINE bool TriangleHH<T>::intersect(const RayType& ray) const
{
    const ValueType det = dot(ray.m_dir, m_n);
    const ValueType tprime = m_d - dot(ray.m_org, m_n);
    const VectorType pprime = det * ray.m_org + tprime * ray.m_dir;

    // Check bounds on u parameter.
    const ValueType uprime = dot(pprime, m_n1) + det * m_d1;
    if (uprime * (det - uprime) < ValueType(0.0))
        return false;

    // Check bounds on v parameter.
    const ValueType vprime = dot(pprime, m_n2) + det * m_d2;
    if (vprime * (det - uprime - vprime) < ValueType(0.0))
        return false;

    // Calculate t parameter and test bounds.
    const ValueType t = tprime / det;
    return t >= ray.m_tmin && t < ray.m_tmax;
}


//
// TriangleHHSupportPlane class implementation.
//

template <typename T>
inline TriangleHHSupportPlane<T>::TriangleHHSupportPlane()
{
}

template <typename T>
inline TriangleHHSupportPlane<T>::TriangleHHSupportPlane(const TriangleHH<T>& triangle)
{
    initialize(triangle);
}

template <typename T>
inline void TriangleHHSupportPlane<T>::initialize(const TriangleHH<T>& triangle)
{
    m_n = triangle.m_n;
    m_d = triangle.m_d;
}

template <typename T>
inline T TriangleHHSupportPlane<T>::intersect(
    const VectorType&       org,
    const VectorType&       dir) const
{
    return (m_d - dot(org, m_n)) / dot(dir, m_n);
}

template <typename T>
void PoisonImpl<TriangleHHSupportPlane<T>>::do_poison(TriangleHHSupportPlane<T>& plane)
{
    poison(plane.m_n);
    poison(plane.m_d);
}

}   // namespace foundation
