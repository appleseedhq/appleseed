
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2016 Srinath Ravichandran, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_MATH_BEZIERCURVE_H
#define APPLESEED_FOUNDATION_MATH_BEZIERCURVE_H

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/bezier.h"
#include "foundation/math/matrix.h"
#include "foundation/math/minmax.h"
#include "foundation/math/ray.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>
#include <limits>

namespace foundation
{

//
// Reference:
//
//   Ray Tracing for Curves Primitive
//   Koji Nakamaru, Yoshio Ohno
//   http://wscg.zcu.cz/wscg2002/Papers_2002/A83.pdf
//


//
// Base class for Bezier curves.
// N is the degree of the curve.
// A curve of degree N has N + 1 control points.
//

template <typename T, size_t N>
class BezierCurveBase
{
  public:
    // Types.
    typedef T ValueType;
    typedef Vector<T, 3> VectorType;
    typedef AABB<T, 3> AABBType;
    typedef Matrix<T, 4, 4> MatrixType;

    // Degree of the Bezier curve.
    static const size_t Degree = N;

    // Constructors.
    BezierCurveBase();
    BezierCurveBase(const VectorType ctrl_pts[N + 1], const ValueType width);
    BezierCurveBase(const VectorType ctrl_pts[N + 1], const ValueType width[N + 1]);
    BezierCurveBase(const BezierCurveBase& curve, const MatrixType& xfm);

    size_t get_control_point_count() const;
    const VectorType& get_control_point(const size_t index) const;
    ValueType get_width(const size_t index) const;

    AABBType compute_bbox() const;
    ValueType compute_max_width() const;

  protected:
    template <typename>
    friend class BezierCurveIntersector;

    VectorType  m_ctrl_pts[N + 1];      // control points of the curve
    ValueType   m_width[N + 1];         // per-control point widths

    static VectorType transform_point(const MatrixType& xfm, const VectorType& p);

    size_t compute_recursion_depth(const ValueType epsilon) const;
};


//
// Degree-1 Bezier curve (a straight line).
//

template <typename T>
class BezierCurve1
  : public BezierCurveBase<T, 1>
{
  public:
    typedef BezierCurveBase<T, 1> Base;
    typedef typename Base::ValueType ValueType;
    typedef typename Base::VectorType VectorType;
    typedef typename Base::AABBType AABBType;
    typedef typename Base::MatrixType MatrixType;

    // Constructors.
    BezierCurve1();
    BezierCurve1(const VectorType ctrl_pts[2], const ValueType width);
    BezierCurve1(const VectorType ctrl_pts[2], const ValueType width[2]);
    BezierCurve1(const BezierCurve1& curve, const MatrixType& xfm);

    VectorType evaluate_point(const ValueType t) const;
    ValueType evaluate_width(const ValueType t) const;
    VectorType evaluate_tangent(const ValueType t) const;

    void split(BezierCurve1& c1, BezierCurve1& c2) const;
};


//
// Degree-2 Bezier curve.
//

template <typename T>
class BezierCurve2
  : public BezierCurveBase<T, 2>
{
  public:
    typedef BezierCurveBase<T, 2> Base;
    typedef typename Base::ValueType ValueType;
    typedef typename Base::VectorType VectorType;
    typedef typename Base::AABBType AABBType;
    typedef typename Base::MatrixType MatrixType;

    // Constructors.
    BezierCurve2();
    BezierCurve2(const VectorType ctrl_pts[3], const ValueType width);
    BezierCurve2(const VectorType ctrl_pts[3], const ValueType width[3]);
    BezierCurve2(const BezierCurve2& curve, const MatrixType& xfm);

    VectorType evaluate_point(const ValueType t) const;
    ValueType evaluate_width(const ValueType t) const;
    VectorType evaluate_tangent(const ValueType t) const;

    void split(BezierCurve2& c1, BezierCurve2& c2) const;
};


//
// Degree-3 Bezier curve.
//

template <typename T>
class BezierCurve3
  : public BezierCurveBase<T, 3>
{
  public:
    typedef BezierCurveBase<T, 3> Base;
    typedef typename Base::ValueType ValueType;
    typedef typename Base::VectorType VectorType;
    typedef typename Base::AABBType AABBType;
    typedef typename Base::MatrixType MatrixType;

    // Constructors.
    BezierCurve3();
    BezierCurve3(const VectorType ctrl_pts[4], const ValueType width);
    BezierCurve3(const VectorType ctrl_pts[4], const ValueType width[4]);
    BezierCurve3(const BezierCurve3& curve, const MatrixType& xfm);

    VectorType evaluate_point(const ValueType t) const;
    ValueType evaluate_width(const ValueType t) const;
    VectorType evaluate_tangent(const ValueType t) const;

    void split(BezierCurve3& c1, BezierCurve3& c2) const;
};


//
// Full specializations for degree 1, 2, 3 Bezier curves of type float and double.
//

typedef BezierCurve1<float>  BezierCurve1f;
typedef BezierCurve1<double> BezierCurve1d;
typedef BezierCurve2<float>  BezierCurve2f;
typedef BezierCurve2<double> BezierCurve2d;
typedef BezierCurve3<float>  BezierCurve3f;
typedef BezierCurve3<double> BezierCurve3d;


//
// Bezier curve intersector.
//

template <typename BezierCurveType>
class BezierCurveIntersector
{
  public:
    typedef typename BezierCurveType::ValueType ValueType;
    typedef typename BezierCurveType::VectorType VectorType;
    typedef typename BezierCurveType::AABBType AABBType;
    typedef typename BezierCurveType::MatrixType MatrixType;
    typedef Ray<ValueType, 3> RayType;

    // Compute the intersection between a ray and a curve.
    static bool intersect(
        const BezierCurveType&  curve,
        const RayType&          ray,
        const MatrixType&       xfm,
        ValueType&              u,
        ValueType&              v,
        ValueType&              t,
        const ValueType         epsilon = ValueType(0.05),
        const size_t            max_depth = 5);

    // Return whether a ray intersects a curve.
    static bool intersect(
        const BezierCurveType&  curve,
        const RayType&          ray,
        const MatrixType&       xfm,
        const ValueType         epsilon = ValueType(0.05),
        const size_t            max_depth = 5);

  private:
    // Dot product function that only considers the x and y components of the vectors.
    static ValueType dotxy(const VectorType& lhs, const VectorType& rhs)
    {
        return lhs.x * rhs.x + lhs.y * rhs.y;
    }

    static bool converge(
        const size_t            depth,
        const BezierCurveType&  curve,
        const ValueType         half_max_width,
        const ValueType         v0,
        const ValueType         vn,
        ValueType&              u,
        ValueType&              v,
        ValueType&              t,
        const bool              compute_params);
};


//
// BezierCurveBase class implementation.
//

template <typename T, size_t N>
inline BezierCurveBase<T, N>::BezierCurveBase()
{
}

template <typename T, size_t N>
BezierCurveBase<T, N>::BezierCurveBase(const VectorType ctrl_pts[N + 1], const ValueType width)
{
    assert(width >= ValueType(0.0));

    for (size_t i = 0; i < N + 1; ++i)
    {
        m_ctrl_pts[i] = ctrl_pts[i];
        m_width[i] = width;
    }
}

template <typename T, size_t N>
BezierCurveBase<T, N>::BezierCurveBase(const VectorType ctrl_pts[N + 1], const ValueType width[N + 1])
{
    for (size_t i = 0; i < N + 1; ++i)
    {
        assert(width[i] >= ValueType(0.0));
        m_ctrl_pts[i] = ctrl_pts[i];
        m_width[i] = width[i];
    }
}

template <typename T, size_t N>
BezierCurveBase<T, N>::BezierCurveBase(const BezierCurveBase& curve, const MatrixType& xfm)
{
    for (size_t i = 0; i < N + 1; ++i)
    {
        m_ctrl_pts[i] = transform_point(xfm, curve.m_ctrl_pts[i]);
        m_width[i] = curve.m_width[i];
    }
}

template <typename T, size_t N>
inline size_t BezierCurveBase<T, N>::get_control_point_count() const
{
    return N + 1;
}

template <typename T, size_t N>
inline const typename BezierCurveBase<T, N>::VectorType& BezierCurveBase<T, N>::get_control_point(const size_t index) const
{
    assert(index < N + 1);
    return m_ctrl_pts[index];
}

template <typename T, size_t N>
inline T BezierCurveBase<T, N>::get_width(const size_t index) const
{
    assert(index < N + 1);
    return m_width[index];
}

template <typename T, size_t N>
inline typename BezierCurveBase<T, N>::AABBType BezierCurveBase<T, N>::compute_bbox() const
{
    AABBType bbox;
    bbox.invalidate();

    for (size_t i = 0; i < N + 1; ++i)
        bbox.insert(m_ctrl_pts[i]);

    return bbox;
}

template <typename T, size_t N>
inline T BezierCurveBase<T, N>::compute_max_width() const
{
    ValueType max_width = m_width[0];

    for (size_t i = 1; i < N + 1; ++i)
        max_width = std::max(max_width, m_width[i]);

    return max_width;
}

template <typename T, size_t N>
inline typename BezierCurveBase<T, N>::VectorType BezierCurveBase<T, N>::transform_point(const MatrixType& xfm, const VectorType& p)
{
    const Vector<ValueType, 4> pt(p.x, p.y, p.z, ValueType(1.0));
    const Vector<ValueType, 4> xpt = xfm * pt;

    assert(xpt.w != ValueType(0.0));
    const ValueType rcp_w = ValueType(1.0) / xpt.w;

    return VectorType(xpt.x * rcp_w, xpt.y * rcp_w, xpt.z * rcp_w);
}

template <typename T, size_t N>
size_t BezierCurveBase<T, N>::compute_recursion_depth(const ValueType epsilon) const
{
    if (N < 2)
        return 0;

    ValueType l0 = -std::numeric_limits<ValueType>::max();

    for (size_t i = 0; i <= N - 2; ++i)
    {
        l0 =
            max(
                l0,
                std::abs(m_ctrl_pts[i].x - ValueType(2.0) * m_ctrl_pts[i + 1].x + m_ctrl_pts[i + 2].x),
                std::abs(m_ctrl_pts[i].y - ValueType(2.0) * m_ctrl_pts[i + 1].y + m_ctrl_pts[i + 2].y),
                std::abs(m_ctrl_pts[i].z - ValueType(2.0) * m_ctrl_pts[i + 1].z + m_ctrl_pts[i + 2].z));
    }

    const ValueType value = (SqrtTwo<ValueType>() * N * (N - 1) * l0) / (ValueType(8.0) * epsilon);
    const ValueType RcpLog4 = ValueType(0.7213475204444817);
    const ValueType r0 = std::log(value) * RcpLog4;
    return r0 > ValueType(0.0) ? truncate<size_t>(r0) : 0;
}


//
// BezierCurve1 class implementation.
//

template <typename T>
inline BezierCurve1<T>::BezierCurve1()
{
}

template <typename T>
inline BezierCurve1<T>::BezierCurve1(const VectorType ctrl_pts[2], const ValueType width)
  : Base(ctrl_pts, width)
{
}

template <typename T>
inline BezierCurve1<T>::BezierCurve1(const VectorType ctrl_pts[2], const ValueType width[2])
  : Base(ctrl_pts, width)
{
}

template <typename T>
inline BezierCurve1<T>::BezierCurve1(const BezierCurve1& curve, const MatrixType& xfm)
  : Base(curve, xfm)
{
}

template <typename T>
inline typename BezierCurve1<T>::VectorType BezierCurve1<T>::evaluate_point(const ValueType t) const
{
    return
        evaluate_bezier1(
            Base::m_ctrl_pts[0],
            Base::m_ctrl_pts[1],
            t);
}

template <typename T>
inline typename BezierCurve1<T>::ValueType BezierCurve1<T>::evaluate_width(const ValueType t) const
{
    return
        evaluate_bezier1(
            Base::m_width[0],
            Base::m_width[1],
            t);
}

template <typename T>
inline typename BezierCurve1<T>::VectorType BezierCurve1<T>::evaluate_tangent(const ValueType t) const
{
    return
        evaluate_bezier1_derivative(
            Base::m_ctrl_pts[0],
            Base::m_ctrl_pts[1],
            t);
}

template <typename T>
inline void BezierCurve1<T>::split(BezierCurve1& c1, BezierCurve1& c2) const
{
    const VectorType pq = ValueType(0.5) * (Base::m_ctrl_pts[0] + Base::m_ctrl_pts[1]);

    c1.m_ctrl_pts[0] = Base::m_ctrl_pts[0];
    c1.m_ctrl_pts[1] = pq;

    c2.m_ctrl_pts[0] = pq;
    c2.m_ctrl_pts[1] = Base::m_ctrl_pts[1];

    const ValueType wq = ValueType(0.5) * (Base::m_width[0] + Base::m_width[1]);

    c1.m_width[0] = Base::m_width[0];
    c1.m_width[1] = wq;

    c2.m_width[0] = wq;
    c2.m_width[1] = Base::m_width[1];
}


//
// BezierCurve2 class implementation.
//

template <typename T>
inline BezierCurve2<T>::BezierCurve2()
{
}

template <typename T>
inline BezierCurve2<T>::BezierCurve2(const VectorType ctrl_pts[3], const ValueType width)
  : Base(ctrl_pts, width)
{
}

template <typename T>
inline BezierCurve2<T>::BezierCurve2(const VectorType ctrl_pts[3], const ValueType width[3])
  : Base(ctrl_pts, width)
{
}

template <typename T>
inline BezierCurve2<T>::BezierCurve2(const BezierCurve2& curve, const MatrixType& xfm)
  : Base(curve, xfm)
{
}

template <typename T>
inline typename BezierCurve2<T>::VectorType BezierCurve2<T>::evaluate_point(const ValueType t) const
{
    return
        evaluate_bezier2(
            Base::m_ctrl_pts[0],
            Base::m_ctrl_pts[1],
            Base::m_ctrl_pts[2],
            t);
}

template <typename T>
inline typename BezierCurve2<T>::ValueType BezierCurve2<T>::evaluate_width(const ValueType t) const
{
    return
        evaluate_bezier2(
            Base::m_width[0],
            Base::m_width[1],
            Base::m_width[2],
            t);
}

template <typename T>
inline typename BezierCurve2<T>::VectorType BezierCurve2<T>::evaluate_tangent(const ValueType t) const
{
    return
        evaluate_bezier2_derivative(
            Base::m_ctrl_pts[0],
            Base::m_ctrl_pts[1],
            Base::m_ctrl_pts[2],
            t);
}

template <typename T>
void BezierCurve2<T>::split(BezierCurve2& c1, BezierCurve2& c2) const
{
    const VectorType pm0 = ValueType(0.5) * (Base::m_ctrl_pts[0] + Base::m_ctrl_pts[1]);
    const VectorType pm1 = ValueType(0.5) * (Base::m_ctrl_pts[1] + Base::m_ctrl_pts[2]);

    const VectorType pq = ValueType(0.5) * (pm0 + pm1);

    c1.m_ctrl_pts[0] = Base::m_ctrl_pts[0];
    c1.m_ctrl_pts[1] = pm0;
    c1.m_ctrl_pts[2] = pq;

    c2.m_ctrl_pts[0] = pq;
    c2.m_ctrl_pts[1] = pm1;
    c2.m_ctrl_pts[2] = Base::m_ctrl_pts[2];

    const ValueType wm0 = ValueType(0.5) * (Base::m_width[0] + Base::m_width[1]);
    const ValueType wm1 = ValueType(0.5) * (Base::m_width[1] + Base::m_width[2]);

    const ValueType wq = ValueType(0.5) * (wm0 + wm1);

    c1.m_width[0] = Base::m_width[0];
    c1.m_width[1] = wm0;
    c1.m_width[2] = wq;

    c2.m_width[0] = wq;
    c2.m_width[1] = wm1;
    c2.m_width[2] = Base::m_width[2];
}


//
// BezierCurve3 class implementation.
//

template <typename T>
inline BezierCurve3<T>::BezierCurve3()
{
}

template <typename T>
inline BezierCurve3<T>::BezierCurve3(const VectorType ctrl_pts[4], const ValueType width)
  : Base(ctrl_pts, width)
{
}

template <typename T>
inline BezierCurve3<T>::BezierCurve3(const VectorType ctrl_pts[4], const ValueType width[4])
  : Base(ctrl_pts, width)
{
}

template <typename T>
inline BezierCurve3<T>::BezierCurve3(const BezierCurve3& curve, const MatrixType& xfm)
  : Base(curve, xfm)
{
}

template <typename T>
inline typename BezierCurve3<T>::VectorType BezierCurve3<T>::evaluate_point(const ValueType t) const
{
    return
        evaluate_bezier3(
            Base::m_ctrl_pts[0],
            Base::m_ctrl_pts[1],
            Base::m_ctrl_pts[2],
            Base::m_ctrl_pts[3],
            t);
}

template <typename T>
inline typename BezierCurve3<T>::ValueType BezierCurve3<T>::evaluate_width(const ValueType t) const
{
    return
        evaluate_bezier3(
            Base::m_width[0],
            Base::m_width[1],
            Base::m_width[2],
            Base::m_width[3],
            t);
}

template <typename T>
inline typename BezierCurve3<T>::VectorType BezierCurve3<T>::evaluate_tangent(const ValueType t) const
{
    return
        evaluate_bezier3_derivative(
            Base::m_ctrl_pts[0],
            Base::m_ctrl_pts[1],
            Base::m_ctrl_pts[2],
            Base::m_ctrl_pts[3],
            t);
}

template <typename T>
void BezierCurve3<T>::split(BezierCurve3& c1, BezierCurve3& c2) const
{
    const VectorType pm0 = ValueType(0.5) * (Base::m_ctrl_pts[0] + Base::m_ctrl_pts[1]);
    const VectorType pm1 = ValueType(0.5) * (Base::m_ctrl_pts[1] + Base::m_ctrl_pts[2]);
    const VectorType pm2 = ValueType(0.5) * (Base::m_ctrl_pts[2] + Base::m_ctrl_pts[3]);

    const VectorType pn0 = ValueType(0.5) * (pm0 + pm1);
    const VectorType pn1 = ValueType(0.5) * (pm1 + pm2);
    const VectorType pq  = ValueType(0.5) * (pn0 + pn1);

    c1.m_ctrl_pts[0] = Base::m_ctrl_pts[0];
    c1.m_ctrl_pts[1] = pm0;
    c1.m_ctrl_pts[2] = pn0;
    c1.m_ctrl_pts[3] = pq;

    c2.m_ctrl_pts[0] = pq;
    c2.m_ctrl_pts[1] = pn1;
    c2.m_ctrl_pts[2] = pm2;
    c2.m_ctrl_pts[3] = Base::m_ctrl_pts[3];

    const ValueType wm0 = ValueType(0.5) * (Base::m_width[0] + Base::m_width[1]);
    const ValueType wm1 = ValueType(0.5) * (Base::m_width[1] + Base::m_width[2]);
    const ValueType wm2 = ValueType(0.5) * (Base::m_width[2] + Base::m_width[3]);

    const ValueType wn0 = ValueType(0.5) * (wm0 + wm1);
    const ValueType wn1 = ValueType(0.5) * (wm1 + wm2);
    const ValueType wq =  ValueType(0.5) * (wn0 + wn1);

    c1.m_width[0] = Base::m_width[0];
    c1.m_width[1] = wm0;
    c1.m_width[2] = wn0;
    c1.m_width[3] = wq;

    c2.m_width[0] = wq;
    c2.m_width[1] = wn1;
    c2.m_width[2] = wm2;
    c2.m_width[3] = Base::m_width[3];
}


//
// Projection transform function for ray-curve intersection.
//

template <typename MatrixType, typename RayType>
void make_curve_projection_transform(
    MatrixType&         matrix,
    const RayType&      ray)
{
    typedef typename MatrixType::ValueType ValueType;
    typedef typename RayType::VectorType   VectorType;

    //
    // We build a matrix that will transform a curve such that:
    //
    //   - It lies in the XY plane.
    //   - The ray-curve intersection point is at the origin.
    //
    // This matrix is the product of three transforms (in order):
    //
    //   1. A translation
    //   2. A rotation around Y
    //   3. A rotation around X
    //
    // Unoptimized implementation:
    //
    //     const VectorType dir = normalize(ray.m_dir);
    //     const ValueType d = std::sqrt(dir.x * dir.x + dir.z * dir.z);
    //
    //     if (d > ValueType(0.0))
    //     {
    //         const MatrixType rot_y = MatrixType::make_rotation_y(dir.z / d, -dir.x / d);
    //         const MatrixType rot_x = MatrixType::make_rotation_x(d, dir.y);
    //         const MatrixType tr = MatrixType::make_translation(-ray.m_org);
    //         matrix = rot_x * rot_y * tr;
    //     }
    //     else
    //     {
    //         const ValueType phi = dir.y > ValueType(0.0) ? HalfPi<ValueType>() : -HalfPi<ValueType>();
    //         const MatrixType rot_x = MatrixType::make_rotation_x(phi);
    //         const MatrixType tr = MatrixType::make_translation(-ray.m_org);
    //         matrix = rot_x * tr;
    //     }
    //
    // Note: This function does not depend on the type of the curve, only on the ray.
    //       Hence it's made into a free function.
    //

    const VectorType dir = normalize(ray.m_dir);
    const ValueType d = std::sqrt(dir.x * dir.x + dir.z * dir.z);

    if (d > ValueType(0.0))
    {
        const ValueType rcp_d = ValueType(1.0) / d;

        // First row.
        matrix[ 0] = dir.z * rcp_d;
        matrix[ 1] = ValueType(0.0);
        matrix[ 2] = -dir.x * rcp_d;
        matrix[ 3] = -(matrix[0] * ray.m_org.x + matrix[2] * ray.m_org.z);

        // Second row.
        matrix[ 4] = -(dir.x * dir.y) * rcp_d;
        matrix[ 5] = d;
        matrix[ 6] = -(dir.y * dir.z) * rcp_d;
        matrix[ 7] = -(matrix[4] * ray.m_org.x + matrix[5] * ray.m_org.y + matrix[6] * ray.m_org.z);

        // Third row.
        matrix[ 8] = dir.x;
        matrix[ 9] = dir.y;
        matrix[10] = dir.z;
        matrix[11] = -(matrix[8] * ray.m_org.x + matrix[9] * ray.m_org.y + matrix[10] * ray.m_org.z);

        // Fourth row.
        matrix[12] = ValueType(0.0);
        matrix[13] = ValueType(0.0);
        matrix[14] = ValueType(0.0);
        matrix[15] = ValueType(1.0);
    }
    else
    {
        const ValueType sin_angle = dir.y > ValueType(0.0) ? ValueType(1.0) : -ValueType(1.0);

        // First row.
        matrix[ 0] = ValueType(1.0);
        matrix[ 1] = ValueType(0.0);
        matrix[ 2] = ValueType(0.0);
        matrix[ 3] = -ray.m_org.x;

        // Second row.
        matrix[ 4] = ValueType(0.0);
        matrix[ 5] = ValueType(0.0);
        matrix[ 6] = -sin_angle;
        matrix[ 7] = sin_angle * ray.m_org.z;

        // Third row.
        matrix[ 8] = ValueType(0.0);
        matrix[ 9] = sin_angle;
        matrix[10] = ValueType(0.0);
        matrix[11] = -sin_angle * ray.m_org.y;

        // Fourth row.
        matrix[12] = ValueType(0.0);
        matrix[13] = ValueType(0.0);
        matrix[14] = ValueType(0.0);
        matrix[15] = ValueType(1.0);
    }
}


//
// BezierCurveIntersector class implementation.
//

template <typename BezierCurveType>
bool BezierCurveIntersector<BezierCurveType>::intersect(
    const BezierCurveType&  curve,
    const RayType&          ray,
    const MatrixType&       xfm,
    ValueType&              u,
    ValueType&              v,
    ValueType&              t,
    const ValueType         epsilon,
    const size_t            max_depth)
{
    const BezierCurveType xfm_curve(curve, xfm);
    const ValueType max_width = xfm_curve.compute_max_width();
    const size_t depth = xfm_curve.compute_recursion_depth(max_width * epsilon);

    const ValueType norm_dir = norm(ray.m_dir);
    ValueType scaled_t = t * norm_dir;

    if (converge(
            depth < max_depth ? depth : max_depth,
            xfm_curve,
            ValueType(0.5) * max_width,
            ValueType(0.0), ValueType(1.0),
            u, v,
            scaled_t,
            true))
    {
        t = scaled_t / norm_dir;
        return true;
    }

    return false;
}

template <typename BezierCurveType>
bool BezierCurveIntersector<BezierCurveType>::intersect(
    const BezierCurveType&  curve,
    const RayType&          ray,
    const MatrixType&       xfm,
    const ValueType         epsilon,
    const size_t            max_depth)
{
    const BezierCurveType xfm_curve(curve, xfm);
    const ValueType max_width = xfm_curve.compute_max_width();
    const size_t depth = xfm_curve.compute_recursion_depth(max_width * epsilon);

    const ValueType norm_dir = norm(ray.m_dir);
    ValueType scaled_t = ray.m_tmax * norm_dir;

    ValueType u, v;
    return
        converge(
            depth < max_depth ? depth : max_depth,
            xfm_curve,
            ValueType(0.5) * max_width,
            ValueType(0.0), ValueType(1.0),
            u, v,
            scaled_t,
            false);
}

template <typename BezierCurveType>
bool BezierCurveIntersector<BezierCurveType>::converge(
    const size_t            depth,
    const BezierCurveType&  curve,
    const ValueType         half_max_width,
    const ValueType         v0,
    const ValueType         vn,
    ValueType&              u,
    ValueType&              v,
    ValueType&              t,
    const bool              compute_params)
{
    const AABBType bbox = curve.compute_bbox();

    // Check whether the curve's bounding box overlaps the square centered at 0.
    if (bbox.min.z > t              || bbox.max.z < ValueType(1.0e-6) ||
        bbox.min.x > half_max_width || bbox.max.x < -half_max_width   ||
        bbox.min.y > half_max_width || bbox.max.y < -half_max_width)
        return false;

    if (depth > 0)
    {
        // Split the curve.
        BezierCurveType c1, c2;
        curve.split(c1, c2);

        // Recurse on the two child curves.
        const ValueType vm = (v0 + vn) * ValueType(0.5);
        return
            converge(depth - 1, c1, half_max_width, v0, vm, u, v, t, compute_params) ||
            converge(depth - 1, c2, half_max_width, vm, vn, u, v, t, compute_params);
    }
    else
    {
        // Compute the intersection.

        const VectorType& cp0 = curve.get_control_point(0);
        const VectorType& cpn = curve.get_control_point(BezierCurveType::Degree);
        const VectorType dir = cpn - cp0;

        VectorType dp0 = curve.get_control_point(1) - cp0;
        if (dotxy(dir, dp0) < ValueType(0.0))
            dp0 = -dp0;

        if (dotxy(dp0, cp0) > ValueType(0.0))
            return false;

        VectorType dpn = cpn - curve.get_control_point(BezierCurveType::Degree - 1);
        if (dotxy(dir, dpn) < ValueType(0.0))
            dpn = -dpn;

        if (dotxy(dpn, cpn) < ValueType(0.0))
            return false;

        // Compute w on the line segment.
        const ValueType den = dotxy(dir, dir);
        if (den < ValueType(0.0))
            return false;
        const ValueType w = saturate(-dotxy(cp0, dir) / den);

        // Compute point on curve.
        const VectorType p = curve.evaluate_point(w);

        // Compare Z distances.
        if (p.z < ValueType(0.0) || p.z > t)
            return false;

        // Compute curve width.
        const ValueType width = curve.evaluate_width(w);

        // Compare X-Y distances.
        if (dotxy(p, p) >= ValueType(0.25) * width * width)
            return false;

        if (compute_params)
        {
            // Found an intersection.
            t = p.z;

            // Compute v parameter.
            v = lerp(v0, vn, w);

            // Compute the bitangent and intersection point in XY plane.
            const VectorType tangent = curve.evaluate_tangent(v);
            const Vector<ValueType, 2> bitangent = normalize(Vector<ValueType, 2>(-tangent.y, tangent.x));
            const Vector<ValueType, 2> point(p.x, p.y);

            // Compute the vertical projection of point to the bitangent.
            const ValueType vert_proj = dot(point, bitangent);

            // Compute u parameter.
            u = saturate((vert_proj + ValueType(0.5) * width) / width);
        }

        return true;
    }
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_BEZIERCURVE_H
