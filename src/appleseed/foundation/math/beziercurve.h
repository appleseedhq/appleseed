
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Srinath Ravichandran, The appleseedhq Organization
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
#include "foundation/image/color.h"
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
    typedef Color<T, 3> ColorType;

    // Degree of the Bezier curve.
    static const size_t Degree = N;

    // Constructors.
    BezierCurveBase();
    BezierCurveBase(
        const VectorType    ctrl_pts[N + 1],
        const ValueType     width,
        const ValueType     opacity,
        const ColorType     color);
    BezierCurveBase(
        const VectorType    ctrl_pts[N + 1],
        const ValueType     width[N + 1],
        const ValueType     opacity[N + 1],
        const ColorType     color[N + 1]);
    BezierCurveBase(const BezierCurveBase& curve, const MatrixType& xfm);

    size_t get_control_point_count() const;
    const VectorType& get_control_point(const size_t index) const;
    ValueType get_width(const size_t index) const;
    ValueType get_opacity(const size_t index) const;
    const ColorType& get_color(const size_t index) const;

    AABBType compute_bbox() const;
    ValueType compute_max_width() const;

  protected:
    template <typename>
    friend class BezierCurveIntersector;

    VectorType  m_ctrl_pts[N + 1];      // control points of the curve
    ValueType   m_width[N + 1];         // per-control point widths
    ValueType   m_opacity[N + 1];       // per-control point opacities
    ColorType   m_color[N + 1];         // per-control point colors

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
    typedef typename Base::ColorType ColorType;

    // Constructors.
    BezierCurve1();
    BezierCurve1(
        const VectorType    ctrl_pts[2],
        const ValueType     width,
        const ValueType     opacity,
        const ColorType     color);
    BezierCurve1(
        const VectorType    ctrl_pts[2],
        const ValueType     width[2],
        const ValueType     opacity[2],
        const ColorType     color[2]);
    BezierCurve1(const BezierCurve1& curve, const MatrixType& xfm);

    VectorType evaluate_point(const ValueType t) const;
    ValueType evaluate_width(const ValueType t) const;
    ValueType evaluate_opacity(const ValueType t) const;
    ColorType evaluate_color(const ValueType t) const;
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
    typedef typename Base::ColorType ColorType;

    // Constructors.
    BezierCurve2();
    BezierCurve2(
        const VectorType    ctrl_pts[3],
        const ValueType     width,
        const ValueType     opacity,
        const ColorType     color);
    BezierCurve2(
        const VectorType    ctrl_pts[3],
        const ValueType     width[3],
        const ValueType     opacity[3],
        const ColorType     color[3]);
    BezierCurve2(const BezierCurve2& curve, const MatrixType& xfm);

    VectorType evaluate_point(const ValueType t) const;
    ValueType evaluate_width(const ValueType t) const;
    ValueType evaluate_opacity(const ValueType t) const;
    ColorType evaluate_color(const ValueType t) const;
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
    typedef typename Base::ColorType ColorType;

    // Constructors.
    BezierCurve3();
    BezierCurve3(
        const VectorType    ctrl_pts[4],
        const ValueType     width,
        const ValueType     opacity,
        const ColorType     color);
    BezierCurve3(
        const VectorType    ctrl_pts[4],
        const ValueType     width[4],
        const ValueType     opacity[4],
        const ColorType     color[4]);
    BezierCurve3(const BezierCurve3& curve, const MatrixType& xfm);

    VectorType evaluate_point(const ValueType t) const;
    ValueType evaluate_width(const ValueType t) const;
    ValueType evaluate_opacity(const ValueType t) const;
    ColorType evaluate_color(const ValueType t) const;
    VectorType evaluate_tangent(const ValueType t) const;
    void transform_basis(const MatrixType& xfm);

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
BezierCurveBase<T, N>::BezierCurveBase(
    const VectorType    ctrl_pts[N + 1],
    const ValueType     width,
    const ValueType     opacity,
    const ColorType     color)
{
    assert(width >= ValueType(0.0));

    for (size_t i = 0; i < N + 1; ++i)
    {
        m_ctrl_pts[i] = ctrl_pts[i];
        m_width[i] = width;
        m_opacity[i] = opacity;
        m_color[i] = color;
    }
}

template <typename T, size_t N>
BezierCurveBase<T, N>::BezierCurveBase(
    const VectorType    ctrl_pts[N + 1],
    const ValueType     width[N + 1],
    const ValueType     opacity[N + 1],
    const ColorType     color[N + 1])
{
    for (size_t i = 0; i < N + 1; ++i)
    {
        assert(width[i] >= ValueType(0.0));
        m_ctrl_pts[i] = ctrl_pts[i];
        m_width[i] = width[i];
        m_opacity[i] = opacity[i];
        m_color[i] = color[i];
    }
}

template <typename T, size_t N>
BezierCurveBase<T, N>::BezierCurveBase(
    const BezierCurveBase&  curve,
    const MatrixType&       xfm)
{
    for (size_t i = 0; i < N + 1; ++i)
    {
        m_ctrl_pts[i] = transform_point(xfm, curve.m_ctrl_pts[i]);
        m_width[i] = curve.m_width[i];
        m_opacity[i] = curve.m_opacity[i];
        m_color[i] = curve.m_color[i];
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
inline T BezierCurveBase<T, N>::get_opacity(const size_t index) const
{
    assert(index < N + 1);
    return m_opacity[index];
}

template <typename T, size_t N>
inline const typename BezierCurveBase<T, N>::ColorType& BezierCurveBase<T, N>::get_color(const size_t index) const
{
    assert(index < N + 1);
    return m_color[index];
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
inline BezierCurve1<T>::BezierCurve1(
    const VectorType    ctrl_pts[2],
    const ValueType     width,
    const ValueType     opacity,
    const ColorType     color)
  : Base(ctrl_pts, width, opacity, color)
{
}

template <typename T>
inline BezierCurve1<T>::BezierCurve1(
    const VectorType    ctrl_pts[2],
    const ValueType     width[2],
    const ValueType     opacity[2],
    const ColorType     color[2])
  : Base(ctrl_pts, width, opacity, color)
{
}

template <typename T>
inline BezierCurve1<T>::BezierCurve1(
    const BezierCurve1&     curve,
    const MatrixType&       xfm)
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
inline typename BezierCurve1<T>::ValueType BezierCurve1<T>::evaluate_opacity(const ValueType t) const
{
    return
        evaluate_bezier1(
            Base::m_opacity[0],
            Base::m_opacity[1],
            t);
}

template <typename T>
inline typename BezierCurve1<T>::ColorType BezierCurve1<T>::evaluate_color(const ValueType t) const
{
    return
        evaluate_bezier1(
            Base::m_color[0],
            Base::m_color[1],
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

    const ValueType oq = ValueType(0.5) * (Base::m_opacity[0] + Base::m_opacity[1]);

    c1.m_opacity[0] = Base::m_opacity[0];
    c1.m_opacity[1] = oq;

    c2.m_opacity[0] = oq;
    c2.m_opacity[1] = Base::m_opacity[1];

    const ColorType cq = ValueType(0.5) * (Base::m_color[0] + Base::m_color[1]);

    c1.m_color[0] = Base::m_color[0];
    c1.m_color[1] = cq;

    c2.m_color[0] = cq;
    c2.m_color[1] = Base::m_color[1];
}


//
// BezierCurve2 class implementation.
//

template <typename T>
inline BezierCurve2<T>::BezierCurve2()
{
}

template <typename T>
inline BezierCurve2<T>::BezierCurve2(
    const VectorType    ctrl_pts[3],
    const ValueType     width,
    const ValueType     opacity,
    const ColorType     color)
  : Base(ctrl_pts, width, opacity, color)
{
}

template <typename T>
inline BezierCurve2<T>::BezierCurve2(
    const VectorType    ctrl_pts[3],
    const ValueType     width[3],
    const ValueType     opacity[3],
    const ColorType     color[3])
  : Base(ctrl_pts, width, opacity, color)
{
}

template <typename T>
inline BezierCurve2<T>::BezierCurve2(
    const BezierCurve2&     curve,
    const MatrixType&       xfm)
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
inline typename BezierCurve2<T>::ValueType BezierCurve2<T>::evaluate_opacity(const ValueType t) const
{
    return
        evaluate_bezier2(
            Base::m_opacity[0],
            Base::m_opacity[1],
            Base::m_opacity[2],
            t);
}

template <typename T>
inline typename BezierCurve2<T>::ColorType BezierCurve2<T>::evaluate_color(const ValueType t) const
{
    return
        evaluate_bezier2(
            Base::m_color[0],
            Base::m_color[1],
            Base::m_color[2],
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

    const ValueType om0 = ValueType(0.5) * (Base::m_opacity[0] + Base::m_opacity[1]);
    const ValueType om1 = ValueType(0.5) * (Base::m_opacity[1] + Base::m_opacity[2]);

    const ValueType oq = ValueType(0.5) * (om0 + om1);

    c1.m_opacity[0] = Base::m_opacity[0];
    c1.m_opacity[1] = om0;
    c1.m_opacity[2] = oq;

    c2.m_opacity[0] = oq;
    c2.m_opacity[1] = om1;
    c2.m_opacity[2] = Base::m_opacity[2];

    const ColorType cm0 = ValueType(0.5) * (Base::m_color[0] + Base::m_color[1]);
    const ColorType cm1 = ValueType(0.5) * (Base::m_color[1] + Base::m_color[2]);

    const ColorType cq = ValueType(0.5) * (cm0 + cm1);

    c1.m_color[0] = Base::m_color[0];
    c1.m_color[1] = cm0;
    c1.m_color[2] = cq;

    c2.m_color[0] = cq;
    c2.m_color[1] = cm1;
    c2.m_color[2] = Base::m_color[2];
}


//
// BezierCurve3 class implementation.
//

template <typename T>
inline BezierCurve3<T>::BezierCurve3()
{
}

template <typename T>
inline BezierCurve3<T>::BezierCurve3(
    const VectorType    ctrl_pts[4],
    const ValueType     width,
    const ValueType     opacity,
    const ColorType     color)
  : Base(ctrl_pts, width, opacity, color)
{
}

template <typename T>
inline BezierCurve3<T>::BezierCurve3(
    const VectorType    ctrl_pts[4],
    const ValueType     width[4],
    const ValueType     opacity[4],
    const ColorType     color[4])
  : Base(ctrl_pts, width, opacity, color)
{
}

template <typename T>
inline BezierCurve3<T>::BezierCurve3(
    const BezierCurve3&     curve,
    const MatrixType&       xfm)
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
inline typename BezierCurve3<T>::ValueType BezierCurve3<T>::evaluate_opacity(const ValueType t) const
{
    return
        evaluate_bezier3(
            Base::m_opacity[0],
            Base::m_opacity[1],
            Base::m_opacity[2],
            Base::m_opacity[3],
            t);
}

template <typename T>
inline typename BezierCurve3<T>::ColorType BezierCurve3<T>::evaluate_color(const ValueType t) const
{
    return
        evaluate_bezier3(
            Base::m_color[0],
            Base::m_color[1],
            Base::m_color[2],
            Base::m_color[3],
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
inline void BezierCurve3<T>::transform_basis(const MatrixType& xfm)
{
    Matrix<ValueType, 4, 3> matrix_points = Matrix<ValueType, 4, 3>();
    Matrix<ValueType, 4, 3> matrix_colors = Matrix<ValueType, 4, 3>();
    Matrix<ValueType, 4, 1> matrix_widths = Matrix<ValueType, 4, 1>();
    Matrix<ValueType, 4, 1> matrix_opacities = Matrix<ValueType, 4, 1>();

    for (size_t i = 0; i < 4; ++i)
    {
        matrix_points[3 * i] = Base::m_ctrl_pts[i].x;
        matrix_points[3 * i + 1] = Base::m_ctrl_pts[i].y;
        matrix_points[3 * i + 2] = Base::m_ctrl_pts[i].z;

        matrix_colors[3 * i] = Base::m_color[i].r;
        matrix_colors[3 * i + 1] = Base::m_color[i].g;
        matrix_colors[3 * i + 2] = Base::m_color[i].b;

        matrix_widths[i] = Base::m_width[i];
        matrix_opacities[i] = Base::m_opacity[i];
    }

    Matrix<ValueType, 4, 3> transformed_points = xfm * matrix_points;
    Matrix<ValueType, 4, 3> transformed_colors = xfm * matrix_colors;
    Matrix<ValueType, 4, 1> transformed_widths = xfm * matrix_widths;
    Matrix<ValueType, 4, 1> transformed_opacities = xfm * matrix_opacities;

    for (size_t i = 0; i < 4; ++i)
    {
        Base::m_ctrl_pts[i] = VectorType(transformed_points[3 * i], transformed_points[3 * i + 1], transformed_points[3 * i + 2]);
        Base::m_color[i] = ColorType(transformed_colors[3 * i], transformed_colors[3 * i + 1], transformed_colors[3 * i + 2]);
        Base::m_opacity[i] = transformed_opacities[i];
        Base::m_width[i] = transformed_widths[i];
    }
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

    const ValueType om0 = ValueType(0.5) * (Base::m_opacity[0] + Base::m_opacity[1]);
    const ValueType om1 = ValueType(0.5) * (Base::m_opacity[1] + Base::m_opacity[2]);
    const ValueType om2 = ValueType(0.5) * (Base::m_opacity[2] + Base::m_opacity[3]);

    const ValueType on0 = ValueType(0.5) * (om0 + om1);
    const ValueType on1 = ValueType(0.5) * (om1 + om2);
    const ValueType oq =  ValueType(0.5) * (on0 + on1);

    c1.m_opacity[0] = Base::m_opacity[0];
    c1.m_opacity[1] = om0;
    c1.m_opacity[2] = on0;
    c1.m_opacity[3] = oq;

    c2.m_opacity[0] = oq;
    c2.m_opacity[1] = on1;
    c2.m_opacity[2] = om2;
    c2.m_opacity[3] = Base::m_opacity[3];

    const ColorType cm0 = ValueType(0.5) * (Base::m_color[0] + Base::m_color[1]);
    const ColorType cm1 = ValueType(0.5) * (Base::m_color[1] + Base::m_color[2]);
    const ColorType cm2 = ValueType(0.5) * (Base::m_color[2] + Base::m_color[3]);

    const ColorType cn0 = ValueType(0.5) * (cm0 + cm1);
    const ColorType cn1 = ValueType(0.5) * (cm1 + cm2);
    const ColorType cq =  ValueType(0.5) * (cn0 + cn1);

    c1.m_color[0] = Base::m_color[0];
    c1.m_color[1] = cm0;
    c1.m_color[2] = cn0;
    c1.m_color[3] = cq;

    c2.m_color[0] = cq;
    c2.m_color[1] = cn1;
    c2.m_color[2] = cm2;
    c2.m_color[3] = Base::m_color[3];
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

        // Compute curve width.
        const ValueType width = curve.evaluate_width(w);

        // Compare Z distances.
        if (p.z <= width || p.z > t)
            return false;

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
            const VectorType tangent = curve.evaluate_tangent(w);
            const Vector<ValueType, 2> bitangent = Vector<ValueType, 2>(tangent.y, -tangent.x);
            const Vector<ValueType, 2> point(p.x, p.y);

            // Compute the vertical projection of point to the bitangent.
            const ValueType vert_proj = dot(point, bitangent);

            // Compute distance from point to curve
            const ValueType pt_curve_dist = std::sqrt(dotxy(p, p));

            // Compute u parameter
            u = vert_proj > ValueType(0.0) ?
                ValueType(0.5) + pt_curve_dist / width :
                ValueType(0.5) - pt_curve_dist / width;
        }

        return true;
    }
}

}   // namespace foundation
