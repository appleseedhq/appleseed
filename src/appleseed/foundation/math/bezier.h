
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Srinath Ravichandran, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_MATH_BEZIER_H
#define APPLESEED_FOUNDATION_MATH_BEZIER_H

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/matrix.h"
#include "foundation/math/minmax.h"
#include "foundation/math/ray.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstddef>

namespace foundation
{

//
// Free functions to evaluate Bezier interpolation polynomials.
// T: type of the interpolation parameter
// V: type of the interpolated values
//

template <typename T, typename V>
inline V interpolate_bezier1(const V x, const V y, const T t)
{
    return (T(1.0) - t) * x + t * y;
}

template <typename T, typename V>
inline V interpolate_bezier2(const V x, const V y, const V z, const T t)
{
    // Formula: (1-t)^2 * P0 + 2 * (1-t)*t*P1 + t^2 * P2.

    const T u = T(1.0) - t;
    const T a = u * u;              // (1-t)^2
    const T b = u * t;              // (1-t) * t
    const T c = t * t;              // t^2
    
    return a * x + 2 * b * y + c * z;
}

template <typename T, typename V>
inline V interpolate_bezier3(const V x, const V y, const V z, const V w, const T t)
{
    // Formula: (1-t)^3 * P0 + 3 * (1-t)^2 * t * P1 + 3 * (1-t) * t^2 * P2 + t^3 * P3.

    const T u = T(1.0) - t;
    const T a = u * u * u;          // (1-t)^3
    const T b = u * u * t;          // (1-t)^2 * t
    const T c = u * t * t;          // (1-t) * t^2
    const T d = t * t * t;          // t^3

    return a * x + 3 * b * y + 3 * c * z + d * w;
}


//
// Base class for Bezier curves.
// N is the degree of the curve.
// A curve of degree N has N+1 control points.
//

template <typename T, size_t N>
class BezierBase
{
  public:
    // Types.
    typedef T ValueType;
    typedef Vector<T, 3> VectorType;
    typedef AABB<T, 3> AABBType;
    typedef Matrix<T, 4, 4> MatrixType;

    // Degree of the Bezier curve.
    static const size_t Degree = N;

    BezierBase()
    {
    }

    BezierBase(const VectorType ctrl_pts[N + 1], const ValueType width)
    {
        assert(width >= ValueType(0.0));

        for (size_t i = 0; i < N + 1; ++i)
        {
            m_ctrl_pts[i] = ctrl_pts[i];
            m_width[i] = width;
        }
        
        compute_max_width();
        compute_bounds();
        compute_max_recursion_depth();
    }

    BezierBase(const VectorType ctrl_pts[N + 1], const ValueType width[N + 1])
    {   
        for (size_t i = 0; i < N + 1; ++i)
        {
            assert(width[i] >= ValueType(0.0));
            m_ctrl_pts[i] = ctrl_pts[i];
            m_width[i] = width[i];
        }

        compute_max_width();
        compute_bounds();
        compute_max_recursion_depth();
    }

    size_t get_num_ctrl_pts() const
    {
        return N + 1;
    }

    VectorType get_ctrl_pt(const size_t index) const
    {
        assert(index < N + 1);
        return m_ctrl_pts[index];
    }

    ValueType get_width(const size_t index) const
    {
        assert(index < N + 1);
        return m_width[index];
    }

    ValueType get_max_width() const
    {
        return m_max_width;
    }

    AABBType get_bounds() const
    {
        return m_bounds;
    }

    size_t get_max_recursion_depth() const
    {
        return m_max_recursion_depth;
    }

  protected:
    void compute_max_width() 
    {
        m_max_width = m_width[0];
        for (size_t i = 1; i < N + 1; ++i)
            m_max_width = std::max(m_max_width, m_width[i]);
    }

    void compute_bounds()
    {
        m_bounds.invalidate();
        for (size_t i = 0; i < N + 1; ++i)
            m_bounds.insert(m_ctrl_pts[i]);

        m_bounds.grow(VectorType(m_max_width * ValueType(0.5)));
        m_bounds.robust_grow(ValueType(1e-4));
    }

    void compute_max_recursion_depth()
    {
        if (N == 1)
            m_max_recursion_depth = 0;
        else
        {
            ValueType L0 = ValueType(-1.0);

            for (size_t i = 0; i <= N - 2; ++i)
            {
                L0 =
                    max(
                        L0,
                        std::abs(m_ctrl_pts[i].x - 2 * m_ctrl_pts[i+1].x + m_ctrl_pts[i+2].x),
                        std::abs(m_ctrl_pts[i].y - 2 * m_ctrl_pts[i+1].y + m_ctrl_pts[i+2].y));
            }

            // Handle the boundary test cases for L < 1.0f where log(x) returns a negative value.
            const ValueType sqrt2 = ValueType(1.414);
            const ValueType epsilon = m_max_width * ValueType(0.05);    // 1/20 of max_width
            const ValueType value = (sqrt2 * N * (N - 1) * L0) / (ValueType(8.0) * epsilon);
            const ValueType log_value = log(value, ValueType(4.0));
            const ValueType clamp_value = clamp(log_value, ValueType(0.0), ValueType(5.0));
            m_max_recursion_depth = truncate<size_t>(clamp_value);
        }
    }

    static VectorType transform_point(const MatrixType& xfm, const VectorType& p)
    {
        const Vector<T, 4> pt(p.x, p.y, p.z, ValueType(1.0));
        const Vector<T, 4> xpt = xfm * pt;
        return VectorType(xpt.x / xpt.w, xpt.y / xpt.w, xpt.z / xpt.w);
    }

    VectorType  m_ctrl_pts[N + 1];
    ValueType   m_width[N + 1];
    ValueType   m_max_width;
    AABBType    m_bounds;
    size_t      m_max_recursion_depth;
};


//
// Degree 1 Bezier curve (a straight line).
//

template <typename T>
class Bezier1
  : public BezierBase<T, 1>
{
  public:
    // Types.
    typedef BezierBase<T, 1> Base;
    typedef typename Base::ValueType ValueType;
    typedef typename Base::VectorType VectorType;
    typedef typename Base::AABBType AABBType;
    typedef typename Base::MatrixType MatrixType;

    Bezier1()
    {
    }

    Bezier1(const VectorType ctrl_pts[2], const ValueType width)
      : Base(ctrl_pts, width)
    {
    }

    Bezier1(const VectorType ctrl_pts[2], const ValueType width[2])
      : Base(ctrl_pts, width)
    {
    }

    Bezier1 transform(const MatrixType& xfm) const
    {
        const VectorType ctrl_pts[] =
        {
            Base::transform_point(xfm, Base::m_ctrl_pts[0]),
            Base::transform_point(xfm, Base::m_ctrl_pts[1])
        };

        return Bezier1(ctrl_pts, Base::m_width);
    }

    VectorType operator()(const ValueType t) const
    {
        return interpolate_bezier1(Base::m_ctrl_pts[0], Base::m_ctrl_pts[1], t);
    }

    ValueType get_interpolated_width(const ValueType t) const
    {
        return interpolate_bezier1(Base::m_width[0], Base::m_width[1], t);
    }

    void split(Bezier1& c1, Bezier1& c2) const
    {
        const VectorType midpt = (*this)(ValueType(0.5));
        const ValueType midw = (Base::m_width[0] + Base::m_width[1]) * ValueType(0.5);

        const VectorType lc[] = { Base::m_ctrl_pts[0], midpt };
        const VectorType rc[] = { midpt, Base::m_ctrl_pts[1] };

        const ValueType lw[] = { Base::m_width[0], midw };
        const ValueType rw[] = { midw, Base::m_width[1] };

        c1 = Bezier1(lc, lw);
        c2 = Bezier1(rc, rw);
    }
};


//
// Degree 2 Bezier curve.
//

template <typename T>
class Bezier2
  : public BezierBase<T, 2>
{
  public:
    // Types.
    typedef BezierBase<T, 2> Base;
    typedef typename Base::ValueType ValueType;
    typedef typename Base::VectorType VectorType;
    typedef typename Base::AABBType AABBType;
    typedef typename Base::MatrixType MatrixType;

    Bezier2()
    {
    }

    Bezier2(const VectorType ctrl_pts[3], const ValueType width)
      : Base(ctrl_pts, width)
    {
    }

    Bezier2(const VectorType ctrl_pts[3], const ValueType width[3])
      : Base(ctrl_pts, width)
    {
    }

    Bezier2 transform(const MatrixType& xfm) const
    {
        const VectorType ctrl_pts[] =
        {
            Base::transform_point(xfm, Base::m_ctrl_pts[0]),
            Base::transform_point(xfm, Base::m_ctrl_pts[1]),
            Base::transform_point(xfm, Base::m_ctrl_pts[2])
        };

        return Bezier2(ctrl_pts, Base::m_width);
    }

    VectorType operator()(const ValueType t) const
    {
        return
            interpolate_bezier2(
                Base::m_ctrl_pts[0],
                Base::m_ctrl_pts[1],
                Base::m_ctrl_pts[2],
                t);
    }

    ValueType get_interpolated_width(const ValueType t) const
    {
        return
            interpolate_bezier2(
                Base::m_width[0],
                Base::m_width[1],
                Base::m_width[2],
                t);
    }

    void split(Bezier2& c1, Bezier2& c2) const
    {
        const VectorType midpt = (*this)(ValueType(0.5));

        const VectorType lc[] =
        {
            Base::m_ctrl_pts[0],
            (Base::m_ctrl_pts[0] + Base::m_ctrl_pts[1]) * ValueType(0.5),
            midpt
        };

        const VectorType rc[] =
        {
            midpt,
            (Base::m_ctrl_pts[1] + Base::m_ctrl_pts[2]) * ValueType(0.5),
            Base::m_ctrl_pts[2]
        };

        const ValueType first_mid_width[] =
        {
            (Base::m_width[0] + Base::m_width[1]) * ValueType(0.5),
            (Base::m_width[1] + Base::m_width[2]) * ValueType(0.5)
        };

        const ValueType midw = (first_mid_width[0] + first_mid_width[1]) * ValueType(0.5);
        
        const ValueType lw[] =
        {
            Base::m_width[0],
            first_mid_width[0],
            midw
        };

        const ValueType rw[] =
        {
            midw,
            first_mid_width[1],
            Base::m_width[2]
        };

        c1 = Bezier2(lc, lw);
        c2 = Bezier2(rc, rw);
    }
};


//
// Degree 3 Bezier curve.
//

template <typename T>
class Bezier3
  : public BezierBase<T, 3>
{
  public:
    // Types.
    typedef BezierBase<T, 3> Base;
    typedef typename Base::ValueType ValueType;
    typedef typename Base::VectorType VectorType;
    typedef typename Base::AABBType AABBType;
    typedef typename Base::MatrixType MatrixType;

    Bezier3()
    {
    }

    Bezier3(const VectorType ctrl_pts[4], const ValueType width)
      : Base(ctrl_pts, width)
    {
    }

    Bezier3(const VectorType ctrl_pts[4], const ValueType width[4])
      : Base(ctrl_pts, width)
    {
    }

    Bezier3 transform(const MatrixType& xfm) const
    {
        const VectorType ctrl_pts[] =
        {
            Base::transform_point(xfm, Base::m_ctrl_pts[0]),
            Base::transform_point(xfm, Base::m_ctrl_pts[1]),
            Base::transform_point(xfm, Base::m_ctrl_pts[2]),
            Base::transform_point(xfm, Base::m_ctrl_pts[3])
        };

        return Bezier3(ctrl_pts, Base::m_width);
    }

    VectorType operator()(const ValueType t) const
    {
        return
            interpolate_bezier3(
                Base::m_ctrl_pts[0],
                Base::m_ctrl_pts[1],
                Base::m_ctrl_pts[2],
                Base::m_ctrl_pts[3],
                t);
    }

    ValueType get_interpolated_width(const ValueType t) const
    {
        return
            interpolate_bezier3(
                Base::m_width[0],
                Base::m_width[1],
                Base::m_width[2],
                Base::m_width[3],
                t);
    }

    void split(Bezier3& c1, Bezier3& c2) const
    {
        const VectorType midpt = (*this)(ValueType(0.5));

        const VectorType first_mid_pts[] =
        {
            (Base::m_ctrl_pts[0] + Base::m_ctrl_pts[1]) * ValueType(0.5),
            (Base::m_ctrl_pts[1] + Base::m_ctrl_pts[2]) * ValueType(0.5),
            (Base::m_ctrl_pts[2] + Base::m_ctrl_pts[3]) * ValueType(0.5)
        };

        const VectorType lc[] =
        {
            Base::m_ctrl_pts[0],
            first_mid_pts[0],
            (first_mid_pts[0] + first_mid_pts[1]) * ValueType(0.5),
            midpt
        };

        const VectorType rc[] =
        {
            midpt,
            (first_mid_pts[1] + first_mid_pts[2]) * ValueType(0.5),
            first_mid_pts[2],
            Base::m_ctrl_pts[3]
        };

        const ValueType first_mid_width[] =
        {
            (Base::m_width[0] + Base::m_width[1]) * ValueType(0.5),
            (Base::m_width[1] + Base::m_width[2]) * ValueType(0.5),
            (Base::m_width[2] + Base::m_width[3]) * ValueType(0.5)
        };

        const ValueType second_mid_width[] =
        {
            (first_mid_width[0] + first_mid_width[1]) * ValueType(0.5),
            (first_mid_width[1] + first_mid_width[2]) * ValueType(0.5)
        };

        const ValueType third_mid_width = (second_mid_width[0] + second_mid_width[1]) * ValueType(0.5);

        const ValueType lw[] =
        {
            Base::m_width[0],
            first_mid_width[0],
            second_mid_width[0],
            third_mid_width
        };

        const ValueType rw[] =
        {
            third_mid_width,
            second_mid_width[1],
            first_mid_width[2],
            Base::m_width[3]
        };
        
        c1 = Bezier3(lc, lw);
        c2 = Bezier3(rc, rw);
    }
};


//
// Full specializations for degree 1, 2, 3 Bezier curves of type float and double.
//

typedef Bezier1<float>  Bezier1f;
typedef Bezier1<double> Bezier1d;
typedef Bezier2<float>  Bezier2f;
typedef Bezier2<double> Bezier2d;
typedef Bezier3<float>  Bezier3f;
typedef Bezier3<double> Bezier3d;

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_BEZIER_H
