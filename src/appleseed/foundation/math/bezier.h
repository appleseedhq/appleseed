
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
#include "foundation/math/ray.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <algorithm>
#include <cstddef>

namespace foundation
{
    // Templated free functions for computing interpolated values in Bezier curves.
    // T-> type of arg passed to the function.
    // V-> values that are used to interpolate
    template<typename T, typename V>
    inline V interpolate_bezier1(const V x, const V y, const T t)
    {
        return (T(1.0) - t) * x + t * y;
    }

    template<typename T, typename V>
    inline V interpolate_bezier2(const V x, const V y, const V z, const T t)
    {
        const T u = T(1.0) - t;
        const T a = u * u;               // (1-t)^2
        const T b = u * t;               // (1-t) * t
        const T c = t * t;               // t^2
        
        // Formula : (1-t)^2 * P0 + 2 * (1-t)*t*P1 + t^2 * P2.
        return a * x + 2 * b * y + c * z;
    }

    template<typename T, typename V>
    inline V interpolate_bezier3(const V x, const V y, const V z, const V w, const T t)
    {
        const T u = T(1.0) - t;
        const T a = u * u * u;              // (1-t)^3
        const T b = u * u * t;              // (1-t)^2 * t
        const T c = u * t * t;              // (1-t) * t^2
        const T d = t * t * t;              // t^3

        // Formula : (1-t)^3 * P0 + 3 * (1-t)^2 * t * P1 + 3 * (1-t) * t^2 * P2 + t^3 * P3.
        return a * x + 3 * b * y + 3 * c * z + d * w;
    }
    
    // Degree is N.
    template <typename T, size_t N>
    class BezierBase
    {
      public:
        // Value, vector and AABB types.
        typedef T ValueType;
        typedef Vector<T, 3> VectorType;
        typedef AABB<T, 3> AABBType;

        BezierBase()
        {
        }

        BezierBase(const VectorType ctrl_pts[N + 1], const ValueType width)
        {
            assert(width >= ValueType(0.0));
            for (size_t i = 0; i < N + 1; i++)
            {
                m_ctrl_pts[i] = ctrl_pts[i];
                m_width[i] = width;
            }            
            
            // Pre-compute all values.
            compute_max_width();
            compute_max_recursion_depth();
            compute_bounds();            
        }

        BezierBase(const VectorType ctrl_pts[N + 1], const ValueType width[N + 1])
        {   
            for (size_t i = 0; i < N + 1; i++)
            {
                assert(width[i] >= ValueType(0.0));
                m_ctrl_pts[i] = ctrl_pts[i];
                m_width[i]    = width[i];
            }

            // Pre-compute all values.
            compute_max_width();
            compute_max_recursion_depth();
            compute_bounds();            
        }

        size_t get_num_ctrl_pts() const { return (N + 1); }

        size_t get_degree() const { return N; }

        AABBType get_bounds() const { return m_bounds; }
    
        VectorType get_ctrl_pt(const size_t index) const
        {
            assert(index < (N + 1));
            return m_ctrl_pts[index];
        }
          
        size_t get_max_recursion_depth() const { return m_max_recursion_depth; }

        ValueType get_max_width() const { return m_max_width; }

        ValueType get_width(const size_t index) const
        {
            assert(index < N + 1);
            return m_width[index];
        }     

      protected:
        void compute_bounds()
        {
            m_bounds.invalidate();
            for (size_t i = 0; i < N + 1; i++)
                m_bounds.insert(m_ctrl_pts[i]);

            // We grow the bounds by half width as well as by a small epsilon.
            m_bounds.grow(VectorType(m_max_width * ValueType(0.5)));
            m_bounds.robust_grow(ValueType(1e-4));
        }

        void compute_max_width() 
        {
            m_max_width = m_width[0];
            for (size_t i = 1; i < N + 1; i++)
                m_max_width = std::max(m_max_width, m_width[i]);
        }

        // Compute a cached value of the recursion depth.
        void compute_max_recursion_depth()
        {
            if (N == 1)
                m_max_recursion_depth = 0;
            else
            {
                ValueType epsilon = m_max_width * ValueType(0.05);            // 1/20 of max_width is the epsilon
                ValueType L0 = ValueType(-1.0);
                // Range is [0 -- N-2].
                // N = degree.
                for (size_t i = 0; i <= N - 2; i++)
                {
                    const ValueType temp = std::max(abs(m_ctrl_pts[i].x - 2 * m_ctrl_pts[i+1].x + m_ctrl_pts[i+2].x),
                                                    abs(m_ctrl_pts[i].y - 2 * m_ctrl_pts[i+1].y + m_ctrl_pts[i+2].y));
                    L0 = std::max(temp, L0);
                }

                // Handle the boundary test cases for L < 1.0f where log(x) returns a negative value.
                const ValueType sqrt2 = ValueType(1.414);
                const ValueType value = (sqrt2 * N * (N - 1) * L0) / (ValueType(8.0) * epsilon);
                const ValueType log_value = log(value, ValueType(4.0));
                const ValueType clamp_value = clamp(log_value, ValueType(0.0), ValueType(5.0));
                m_max_recursion_depth = truncate<size_t>(clamp_value);
            }
        }
         
        VectorType m_ctrl_pts[N + 1];
        AABBType   m_bounds;
        ValueType  m_width[N + 1];
        ValueType  m_max_width;
        size_t     m_max_recursion_depth;
    };

    // Basically a single straight line.
    // Degree 1 curve.
    template<typename T>
    class Bezier1 : public BezierBase<T, 1>
    {
      public:
        
        // Value, vector, aabb and matrix types.
        typedef T ValueType;
        typedef Vector<T, 3> VectorType;
        typedef AABB<T, 3> AABBType;
        typedef Matrix<T, 4, 4> MatrixType;

        Bezier1()
        {
        }

        Bezier1(const VectorType ctrl_pts[2], const ValueType width)
          : BezierBase(ctrl_pts, width)
        {
        }

        Bezier1(const VectorType ctrl_pts[2], const ValueType width[2])
          : BezierBase(ctrl_pts, width)
        {
        }

        Bezier1 transform(const MatrixType& xfm) const
        {
            VectorType ctrl_pts[2];
            for (size_t i = 0; i < 2; i++)
            {
                const Vector<T, 4> pt(m_ctrl_pts[i].x, m_ctrl_pts[i].y, m_ctrl_pts[i].z, ValueType(1.0));
                const Vector<T, 4> xpt = xfm * pt;
                ctrl_pts[i] = VectorType(xpt.x / xpt.w, xpt.y / xpt.w, xpt.z / xpt.w);
            }
            return Bezier1(ctrl_pts, m_width);
        }

        // Formula : (1 - t) * P0 + t * P1.
        VectorType operator() (const ValueType t) const
        {
            return interpolate_bezier1(m_ctrl_pts[0], m_ctrl_pts[1], t);
        }

        ValueType get_interpolated_width(const ValueType t) const
        {
            return interpolate_bezier1(m_width[0], m_width[1], t);
        }
        
        void split(Bezier1& c1, Bezier1& c2) const
        {
            const VectorType midpt = (*this)(ValueType(0.5));

            const VectorType lc[2] = {m_ctrl_pts[0], midpt};
            const VectorType rc[2] = {midpt, m_ctrl_pts[1]};

            const ValueType mw = (m_width[0] + m_width[1]) * ValueType(0.5);
            const ValueType lw[2] = {m_width[0], mw};
            const ValueType rw[2] = {mw, m_width[1]};

            c1 = Bezier1(lc, lw);
            c2 = Bezier1(rc, rw);
        }
    };

    // Degree 2 curve.
    template<typename T>
    class Bezier2 : public BezierBase<T, 2>
    {
      public:

        // Value, vector, aabb and matrix types.
        typedef T ValueType;
        typedef Vector<T, 3> VectorType;
        typedef AABB<T, 3> AABBType;
        typedef Matrix<T, 4, 4> MatrixType;

        Bezier2()
        {
        }

        Bezier2(const VectorType ctrl_pts[3], const ValueType width)
          : BezierBase(ctrl_pts, width)
        {
        }

        Bezier2(const VectorType ctrl_pts[3], const ValueType width[3])
          : BezierBase(ctrl_pts, width)
        {
        }

        Bezier2 transform(const MatrixType& xfm) const
        {
            VectorType ctrl_pts[3];
            for (size_t i = 0; i < 3; i++)
            {
                const Vector<T, 4> pt(m_ctrl_pts[i].x, m_ctrl_pts[i].y, m_ctrl_pts[i].z, ValueType(1.0));
                const Vector<T, 4> xpt = xfm * pt;
                ctrl_pts[i] = VectorType(xpt.x / xpt.w, xpt.y / xpt.w, xpt.z / xpt.w);
            }
            return Bezier2(ctrl_pts, m_width);
        }

        VectorType operator() (const ValueType t) const
        {
            return interpolate_bezier2(m_ctrl_pts[0], m_ctrl_pts[1], m_ctrl_pts[2], t);
        }

        ValueType get_interpolated_width(const ValueType t) const
        {
            return interpolate_bezier2(m_width[0], m_width[1], m_width[2], t);
        }

        void split(Bezier2& c1, Bezier2& c2) const
        {
            VectorType first_mid_pts[2];
            VectorType second_mid_pts[1];
            const VectorType mid_pt = (*this)(ValueType(0.5));

            first_mid_pts[0] = (m_ctrl_pts[0] + m_ctrl_pts[1]) * ValueType(0.5);
            first_mid_pts[1] = (m_ctrl_pts[1] + m_ctrl_pts[2]) * ValueType(0.5);
            
            const VectorType lc[3] = {m_ctrl_pts[0], first_mid_pts[0], mid_pt};
            const VectorType rc[3] = {mid_pt, first_mid_pts[1], m_ctrl_pts[2]};

            // Split the weight components
            ValueType first_mid_width[2];
            ValueType second_mid_width[1];

            first_mid_width[0] = (m_width[0] + m_width[1]) * ValueType(0.5);
            first_mid_width[1] = (m_width[1] + m_width[2]) * ValueType(0.5);
            second_mid_width[0] = (first_mid_width[0] + first_mid_width[1]) * ValueType(0.5);
            
            const ValueType lw[3] = {m_width[0], first_mid_width[0], second_mid_width[0]};
            const ValueType rw[3] = {second_mid_width[0], first_mid_width[1], m_width[2]};

            c1 = Bezier2(lc, lw);
            c2 = Bezier2(rc, rw);
        }
    };

    // Degree 3 curve.
    template<typename T>
    class Bezier3 : public BezierBase<T, 3>
    {
      public:

        // Value, vector, aabb and matrix types.
        typedef T ValueType;
        typedef Vector<T, 3> VectorType;
        typedef AABB<T, 3> AABBType;
        typedef Matrix<T, 4, 4> MatrixType;

        Bezier3()
        {
        }

        Bezier3(const VectorType ctrl_pts[4], const ValueType width)
          : BezierBase(ctrl_pts, width)
        {
        }

        Bezier3(const VectorType ctrl_pts[4], const ValueType width[4])
          : BezierBase(ctrl_pts, width)
        {
        }

        Bezier3 transform(const MatrixType& xfm) const
        {
            VectorType ctrl_pts[4];
            for (size_t i = 0; i < 4; i++)
            {
                const Vector<T, 4> pt(m_ctrl_pts[i].x, m_ctrl_pts[i].y, m_ctrl_pts[i].z, ValueType(1.0));
                const Vector<T, 4> xpt = xfm * pt;
                ctrl_pts[i] = VectorType(xpt.x / xpt.w, xpt.y / xpt.w, xpt.z / xpt.w);
            }
            return Bezier3(ctrl_pts, m_width);
        }

        VectorType operator() (const ValueType t) const
        {
            return interpolate_bezier3(m_ctrl_pts[0], m_ctrl_pts[1], m_ctrl_pts[2], m_ctrl_pts[3], t);
        }

        ValueType get_interpolated_width(const ValueType t) const
        {
            return interpolate_bezier3(m_width[0], m_width[1], m_width[2], m_width[3], t);
        }

        void split(Bezier3& c1, Bezier3& c2) const
        {
            VectorType first_mid_pts[3];
            VectorType second_mid_pts[2];
            const VectorType mid_pt = (*this)((ValueType)0.5);

            first_mid_pts[0] = (m_ctrl_pts[0] + m_ctrl_pts[1]) * ValueType(0.5);
            first_mid_pts[1] = (m_ctrl_pts[1] + m_ctrl_pts[2]) * ValueType(0.5);
            first_mid_pts[2] = (m_ctrl_pts[2] + m_ctrl_pts[3]) * ValueType(0.5);

            second_mid_pts[0] = (first_mid_pts[0] + first_mid_pts[1]) * ValueType(0.5);
            second_mid_pts[1] = (first_mid_pts[1] + first_mid_pts[2]) * ValueType(0.5);

            const VectorType lc[4] = {m_ctrl_pts[0], first_mid_pts[0], second_mid_pts[0], mid_pt};
            const VectorType rc[4] = {mid_pt, second_mid_pts[1], first_mid_pts[2], m_ctrl_pts[3]};

            // Split the weight components.
            ValueType first_mid_width[3];
            ValueType second_mid_width[2];
            ValueType third_mid_width[1];

            first_mid_width[0] = (m_width[0] + m_width[1]) * ValueType(0.5);
            first_mid_width[1] = (m_width[1] + m_width[2]) * ValueType(0.5);
            first_mid_width[2] = (m_width[2] + m_width[3]) * ValueType(0.5);

            second_mid_width[0] = (first_mid_width[0] + first_mid_width[1]) * ValueType(0.5);
            second_mid_width[1] = (first_mid_width[1] + first_mid_width[2]) * ValueType(0.5);
             
            third_mid_width[0] = (second_mid_width[0] + second_mid_width[1]) * ValueType(0.5);

            const ValueType lw[4] = {m_width[0], first_mid_width[0], second_mid_width[0], third_mid_width[0]};
            const ValueType rw[4] = {third_mid_width[0], second_mid_width[1], first_mid_width[2], m_width[3]};
            
            c1 = Bezier3(lc, lw);
            c2 = Bezier3(rc, rw);
        }
    };

    // Full specializations for degree 1,2,3 beziers of type float and double.
    typedef Bezier1<float>  Bezier1f;
    typedef Bezier1<double> Bezier1d;

    typedef Bezier2<float>  Bezier2f;
    typedef Bezier2<double> Bezier2d;

    typedef Bezier3<float>  Bezier3f;
    typedef Bezier3<double> Bezier3d;
};

#endif
