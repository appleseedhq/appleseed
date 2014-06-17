
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

#ifndef APPLESEED_FOUNDATION_MATH_CURVE_H
#define APPLESEED_FOUNDATION_MATH_CURVE_H

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/bezier.h"
#include "foundation/math/matrix.h"
#include "foundation/math/ray.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>

namespace foundation
{

    // Generic function that can be used to compute the transform matrix required for ray-curve intersection.
    // Since a ray will be tested against multiple curves, it would be good to compute it once and use it everywhere else.
    // Templated over type(T), with MatrixType(M) and RayType(R)
    template<typename T>
    Matrix<T, 4, 4> ray_curve_intersection_xfm(const Ray<T, 3>& r)
    {

        typedef T ValueType;
        typedef Matrix<T, 4, 4> MatrixType;
        typedef Vector<T, 3> VectorType;

        MatrixType translate = MatrixType::translation(-r.m_org);
        MatrixType rotate;

        // We manually create the rotation matrix required for us.
        VectorType rdir = normalize(r.m_dir);
        ValueType d = std::sqrt(rdir.x * rdir.x + rdir.z * rdir.z);

        // We are actually required to do a test against 0 but we consider a small epsilon for our calculations.
        if (d >= ValueType(1e-6))
        {
            ValueType inv_d = ValueType(1.0)/d;
            rotate(0, 0) = rdir.z * inv_d;             rotate(0, 1) = 0.0f;   rotate(0, 2) = -rdir.x * inv_d;            rotate(0, 3) = ValueType(0.0);
            rotate(1, 0) = -(rdir.x * rdir.y) * inv_d; rotate(1, 1) = d;      rotate(1, 2) = -(rdir.y * rdir.z) * inv_d; rotate(1, 3) = ValueType(0.0);
            rotate(2, 0) = rdir.x;                     rotate(2, 1) = rdir.y; rotate(2, 2) = rdir.z;                     rotate(2, 3) = ValueType(0.0);
            rotate(3, 0) = ValueType(0.0);   rotate(3, 1) = ValueType(0.0);   rotate(3, 2) = ValueType(0.0);             rotate(3, 3) = ValueType(1.0);
        }
        else
        {
            // We replace the matrix by the one that rotates about x-axis by pi/2. 
            // Sign of rotation depends upon sign of 'y' component of direction vector.
            ValueType angle = rdir.y > ValueType(0.0) ? (ValueType(HalfPi)) : -(ValueType(HalfPi));
            rotate = MatrixType::rotation_x(angle);
        }
        return rotate * translate;
    }

    // Curve class
    // Includes a bezier curve of degree N that composes the curve.
    template <typename BezierType>
    class Curve
    {
      public:

        // Typdefs used throughout the class.
        typedef typename BezierType::ValueType ValueType;
        typedef Ray<ValueType, 3> RayType;
        typedef Matrix<ValueType, 4, 4> MatrixType;
        typedef AABB<ValueType, 3> AABBType;
        typedef Vector<ValueType, 3> VectorType;
        
        Curve()
        {
        }

        Curve(const BezierType& bezier)
            : m_bezier(bezier)
        {
        }

        bool intersect(const RayType& ray, const MatrixType& xfm, ValueType& t) const
        {
            ValueType vhit;
            ValueType ttmp = std::numeric_limits<ValueType>::max();
            const BezierType xfm_bezier = m_bezier.transform(xfm);
            const size_t depth = xfm_bezier.get_max_recursion_depth();   // epsilon = 1/20 of the width
            return converge(depth, xfm_bezier, xfm, 0, 1, vhit, ttmp);
        }

        AABBType get_bounds() const { return m_bezier.get_bounds(); }

        size_t get_num_ctrl_pts() const { return m_bezier.get_num_ctrl_pts(); }

        VectorType get_ctrl_pt(size_t index) const { return m_bezier.get_ctrl_pt(index); }

      private:
        bool converge(const size_t depth, 
                      const BezierType& bezier, 
                      const Matrix4f& xfm, 
                      const ValueType v0, 
                      const ValueType vn, 
                      ValueType& hit, 
                      ValueType& phit) const
        {
            const ValueType curve_width = bezier.get_max_width() * ValueType(0.5);
            
            // Bounding box has to incorporate the width of the curve segment also.
            const AABBType box = bezier.get_bounds();
            
            if (box.min.z >= phit        || box.max.z <= ValueType(1e-6) || 
                box.min.x >= curve_width || box.max.x <= -curve_width    || 
                box.min.y >= curve_width || box.max.y <= -curve_width)
                return false;
            else if (depth == 0)
            {
                // Compute the intersection.
                const size_t n = bezier.get_degree();
                const VectorType dir = bezier.get_ctrl_pt(n) - bezier.get_ctrl_pt(0);
                VectorType dp0 = bezier.get_ctrl_pt(1) - bezier.get_ctrl_pt(0);

                if (dot(dir, dp0) < 0)
                    dp0 = -dp0;

                if (dot(dp0, bezier.get_ctrl_pt(0)) > 0)
                    return false;

                VectorType dpn = bezier.get_ctrl_pt(n) - bezier.get_ctrl_pt(n-1);
                
                if (dot(dir, dpn) < 0)
                    dpn = -dpn;

                if (dot(dpn, bezier.get_ctrl_pt(n)) < 0)
                    return false;

                // Compute w on the line segment.
                ValueType w = dir.x * dir.x + dir.y * dir.y;
                if (w < ValueType(1e-6)) return false;
                w = - (bezier.get_ctrl_pt(0).x * dir.x + bezier.get_ctrl_pt(0).y * dir.y) / w;
                w = saturate(w);

                // Compute v on the line segment.
                const ValueType v = v0 * (1.0f - w) + vn * w;

                // Compute point on original unsplit curve.
                VectorType orig_p = m_bezier(v);

                // Transform back to orignal required frame.
                Vector<ValueType, 4> xfm_pt = xfm * Vector<ValueType, 4>(orig_p.x, orig_p.y, orig_p.z, ValueType(1.0));
                VectorType p(xfm_pt.x / xfm_pt.w, xfm_pt.y / xfm_pt.w, xfm_pt.z / xfm_pt.w);

                // Compute correct interpolated with on the transformed curve and not original curve.
                // NOTE: We use the modified curve and not the actual curve because, the width values are correctly split and interpolated
                //       during split operation. Inorder to have a smooth transition between the control point widths, we use the transformed curve.
                const ValueType half_width = ValueType(0.5) * bezier.get_interpolated_width(w);

                if (p.x * p.x + p.y * p.y >= half_width * half_width)
                    return false;

                if (p.z <= ValueType(1e-6) || phit < p.z)
                    return false;

                // Found an intersection.
                phit = p.z;
                hit  = v;
                return true;
            }
            else
            {
                // Split the curve and do a recursive operation again.
                const ValueType vm = (v0 + vn) * ValueType(0.5);
                BezierType b1, b2;
                bezier.split(b1, b2);
                
                ValueType v_left, v_right;
                ValueType t_left = std::numeric_limits<ValueType>::max(), t_right = std::numeric_limits<ValueType>::max();
                bool hit_left  = converge(depth-1, b1, xfm, v0, vm, v_left, t_left);
                bool hit_right = converge(depth-1, b2, xfm, vm, vn, v_right, t_right);
                if (hit_left || hit_right)
                {
                    if (t_left < t_right)
                    {
                        hit = v_left;
                        phit = t_left;
                    }
                    else
                    {
                        hit = v_right;
                        phit = t_right;
                    }
                }
                return hit_left||hit_right;
            }
        }

        BezierType m_bezier;
    };

    // Full template specializations for Bezier curve containing degree 1,2,3 curves of type float and double.
    typedef Curve<Bezier1f> Curve1f;
    typedef Curve<Bezier1d> Curve1d;

    typedef Curve<Bezier2f> Curve2f;
    typedef Curve<Bezier2d> Curve2d;

    typedef Curve<Bezier3f> Curve3f;
    typedef Curve<Bezier3d> Curve3d;
};

#endif
