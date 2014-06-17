
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

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/genericimagefilewriter.h"
#include "foundation/image/icanvas.h"
#include "foundation/image/image.h"
#include "foundation/math/bezier.h"
#include "foundation/math/curve.h"
#include "foundation/math/intersection/rayaabb.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <vector>

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Math_Bezier)
{
    // Render a bunch of curves to an image on disk.
    template <typename T>
    void render_curves_to_image(const T curves[], const size_t curve_count, const char* filename) 
    {
        // Typedefs used within the function
        typedef typename T::ValueType ValueType;
        typedef Vector<ValueType, 3> VectorType;
        typedef AABB<ValueType, 3> AABBType;
        typedef Ray<ValueType, 3> RayType;
        typedef Matrix<ValueType, 4, 4> MatrixType;
        typedef RayInfo<ValueType, 3> RayInfoType;

        const size_t ImageWidth = 500;
        const size_t ImageHeight = 500;
        Image img(ImageWidth, ImageHeight, ImageWidth, ImageHeight, 3, PixelFormatFloat);

        const ValueType XStep = ValueType(2.0) / ImageWidth;
        const ValueType YStep = ValueType(2.0) / ImageHeight;

        for (size_t c = 0; c < curve_count; ++c)
        {
            const T& curve = curves[c];
            const AABBType& curve_bounds = curve.get_bounds();

            // Create an array of overgrow bounding boxes to represent the control points.
            vector<AABBType> control_points;
            control_points.reserve(curve.get_num_ctrl_pts());
            for (size_t i = 0; i < curve.get_num_ctrl_pts(); ++i)
            {
                AABBType bbox;
                bbox.invalidate();
                bbox.insert(curve.get_ctrl_pt(i));
                bbox.grow(VectorType(ValueType(0.01)));
                control_points.push_back(bbox);
            }

            for (size_t y = 0; y < ImageHeight; ++y)
            {
                for (size_t x = 0; x < ImageWidth; ++x)
                {
                    // We assume the curve is on x-y plane with z = 0 or along the positive z axis or anywhere before z = -3.
                    const ValueType pix_x = ValueType(-1.0) + x * XStep;
                    const ValueType pix_y = ValueType(1.0) - y * YStep;

                    const RayType ray(
                        VectorType(pix_x, pix_y, ValueType(-3.0)),
                        VectorType(ValueType(0.0), ValueType(0.0), ValueType(1.0)));

                    const MatrixType xfm = ray_curve_intersection_xfm(ray);

                    Color3f color(0.0f);

                    // Draw the bounding box of the curve.
                    if (intersect(ray, RayInfoType(ray), curve_bounds))
                        color[1] = 0.5f;

                    // Draw the curve.
                    ValueType t;
                    if (curve.intersect(ray, xfm, t))
                    {
                        color[0] = 0.2f;
                        color[2] = 0.7f;
                    }

                    // Draw control points.
                    for (size_t i = 0; i < control_points.size(); ++i)
                    {
                        if (intersect(ray, RayInfoType(ray), control_points[i]))
                        {
                            color = Color3f(1.0f, 1.0f, 0.0f);
                            break;
                        }
                    }

                    img.set_pixel(x, y, color);
                }
            }
        }
        
        GenericImageFileWriter writer;
        writer.write(filename, img);    
    }

    // Test case for degree 1 curves.
    TEST_CASE(Ray_Curve_Segment_Intersection_Bezier1)
    {
        const Vector3f ControlPoints[2] = {Vector3f(-0.5f, -0.5f, 0.0f), Vector3f(0.5f, 0.5f, 0.0f)};
        const Bezier1f b1(ControlPoints, 0.06f);
        const Curve1f test_curve[1] = {Curve1f(b1)};
        
        render_curves_to_image(test_curve, 1, "unit tests/outputs/test_curve_degree1_curve.png");
    }
    
    TEST_CASE(Ray_Curve_Segment_Intersection_Bezier1_Hor)
    {
        const Vector3f ControlPoints[2] = {Vector3f(-0.5f, 0.0f, 0.0f), Vector3f(0.5f, -0.0f, 0.0f)};
        const Bezier1f b1(ControlPoints, 0.06f);
        const Curve1f test_curve[1] = {Curve1f(b1)};
        
        render_curves_to_image(test_curve, 1, "unit tests/outputs/test_curve_degree1_curve_hor.png");
    }

    TEST_CASE(Ray_Curve_Segment_Intersection_Bezier1_Vert)
    {
        const Vector3f ControlPoints[2] = {Vector3f(0.0f, 0.5f, 0.0f), Vector3f(0.0f, -0.5f, 0.0f)};
        const Bezier1f b1(ControlPoints, 0.06f);
        const Curve1f test_curve[1] = {Curve1f(b1)};
        
        render_curves_to_image(test_curve, 1, "unit tests/outputs/test_curve_degree1_curve_vert.png");
    }
    
    // Test cases for degree 2 curves.
    TEST_CASE(Ray_Curve_Segment_Intersection_Bezier2)
    {
        const Vector3f ControlPoints[3] = {Vector3f(-0.5f, 0.0f, 0.0f), Vector3f(0.0f, 0.5f, 0.0f), Vector3f(0.50f, 0.0f, 0.0f)};
        const Bezier2f b1(ControlPoints, 0.06f);
        const Curve2f test_curve[1] = {Curve2f(b1)};
        
        render_curves_to_image(test_curve, 1, "unit tests/outputs/test_curve_degree2_curve.png");
    }

    TEST_CASE(Ray_Curve_Segment_Intersection_Bezier2_Hor)
    {
        const Vector3f ControlPoints[3] = {Vector3f(-0.5f, 0.0f, 0.0f), Vector3f(0.0f, 0.0f, 0.0f), Vector3f(0.50f, 0.0f, 0.0f)};
        const Bezier2f b1(ControlPoints, 0.06f);
        const Curve2f test_curve[1] = {Curve2f(b1)};
        
        render_curves_to_image(test_curve, 1, "unit tests/outputs/test_curve_degree2_curve_hor.png");
    }

    TEST_CASE(Ray_Curve_Segment_Intersection_Bezier2_Vert)
    {
        const Vector3f ControlPoints[3] = {Vector3f(0.0f, 0.5f, 0.0f), Vector3f(0.0f, 0.0f, 0.0f), Vector3f(0.0f, -0.50f, 0.0f)};
        const Bezier2f b1(ControlPoints, 0.06f);
        const Curve2f test_curve[1] = {Curve2f(b1)};
        
        render_curves_to_image(test_curve, 1, "unit tests/outputs/test_curve_degree2_curve_vert.png");
    }
    
    // Test cases for degree 3 curves.
    TEST_CASE(Ray_Curve_Segment_Intersection_Bezier3)
    {
        const Vector3f ControlPoints[4] = {Vector3f(-0.5f, 0.0f, 0.0f), Vector3f(-0.20f, 0.20f, 0.0f), Vector3f(0.20f, -0.20f, 0.0f), Vector3f(0.5f, 0.0f, 0.0f)};
        const Bezier3f b1(ControlPoints, 0.06f);
        const Curve3f test_curve[1] = {Curve3f(b1)};
        
        render_curves_to_image(test_curve, 1, "unit tests/outputs/test_curve_degree3_curve.png");
    }

    TEST_CASE(Ray_Curve_Segment_Intersection_Bezier3_Hor)
    {
        const Vector3f ControlPoints[4] = {Vector3f(-0.5f, 0.0f, 0.0f), Vector3f(-0.25f, 0.0f, 0.0f), Vector3f(0.25f, 0.0f, 0.0f), Vector3f(0.5f, 0.0f, 0.0f)};
        const Bezier3f b1(ControlPoints, 0.06f);
        const Curve3f test_curve[1] = {Curve3f(b1)};
        
        render_curves_to_image(test_curve, 1, "unit tests/outputs/test_curve_degree3_curve_hor.png");
    }

    TEST_CASE(Ray_Curve_Segment_Intersection_Bezier3_Vert)
    {
        const Vector3f ControlPoints[4] = {Vector3f(0.0f, 0.50f, 0.0f), Vector3f(0.0f, 0.25f, 0.0f), Vector3f(0.0f, -0.25f, 0.0f), Vector3f(0.0f, -0.50f, 0.0f)};
        const Bezier3f b1(ControlPoints, 0.06f);
        const Curve3f test_curve[1] = {Curve3f(b1)};
        
        render_curves_to_image(test_curve, 1, "unit tests/outputs/test_curve_degree3_curve_vert.png");
    }

    // Test cases for variable width curves
    TEST_CASE(Ray_Curve_Segment_Intersection_Bezier1_Variable_Width)
    {
        const Vector3f ControlPoints[2] = {Vector3f(-0.5f, 0.5f, 0.0f), Vector3f(0.5f, -0.5f, 0.0f)};
        const float widths[2] = {0.06f, 0.01f};
        const Bezier1f b1(ControlPoints, widths);
        const Curve1f test_curve[1] = {Curve1f(b1)};
        
        render_curves_to_image(test_curve, 1, "unit tests/outputs/test_curve_degree1_curve_var_width.png");
    }
        
    TEST_CASE(Ray_Curve_Segment_Intersection_Bezier2_Variable_Width)
    {
        const Vector3f ControlPoints[3] = {Vector3f(-0.5f, 0.0f, 0.0f), Vector3f(0.0f, 0.5f, 0.0f), Vector3f(0.50f, 0.0f, 0.0f)};
        const float widths[3] = {0.01f, 0.08f, 0.01f};
        const Bezier2f b1(ControlPoints, widths);
        const Curve2f test_curve[1] = {Curve2f(b1)};
        
        render_curves_to_image(test_curve, 1, "unit tests/outputs/test_curve_degree2_curve_var_width.png");
    }

    TEST_CASE(Ray_Curve_Segment_Intersection_Bezier3_Variable_Width)
    {
        const Vector3f ControlPoints[4] = {Vector3f(-0.5f, 0.0f, 0.0f), Vector3f(-0.20f, 0.20f, 0.0f), Vector3f(0.20f, -0.20f, 0.0f), Vector3f(0.5f, 0.0f, 0.0f)};
        const float widths[4] = {0.03f, 0.1f, 0.06f, 0.02f};
        const Bezier3f b1(ControlPoints, widths);
        const Curve3f test_curve[1] = {Curve3f(b1)};
        
        render_curves_to_image(test_curve, 1, "unit tests/outputs/test_curve_degree3_curve_var_width.png");
    }
    
    // Test cases for multiple curve segments - to check continuity.
    TEST_CASE(Ray_Multiple_Curve_Segment_Intersection_Bezier1_Constant_Width)
    {
        const Vector3f ctrlPts1[2] = {Vector3f(-0.5f, -0.5f, 0.0f), Vector3f(0.0f, 0.0f, 0.0f)};
        const Vector3f ctrlPts2[2] = {Vector3f(0.0f, 0.0f, 0.0f), Vector3f(0.5f, 0.5f, 0.0f)};
        const Bezier1f beziers[2] = {Bezier1f(ctrlPts1, 0.06f), Bezier1f(ctrlPts2, 0.06f)};
        const Curve1f test_curves[2] = {beziers[0], beziers[1]};
        
        render_curves_to_image(test_curves, 2, "unit tests/outputs/test_curve_degree1_mc_cw.png");
    }
    
    TEST_CASE(Ray_Multiple_Curve_Segment_Intersection_Bezier1_Variable_Width)
    {
        const Vector3f ctrlPts1[2] = {Vector3f(-0.5f, -0.5f, 0.0f), Vector3f(0.0f, 0.0f, 0.0f)};
        const Vector3f ctrlPts2[2] = {Vector3f(0.0f, 0.0f, 0.0f), Vector3f(0.5f, 0.5f, 0.0f)};
        const float width1[2] = {0.06f, 0.01f};
        const float width2[2] = {0.01f, 0.06f};
        const Bezier1f beziers[2] = {Bezier1f(ctrlPts1, width1), Bezier1f(ctrlPts2, width2)};
        const Curve1f test_curves[2] = {beziers[0], beziers[1]};
        
        render_curves_to_image(test_curves, 2, "unit tests/outputs/test_curve_degree1_mc_vw.png");
    }

    TEST_CASE(Ray_Multiple_Curve_Segment_Intersection_Bezier2_Constant_Width)
    {
        const Vector3f ctrlPts1[3] = {Vector3f(-0.7f, 0.0f, 0.0f), Vector3f(-0.40f, 0.5f, 0.0f), Vector3f(0.00f, 0.0f, 0.0f)};
        const Vector3f ctrlPts2[3] = {Vector3f(0.0f, 0.0f, 0.0f), Vector3f(0.40f, -0.5f, 0.0f), Vector3f(0.70f, 0.0f, 0.0f)};
        const Bezier2f b1(ctrlPts1, 0.06f);
        const Bezier2f b2(ctrlPts2, 0.06f);
        const Curve2f test_curves[2] = {Curve2f(b1), Curve2f(b2)};
        
        render_curves_to_image(test_curves, 2, "unit tests/outputs/test_curve_degree2_mc_cw.png");
    }
    
    TEST_CASE(Ray_Multiple_Curve_Segment_Intersection_Bezier2_Variable_Width)
    {
        const Vector3f ctrlPts1[3] = {Vector3f(-0.7f, 0.0f, 0.0f), Vector3f(-0.40f, 0.5f, 0.0f), Vector3f(0.00f, 0.0f, 0.0f)};
        const Vector3f ctrlPts2[3] = {Vector3f(0.0f, 0.0f, 0.0f), Vector3f(0.40f, -0.5f, 0.0f), Vector3f(0.70f, 0.0f, 0.0f)};
        const float widths[3] = {0.01f, 0.08f, 0.01f};
        const Bezier2f b1(ctrlPts1, widths);
        const Bezier2f b2(ctrlPts2, widths);
        const Curve2f test_curves[2] = {Curve2f(b1), Curve2f(b2)};
        
        render_curves_to_image(test_curves, 2, "unit tests/outputs/test_curve_degree2_mc_vw.png");
    }

    TEST_CASE(Ray_Multiple_Curve_Segment_Intersection_Bezier3_Constant_Width)
    {
        const Vector3f ctrlPts1[4] = {Vector3f(-0.7f, 0.0f, 0.0f), Vector3f(-0.20f, 0.50f, 0.0f), Vector3f(-0.50f, -0.50f, 0.0f), Vector3f(0.0f, 0.0f, 0.0f)};
        const Vector3f ctrlPts2[4] = {Vector3f(-0.0f, 0.0f, 0.0f), Vector3f(0.4f, 0.30f, 0.0f), Vector3f(0.40f, -0.30f, 0.0f), Vector3f(0.6f, 0.0f, 0.0f)};
        const Bezier3f b1(ctrlPts1, 0.06f);
        const Bezier3f b2(ctrlPts2, 0.06f);
        const Curve3f test_curves[2] = {Curve3f(b1), Curve3f(b2)};
        
        render_curves_to_image(test_curves, 2, "unit tests/outputs/test_curve_degree3_mc_cw.png");
    }
    
    TEST_CASE(Ray_Multiple_Curve_Segment_Intersection_Bezier3_Variable_Width)
    {
        const Vector3f ctrlPts1[4] = {Vector3f(-0.7f, 0.0f, 0.0f), Vector3f(-0.20f, 0.50f, 0.0f), Vector3f(-0.50f, -0.50f, 0.0f), Vector3f(0.0f, 0.0f, 0.0f)};
        const Vector3f ctrlPts2[4] = {Vector3f(-0.0f, 0.0f, 0.0f), Vector3f(0.4f, 0.30f, 0.0f), Vector3f(0.40f, -0.30f, 0.0f), Vector3f(0.6f, 0.0f, 0.0f)};
        const float widths[4] = {0.03f, 0.1f, 0.06f, 0.03f};
        const Bezier3f b1(ctrlPts1, widths);
        const Bezier3f b2(ctrlPts2, widths);
        const Curve3f test_curves[2] = {Curve3f(b1), Curve3f(b2)};

        render_curves_to_image(test_curves, 2, "unit tests/outputs/test_curve_degree3_mc_vw.png");
    }

    TEST_CASE(Ray_Curve_Segment_Intersection_Bezier2_Constant_Width_Small_Curve)
    {
        const Vector3f ControlPoints[4] = {Vector3f(-0.5f, 0.0f, 0.0f), Vector3f(-0.4f, 0.0f, 0.0f), Vector3f(-0.3f, 0.0f, 0.0f), Vector3f(-0.2f, 0.0f, 0.0f)};
        const Bezier3f b1(ControlPoints, 0.06f);
        const Curve3f test_curve[1] = {Curve3f(b1)};

        render_curves_to_image(test_curve, 1, "unit tests/outputs/test_curve_degree3_curve_hor_sc.png");
    }
}
