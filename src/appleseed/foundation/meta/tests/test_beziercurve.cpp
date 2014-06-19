
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
#include "foundation/image/image.h"
#include "foundation/image/pixel.h"
#include "foundation/math/intersection/rayaabb.h"
#include "foundation/math/beziercurve.h"
#include "foundation/math/matrix.h"
#include "foundation/math/ray.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/countof.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Math_BezierCurve)
{
#pragma warning (push)
#pragma warning (disable : 4723)    // potential divide by 0

    // Render a bunch of Bezier curves to an image on disk.
    // The control points of the curves are expressed in [-1,1]^2
    // where (-1,-1) is at the bottom-left corner of the image.
    template <typename BezierCurveType>
    void render_curves_to_image(
        const BezierCurveType   curves[],
        const size_t            curve_count,
        const char*             filename) 
    {
        typedef typename BezierCurveType::ValueType ValueType;
        typedef typename BezierCurveType::VectorType VectorType;
        typedef typename BezierCurveType::MatrixType MatrixType;
        typedef Ray<ValueType, 3> RayType;
        typedef RayInfo<ValueType, 3> RayInfoType;
        typedef BezierCurveIntersector<BezierCurveType> BezierCurveIntersectorType;

        const size_t ImageWidth = 500;
        const size_t ImageHeight = 500;

        const ValueType RcpImageWidth = ValueType(1.0) / ImageWidth;
        const ValueType RcpImageHeight = ValueType(1.0) / ImageHeight;

        Image image(ImageWidth, ImageHeight, ImageWidth, ImageHeight, 3, PixelFormatFloat);

        for (size_t y = 0; y < ImageHeight; ++y)
        {
            for (size_t x = 0; x < ImageWidth; ++x)
            {
                Color3f color(0.0f);

                // Compute the coordinates of the center of the pixel.
                const ValueType pix_x = (ValueType(2.0) * x + ValueType(1.0)) * RcpImageWidth - ValueType(1.0);
                const ValueType pix_y = ValueType(1.0) - (ValueType(2.0) * y + ValueType(1.0)) * RcpImageHeight;

                // Build a ray. We assume the curve is on the x-y plane with z = 0.
                const RayType ray(
                    VectorType(pix_x, pix_y, ValueType(-3.0)),
                    VectorType(ValueType(0.0), ValueType(0.0), ValueType(1.0)));
                const RayInfoType ray_info(ray);

                for (size_t c = 0; c < curve_count; ++c)
                {
                    const BezierCurveType& curve = curves[c];

                    // Draw the bounding box of the curve.
                    if (intersect(ray, ray_info, curve.get_bounds()))
                        color[1] = 0.5f;

                    // Draw the curve.
                    const MatrixType curve_transform =
                        BezierCurveIntersectorType::compute_curve_transform(ray);
                    BezierCurveIntersectorType intersector;
                    ValueType t;
                    if (intersector.intersect(curve, ray, curve_transform, t))
                    {
                        color[0] = 0.2f;
                        color[2] = 0.7f;
                    }

                    // Draw control points.
                    const size_t control_point_count = curve.get_control_point_count();
                    for (size_t i = 0; i < control_point_count; ++i)
                    {
                        const VectorType& cp = curve.get_control_point(i);
                        const ValueType dx = pix_x - cp.x;
                        const ValueType dy = pix_y - cp.y;
                        if (square(dx) + square(dy) < square(ValueType(0.02)))
                        {
                            color = Color3f(1.0f, 1.0f, 0.0f);
                            break;
                        }
                    }
                }

                image.set_pixel(x, y, color);
            }
        }

        GenericImageFileWriter writer;
        writer.write(filename, image);
    }

#pragma warning (pop)


    //
    // Degree 1 Bezier curves.
    //

    TEST_CASE(Ray_SingleBezierCurve_Intersection_Bezier1)
    {
        const Vector3f ControlPoints[] = { Vector3f(-0.5f, -0.5f, 0.0f), Vector3f(0.5f, 0.5f, 0.0f) };
        const BezierCurve1f Curves[] = { BezierCurve1f(ControlPoints, 0.06f) };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurve_degree1.png");
    }

    TEST_CASE(Ray_SingleBezierCurve_Intersection_Bezier1_Horizontal)
    {
        const Vector3f ControlPoints[] = { Vector3f(-0.5f, 0.0f, 0.0f), Vector3f(0.5f, -0.0f, 0.0f) };
        const BezierCurve1f Curves[] = { BezierCurve1f(ControlPoints, 0.06f) };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurve_degree1_horizontal.png");
    }

    TEST_CASE(Ray_SingleBezierCurve_Intersection_Bezier1_Vertical)
    {
        const Vector3f ControlPoints[] = { Vector3f(0.0f, 0.5f, 0.0f), Vector3f(0.0f, -0.5f, 0.0f) };
        const BezierCurve1f Curves[] = { BezierCurve1f(ControlPoints, 0.06f) };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurve_degree1_vertical.png");
    }


    //
    // Degree 2 Bezier curves.
    //

    TEST_CASE(Ray_SingleBezierCurve_Intersection_Bezier2)
    {
        const Vector3f ControlPoints[] = { Vector3f(-0.5f, 0.0f, 0.0f), Vector3f(0.0f, 0.5f, 0.0f), Vector3f(0.50f, 0.0f, 0.0f) };
        const BezierCurve2f Curves[] = { BezierCurve2f(ControlPoints, 0.06f) };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurve_degree2.png");
    }

    TEST_CASE(Ray_SingleBezierCurve_Intersection_Bezier2_Horizontal)
    {
        const Vector3f ControlPoints[] = { Vector3f(-0.5f, 0.0f, 0.0f), Vector3f(0.0f, 0.0f, 0.0f), Vector3f(0.50f, 0.0f, 0.0f) };
        const BezierCurve2f Curves[] = { BezierCurve2f(ControlPoints, 0.06f) };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurve_degree2_horizontal.png");
    }

    TEST_CASE(Ray_SingleBezierCurve_Intersection_Bezier2_Vertical)
    {
        const Vector3f ControlPoints[] = { Vector3f(0.0f, 0.5f, 0.0f), Vector3f(0.0f, 0.0f, 0.0f), Vector3f(0.0f, -0.50f, 0.0f) };
        const BezierCurve2f Curves[] = { BezierCurve2f(ControlPoints, 0.06f) };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurve_degree2_vertical.png");
    }


    //
    // Degree 3 Bezier curves.
    //

    TEST_CASE(Ray_SingleBezierCurve_Intersection_Bezier3)
    {
        const Vector3f ControlPoints[] = { Vector3f(-0.5f, 0.0f, 0.0f), Vector3f(-0.20f, 0.20f, 0.0f), Vector3f(0.20f, -0.20f, 0.0f), Vector3f(0.5f, 0.0f, 0.0f) };
        const BezierCurve3f Curves[] = { BezierCurve3f(ControlPoints, 0.06f) };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurve_degree3.png");
    }

    TEST_CASE(Ray_SingleBezierCurve_Intersection_Bezier3_Horizontal)
    {
        const Vector3f ControlPoints[] = { Vector3f(-0.5f, 0.0f, 0.0f), Vector3f(-0.25f, 0.0f, 0.0f), Vector3f(0.25f, 0.0f, 0.0f), Vector3f(0.5f, 0.0f, 0.0f) };
        const BezierCurve3f Curves[] = { BezierCurve3f(ControlPoints, 0.06f) };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurve_degree3_horizontal.png");
    }

    TEST_CASE(Ray_SingleBezierCurve_Intersection_Bezier3_Vertical)
    {
        const Vector3f ControlPoints[] = { Vector3f(0.0f, 0.50f, 0.0f), Vector3f(0.0f, 0.25f, 0.0f), Vector3f(0.0f, -0.25f, 0.0f), Vector3f(0.0f, -0.50f, 0.0f) };
        const BezierCurve3f Curves[] = { BezierCurve3f(ControlPoints, 0.06f) };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurve_degree3_vertical.png");
    }


    //
    // Variable-width Bezier curves.
    //

    TEST_CASE(Ray_SingleBezierCurve_Intersection_Bezier1_Variable_Width)
    {
        const Vector3f ControlPoints[] = { Vector3f(-0.5f, 0.5f, 0.0f), Vector3f(0.5f, -0.5f, 0.0f) };
        const float Widths[] = { 0.06f, 0.01f };
        const BezierCurve1f Curves[] = { BezierCurve1f(ControlPoints, Widths) };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurve_degree1_vw.png");
    }

    TEST_CASE(Ray_SingleBezierCurve_Intersection_Bezier2_Variable_Width)
    {
        const Vector3f ControlPoints[] = { Vector3f(-0.5f, 0.0f, 0.0f), Vector3f(0.0f, 0.5f, 0.0f), Vector3f(0.50f, 0.0f, 0.0f) };
        const float Widths[] = { 0.01f, 0.08f, 0.01f };
        const BezierCurve2f Curves[] = { BezierCurve2f(ControlPoints, Widths) };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurve_degree2_vw.png");
    }

    TEST_CASE(Ray_SingleBezierCurve_Intersection_Bezier3_Variable_Width)
    {
        const Vector3f ControlPoints[] = { Vector3f(-0.5f, 0.0f, 0.0f), Vector3f(-0.20f, 0.20f, 0.0f), Vector3f(0.20f, -0.20f, 0.0f), Vector3f(0.5f, 0.0f, 0.0f) };
        const float Widths[] = { 0.03f, 0.1f, 0.06f, 0.02f };
        const BezierCurve3f Curves[] = { BezierCurve3f(ControlPoints, Widths) };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurve_degree3_vw.png");
    }


    //
    // Edge cases.
    //

    TEST_CASE(Ray_SingleBezierCurve_Intersection_Bezier2_Small_Curve)
    {
        const Vector3f ControlPoints[] = { Vector3f(-0.5f, 0.0f, 0.0f), Vector3f(-0.4f, 0.0f, 0.0f), Vector3f(-0.3f, 0.0f, 0.0f), Vector3f(-0.2f, 0.0f, 0.0f) };
        const BezierCurve3f Curves[] = { BezierCurve3f(ControlPoints, 0.06f) };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurve_degree2_small.png");
    }


    //
    // Check continuity across connected Bezier curves.
    //

    TEST_CASE(Ray_MultipleBezierCurves_Intersection_Bezier1_Constant_Width)
    {
        const Vector3f ControlPoints1[] = { Vector3f(-0.5f, -0.5f, 0.0f), Vector3f(0.0f, 0.0f, 0.0f) };
        const Vector3f ControlPoints2[] = { Vector3f(0.0f, 0.0f, 0.0f), Vector3f(0.5f, 0.5f, 0.0f) };
        const BezierCurve1f Curves[] =
        {
            BezierCurve1f(ControlPoints1, 0.06f),
            BezierCurve1f(ControlPoints2, 0.06f)
        };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurve_degree1_mc_cw.png");
    }

    TEST_CASE(Ray_MultipleBezierCurves_Intersection_Bezier1_Variable_Width)
    {
        const Vector3f ControlPoints1[] = { Vector3f(-0.5f, -0.5f, 0.0f), Vector3f(0.0f, 0.0f, 0.0f) };
        const Vector3f ControlPoints2[] = { Vector3f(0.0f, 0.0f, 0.0f), Vector3f(0.5f, 0.5f, 0.0f) };
        const float Widths1[] = { 0.06f, 0.01f };
        const float Widths2[] = { 0.01f, 0.06f };
        const BezierCurve1f Curves[] =
        {
            BezierCurve1f(ControlPoints1, Widths1),
            BezierCurve1f(ControlPoints2, Widths2)
        };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurve_degree1_mc_vw.png");
    }

    TEST_CASE(Ray_MultipleBezierCurves_Intersection_Bezier2_Constant_Width)
    {
        const Vector3f ControlPoints1[] = { Vector3f(-0.7f, 0.0f, 0.0f), Vector3f(-0.40f, 0.5f, 0.0f), Vector3f(0.00f, 0.0f, 0.0f) };
        const Vector3f ControlPoints2[] = { Vector3f(0.0f, 0.0f, 0.0f), Vector3f(0.40f, -0.5f, 0.0f), Vector3f(0.70f, 0.0f, 0.0f) };
        const BezierCurve2f Curves[] =
        {
            BezierCurve2f(ControlPoints1, 0.06f),
            BezierCurve2f(ControlPoints2, 0.06f)
        };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurve_degree2_mc_cw.png");
    }

    TEST_CASE(Ray_MultipleBezierCurves_Intersection_Bezier2_Variable_Width)
    {
        const Vector3f ControlPoints1[] = { Vector3f(-0.7f, 0.0f, 0.0f), Vector3f(-0.40f, 0.5f, 0.0f), Vector3f(0.00f, 0.0f, 0.0f) };
        const Vector3f ControlPoints2[] = { Vector3f(0.0f, 0.0f, 0.0f), Vector3f(0.40f, -0.5f, 0.0f), Vector3f(0.70f, 0.0f, 0.0f) };
        const float Widths[] = { 0.01f, 0.08f, 0.01f };
        const BezierCurve2f Curves[] =
        {
            BezierCurve2f(ControlPoints1, Widths),
            BezierCurve2f(ControlPoints2, Widths)
        };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurve_degree2_mc_vw.png");
    }

    TEST_CASE(Ray_MultipleBezierCurves_Intersection_Bezier3_Constant_Width)
    {
        const Vector3f ControlPoints1[] = { Vector3f(-0.7f, 0.0f, 0.0f), Vector3f(-0.20f, 0.50f, 0.0f), Vector3f(-0.50f, -0.50f, 0.0f), Vector3f(0.0f, 0.0f, 0.0f) };
        const Vector3f ControlPoints2[] = { Vector3f(-0.0f, 0.0f, 0.0f), Vector3f(0.4f, 0.30f, 0.0f), Vector3f(0.40f, -0.30f, 0.0f), Vector3f(0.6f, 0.0f, 0.0f) };
        const BezierCurve3f Curves[] =
        {
            BezierCurve3f(ControlPoints1, 0.06f),
            BezierCurve3f(ControlPoints2, 0.06f)
        };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurve_degree3_mc_cw.png");
    }

    TEST_CASE(Ray_MultipleBezierCurves_Intersection_Bezier3_Variable_Width)
    {
        const Vector3f ControlPoints1[] = { Vector3f(-0.7f, 0.0f, 0.0f), Vector3f(-0.20f, 0.50f, 0.0f), Vector3f(-0.50f, -0.50f, 0.0f), Vector3f(0.0f, 0.0f, 0.0f) };
        const Vector3f ControlPoints2[] = { Vector3f(-0.0f, 0.0f, 0.0f), Vector3f(0.4f, 0.30f, 0.0f), Vector3f(0.40f, -0.30f, 0.0f), Vector3f(0.6f, 0.0f, 0.0f) };
        const float Widths[] = { 0.03f, 0.1f, 0.06f, 0.03f };
        const BezierCurve3f Curves[] =
        {
            BezierCurve3f(ControlPoints1, Widths),
            BezierCurve3f(ControlPoints2, Widths)
        };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurve_degree3_mc_vw.png");
    }
}
