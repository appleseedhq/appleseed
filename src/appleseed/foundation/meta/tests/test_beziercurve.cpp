
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2017 Srinath Ravichandran, The appleseedhq Organization
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
#include "foundation/math/beziercurve.h"
#include "foundation/math/matrix.h"
#include "foundation/math/ray.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/countof.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <limits>
#include <vector>

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Math_BezierCurveIntersector)
{
#pragma warning (push)
#pragma warning (disable : 4723)    // potential division by 0

    // Render a bunch of Bezier curves to an image on disk.
    // The control points of the curves are expressed in [-1,1]^2
    // where (-1,-1) is at the bottom-left corner of the image.
    template <typename BezierCurveType>
    void render_curves_to_image(
        const BezierCurveType   curves[],
        const size_t            curve_count,
        const char*             filename,
        const bool              textured)
    {
        typedef typename BezierCurveType::ValueType ValueType;
        typedef typename BezierCurveType::VectorType VectorType;
        typedef typename BezierCurveType::MatrixType MatrixType;
        typedef Ray<ValueType, 3> RayType;
        typedef RayInfo<ValueType, 3> RayInfoType;
        typedef BezierCurveIntersector<BezierCurveType> BezierCurveIntersectorType;

        const size_t ImageWidth = 512;
        const size_t ImageHeight = 512;

        const ValueType RcpImageWidth = ValueType(1.0) / ImageWidth;
        const ValueType RcpImageHeight = ValueType(1.0) / ImageHeight;

        Image image(ImageWidth, ImageHeight, ImageWidth, ImageHeight, 3, PixelFormatFloat);

        for (size_t y = 0; y < ImageHeight; ++y)
        {
            for (size_t x = 0; x < ImageWidth; ++x)
            {
                Color3f color(0.0f);

                // Compute the normalized coordinates of the center of the pixel in [-1,1]^2.
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

                    // Draw the curve.
                    MatrixType curve_transform;
                    make_curve_projection_transform(curve_transform, ray);
                    if (textured)
                    {
                        ValueType u, v, t = numeric_limits<ValueType>::max();
                        if (BezierCurveIntersectorType::intersect(curve, ray, curve_transform, u, v, t))
                        {
                            // Checkboard pattern.
                            const int b = (truncate<int>(4.0 * u) ^ truncate<int>(32.0 * v)) & 1;
                            color = b ? Color3f(0.8f) : Color3f(0.2f);
                        }
                    }
                    else
                    {
                        if (BezierCurveIntersectorType::intersect(curve, ray, curve_transform))
                        {
                            color[0] = 0.2f;
                            color[2] = 0.7f;
                        }
                    }

                    // Draw control points.
                    if (!textured)
                    {
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

    TEST_CASE(RenderSingleBezier1Curve)
    {
        const Vector3f ControlPoints[] = { Vector3f(-0.5f, -0.5f, 0.0f), Vector3f(0.5f, 0.5f, 0.0f) };
        const BezierCurve1f Curves[] = { BezierCurve1f(ControlPoints, 0.06f) };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurveintersector_singlebezier1curve.png", false);
    }

    TEST_CASE(RenderSingleBezier1Curve_Horizontal)
    {
        const Vector3f ControlPoints[] = { Vector3f(-0.5f, 0.0f, 0.0f), Vector3f(0.5f, -0.0f, 0.0f) };
        const BezierCurve1f Curves[] = { BezierCurve1f(ControlPoints, 0.06f) };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurveintersector_singlebezier1curve_horizontal.png", false);
    }

    TEST_CASE(RenderSingleBezier1Curve_Vertical)
    {
        const Vector3f ControlPoints[] = { Vector3f(0.0f, 0.5f, 0.0f), Vector3f(0.0f, -0.5f, 0.0f) };
        const BezierCurve1f Curves[] = { BezierCurve1f(ControlPoints, 0.06f) };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurveintersector_singlebezier1curve_vertical.png", false);
    }


    //
    // Degree 2 Bezier curves.
    //

    TEST_CASE(RenderSingleBezier2Curve)
    {
        const Vector3f ControlPoints[] = { Vector3f(-0.5f, 0.0f, 0.0f), Vector3f(0.0f, 0.5f, 0.0f), Vector3f(0.50f, 0.0f, 0.0f) };
        const BezierCurve2f Curves[] = { BezierCurve2f(ControlPoints, 0.06f) };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurveintersector_singlebezier2curve.png", false);
    }

    TEST_CASE(RenderSingleBezier2Curve_Horizontal)
    {
        const Vector3f ControlPoints[] = { Vector3f(-0.5f, 0.0f, 0.0f), Vector3f(0.0f, 0.0f, 0.0f), Vector3f(0.50f, 0.0f, 0.0f) };
        const BezierCurve2f Curves[] = { BezierCurve2f(ControlPoints, 0.06f) };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurveintersector_singlebezier2curve_horizontal.png", false);
    }

    TEST_CASE(RenderSingleBezier2Curve_Vertical)
    {
        const Vector3f ControlPoints[] = { Vector3f(0.0f, 0.5f, 0.0f), Vector3f(0.0f, 0.0f, 0.0f), Vector3f(0.0f, -0.50f, 0.0f) };
        const BezierCurve2f Curves[] = { BezierCurve2f(ControlPoints, 0.06f) };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurveintersector_singlebezier2curve_vertical.png", false);
    }


    //
    // Degree 3 Bezier curves.
    //

    TEST_CASE(RenderSingleBezier3Curve)
    {
        const Vector3f ControlPoints[] = { Vector3f(-0.5f, 0.0f, 0.0f), Vector3f(-0.20f, 0.20f, 0.0f), Vector3f(0.20f, -0.20f, 0.0f), Vector3f(0.5f, 0.0f, 0.0f) };
        const BezierCurve3f Curves[] = { BezierCurve3f(ControlPoints, 0.06f) };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurveintersector_singlebezier3curve.png", false);
    }

    TEST_CASE(RenderSingleBezier3Curve_Horizontal)
    {
        const Vector3f ControlPoints[] = { Vector3f(-0.5f, 0.0f, 0.0f), Vector3f(-0.25f, 0.0f, 0.0f), Vector3f(0.25f, 0.0f, 0.0f), Vector3f(0.5f, 0.0f, 0.0f) };
        const BezierCurve3f Curves[] = { BezierCurve3f(ControlPoints, 0.06f) };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurveintersector_singlebezier3curve_horizontal.png", false);
    }

    TEST_CASE(RenderSingleBezier3Curve_Vertical)
    {
        const Vector3f ControlPoints[] = { Vector3f(0.0f, 0.50f, 0.0f), Vector3f(0.0f, 0.25f, 0.0f), Vector3f(0.0f, -0.25f, 0.0f), Vector3f(0.0f, -0.50f, 0.0f) };
        const BezierCurve3f Curves[] = { BezierCurve3f(ControlPoints, 0.06f) };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurveintersector_singlebezier3curve_vertical.png", false);
    }


    //
    // Variable-width Bezier curves.
    //

    TEST_CASE(RenderSingleBezier1Curve_VariableWidth)
    {
        const Vector3f ControlPoints[] = { Vector3f(-0.5f, 0.5f, 0.0f), Vector3f(0.5f, -0.5f, 0.0f) };
        const float Widths[] = { 0.06f, 0.01f };
        const BezierCurve1f Curves[] = { BezierCurve1f(ControlPoints, Widths) };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurveintersector_singlebezier1curve_variablewidth.png", false);
    }

    TEST_CASE(RenderSingleBezier2Curve_VariableWidth)
    {
        const Vector3f ControlPoints[] = { Vector3f(-0.5f, 0.0f, 0.0f), Vector3f(0.0f, 0.5f, 0.0f), Vector3f(0.50f, 0.0f, 0.0f) };
        const float Widths[] = { 0.01f, 0.08f, 0.01f };
        const BezierCurve2f Curves[] = { BezierCurve2f(ControlPoints, Widths) };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurveintersector_singlebezier2curve_variablewidth.png", false);
    }

    TEST_CASE(RenderSingleBezier3Curve_VariableWidth)
    {
        const Vector3f ControlPoints[] = { Vector3f(-0.5f, 0.0f, 0.0f), Vector3f(-0.20f, 0.20f, 0.0f), Vector3f(0.20f, -0.20f, 0.0f), Vector3f(0.5f, 0.0f, 0.0f) };
        const float Widths[] = { 0.03f, 0.1f, 0.06f, 0.02f };
        const BezierCurve3f Curves[] = { BezierCurve3f(ControlPoints, Widths) };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurveintersector_singlebezier3curve_variablewidth.png", false);
    }


    //
    // Edge cases.
    //

    TEST_CASE(RenderSingleBezier2Curve_Short)
    {
        const Vector3f ControlPoints[] = { Vector3f(-0.5f, 0.0f, 0.0f), Vector3f(-0.4f, 0.0f, 0.0f), Vector3f(-0.3f, 0.0f, 0.0f), Vector3f(-0.2f, 0.0f, 0.0f) };
        const BezierCurve3f Curves[] = { BezierCurve3f(ControlPoints, 0.06f) };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurveintersector_singlebezier2curve_short.png", false);
    }


    //
    // Check continuity across connected Bezier curves.
    //

    TEST_CASE(RenderTwoConnectedBezier1Curves)
    {
        const Vector3f ControlPoints1[] = { Vector3f(-0.5f, -0.5f, 0.0f), Vector3f(0.0f, 0.0f, 0.0f) };
        const Vector3f ControlPoints2[] = { Vector3f(0.0f, 0.0f, 0.0f), Vector3f(0.5f, 0.5f, 0.0f) };
        const BezierCurve1f Curves[] =
        {
            BezierCurve1f(ControlPoints1, 0.06f),
            BezierCurve1f(ControlPoints2, 0.06f)
        };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurveintersector_twoconnectedbezier1curves.png", false);
    }

    TEST_CASE(RenderTwoConnectedBezier1Curves_VariableWidth)
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

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurveintersector_twoconnectedbezier1curves_variablewidth.png", false);
    }

    TEST_CASE(RenderTwoConnectedBezier2Curves)
    {
        const Vector3f ControlPoints1[] = { Vector3f(-0.7f, 0.0f, 0.0f), Vector3f(-0.4f, 0.5f, 0.0f), Vector3f(0.0f, 0.0f, 0.0f) };
        const Vector3f ControlPoints2[] = { Vector3f(0.0f, 0.0f, 0.0f), Vector3f(0.4f, -0.5f, 0.0f), Vector3f(0.7f, 0.0f, 0.0f) };
        const BezierCurve2f Curves[] =
        {
            BezierCurve2f(ControlPoints1, 0.06f),
            BezierCurve2f(ControlPoints2, 0.06f)
        };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurveintersector_twoconnectedbezier2curves.png", false);
    }

    TEST_CASE(RenderTwoConnectedBezier2Curves_VariableWidth)
    {
        const Vector3f ControlPoints1[] = { Vector3f(-0.7f, 0.0f, 0.0f), Vector3f(-0.4f, 0.5f, 0.0f), Vector3f(0.0f, 0.0f, 0.0f) };
        const Vector3f ControlPoints2[] = { Vector3f(0.0f, 0.0f, 0.0f), Vector3f(0.4f, -0.5f, 0.0f), Vector3f(0.7f, 0.0f, 0.0f) };
        const float Widths[] = { 0.01f, 0.08f, 0.01f };
        const BezierCurve2f Curves[] =
        {
            BezierCurve2f(ControlPoints1, Widths),
            BezierCurve2f(ControlPoints2, Widths)
        };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurveintersector_twoconnectedbezier2curves_variablewidth.png", false);
    }

    TEST_CASE(RenderTwoConnectedBezier3Curves)
    {
        const Vector3f ControlPoints1[] = { Vector3f(-0.7f, 0.0f, 0.0f), Vector3f(-0.2f, 0.5f, 0.0f), Vector3f(-0.5f, -0.5f, 0.0f), Vector3f(0.0f, 0.0f, 0.0f) };
        const Vector3f ControlPoints2[] = { Vector3f(0.0f, 0.0f, 0.0f), Vector3f(0.5f, 0.5f, 0.0f), Vector3f(0.2f, -0.5f, 0.0f), Vector3f(0.7f, 0.0f, 0.0f) };
        const BezierCurve3f Curves[] =
        {
            BezierCurve3f(ControlPoints1, 0.06f),
            BezierCurve3f(ControlPoints2, 0.06f)
        };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurveintersector_twoconnectedbezier3curves.png", false);
    }

    TEST_CASE(RenderTwoConnectedBezier3Curves_VariableWidth)
    {
        const Vector3f ControlPoints1[] = { Vector3f(-0.7f, 0.0f, 0.0f), Vector3f(-0.2f, 0.5f, 0.0f), Vector3f(-0.5f, -0.5f, 0.0f), Vector3f(0.0f, 0.0f, 0.0f) };
        const Vector3f ControlPoints2[] = { Vector3f(0.0f, 0.0f, 0.0f), Vector3f(0.5f, 0.5f, 0.0f), Vector3f(0.2f, -0.5f, 0.0f), Vector3f(0.7f, 0.0f, 0.0f) };
        const float Widths[] = { 0.03f, 0.1f, 0.06f, 0.03f };
        const BezierCurve3f Curves[] =
        {
            BezierCurve3f(ControlPoints1, Widths),
            BezierCurve3f(ControlPoints2, Widths)
        };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurveintersector_twoconnectedbezier3curves_variablewidth.png", false);
    }

    TEST_CASE(RenderMultipleBezier3Curves)
    {
        const Vector3f ControlPoints[] =
        {
            Vector3f(-0.827751f, -0.269373f, 0.0f),
            Vector3f(-0.614035f,  0.557196f, 0.0f),
            Vector3f(-0.298246f, -0.505535f, 0.0f),
            Vector3f(-0.094099f,  0.586716f, 0.0f),
            Vector3f( 0.199362f, -0.173432f, 0.0f),
            Vector3f( 0.416268f,  0.704797f, 0.0f),
            Vector3f( 0.642743f, -0.682657f, 0.0f),
            Vector3f( 0.897927f,  0.468635f, 0.0f)
        };

        //
        // Create a new set of control points by adding midpoints between every other pairs
        // of control points. See http://stackoverflow.com/a/3516110/393756 for details.
        //

        vector<Vector3f> new_points;

        for (size_t i = 0; i < countof(ControlPoints); ++i)
        {
            new_points.push_back(ControlPoints[i]);

            if (i > 0 && i % 2 == 0 && i + 1 < countof(ControlPoints))
            {
                // Add a midpoint.
                new_points.push_back(0.5f * (ControlPoints[i] + ControlPoints[i + 1]));
            }
        }

        vector<BezierCurve3f> curves;

        for (size_t i = 0, e = new_points.size(); i + 3 < e; i += 3)
            curves.emplace_back(&new_points[i], 0.05f);

        render_curves_to_image(&curves[0], curves.size(), "unit tests/outputs/test_beziercurveintersector_multiplebezier3curves.png", false);
    }


    //
    // Check intersection distance.
    //

    TEST_CASE(Intersect_Bezier1CurveAndRayAlongX_ReturnsCorrectHitDistance)
    {
        const Vector3f ControlPoints[] = { Vector3f(-0.5f, -0.5f, -0.5f), Vector3f(0.5f, 0.5f, 0.5f) };
        const BezierCurve1f Curves[] = { BezierCurve1f(ControlPoints, 0.06f) };

        const Ray3f ray(Vector3f(-3.0f, 0.0f, 0.0f), Vector3f(1.0f, 0.0f, 0.0f));

        Matrix4f xfm_matrix;
        make_curve_projection_transform(xfm_matrix, ray);

        float u, v, t = numeric_limits<float>::max();
        const bool hit = BezierCurveIntersector<BezierCurve1f>::intersect(Curves[0], ray, xfm_matrix, u, v, t);

        ASSERT_TRUE(hit);
        EXPECT_FEQ(3.0f, t);

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurveintersector_bezier1curve_rayalongx.png", false);
    }

    TEST_CASE(Intersect_Bezier1CurveAndRayAlongY_ReturnsCorrectHitDistance)
    {
        const Vector3f ControlPoints[] = { Vector3f(-0.5f, -0.5f, -0.5f), Vector3f(0.5f, 0.5f, 0.5f) };
        const BezierCurve1f Curves[] = { BezierCurve1f(ControlPoints, 0.06f) };

        const Ray3f ray(Vector3f(0.0f, 3.0f, 0.0f), Vector3f(0.0f, -1.0f, 0.0f));

        Matrix4f xfm_matrix;
        make_curve_projection_transform(xfm_matrix, ray);

        float u, v, t = numeric_limits<float>::max();
        const bool hit = BezierCurveIntersector<BezierCurve1f>::intersect(Curves[0], ray, xfm_matrix, u, v, t);

        ASSERT_TRUE(hit);
        EXPECT_FEQ(3.0f, t);

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurveintersector_bezier1curve_rayalongy.png", false);
    }

    TEST_CASE(Intersect_Bezier1CurveAndRayAlongZ_ReturnsCorrectHitDistance)
    {
        const Vector3f ControlPoints[] = { Vector3f(-0.5f, -0.5f, -0.5f), Vector3f(0.5f, 0.5f, 0.5f) };
        const BezierCurve1f Curves[] = { BezierCurve1f(ControlPoints, 0.06f) };

        const Ray3f ray(Vector3f(0.0f, 0.0f, -3.0f), Vector3f(0.0f, 0.0f, 1.0f));

        Matrix4f xfm_matrix;
        make_curve_projection_transform(xfm_matrix, ray);

        float u, v, t = numeric_limits<float>::max();
        const bool hit = BezierCurveIntersector<BezierCurve1f>::intersect(Curves[0], ray, xfm_matrix, u, v, t);

        ASSERT_TRUE(hit);
        EXPECT_FEQ(3.0f, t);

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurveintersector_bezier1curve_rayalongz.png", false);
    }

    TEST_CASE(Intersect_GivenBezier1CurveAndNonUnitRayDirection_ReturnsCorrectHitDistance)
    {
        const Vector3f ControlPoints[] = { Vector3f(-0.5f, -0.5f, -0.5f), Vector3f(0.5f, 0.5f, 0.5f) };
        const BezierCurve1f Curve(ControlPoints, 0.06f);

        const Ray3f ray(Vector3f(0.0f, 0.0f, -6.0f), Vector3f(0.0f, 0.0f, 3.0f));

        Matrix4f xfm_matrix;
        make_curve_projection_transform(xfm_matrix, ray);

        float u, v, t = numeric_limits<float>::max();
        const bool hit = BezierCurveIntersector<BezierCurve1f>::intersect(Curve, ray, xfm_matrix, u, v, t);

        ASSERT_TRUE(hit);
        EXPECT_FEQ(2.0f, t);
    }

    TEST_CASE(Intersect_GivenBezier1CurveAndNonUnitRayDirectionAndCloserIntersection_ReturnsNoHit)
    {
        const Vector3f ControlPoints[] = { Vector3f(-0.5f, -0.5f, -0.5f), Vector3f(0.5f, 0.5f, 0.5f) };
        const BezierCurve1f Curve(ControlPoints, 0.06f);

        const Ray3f ray(Vector3f(0.0f, 0.0f, -6.0f), Vector3f(0.0f, 0.0f, 0.1f));

        Matrix4f xfm_matrix;
        make_curve_projection_transform(xfm_matrix, ray);

        float u, v, t = 20.0f;
        const bool hit = BezierCurveIntersector<BezierCurve1f>::intersect(Curve, ray, xfm_matrix, u, v, t);

        ASSERT_FALSE(hit);
        EXPECT_FEQ(20.0f, t);
    }


    //
    // Check barycentric coordinates of ray-curve intersections.
    //

    TEST_CASE(RenderSingleBezier3Curve_CheckboardTexture)
    {
        const Vector3f ControlPoints[] = { Vector3f(-0.7f, 0.0f, 0.0f), Vector3f(-0.2f, 0.8f, 0.0f), Vector3f(0.2f, -0.8f, 0.0f), Vector3f(0.7f, 0.0f, 0.0f) };
        const BezierCurve3f Curves[] = { BezierCurve3f(ControlPoints, 0.1f) };

        render_curves_to_image(Curves, countof(Curves), "unit tests/outputs/test_beziercurveintersector_bezier3curve_checkboard.png", true);
    }
}
