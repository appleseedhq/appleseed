
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

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

// Imath headers.
#ifdef APPLESEED_ENABLE_IMATH_INTEROP
#include "foundation/platform/_beginexrheaders.h"
#include "Imath/ImathVec.h"
#include "foundation/platform/_endexrheaders.h"
#endif

// Standard headers.
#include <cmath>

using namespace foundation;

TEST_SUITE(Foundation_Math_Vector)
{
    typedef Vector<float, 5> Vector5f;
    typedef Vector<double, 5> Vector5d;

    TEST_CASE(ConstructVectorWithSingleValue)
    {
        const Vector5d v(7.0);

        EXPECT_EQ(7.0, v[0]);
        EXPECT_EQ(7.0, v[1]);
        EXPECT_EQ(7.0, v[2]);
        EXPECT_EQ(7.0, v[3]);
        EXPECT_EQ(7.0, v[4]);
    }

    TEST_CASE(ConstructVectorFromArrayOfValues)
    {
        static const double Values[] = { 1.0, 5.0, 19.0, -4.0, -100.0 };
        const auto v(Vector5d::from_array(Values));

        EXPECT_EQ(Values[0], v[0]);
        EXPECT_EQ(Values[1], v[1]);
        EXPECT_EQ(Values[2], v[2]);
        EXPECT_EQ(Values[3], v[3]);
        EXPECT_EQ(Values[4], v[4]);
    }

    TEST_CASE(ConstructVectorByTypeConversion)
    {
        static const double Values[] = { 1.0, 5.0, 19.0, -4.0, -100.0 };
        const auto v(Vector5d::from_array(Values));
        const Vector5f vf(v);

        EXPECT_FEQ(static_cast<float>(Values[0]), vf[0]);
        EXPECT_FEQ(static_cast<float>(Values[1]), vf[1]);
        EXPECT_FEQ(static_cast<float>(Values[2]), vf[2]);
        EXPECT_FEQ(static_cast<float>(Values[3]), vf[3]);
        EXPECT_FEQ(static_cast<float>(Values[4]), vf[4]);
    }

    TEST_CASE(TestEquality)
    {
        const Vector3d u(1.0, 5.0, 19.0);
        const Vector3d v(1.0, 5.0, 19.0);
        const Vector3d a(1.0, 5.0,  0.0);
        const Vector3d b(1.0, 0.0, 19.0);
        const Vector3d c(1.0, 5.0,  0.0);

        EXPECT_TRUE(u == v);
        EXPECT_FALSE(u == a);
        EXPECT_FALSE(u == b);
        EXPECT_FALSE(u == c);
    }

    TEST_CASE(TestInequality)
    {
        const Vector3d u(1.0, 5.0, 19.0);
        const Vector3d v(1.0, 5.0, 19.0);
        const Vector3d a(0.0, 5.0, 19.0);
        const Vector3d b(1.0, 0.0, 19.0);
        const Vector3d c(1.0, 5.0,  0.0);

        EXPECT_FALSE(u != v);
        EXPECT_TRUE(u != a);
        EXPECT_TRUE(u != b);
        EXPECT_TRUE(u != c);
    }

    TEST_CASE(TestApproximateEquality)
    {
        const Vector3d u(1.0, 5.0, 19.0);
        const Vector3d v(1.0, 5.0, 19.0);
        const Vector3d a(0.0, 5.0, 19.0);
        const Vector3d b(1.0, 0.0, 19.0);
        const Vector3d c(1.0, 5.0,  0.0);

        EXPECT_TRUE(feq(u, v));
        EXPECT_FALSE(feq(u, a));
        EXPECT_FALSE(feq(u, b));
        EXPECT_FALSE(feq(u, c));
    }

    TEST_CASE(TestComparisonToZero)
    {
        const Vector3d v(1.0, 5.0, 19.0);
        const Vector3d a(0.0, 5.0, 19.0);
        const Vector3d b(0.0, 0.0,  0.0);

        EXPECT_FALSE(fz(v));
        EXPECT_FALSE(fz(a));
        EXPECT_TRUE(fz(b));
    }

    TEST_CASE(TestAddition)
    {
        EXPECT_FEQ(Vector3d(5.0, 7.0, 9.0), Vector3d(1.0, 2.0, 3.0) + Vector3d(4.0, 5.0, 6.0));
    }

    TEST_CASE(TestSubtraction)
    {
        EXPECT_FEQ(Vector3d(1.0, 3.0, 5.0), Vector3d(4.0, 5.0, 6.0) - Vector3d(3.0, 2.0, 1.0));
    }

    TEST_CASE(TestNegation)
    {
        EXPECT_FEQ(Vector3d(-1.0, -2.0, -3.0), -Vector3d(1.0, 2.0, 3.0));
    }

    TEST_CASE(TestRightMultiplicationByScalar)
    {
        EXPECT_FEQ(Vector3d(2.0, 4.0, 6.0), Vector3d(1.0, 2.0, 3.0) * 2.0);
    }

    TEST_CASE(TestLeftMultiplicationByScalar)
    {
        EXPECT_FEQ(Vector3d(2.0, 4.0, 6.0), 2.0 * Vector3d(1.0, 2.0, 3.0));
    }

    TEST_CASE(TestDivisionByScalar)
    {
        EXPECT_FEQ(Vector3d(1.0, 2.0, 3.0), Vector3d(2.0, 4.0, 6.0) / 2.0);
    }

    TEST_CASE(TestMultiplicationByVector)
    {
        EXPECT_FEQ(Vector3d(6.0, 20.0, 42.0), Vector3d(2.0, 4.0, 6.0) * Vector3d(3.0, 5.0, 7.0));
    }

    TEST_CASE(TestDivisionByVector)
    {
        EXPECT_FEQ(Vector3d(1.0, 2.0, 3.0), Vector3d(2.0, 4.0, 6.0) / Vector3d(2.0, 2.0, 2.0));
    }

    TEST_CASE(TestInPlaceAddition)
    {
        Vector3d v(1.0, 2.0, 3.0); v += Vector3d(4.0, 5.0, 6.0);
        EXPECT_FEQ(Vector3d(5.0, 7.0, 9.0), v);
    }

    TEST_CASE(TestInPlaceSubtraction)
    {
        Vector3d v(4.0, 5.0, 6.0); v -= Vector3d(3.0, 2.0, 1.0);
        EXPECT_FEQ(Vector3d(1.0, 3.0, 5.0), v);
    }

    TEST_CASE(TestInPlaceMultiplicationByScalar)
    {
        Vector3d v(1.0, 2.0, 3.0); v *= 2.0;
        EXPECT_FEQ(Vector3d(2.0, 4.0, 6.0), v);
    }

    TEST_CASE(TestInPlaceDivisionByScalar)
    {
        Vector3d v(2.0, 4.0, 6.0); v /= 2.0;
        EXPECT_FEQ(Vector3d(1.0, 2.0, 3.0), v);
    }

    TEST_CASE(TestInPlaceMultiplicationByVector)
    {
        Vector3d v(2.0, 4.0, 6.0); v *= Vector3d(3.0, 5.0, 7.0);
        EXPECT_FEQ(Vector3d(6.0, 20.0, 42.0), v);
    }

    TEST_CASE(TestInPlaceDivisionByVector)
    {
        Vector3d v(2.0, 4.0, 6.0); v /= Vector3d(2.0, 2.0, 2.0);
        EXPECT_FEQ(Vector3d(1.0, 2.0, 3.0), v);
    }

    TEST_CASE(TestDotProduct)
    {
        EXPECT_FEQ(68.0, dot(Vector3d(2.0, 4.0, 6.0), Vector3d(3.0, 5.0, 7.0)));
    }

    TEST_CASE(TestSquareNorm)
    {
        EXPECT_EQ(0.0, square_norm(Vector3d(0.0)));
        EXPECT_FEQ(56.0, square_norm(Vector3d(2.0, 4.0, 6.0)));
    }

    TEST_CASE(TestNorm)
    {
        EXPECT_EQ(0.0, norm(Vector3d(0.0)));
        EXPECT_FEQ(std::sqrt(56.0), norm(Vector3d(2.0, 4.0, 6.0)));
    }

    TEST_CASE(TestNormalize)
    {
        EXPECT_FEQ(Vector3d(1.0, 0.0, 0.0), normalize(Vector3d(4.0, 0.0, 0.0)));
        EXPECT_FEQ(Vector3d(0.0, 1.0, 0.0), normalize(Vector3d(0.0, 4.0, 0.0)));
        EXPECT_FEQ(Vector3d(0.0, 0.0, 1.0), normalize(Vector3d(0.0, 0.0, 4.0)));
    }

    TEST_CASE(TestSafeNormalize)
    {
        EXPECT_FEQ(Vector3d(1.0, 0.0, 0.0), safe_normalize(Vector3d(4.0, 0.0, 0.0)));
        EXPECT_FEQ(Vector3d(0.0, 1.0, 0.0), safe_normalize(Vector3d(0.0, 4.0, 0.0)));
        EXPECT_FEQ(Vector3d(0.0, 0.0, 1.0), safe_normalize(Vector3d(0.0, 0.0, 4.0)));
        EXPECT_FEQ(Vector3d(1.0, 0.0, 0.0), safe_normalize(Vector3d(0.0, 0.0, 0.0)));
    }

    TEST_CASE(TestImproveNormalization)
    {
        Vector3d v(-0.48859909517572381, 0.021669236596684682, -0.87223928390023286);
        ASSERT_FALSE(is_normalized(v, 1.0e-14));

        v = improve_normalization(v);

        EXPECT_TRUE(is_normalized(v, 1.0e-14));
    }

    TEST_CASE(TestIsNormalized)
    {
        const Vector3d v(3.0, -5.0, 7.0);

        EXPECT_FALSE(is_normalized(v));
        EXPECT_TRUE(is_normalized(normalize(v)));

        EXPECT_FALSE(is_normalized(Vector3d(0.0)));
        EXPECT_TRUE(is_normalized(Vector3d(1.0, 0.0, 0.0)));
        EXPECT_TRUE(is_normalized(Vector3d(0.0, 1.0, 0.0)));
        EXPECT_TRUE(is_normalized(Vector3d(0.0, 0.0, 1.0)));
    }

    TEST_CASE(TestReflect)
    {
        const Vector3d i(1.0, 2.0, 3.0);
        const Vector3d n(0.0, 1.0, 0.0);

        const Vector3d r = reflect(i, n);

        EXPECT_FEQ(Vector3d(-1.0, 2.0, -3.0), r);
    }

    TEST_CASE(TestRefract_IncidentDirectionAndNormalAreInTheSameHemisphere)
    {
        const Vector2d i = normalize(Vector2d(-1.0, 1.0));
        const Vector2d n(0.0, 1.0);
        const double eta = 0.9;

        Vector2d r;
        EXPECT_TRUE(refract(i, n, eta, r));

        EXPECT_FEQ(Vector2d(0.63639610306789274, -0.77136243102707558), r);
    }

    TEST_CASE(TestClamp)
    {
        const Vector3d v(-1.0, 2.0, 3.0);

        EXPECT_EQ(Vector3d(0.0), clamp(v, 0.0, 0.0));
        EXPECT_EQ(Vector3d(2.0), clamp(v, 2.0, 2.0));
        EXPECT_EQ(Vector3d(0.0, 2.0, 2.0), clamp(v, 0.0, 2.0));
    }

    TEST_CASE(TestSaturate)
    {
        EXPECT_EQ(Vector3d(0.0, 1.0, 1.0), saturate(Vector3d(-1.0, 2.0, 3.0)));
    }

    TEST_CASE(TestComponentWiseMin)
    {
        const Vector2d a(2.0, -4.0);
        const Vector2d b(-3.0, -2.0);

        EXPECT_EQ(Vector2d(-3.0, -4.0), component_wise_min(a, b));
    }

    TEST_CASE(TestComponentWiseMax)
    {
        const Vector2d a(2.0, -4.0);
        const Vector2d b(-3.0, -2.0);

        EXPECT_EQ(Vector2d(2.0, -2.0), component_wise_max(a, b));
    }

    TEST_CASE(TestMinValue)
    {
        EXPECT_EQ(1.0, min_value(Vector3d(1.0, 2.0, 3.0)));
        EXPECT_EQ(1.0, min_value(Vector3d(2.0, 1.0, 3.0)));
        EXPECT_EQ(1.0, min_value(Vector3d(3.0, 2.0, 1.0)));
    }

    TEST_CASE(TestMaxValue)
    {
        EXPECT_EQ(3.0, max_value(Vector3d(1.0, 2.0, 3.0)));
        EXPECT_EQ(3.0, max_value(Vector3d(2.0, 1.0, 3.0)));
        EXPECT_EQ(3.0, max_value(Vector3d(3.0, 2.0, 1.0)));
    }

    TEST_CASE(TestMinIndex)
    {
        EXPECT_EQ(0, min_index(Vector3d(1.0, 2.0, 3.0)));
        EXPECT_EQ(1, min_index(Vector3d(2.0, 1.0, 3.0)));
        EXPECT_EQ(2, min_index(Vector3d(3.0, 2.0, 1.0)));
    }

    TEST_CASE(TestMaxIndex)
    {
        EXPECT_EQ(0, max_index(Vector3d(3.0, 2.0, 1.0)));
        EXPECT_EQ(1, max_index(Vector3d(2.0, 3.0, 1.0)));
        EXPECT_EQ(2, max_index(Vector3d(1.0, 2.0, 3.0)));
    }

    TEST_CASE(TestMinAbsIndex)
    {
        EXPECT_EQ(0, min_abs_index(Vector3d(-1.0, -2.0, -3.0)));
        EXPECT_EQ(1, min_abs_index(Vector3d(-2.0, -1.0, -3.0)));
        EXPECT_EQ(2, min_abs_index(Vector3d(-3.0, -2.0, -1.0)));
    }

    TEST_CASE(TestMaxAbsIndex)
    {
        EXPECT_EQ(0, max_abs_index(Vector3d(-3.0, -2.0, -1.0)));
        EXPECT_EQ(1, max_abs_index(Vector3d(-2.0, -3.0, -1.0)));
        EXPECT_EQ(2, max_abs_index(Vector3d(-1.0, -2.0, -3.0)));
    }
}

TEST_SUITE(Foundation_Math_Vector2)
{
    TEST_CASE(ConstructVectorWithSingleValue)
    {
        const Vector2d v(7.0);

        EXPECT_EQ(7.0, v[0]);
        EXPECT_EQ(7.0, v[1]);
    }

    TEST_CASE(ConstructVectorWithTwoValues)
    {
        const Vector2d v(1.0, 5.0);

        EXPECT_EQ(1.0, v[0]);
        EXPECT_EQ(5.0, v[1]);
    }

    TEST_CASE(ConstructVectorFromArrayOfValues)
    {
        static const double Values[] = { 1.0, 5.0 };
        const auto v(Vector2d::from_array(Values));

        EXPECT_EQ(Values[0], v[0]);
        EXPECT_EQ(Values[1], v[1]);
    }

    TEST_CASE(ConstructVectorByTypeConversion)
    {
        static const double Values[] = { 1.0, 5.0 };
        const auto v(Vector2d::from_array(Values));
        const Vector2f vf(v);

        EXPECT_FEQ(static_cast<float>(Values[0]), vf[0]);
        EXPECT_FEQ(static_cast<float>(Values[1]), vf[1]);
    }

#ifdef APPLESEED_ENABLE_IMATH_INTEROP

    TEST_CASE(ConstructFromImathVec2)
    {
        const Imath::V2d source(1.0, 2.0);
        const Vector2d copy(source);

        EXPECT_EQ(Vector2d(1.0, 2.0), copy);
    }

    TEST_CASE(ConvertToImathVec2)
    {
        const Vector2d source(1.0, 2.0);
        const Imath::V2d copy(source);

        EXPECT_EQ(Imath::V2d(1.0, 2.0), copy);
    }

#endif

    TEST_CASE(TestDeterminant)
    {
        EXPECT_FEQ(17.0, det(Vector2d(2.0, 3.0), Vector2d(-1.0, 7.0)));
    }
}

TEST_SUITE(Foundation_Math_Vector3)
{
    TEST_CASE(ConstructVectorWithSingleValue)
    {
        const Vector3d v(7.0);

        EXPECT_EQ(7.0, v[0]);
        EXPECT_EQ(7.0, v[1]);
        EXPECT_EQ(7.0, v[2]);
    }

    TEST_CASE(ConstructVectorWithTreeValues)
    {
        const Vector3d v(1.0, 5.0, 19.0);

        EXPECT_EQ(1.0, v[0]);
        EXPECT_EQ(5.0, v[1]);
        EXPECT_EQ(19.0, v[2]);
    }

    TEST_CASE(ConstructVectorFromArrayOfValues)
    {
        static const double Values[] = { 1.0, 5.0, 19.0 };
        const auto v(Vector3d::from_array(Values));

        EXPECT_EQ(Values[0], v[0]);
        EXPECT_EQ(Values[1], v[1]);
        EXPECT_EQ(Values[2], v[2]);
    }

    TEST_CASE(ConstructVectorByTypeConversion)
    {
        static const double Values[] = { 1.0, 5.0, 19.0 };
        const auto v(Vector3d::from_array(Values));
        const Vector3f vf(v);

        EXPECT_FEQ(static_cast<float>(Values[0]), vf[0]);
        EXPECT_FEQ(static_cast<float>(Values[1]), vf[1]);
        EXPECT_FEQ(static_cast<float>(Values[2]), vf[2]);
    }

#ifdef APPLESEED_ENABLE_IMATH_INTEROP

    TEST_CASE(ConstructFromImathVec3)
    {
        const Imath::V3d source(1.0, 2.0, 3.0);
        const Vector3d copy(source);

        EXPECT_EQ(Vector3d(1.0, 2.0, 3.0), copy);
    }

    TEST_CASE(ConvertToImathVec3)
    {
        const Vector3d source(1.0, 2.0, 3.0);
        const Imath::V3d copy(source);

        EXPECT_EQ(Imath::V3d(1.0, 2.0, 3.0), copy);
    }

#endif

    TEST_CASE(TestCrossProduct)
    {
        const Vector3d w = cross(Vector3d(1.0, 0.0, 0.0), Vector3d(0.0, 1.0, 0.0));
        const Vector3d u = cross(Vector3d(0.0, 1.0, 0.0), Vector3d(0.0, 0.0, 1.0));
        const Vector3d v = cross(Vector3d(0.0, 0.0, 1.0), Vector3d(1.0, 0.0, 0.0));

        EXPECT_FEQ(Vector3d(1.0, 0.0, 0.0), u);
        EXPECT_FEQ(Vector3d(0.0, 1.0, 0.0), v);
        EXPECT_FEQ(Vector3d(0.0, 0.0, 1.0), w);
    }
}

TEST_SUITE(Foundation_Math_Vector4)
{
    TEST_CASE(ConstructVectorWithSingleValue)
    {
        const Vector4d v(7.0);

        EXPECT_EQ(7.0, v[0]);
        EXPECT_EQ(7.0, v[1]);
        EXPECT_EQ(7.0, v[2]);
        EXPECT_EQ(7.0, v[3]);
    }

    TEST_CASE(ConstructVectorWithFourValues)
    {
        const Vector4d v(1.0, 5.0, 19.0, -100.0);

        EXPECT_EQ(1.0, v[0]);
        EXPECT_EQ(5.0, v[1]);
        EXPECT_EQ(19.0, v[2]);
        EXPECT_EQ(-100.0, v[3]);
    }

    TEST_CASE(ConstructVectorFromArrayOfValues)
    {
        static const double Values[] = { 1.0, 5.0, 19.0, -100.0 };
        const auto v(Vector4d::from_array(Values));

        EXPECT_EQ(Values[0], v[0]);
        EXPECT_EQ(Values[1], v[1]);
        EXPECT_EQ(Values[2], v[2]);
        EXPECT_EQ(Values[3], v[3]);
    }

    TEST_CASE(ConstructVectorByTypeConversion)
    {
        static const double Values[] = { 1.0, 5.0, 19.0, -100.0 };
        const auto v(Vector4d::from_array(Values));
        const Vector4f vf(v);

        EXPECT_FEQ(static_cast<float>(Values[0]), vf[0]);
        EXPECT_FEQ(static_cast<float>(Values[1]), vf[1]);
        EXPECT_FEQ(static_cast<float>(Values[2]), vf[2]);
        EXPECT_FEQ(static_cast<float>(Values[3]), vf[3]);
    }
}
