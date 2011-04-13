
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

// Standard headers.
#include <cmath>

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Math_Vector)
{
    typedef Vector<double, 2> Vec2;
    typedef Vector<double, 3> Vec3;
    typedef Vector<double, 5> Vec5;

    TEST_CASE(ConstructVectorWithArrayOfValues)
    {
        static const double Values[] = { 1.0, 5.0, 19.0, -4.0, -100.0 };
        const Vec5 v(Values);

        EXPECT_EQ(Values[0], v[0]);
        EXPECT_EQ(Values[1], v[1]);
        EXPECT_EQ(Values[2], v[2]);
        EXPECT_EQ(Values[3], v[3]);
        EXPECT_EQ(Values[4], v[4]);
    }

    TEST_CASE(ConstructVectorWithSingleValue)
    {
        const Vec5 v(7.0);

        EXPECT_EQ(7.0, v[0]);
        EXPECT_EQ(7.0, v[1]);
        EXPECT_EQ(7.0, v[2]);
        EXPECT_EQ(7.0, v[3]);
        EXPECT_EQ(7.0, v[4]);
    }

    TEST_CASE(ConstructVectorByTypeConversion)
    {
        static const double Values[] = { 1.0, 5.0, 19.0, -4.0, -100.0 };
        const Vec5 v(Values);
        const Vector<float, 5> vf(v);

        EXPECT_FEQ(static_cast<float>(Values[0]), vf[0]);
        EXPECT_FEQ(static_cast<float>(Values[1]), vf[1]);
        EXPECT_FEQ(static_cast<float>(Values[2]), vf[2]);
        EXPECT_FEQ(static_cast<float>(Values[3]), vf[3]);
        EXPECT_FEQ(static_cast<float>(Values[4]), vf[4]);
    }

    TEST_CASE(TestEquality)
    {
        const Vec3 u(1.0, 5.0, 19.0);
        const Vec3 v(1.0, 5.0, 19.0);
        const Vec3 a(1.0, 5.0,  0.0);
        const Vec3 b(1.0, 0.0, 19.0);
        const Vec3 c(1.0, 5.0,  0.0);

        EXPECT_TRUE(u == v);
        EXPECT_FALSE(u == a);
        EXPECT_FALSE(u == b);
        EXPECT_FALSE(u == c);
    }

    TEST_CASE(TestInequality)
    {
        const Vec3 u(1.0, 5.0, 19.0);
        const Vec3 v(1.0, 5.0, 19.0);
        const Vec3 a(0.0, 5.0, 19.0);
        const Vec3 b(1.0, 0.0, 19.0);
        const Vec3 c(1.0, 5.0,  0.0);

        EXPECT_FALSE(u != v);
        EXPECT_TRUE(u != a);
        EXPECT_TRUE(u != b);
        EXPECT_TRUE(u != c);
    }

    TEST_CASE(TestApproximateEquality)
    {
        const Vec3 u(1.0, 5.0, 19.0);
        const Vec3 v(1.0, 5.0, 19.0);
        const Vec3 a(0.0, 5.0, 19.0);
        const Vec3 b(1.0, 0.0, 19.0);
        const Vec3 c(1.0, 5.0,  0.0);

        EXPECT_TRUE(feq(u, v));
        EXPECT_FALSE(feq(u, a));
        EXPECT_FALSE(feq(u, b));
        EXPECT_FALSE(feq(u, c));
    }

    TEST_CASE(TestComparisonToZero)
    {
        const Vec3 v(1.0, 5.0, 19.0);
        const Vec3 a(0.0, 5.0, 19.0);
        const Vec3 b(0.0, 0.0,  0.0);

        EXPECT_FALSE(fz(v));
        EXPECT_FALSE(fz(a));
        EXPECT_TRUE(fz(b));
    }

    TEST_CASE(TestAddition)
    {
        EXPECT_FEQ(Vec3(5.0, 7.0, 9.0), Vec3(1.0, 2.0, 3.0) + Vec3(4.0, 5.0, 6.0));
    }

    TEST_CASE(TestSubstraction)
    {
        EXPECT_FEQ(Vec3(1.0, 3.0, 5.0), Vec3(4.0, 5.0, 6.0) - Vec3(3.0, 2.0, 1.0));
    }

    TEST_CASE(TestNegation)
    {
        EXPECT_FEQ(Vec3(-1.0, -2.0, -3.0), -Vec3(1.0, 2.0, 3.0));
    }

    TEST_CASE(TestRightMultiplicationByScalar)
    {
        EXPECT_FEQ(Vec3(2.0, 4.0, 6.0), Vec3(1.0, 2.0, 3.0) * 2.0);
    }

    TEST_CASE(TestLeftMultiplicationByScalar)
    {
        EXPECT_FEQ(Vec3(2.0, 4.0, 6.0), 2.0 * Vec3(1.0, 2.0, 3.0));
    }

    TEST_CASE(TestDivisionByScalar)
    {
        EXPECT_FEQ(Vec3(1.0, 2.0, 3.0), Vec3(2.0, 4.0, 6.0) / 2.0);
    }

    TEST_CASE(TestMultiplicationByVector)
    {
        EXPECT_FEQ(Vec3(6.0, 20.0, 42.0), Vec3(2.0, 4.0, 6.0) * Vec3(3.0, 5.0, 7.0));
    }

    TEST_CASE(TestDivisionByVector)
    {
        EXPECT_FEQ(Vec3(1.0, 2.0, 3.0), Vec3(2.0, 4.0, 6.0) / Vec3(2.0, 2.0, 2.0));
    }

    TEST_CASE(TestInPlaceAddition)
    {
        Vec3 v(1.0, 2.0, 3.0); v += Vec3(4.0, 5.0, 6.0);
        EXPECT_FEQ(Vec3(5.0, 7.0, 9.0), v);
    }

    TEST_CASE(TestInPlaceSubstraction)
    {
        Vec3 v(4.0, 5.0, 6.0); v -= Vec3(3.0, 2.0, 1.0);
        EXPECT_FEQ(Vec3(1.0, 3.0, 5.0), v);
    }

    TEST_CASE(TestInPlaceMultiplicationByScalar)
    {
        Vec3 v(1.0, 2.0, 3.0); v *= 2.0;
        EXPECT_FEQ(Vec3(2.0, 4.0, 6.0), v);
    }

    TEST_CASE(TestInPlaceDivisionByScalar)
    {
        Vec3 v(2.0, 4.0, 6.0); v /= 2.0;
        EXPECT_FEQ(Vec3(1.0, 2.0, 3.0), v);
    }

    TEST_CASE(TestInPlaceMultiplicationByVector)
    {
        Vec3 v(2.0, 4.0, 6.0); v *= Vec3(3.0, 5.0, 7.0);
        EXPECT_FEQ(Vec3(6.0, 20.0, 42.0), v);
    }

    TEST_CASE(TestInPlaceDivisionByVector)
    {
        Vec3 v(2.0, 4.0, 6.0); v /= Vec3(2.0, 2.0, 2.0);
        EXPECT_FEQ(Vec3(1.0, 2.0, 3.0), v);
    }

    TEST_CASE(TestDotProduct)
    {
        EXPECT_FEQ(68.0, dot(Vec3(2.0, 4.0, 6.0), Vec3(3.0, 5.0, 7.0)));
    }

    TEST_CASE(TestSquareNorm)
    {
        EXPECT_EQ(0.0, square_norm(Vec3(0.0)));
        EXPECT_FEQ(56.0, square_norm(Vec3(2.0, 4.0, 6.0)));
    }

    TEST_CASE(TestNorm)
    {
        EXPECT_EQ(0.0, norm(Vec3(0.0)));
        EXPECT_FEQ(sqrt(56.0), norm(Vec3(2.0, 4.0, 6.0)));
    }

    TEST_CASE(TestNormalize)
    {
        EXPECT_FEQ(Vec3(1.0, 0.0, 0.0), normalize(Vec3(4.0, 0.0, 0.0)));
        EXPECT_FEQ(Vec3(0.0, 1.0, 0.0), normalize(Vec3(0.0, 4.0, 0.0)));
        EXPECT_FEQ(Vec3(0.0, 0.0, 1.0), normalize(Vec3(0.0, 0.0, 4.0)));
    }

    TEST_CASE(TestIsNormalized)
    {
        const Vec3 v(3.0, -5.0, 7.0);

        EXPECT_FALSE(is_normalized(v));
        EXPECT_TRUE(is_normalized(normalize(v)));

        EXPECT_FALSE(is_normalized(Vec3(0.0)));
        EXPECT_TRUE(is_normalized(Vec3(1.0, 0.0, 0.0)));
        EXPECT_TRUE(is_normalized(Vec3(0.0, 1.0, 0.0)));
        EXPECT_TRUE(is_normalized(Vec3(0.0, 0.0, 1.0)));
    }

    TEST_CASE(TestReflect)
    {
        const Vec3 i(1.0, 2.0, 3.0);
        const Vec3 n(0.0, 1.0, 0.0);

        const Vec3 r = reflect(i, n);

        EXPECT_FEQ(Vec3(-1.0, 2.0, -3.0), r);
    }

    TEST_CASE(TestRefract_IncidentDirectionAndNormalAreInTheSameHemisphere)
    {
        const Vec2 i = normalize(Vec2(-1.0, 1.0));
        const Vec2 n(0.0, 1.0);
        const double eta = 0.9;

        const Vec2 r = refract(i, n, eta);

        EXPECT_FEQ(Vec2(0.63639610306789274, -0.77136243102707558), r);
    }

    TEST_CASE(TestRefract_IncidentDirectionAndNormalAreInOppositeHemispheres)
    {
        const Vec2 i = normalize(Vec2(-1.0, 1.0));
        const Vec2 n(0.0, -1.0);
        const double eta = 0.9;

        const Vec2 r = refract(i, n, eta);

        EXPECT_FEQ(Vec2(0.63639610306789274, -0.77136243102707558), r);
    }

    TEST_CASE(TestClamp)
    {
        const Vec3 v(-1.0, 2.0, 3.0);

        EXPECT_EQ(Vec3(0.0), clamp(v, 0.0, 0.0));
        EXPECT_EQ(Vec3(2.0), clamp(v, 2.0, 2.0));
        EXPECT_EQ(Vec3(0.0, 2.0, 2.0), clamp(v, 0.0, 2.0));
    }

    TEST_CASE(TestSaturate)
    {
        EXPECT_EQ(Vec3(0.0, 1.0, 1.0), saturate(Vec3(-1.0, 2.0, 3.0)));
    }

    TEST_CASE(TestMin)
    {
        const Vec2 a(2.0, -4.0);
        const Vec2 b(-3.0, -2.0);

        EXPECT_EQ(Vec2(-3.0, -4.0), min(a, b));
    }

    TEST_CASE(TestMax)
    {
        const Vec2 a(2.0, -4.0);
        const Vec2 b(-3.0, -2.0);

        EXPECT_EQ(Vec2(2.0, -2.0), max(a, b));
    }

    TEST_CASE(TestMinValue)
    {
        EXPECT_EQ(1.0, min_value(Vec3(1.0, 2.0, 3.0)));
        EXPECT_EQ(1.0, min_value(Vec3(2.0, 1.0, 3.0)));
        EXPECT_EQ(1.0, min_value(Vec3(3.0, 2.0, 1.0)));
    }

    TEST_CASE(TestMaxValue)
    {
        EXPECT_EQ(3.0, max_value(Vec3(1.0, 2.0, 3.0)));
        EXPECT_EQ(3.0, max_value(Vec3(2.0, 1.0, 3.0)));
        EXPECT_EQ(3.0, max_value(Vec3(3.0, 2.0, 1.0)));
    }

    TEST_CASE(TestMinIndex)
    {
        EXPECT_EQ(0, min_index(Vec3(1.0, 2.0, 3.0)));
        EXPECT_EQ(1, min_index(Vec3(2.0, 1.0, 3.0)));
        EXPECT_EQ(2, min_index(Vec3(3.0, 2.0, 1.0)));
    }

    TEST_CASE(TestMaxIndex)
    {
        EXPECT_EQ(0, max_index(Vec3(3.0, 2.0, 1.0)));
        EXPECT_EQ(1, max_index(Vec3(2.0, 3.0, 1.0)));
        EXPECT_EQ(2, max_index(Vec3(1.0, 2.0, 3.0)));
    }

    TEST_CASE(TestMinAbsIndex)
    {
        EXPECT_EQ(0, min_abs_index(Vec3(-1.0, -2.0, -3.0)));
        EXPECT_EQ(1, min_abs_index(Vec3(-2.0, -1.0, -3.0)));
        EXPECT_EQ(2, min_abs_index(Vec3(-3.0, -2.0, -1.0)));
    }

    TEST_CASE(TestMaxAbsIndex)
    {
        EXPECT_EQ(0, max_abs_index(Vec3(-3.0, -2.0, -1.0)));
        EXPECT_EQ(1, max_abs_index(Vec3(-2.0, -3.0, -1.0)));
        EXPECT_EQ(2, max_abs_index(Vec3(-1.0, -2.0, -3.0)));
    }
}

TEST_SUITE(Foundation_Math_Vector2)
{
    typedef Vector<double, 2> Vec2;

    TEST_CASE(ConstructVectorWithArrayOfValues)
    {
        static const double Values[] = { 1.0, 5.0 };
        const Vec2 v(Values);

        EXPECT_EQ(Values[0], v[0]);
        EXPECT_EQ(Values[1], v[1]);
    }

    TEST_CASE(ConstructVectorWithSingleValue)
    {
        const Vec2 v(7.0);

        EXPECT_EQ(7.0, v[0]);
        EXPECT_EQ(7.0, v[1]);
    }

    TEST_CASE(ConstructVectorWithTwoValues)
    {
        const Vec2 v(1.0, 5.0);

        EXPECT_EQ(1.0, v[0]);
        EXPECT_EQ(5.0, v[1]);
    }

    TEST_CASE(ConstructVectorByTypeConversion)
    {
        static const double Values[] = { 1.0, 5.0 };
        const Vec2 v(Values);
        const Vector<float, 2> vf(v);

        EXPECT_FEQ(static_cast<float>(Values[0]), vf[0]);
        EXPECT_FEQ(static_cast<float>(Values[1]), vf[1]);
    }

    TEST_CASE(TestDeterminant)
    {
        EXPECT_FEQ(17.0, det(Vec2(2.0, 3.0), Vec2(-1.0, 7.0)));
    }
}

TEST_SUITE(Foundation_Math_Vector3)
{
    typedef Vector<double, 3> Vec3;

    TEST_CASE(ConstructVectorWithArrayOfValues)
    {
        static const double Values[] = { 1.0, 5.0, 19.0 };
        const Vec3 v(Values);

        EXPECT_EQ(Values[0], v[0]);
        EXPECT_EQ(Values[1], v[1]);
        EXPECT_EQ(Values[2], v[2]);
    }

    TEST_CASE(ConstructVectorWithSingleValue)
    {
        const Vec3 v(7.0);

        EXPECT_EQ(7.0, v[0]);
        EXPECT_EQ(7.0, v[1]);
        EXPECT_EQ(7.0, v[2]);
    }

    TEST_CASE(ConstructVectorWithTreeValues)
    {
        const Vec3 v(1.0, 5.0, 19.0);

        EXPECT_EQ(1.0, v[0]);
        EXPECT_EQ(5.0, v[1]);
        EXPECT_EQ(19.0, v[2]);
    }

    TEST_CASE(ConstructVectorByTypeConversion)
    {
        static const double Values[] = { 1.0, 5.0, 19.0 };
        const Vec3 v(Values);
        const Vector<float, 3> vf(v);

        EXPECT_FEQ(static_cast<float>(Values[0]), vf[0]);
        EXPECT_FEQ(static_cast<float>(Values[1]), vf[1]);
        EXPECT_FEQ(static_cast<float>(Values[2]), vf[2]);
    }

    TEST_CASE(TestCrossProduct)
    {
        const Vec3 w = cross(Vec3(1.0, 0.0, 0.0), Vec3(0.0, 1.0, 0.0));
        const Vec3 u = cross(Vec3(0.0, 1.0, 0.0), Vec3(0.0, 0.0, 1.0));
        const Vec3 v = cross(Vec3(0.0, 0.0, 1.0), Vec3(1.0, 0.0, 0.0));

        EXPECT_FEQ(Vec3(1.0, 0.0, 0.0), u);
        EXPECT_FEQ(Vec3(0.0, 1.0, 0.0), v);
        EXPECT_FEQ(Vec3(0.0, 0.0, 1.0), w);
    }
}

TEST_SUITE(Foundation_Math_Vector4)
{
    typedef Vector<double, 4> Vec4;

    TEST_CASE(ConstructVectorWithArrayOfValues)
    {
        static const double Values[] = { 1.0, 5.0, 19.0, -100.0 };
        const Vec4 v(Values);

        EXPECT_EQ(Values[0], v[0]);
        EXPECT_EQ(Values[1], v[1]);
        EXPECT_EQ(Values[2], v[2]);
        EXPECT_EQ(Values[3], v[3]);
    }

    TEST_CASE(ConstructVectorWithSingleValue)
    {
        const Vec4 v(7.0);

        EXPECT_EQ(7.0, v[0]);
        EXPECT_EQ(7.0, v[1]);
        EXPECT_EQ(7.0, v[2]);
        EXPECT_EQ(7.0, v[3]);
    }

    TEST_CASE(ConstructVectorWithFourValues)
    {
        const Vec4 v(1.0, 5.0, 19.0, -100.0);

        EXPECT_EQ(1.0, v[0]);
        EXPECT_EQ(5.0, v[1]);
        EXPECT_EQ(19.0, v[2]);
        EXPECT_EQ(-100.0, v[3]);
    }

    TEST_CASE(ConstructVectorByTypeConversion)
    {
        static const double Values[] = { 1.0, 5.0, 19.0, -100.0 };
        const Vec4 v(Values);
        const Vector<float, 4> vf(v);

        EXPECT_FEQ(static_cast<float>(Values[0]), vf[0]);
        EXPECT_FEQ(static_cast<float>(Values[1]), vf[1]);
        EXPECT_FEQ(static_cast<float>(Values[2]), vf[2]);
        EXPECT_FEQ(static_cast<float>(Values[3]), vf[3]);
    }
}
