
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

FOUNDATION_TEST_SUITE(Foundation_Math_Vector)
{
    using namespace foundation;
    using namespace std;

    typedef Vector<double, 2> Vec2;
    typedef Vector<double, 3> Vec3;
    typedef Vector<double, 5> Vec5;

    FOUNDATION_TEST_CASE(ConstructVectorWithArrayOfValues)
    {
        static const double Values[] = { 1.0, 5.0, 19.0, -4.0, -100.0 };
        const Vec5 v(Values);

        FOUNDATION_EXPECT_EQ(Values[0], v[0]);
        FOUNDATION_EXPECT_EQ(Values[1], v[1]);
        FOUNDATION_EXPECT_EQ(Values[2], v[2]);
        FOUNDATION_EXPECT_EQ(Values[3], v[3]);
        FOUNDATION_EXPECT_EQ(Values[4], v[4]);
    }

    FOUNDATION_TEST_CASE(ConstructVectorWithSingleValue)
    {
        const Vec5 v(7.0);

        FOUNDATION_EXPECT_EQ(7.0, v[0]);
        FOUNDATION_EXPECT_EQ(7.0, v[1]);
        FOUNDATION_EXPECT_EQ(7.0, v[2]);
        FOUNDATION_EXPECT_EQ(7.0, v[3]);
        FOUNDATION_EXPECT_EQ(7.0, v[4]);
    }

    FOUNDATION_TEST_CASE(ConstructVectorByTypeConversion)
    {
        static const double Values[] = { 1.0, 5.0, 19.0, -4.0, -100.0 };
        const Vec5 v(Values);
        const Vector<float, 5> vf(v);

        FOUNDATION_EXPECT_FEQ(static_cast<float>(Values[0]), vf[0]);
        FOUNDATION_EXPECT_FEQ(static_cast<float>(Values[1]), vf[1]);
        FOUNDATION_EXPECT_FEQ(static_cast<float>(Values[2]), vf[2]);
        FOUNDATION_EXPECT_FEQ(static_cast<float>(Values[3]), vf[3]);
        FOUNDATION_EXPECT_FEQ(static_cast<float>(Values[4]), vf[4]);
    }

    FOUNDATION_TEST_CASE(TestEquality)
    {
        const Vec3 u(1.0, 5.0, 19.0);
        const Vec3 v(1.0, 5.0, 19.0);
        const Vec3 a(1.0, 5.0,  0.0);
        const Vec3 b(1.0, 0.0, 19.0);
        const Vec3 c(1.0, 5.0,  0.0);

        FOUNDATION_EXPECT_TRUE(u == v);
        FOUNDATION_EXPECT_FALSE(u == a);
        FOUNDATION_EXPECT_FALSE(u == b);
        FOUNDATION_EXPECT_FALSE(u == c);
    }

    FOUNDATION_TEST_CASE(TestInequality)
    {
        const Vec3 u(1.0, 5.0, 19.0);
        const Vec3 v(1.0, 5.0, 19.0);
        const Vec3 a(0.0, 5.0, 19.0);
        const Vec3 b(1.0, 0.0, 19.0);
        const Vec3 c(1.0, 5.0,  0.0);

        FOUNDATION_EXPECT_FALSE(u != v);
        FOUNDATION_EXPECT_TRUE(u != a);
        FOUNDATION_EXPECT_TRUE(u != b);
        FOUNDATION_EXPECT_TRUE(u != c);
    }

    FOUNDATION_TEST_CASE(TestApproximateEquality)
    {
        const Vec3 u(1.0, 5.0, 19.0);
        const Vec3 v(1.0, 5.0, 19.0);
        const Vec3 a(0.0, 5.0, 19.0);
        const Vec3 b(1.0, 0.0, 19.0);
        const Vec3 c(1.0, 5.0,  0.0);

        FOUNDATION_EXPECT_TRUE(feq(u, v));
        FOUNDATION_EXPECT_FALSE(feq(u, a));
        FOUNDATION_EXPECT_FALSE(feq(u, b));
        FOUNDATION_EXPECT_FALSE(feq(u, c));
    }

    FOUNDATION_TEST_CASE(TestComparisonToZero)
    {
        const Vec3 v(1.0, 5.0, 19.0);
        const Vec3 a(0.0, 5.0, 19.0);
        const Vec3 b(0.0, 0.0,  0.0);

        FOUNDATION_EXPECT_FALSE(fz(v));
        FOUNDATION_EXPECT_FALSE(fz(a));
        FOUNDATION_EXPECT_TRUE(fz(b));
    }

    FOUNDATION_TEST_CASE(TestAddition)
    {
        FOUNDATION_EXPECT_FEQ(Vec3(5.0, 7.0, 9.0), Vec3(1.0, 2.0, 3.0) + Vec3(4.0, 5.0, 6.0));
    }

    FOUNDATION_TEST_CASE(TestSubstraction)
    {
        FOUNDATION_EXPECT_FEQ(Vec3(1.0, 3.0, 5.0), Vec3(4.0, 5.0, 6.0) - Vec3(3.0, 2.0, 1.0));
    }

    FOUNDATION_TEST_CASE(TestNegation)
    {
        FOUNDATION_EXPECT_FEQ(Vec3(-1.0, -2.0, -3.0), -Vec3(1.0, 2.0, 3.0));
    }

    FOUNDATION_TEST_CASE(TestRightMultiplicationByScalar)
    {
        FOUNDATION_EXPECT_FEQ(Vec3(2.0, 4.0, 6.0), Vec3(1.0, 2.0, 3.0) * 2.0);
    }

    FOUNDATION_TEST_CASE(TestLeftMultiplicationByScalar)
    {
        FOUNDATION_EXPECT_FEQ(Vec3(2.0, 4.0, 6.0), 2.0 * Vec3(1.0, 2.0, 3.0));
    }

    FOUNDATION_TEST_CASE(TestDivisionByScalar)
    {
        FOUNDATION_EXPECT_FEQ(Vec3(1.0, 2.0, 3.0), Vec3(2.0, 4.0, 6.0) / 2.0);
    }

    FOUNDATION_TEST_CASE(TestMultiplicationByVector)
    {
        FOUNDATION_EXPECT_FEQ(Vec3(6.0, 20.0, 42.0), Vec3(2.0, 4.0, 6.0) * Vec3(3.0, 5.0, 7.0));
    }

    FOUNDATION_TEST_CASE(TestDivisionByVector)
    {
        FOUNDATION_EXPECT_FEQ(Vec3(1.0, 2.0, 3.0), Vec3(2.0, 4.0, 6.0) / Vec3(2.0, 2.0, 2.0));
    }

    FOUNDATION_TEST_CASE(TestInPlaceAddition)
    {
        Vec3 v(1.0, 2.0, 3.0); v += Vec3(4.0, 5.0, 6.0);
        FOUNDATION_EXPECT_FEQ(Vec3(5.0, 7.0, 9.0), v);
    }

    FOUNDATION_TEST_CASE(TestInPlaceSubstraction)
    {
        Vec3 v(4.0, 5.0, 6.0); v -= Vec3(3.0, 2.0, 1.0);
        FOUNDATION_EXPECT_FEQ(Vec3(1.0, 3.0, 5.0), v);
    }

    FOUNDATION_TEST_CASE(TestInPlaceMultiplicationByScalar)
    {
        Vec3 v(1.0, 2.0, 3.0); v *= 2.0;
        FOUNDATION_EXPECT_FEQ(Vec3(2.0, 4.0, 6.0), v);
    }

    FOUNDATION_TEST_CASE(TestInPlaceDivisionByScalar)
    {
        Vec3 v(2.0, 4.0, 6.0); v /= 2.0;
        FOUNDATION_EXPECT_FEQ(Vec3(1.0, 2.0, 3.0), v);
    }

    FOUNDATION_TEST_CASE(TestInPlaceMultiplicationByVector)
    {
        Vec3 v(2.0, 4.0, 6.0); v *= Vec3(3.0, 5.0, 7.0);
        FOUNDATION_EXPECT_FEQ(Vec3(6.0, 20.0, 42.0), v);
    }

    FOUNDATION_TEST_CASE(TestInPlaceDivisionByVector)
    {
        Vec3 v(2.0, 4.0, 6.0); v /= Vec3(2.0, 2.0, 2.0);
        FOUNDATION_EXPECT_FEQ(Vec3(1.0, 2.0, 3.0), v);
    }

    FOUNDATION_TEST_CASE(TestDotProduct)
    {
        FOUNDATION_EXPECT_FEQ(68.0, dot(Vec3(2.0, 4.0, 6.0), Vec3(3.0, 5.0, 7.0)));
    }

    FOUNDATION_TEST_CASE(TestSquareNorm)
    {
        FOUNDATION_EXPECT_EQ(0.0, square_norm(Vec3(0.0)));
        FOUNDATION_EXPECT_FEQ(56.0, square_norm(Vec3(2.0, 4.0, 6.0)));
    }

    FOUNDATION_TEST_CASE(TestNorm)
    {
        FOUNDATION_EXPECT_EQ(0.0, norm(Vec3(0.0)));
        FOUNDATION_EXPECT_FEQ(sqrt(56.0), norm(Vec3(2.0, 4.0, 6.0)));
    }

    FOUNDATION_TEST_CASE(TestNormalize)
    {
        FOUNDATION_EXPECT_FEQ(Vec3(1.0, 0.0, 0.0), normalize(Vec3(4.0, 0.0, 0.0)));
        FOUNDATION_EXPECT_FEQ(Vec3(0.0, 1.0, 0.0), normalize(Vec3(0.0, 4.0, 0.0)));
        FOUNDATION_EXPECT_FEQ(Vec3(0.0, 0.0, 1.0), normalize(Vec3(0.0, 0.0, 4.0)));
    }

    FOUNDATION_TEST_CASE(TestIsNormalized)
    {
        const Vec3 v(3.0, -5.0, 7.0);

        FOUNDATION_EXPECT_FALSE(is_normalized(v));
        FOUNDATION_EXPECT_TRUE(is_normalized(normalize(v)));

        FOUNDATION_EXPECT_FALSE(is_normalized(Vec3(0.0)));
        FOUNDATION_EXPECT_TRUE(is_normalized(Vec3(1.0, 0.0, 0.0)));
        FOUNDATION_EXPECT_TRUE(is_normalized(Vec3(0.0, 1.0, 0.0)));
        FOUNDATION_EXPECT_TRUE(is_normalized(Vec3(0.0, 0.0, 1.0)));
    }

    FOUNDATION_TEST_CASE(TestReflect)
    {
        const Vec3 i(1.0, 2.0, 3.0);
        const Vec3 n(0.0, 1.0, 0.0);

        const Vec3 r = reflect(i, n);

        FOUNDATION_EXPECT_FEQ(Vec3(-1.0, 2.0, -3.0), r);
    }

    FOUNDATION_TEST_CASE(TestRefract_IncidentDirectionAndNormalAreInTheSameHemisphere)
    {
        const Vec2 i = normalize(Vec2(-1.0, 1.0));
        const Vec2 n(0.0, 1.0);
        const double eta = 0.9;

        const Vec2 r = refract(i, n, eta);

        FOUNDATION_EXPECT_FEQ(Vec2(0.63639610306789274, -0.77136243102707558), r);
    }

    FOUNDATION_TEST_CASE(TestRefract_IncidentDirectionAndNormalAreInOppositeHemispheres)
    {
        const Vec2 i = normalize(Vec2(-1.0, 1.0));
        const Vec2 n(0.0, -1.0);
        const double eta = 0.9;

        const Vec2 r = refract(i, n, eta);

        FOUNDATION_EXPECT_FEQ(Vec2(0.63639610306789274, -0.77136243102707558), r);
    }

    FOUNDATION_TEST_CASE(TestClamp)
    {
        const Vec3 v(-1.0, 2.0, 3.0);

        FOUNDATION_EXPECT_EQ(Vec3(0.0), clamp(v, 0.0, 0.0));
        FOUNDATION_EXPECT_EQ(Vec3(2.0), clamp(v, 2.0, 2.0));
        FOUNDATION_EXPECT_EQ(Vec3(0.0, 2.0, 2.0), clamp(v, 0.0, 2.0));
    }

    FOUNDATION_TEST_CASE(TestSaturate)
    {
        FOUNDATION_EXPECT_EQ(Vec3(0.0, 1.0, 1.0), saturate(Vec3(-1.0, 2.0, 3.0)));
    }

    FOUNDATION_TEST_CASE(TestMinValue)
    {
        FOUNDATION_EXPECT_EQ(1.0, min_value(Vec3(1.0, 2.0, 3.0)));
        FOUNDATION_EXPECT_EQ(1.0, min_value(Vec3(2.0, 1.0, 3.0)));
        FOUNDATION_EXPECT_EQ(1.0, min_value(Vec3(3.0, 2.0, 1.0)));
    }

    FOUNDATION_TEST_CASE(TestMaxValue)
    {
        FOUNDATION_EXPECT_EQ(3.0, max_value(Vec3(1.0, 2.0, 3.0)));
        FOUNDATION_EXPECT_EQ(3.0, max_value(Vec3(2.0, 1.0, 3.0)));
        FOUNDATION_EXPECT_EQ(3.0, max_value(Vec3(3.0, 2.0, 1.0)));
    }

    FOUNDATION_TEST_CASE(TestMinIndex)
    {
        FOUNDATION_EXPECT_EQ(0, min_index(Vec3(1.0, 2.0, 3.0)));
        FOUNDATION_EXPECT_EQ(1, min_index(Vec3(2.0, 1.0, 3.0)));
        FOUNDATION_EXPECT_EQ(2, min_index(Vec3(3.0, 2.0, 1.0)));
    }

    FOUNDATION_TEST_CASE(TestMaxIndex)
    {
        FOUNDATION_EXPECT_EQ(0, max_index(Vec3(3.0, 2.0, 1.0)));
        FOUNDATION_EXPECT_EQ(1, max_index(Vec3(2.0, 3.0, 1.0)));
        FOUNDATION_EXPECT_EQ(2, max_index(Vec3(1.0, 2.0, 3.0)));
    }

    FOUNDATION_TEST_CASE(TestMinAbsIndex)
    {
        FOUNDATION_EXPECT_EQ(0, min_abs_index(Vec3(-1.0, -2.0, -3.0)));
        FOUNDATION_EXPECT_EQ(1, min_abs_index(Vec3(-2.0, -1.0, -3.0)));
        FOUNDATION_EXPECT_EQ(2, min_abs_index(Vec3(-3.0, -2.0, -1.0)));
    }

    FOUNDATION_TEST_CASE(TestMaxAbsIndex)
    {
        FOUNDATION_EXPECT_EQ(0, max_abs_index(Vec3(-3.0, -2.0, -1.0)));
        FOUNDATION_EXPECT_EQ(1, max_abs_index(Vec3(-2.0, -3.0, -1.0)));
        FOUNDATION_EXPECT_EQ(2, max_abs_index(Vec3(-1.0, -2.0, -3.0)));
    }
}

FOUNDATION_TEST_SUITE(Foundation_Math_Vector2)
{
    using namespace foundation;
    using namespace std;

    typedef Vector<double, 2> Vec2;

    FOUNDATION_TEST_CASE(ConstructVectorWithArrayOfValues)
    {
        static const double Values[] = { 1.0, 5.0 };
        const Vec2 v(Values);

        FOUNDATION_EXPECT_EQ(Values[0], v[0]);
        FOUNDATION_EXPECT_EQ(Values[1], v[1]);
    }

    FOUNDATION_TEST_CASE(ConstructVectorWithSingleValue)
    {
        const Vec2 v(7.0);

        FOUNDATION_EXPECT_EQ(7.0, v[0]);
        FOUNDATION_EXPECT_EQ(7.0, v[1]);
    }

    FOUNDATION_TEST_CASE(ConstructVectorWithTwoValues)
    {
        const Vec2 v(1.0, 5.0);

        FOUNDATION_EXPECT_EQ(1.0, v[0]);
        FOUNDATION_EXPECT_EQ(5.0, v[1]);
    }

    FOUNDATION_TEST_CASE(ConstructVectorByTypeConversion)
    {
        static const double Values[] = { 1.0, 5.0 };
        const Vec2 v(Values);
        const Vector<float, 2> vf(v);

        FOUNDATION_EXPECT_FEQ(static_cast<float>(Values[0]), vf[0]);
        FOUNDATION_EXPECT_FEQ(static_cast<float>(Values[1]), vf[1]);
    }

    FOUNDATION_TEST_CASE(TestDeterminant)
    {
        FOUNDATION_EXPECT_FEQ(17.0, det(Vec2(2.0, 3.0), Vec2(-1.0, 7.0)));
    }
}

FOUNDATION_TEST_SUITE(Foundation_Math_Vector3)
{
    using namespace foundation;
    using namespace std;

    typedef Vector<double, 3> Vec3;

    FOUNDATION_TEST_CASE(ConstructVectorWithArrayOfValues)
    {
        static const double Values[] = { 1.0, 5.0, 19.0 };
        const Vec3 v(Values);

        FOUNDATION_EXPECT_EQ(Values[0], v[0]);
        FOUNDATION_EXPECT_EQ(Values[1], v[1]);
        FOUNDATION_EXPECT_EQ(Values[2], v[2]);
    }

    FOUNDATION_TEST_CASE(ConstructVectorWithSingleValue)
    {
        const Vec3 v(7.0);

        FOUNDATION_EXPECT_EQ(7.0, v[0]);
        FOUNDATION_EXPECT_EQ(7.0, v[1]);
        FOUNDATION_EXPECT_EQ(7.0, v[2]);
    }

    FOUNDATION_TEST_CASE(ConstructVectorWithTreeValues)
    {
        const Vec3 v(1.0, 5.0, 19.0);

        FOUNDATION_EXPECT_EQ(1.0, v[0]);
        FOUNDATION_EXPECT_EQ(5.0, v[1]);
        FOUNDATION_EXPECT_EQ(19.0, v[2]);
    }

    FOUNDATION_TEST_CASE(ConstructVectorByTypeConversion)
    {
        static const double Values[] = { 1.0, 5.0, 19.0 };
        const Vec3 v(Values);
        const Vector<float, 3> vf(v);

        FOUNDATION_EXPECT_FEQ(static_cast<float>(Values[0]), vf[0]);
        FOUNDATION_EXPECT_FEQ(static_cast<float>(Values[1]), vf[1]);
        FOUNDATION_EXPECT_FEQ(static_cast<float>(Values[2]), vf[2]);
    }

    FOUNDATION_TEST_CASE(TestCrossProduct)
    {
        const Vec3 w = cross(Vec3(1.0, 0.0, 0.0), Vec3(0.0, 1.0, 0.0));
        const Vec3 u = cross(Vec3(0.0, 1.0, 0.0), Vec3(0.0, 0.0, 1.0));
        const Vec3 v = cross(Vec3(0.0, 0.0, 1.0), Vec3(1.0, 0.0, 0.0));

        FOUNDATION_EXPECT_FEQ(Vec3(1.0, 0.0, 0.0), u);
        FOUNDATION_EXPECT_FEQ(Vec3(0.0, 1.0, 0.0), v);
        FOUNDATION_EXPECT_FEQ(Vec3(0.0, 0.0, 1.0), w);
    }
}

FOUNDATION_TEST_SUITE(Foundation_Math_Vector4)
{
    using namespace foundation;
    using namespace std;

    typedef Vector<double, 4> Vec4;

    FOUNDATION_TEST_CASE(ConstructVectorWithArrayOfValues)
    {
        static const double Values[] = { 1.0, 5.0, 19.0, -100.0 };
        const Vec4 v(Values);

        FOUNDATION_EXPECT_EQ(Values[0], v[0]);
        FOUNDATION_EXPECT_EQ(Values[1], v[1]);
        FOUNDATION_EXPECT_EQ(Values[2], v[2]);
        FOUNDATION_EXPECT_EQ(Values[3], v[3]);
    }

    FOUNDATION_TEST_CASE(ConstructVectorWithSingleValue)
    {
        const Vec4 v(7.0);

        FOUNDATION_EXPECT_EQ(7.0, v[0]);
        FOUNDATION_EXPECT_EQ(7.0, v[1]);
        FOUNDATION_EXPECT_EQ(7.0, v[2]);
        FOUNDATION_EXPECT_EQ(7.0, v[3]);
    }

    FOUNDATION_TEST_CASE(ConstructVectorWithFourValues)
    {
        const Vec4 v(1.0, 5.0, 19.0, -100.0);

        FOUNDATION_EXPECT_EQ(1.0, v[0]);
        FOUNDATION_EXPECT_EQ(5.0, v[1]);
        FOUNDATION_EXPECT_EQ(19.0, v[2]);
        FOUNDATION_EXPECT_EQ(-100.0, v[3]);
    }

    FOUNDATION_TEST_CASE(ConstructVectorByTypeConversion)
    {
        static const double Values[] = { 1.0, 5.0, 19.0, -100.0 };
        const Vec4 v(Values);
        const Vector<float, 4> vf(v);

        FOUNDATION_EXPECT_FEQ(static_cast<float>(Values[0]), vf[0]);
        FOUNDATION_EXPECT_FEQ(static_cast<float>(Values[1]), vf[1]);
        FOUNDATION_EXPECT_FEQ(static_cast<float>(Values[2]), vf[2]);
        FOUNDATION_EXPECT_FEQ(static_cast<float>(Values[3]), vf[3]);
    }
}
