
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
#include "foundation/math/matrix.h"
#include "foundation/math/vector.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>

using namespace foundation;

TEST_SUITE(Foundation_Math_MatrixMN)
{
    typedef Matrix<double, 2, 3> Mat23;

    static const double Values[] =
    {
        1.0, 2.0, 3.0,
        4.0, 5.0, 6.0
    };

    TEST_CASE(TestProperties)
    {
        EXPECT_EQ(2, Mat23::Rows);
        EXPECT_EQ(3, Mat23::Columns);
        EXPECT_EQ(6, Mat23::Components);
    }

    TEST_CASE(ConstructMatrixWithArrayOfValues)
    {
        const Mat23 m(Values);

        EXPECT_SEQUENCE_EQ(6, Values, &m[0]);
    }

    TEST_CASE(ConstructMatrixWithSingleValue)
    {
        const Mat23 m(42.0);

        for (size_t i = 0; i < 6; ++i)
            EXPECT_EQ(42.0, m[i]);
    }

    TEST_CASE(ConstructMatrixByTypeConversion)
    {
        const Mat23 m(Values);
        const Matrix<float, 2, 3> mf(m);

        for (size_t i = 0; i < 6; ++i)
            EXPECT_FEQ(static_cast<float>(Values[i]), mf[i]);
    }

    TEST_CASE(TestFortranStyleSubscripting)
    {
        const Mat23 m(Values);

        EXPECT_EQ(1.0, m(0, 0));
        EXPECT_EQ(2.0, m(0, 1));
        EXPECT_EQ(3.0, m(0, 2));
        EXPECT_EQ(6.0, m(1, 2));
    }

    TEST_CASE(TestEquality)
    {
        const Mat23 m1(Values);
        const Mat23 m2(Values);
        const Mat23 m3(42.0);

        EXPECT_TRUE(m1 == m2);
        EXPECT_FALSE(m1 == m3);
    }

    TEST_CASE(TestInequality)
    {
        const Mat23 m1(Values);
        const Mat23 m2(Values);
        const Mat23 m3(42.0);

        EXPECT_FALSE(m1 != m2);
        EXPECT_TRUE(m1 != m3);
    }

    TEST_CASE(TestApproximateEquality)
    {
        const Mat23 m1(Values);
        const Mat23 m2(Values);
        const Mat23 m3(42.0);

        EXPECT_TRUE(feq(m1, m2));
        EXPECT_FALSE(feq(m1, m3));
    }

    TEST_CASE(TestComparisonToZero)
    {
        const Mat23 m1(Values);
        const Mat23 m2(0.0);

        EXPECT_FALSE(fz(m1));
        EXPECT_TRUE(fz(m2));
    }

    TEST_CASE(TestAddition)
    {
        static const double OtherValues[] =
        {
            10.0, 20.0, 30.0,
            40.0, 50.0, 60.0
        };

        static const double ExpectedValues[] =
        {
            11.0, 22.0, 33.0,
            44.0, 55.0, 66.0
        };

        EXPECT_FEQ(Mat23(ExpectedValues), Mat23(Values) + Mat23(OtherValues));
    }

    TEST_CASE(TestSubstraction)
    {
        static const double OtherValues[] =
        {
            10.0, 20.0, 30.0,
            40.0, 50.0, 60.0
        };

        static const double ExpectedValues[] =
        {
             9.0, 18.0, 27.0,
            36.0, 45.0, 54.0
        };

        EXPECT_FEQ(Mat23(ExpectedValues), Mat23(OtherValues) - Mat23(Values));
    }

    TEST_CASE(TestNegation)
    {
        static const double ExpectedValues[] =
        {
            -1.0, -2.0, -3.0,
            -4.0, -5.0, -6.0
        };

        EXPECT_EQ(Mat23(ExpectedValues), -Mat23(Values));
    }

    TEST_CASE(TestRightMultiplicationByScalar)
    {
        static const double ExpectedValues[] =
        {
             2.0,  4.0,  6.0,
             8.0, 10.0, 12.0
        };

        EXPECT_FEQ(Mat23(ExpectedValues), Mat23(Values) * 2.0);
    }

    TEST_CASE(TestLeftMultiplicationByScalar)
    {
        static const double ExpectedValues[] =
        {
             2.0,  4.0,  6.0,
             8.0, 10.0, 12.0
        };

        EXPECT_FEQ(Mat23(ExpectedValues), 2.0 * Mat23(Values));
    }

    TEST_CASE(TestDivisionByScalar)
    {
        static const double ExpectedValues[] =
        {
             0.5, 1.0, 1.5,
             2.0, 2.5, 3.0
        };

        EXPECT_FEQ(Mat23(ExpectedValues), Mat23(Values) / 2.0);
    }

    TEST_CASE(TestInPlaceAddition)
    {
        static const double OtherValues[] =
        {
            10.0, 20.0, 30.0,
            40.0, 50.0, 60.0
        };

        static const double ExpectedValues[] =
        {
            11.0, 22.0, 33.0,
            44.0, 55.0, 66.0
        };

        Mat23 m(Values);
        m += Mat23(OtherValues);

        EXPECT_FEQ(Mat23(ExpectedValues), m);
    }

    TEST_CASE(TestInPlaceSubstraction)
    {
        static const double OtherValues[] =
        {
            10.0, 20.0, 30.0,
            40.0, 50.0, 60.0
        };

        static const double ExpectedValues[] =
        {
             9.0, 18.0, 27.0,
            36.0, 45.0, 54.0
        };

        Mat23 m(OtherValues);
        m -= Mat23(Values);

        EXPECT_FEQ(Mat23(ExpectedValues), m);
    }

    TEST_CASE(TestInPlaceMultiplicationByScalar)
    {
        static const double ExpectedValues[] =
        {
             2.0,  4.0,  6.0,
             8.0, 10.0, 12.0
        };

        Mat23 m(Values);
        m *= 2.0;

        EXPECT_FEQ(Mat23(ExpectedValues), m);
    }

    TEST_CASE(TestInPlaceDivisionByScalar)
    {
        static const double ExpectedValues[] =
        {
             0.5, 1.0, 1.5,
             2.0, 2.5, 3.0
        };

        Mat23 m(Values);
        m /= 2.0;

        EXPECT_FEQ(Mat23(ExpectedValues), m);
    }

    TEST_CASE(TestMatrixMatrixMultiplication)
    {
        // 4x3
        static const double LhsValues[] =
        {
             2.0,  7.0, -5.0,
             5.0, -6.0, -2.0,
             4.0, -1.0,  7.0,
            -1.0, -7.0, -8.0
        };

        // 3x2
        static const double RhsValues[] =
        {
            -5.0, -1.0,
             3.0, -4.0,
             9.0, -6.0
        };

        // 4x2
        static const double ExpectedValues[] =
        {
            -34.0,   0.0,
            -61.0,  31.0,
             40.0, -42.0,
            -88.0,  77.0
        };

        const Matrix<double, 4, 2> Expected(ExpectedValues);

        const Matrix<double, 4, 2> Received =
            Matrix<double, 4, 3>(LhsValues) * Matrix<double, 3, 2>(RhsValues);

        EXPECT_FEQ(Expected, Received);
    }

    TEST_CASE(TestMatrixTransposition)
    {
        static const double ExpectedValues[] =
        {
            1.0, 4.0,
            2.0, 5.0,
            3.0, 6.0
        };

        const Mat23 m(Values);

        typedef Matrix<double, 3, 2> Mat32d;

        EXPECT_EQ(Mat32d(ExpectedValues), transpose(m));
    }
}

TEST_SUITE(Foundation_Math_MatrixNN)
{
    typedef Matrix<double, 5, 5> Mat55;

    static const double Values[] =
    {
         1.0,  2.0,  3.0,  4.0,  5.0,
         6.0,  7.0,  8.0,  9.0, 10.0,
        11.0, 12.0, 13.0, 14.0, 15.0,
        16.0, 17.0, 18.0, 19.0, 20.0,
        21.0, 22.0, 23.0, 24.0, 25.0
    };

    TEST_CASE(TestProperties)
    {
        EXPECT_EQ(5, Mat55::Rows);
        EXPECT_EQ(5, Mat55::Columns);
        EXPECT_EQ(25, Mat55::Components);
    }

    TEST_CASE(ConstructMatrixWithArrayOfValues)
    {
        const Mat55 m(Values);

        EXPECT_SEQUENCE_EQ(25, Values, &m[0]);
    }

    TEST_CASE(ConstructMatrixWithSingleValue)
    {
        const Mat55 m(42.0);

        for (size_t i = 0; i < 25; ++i)
            EXPECT_EQ(42.0, m[i]);
    }

    TEST_CASE(ConstructMatrixByTypeConversion)
    {
        const Mat55 m(Values);
        const Matrix<float, 5, 5> mf(m);

        for (size_t i = 0; i < 25; ++i)
            EXPECT_FEQ(static_cast<float>(Values[i]), mf[i]);
    }

    TEST_CASE(ConstructIdentityMatrix)
    {
        static const double Values[] =
        {
             1.0, 0.0, 0.0, 0.0, 0.0,
             0.0, 1.0, 0.0, 0.0, 0.0,
             0.0, 0.0, 1.0, 0.0, 0.0,
             0.0, 0.0, 0.0, 1.0, 0.0,
             0.0, 0.0, 0.0, 0.0, 1.0
        };

        const Mat55 m(Mat55::identity());

        EXPECT_SEQUENCE_EQ(25, Values, &m[0]);
    }

    TEST_CASE(TestFortranStyleSubscripting)
    {
        const Mat55 m(Values);

        EXPECT_EQ(1.0, m(0, 0));
        EXPECT_EQ(2.0, m(0, 1));
        EXPECT_EQ(3.0, m(0, 2));
        EXPECT_EQ(25.0, m(4, 4));
    }

    TEST_CASE(TestInversionOfInvertibleMatrix)
    {
        static const double Values[] =
        {
            5.0, 3.0, 7.0,
            2.0, 4.0, 9.0,
            3.0, 6.0, 4.0
        };

        static const double ExpectedValues[] =
        {
            2.0 / 7, -30.0 / 133, 1.0 / 133,
            -1.0 / 7, 1.0 / 133, 31.0 / 133,
            0.0, 3.0 / 19, -2.0 / 19
        };

        typedef Matrix<double, 3, 3> Mat33;

        EXPECT_FEQ(Matrix3d(ExpectedValues), inverse(Matrix3d(Values)));
    }

    TEST_CASE(TestInversionOfSingularMatrix)
    {
        static const double Values[] =
        {
            1.0, 2.0, 3.0,
            4.0, 5.0, 6.0,
            7.0, 8.0, 9.0
        };

        EXPECT_EXCEPTION(ExceptionSingularMatrix,
        {
            inverse(Matrix<double, 3, 3>(Values));
        });
    }
}

TEST_SUITE(Foundation_Math_Matrix33)
{
    static const double Values[] =
    {
        1.0, 2.0, 3.0,
        4.0, 5.0, 6.0,
        7.0, 8.0, 9.0
    };

    TEST_CASE(TestProperties)
    {
        EXPECT_EQ(3, Matrix3d::Rows);
        EXPECT_EQ(3, Matrix3d::Columns);
        EXPECT_EQ(9, Matrix3d::Components);
    }

    TEST_CASE(ConstructMatrixWithArrayOfValues)
    {
        const Matrix3d m(Values);

        EXPECT_SEQUENCE_EQ(9, Values, &m[0]);
    }

    TEST_CASE(ConstructMatrixWithSingleValue)
    {
        const Matrix3d m(42.0);

        for (size_t i = 0; i < 9; ++i)
            EXPECT_EQ(42.0, m[i]);
    }

    TEST_CASE(ConstructMatrixByTypeConversion)
    {
        const Matrix3d m(Values);
        const Matrix<float, 3, 3> mf(m);

        for (size_t i = 0; i < 9; ++i)
            EXPECT_FEQ(static_cast<float>(Values[i]), mf[i]);
    }

    TEST_CASE(ConstructIdentityMatrix)
    {
        static const double Values[] =
        {
             1.0, 0.0, 0.0,
             0.0, 1.0, 0.0,
             0.0, 0.0, 1.0
        };

        const Matrix3d m(Matrix3d::identity());

        EXPECT_SEQUENCE_EQ(9, Values, &m[0]);
    }

    TEST_CASE(TestFortranStyleSubscripting)
    {
        const Matrix3d m(Values);

        EXPECT_EQ(1.0, m(0, 0));
        EXPECT_EQ(2.0, m(0, 1));
        EXPECT_EQ(3.0, m(0, 2));
        EXPECT_EQ(9.0, m(2, 2));
    }

    TEST_CASE(TestMatrixMatrixMultiplication)
    {
        static const double LhsValues[] =
        {
            -5.0,  2.0,  3.0,
            -8.0, -7.0,  4.0,
             1.0,  6.0, -2.0
        };

        static const double RhsValues[] =
        {
            -5.0, -1.0,  1.0,
             3.0, -4.0,  8.0,
             9.0, -6.0, -9.0
        };

        static const double ExpectedValues[] =
        {
            58.0, -21.0,  -16.0,
            55.0,  12.0, -100.0,
            -5.0, -13.0,   67.0
        };

        EXPECT_FEQ(Matrix3d(ExpectedValues), Matrix3d(LhsValues) * Matrix3d(RhsValues));
    }
}

TEST_SUITE(Foundation_Math_Matrix44)
{
    static const double Values[] =
    {
         1.0,  2.0,  3.0,  4.0,
         5.0,  6.0,  7.0,  8.0,
         9.0, 10.0, 11.0, 12.0,
        13.0, 14.0, 15.0, 16.0
    };

    TEST_CASE(TestProperties)
    {
        EXPECT_EQ(4, Matrix4d::Rows);
        EXPECT_EQ(4, Matrix4d::Columns);
        EXPECT_EQ(16, Matrix4d::Components);
    }

    TEST_CASE(ConstructMatrixWithArrayOfValues)
    {
        const Matrix4d m(Values);

        EXPECT_SEQUENCE_EQ(16, Values, &m[0]);
    }

    TEST_CASE(ConstructMatrixWithSingleValue)
    {
        const Matrix4d m(42.0);

        for (size_t i = 0; i < 16; ++i)
            EXPECT_EQ(42.0, m[i]);
    }

    TEST_CASE(ConstructMatrixByTypeConversion)
    {
        const Matrix4d m(Values);
        const Matrix<float, 4, 4> mf(m);

        for (size_t i = 0; i < 16; ++i)
            EXPECT_FEQ(static_cast<float>(Values[i]), mf[i]);
    }

    TEST_CASE(ConstructIdentityMatrix)
    {
        static const double Values[] =
        {
             1.0, 0.0, 0.0, 0.0,
             0.0, 1.0, 0.0, 0.0,
             0.0, 0.0, 1.0, 0.0,
             0.0, 0.0, 0.0, 1.0
        };

        const Matrix4d m(Matrix4d::identity());

        EXPECT_SEQUENCE_EQ(16, Values, &m[0]);
    }

    TEST_CASE(TestFortranStyleSubscripting)
    {
        const Matrix4d m(Values);

        EXPECT_EQ(1.0, m(0, 0));
        EXPECT_EQ(2.0, m(0, 1));
        EXPECT_EQ(3.0, m(0, 2));
        EXPECT_EQ(16.0, m(3, 3));
    }

    TEST_CASE(TestMatrixMatrixMultiplication)
    {
        static const double LhsValues[] =
        {
            -7.0, -5.0,  5.0, -8.0,
             6.0,  1.0,  9.0,  3.0,
             8.0, -3.0,  7.0,  2.0,
            -9.0,  4.0, -4.0, -6.0
        };

        static const double RhsValues[] =
        {
             4.0,  1.0, -9.0,  7.0,
            -5.0,  6.0, -6.0, -4.0,
             5.0,  9.0,  2.0, -2.0,
            -3.0,  8.0, -1.0,  3.0
        };

        static const double ExpectedValues[] =
        {
             46.0, -56.0, 111.0, -63.0,
             55.0, 117.0, -45.0,  29.0,
             76.0,  69.0, -42.0,  60.0,
            -58.0, -69.0,  55.0, -89.0
        };

        EXPECT_FEQ(Matrix4d(ExpectedValues), Matrix4d(LhsValues) * Matrix4d(RhsValues));
    }

    TEST_CASE(TestMatrixVectorMultiplication)
    {
        static const double Values[] =
        {
            -38.0,  23.0, 14.0,  58.0,
            -92.0,  -8.0, 36.0, -90.0,
             96.0, -19.0, -4.0,  38.0,
            -99.0, -67.0, 68.0,  45.0
        };

        const Vector<double, 4> Vec(73.0, 76.0, -68.0, 67.0);
        const Vector<double, 4> Expected(1908.0, -15802.0, 8382.0, -13928.0);

        EXPECT_FEQ(Expected, Matrix4d(Values) * Vec);
    }

    TEST_CASE(TestVectorMatrixMultiplication)
    {
        static const double Values[] =
        {
            -99.0, -67.0, 68.0,  45.0,
             76.0,   6.0, 72.0, -28.0,
            -61.0, -59.0,  6.0, -87.0,
            -38.0,  23.0, 14.0,  58.0
        };

        const Vector<double, 4> Vec(11.0, 93.0, -14.0, 73.0);
        const Vector<double, 4> Expected(-4987.0, -1658.0, -12593.0, 5759.0);

        EXPECT_FEQ(Expected, Vec * Matrix4d(Values));
    }
}
