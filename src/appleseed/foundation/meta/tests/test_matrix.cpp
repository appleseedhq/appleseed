
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "foundation/math/quaternion.h"
#include "foundation/math/scalar.h"
#include "foundation/math/vector.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

// Imath headers.
#ifdef APPLESEED_ENABLE_IMATH_INTEROP
#include "foundation/platform/exrheaderguards.h"
BEGIN_EXR_INCLUDES
#include "OpenEXR/ImathMatrix.h"
END_EXR_INCLUDES
#endif

// Standard headers.
#include <cstddef>

using namespace foundation;

TEST_SUITE(Foundation_Math_MatrixMN)
{
    typedef Matrix<double, 2, 3> Mat23d;
    typedef Matrix<double, 3, 2> Mat32d;

    static const double Values[] =
    {
        1.0, 2.0, 3.0,
        4.0, 5.0, 6.0
    };

    TEST_CASE(TestProperties)
    {
        EXPECT_EQ(2, Mat23d::Rows);
        EXPECT_EQ(3, Mat23d::Columns);
        EXPECT_EQ(6, Mat23d::Components);
    }

    TEST_CASE(ConstructMatrixWithArrayOfValues)
    {
        const Mat23d m(Values);

        EXPECT_SEQUENCE_EQ(6, Values, &m[0]);
    }

    TEST_CASE(ConstructMatrixWithSingleValue)
    {
        const Mat23d m(42.0);

        for (size_t i = 0; i < 6; ++i)
            EXPECT_EQ(42.0, m[i]);
    }

    TEST_CASE(ConstructMatrixByTypeConversion)
    {
        const Mat23d m(Values);
        const Matrix<float, 2, 3> mf(m);

        for (size_t i = 0; i < 6; ++i)
            EXPECT_FEQ(static_cast<float>(Values[i]), mf[i]);
    }

    TEST_CASE(TestFortranStyleSubscripting)
    {
        const Mat23d m(Values);

        EXPECT_EQ(1.0, m(0, 0));
        EXPECT_EQ(2.0, m(0, 1));
        EXPECT_EQ(3.0, m(0, 2));
        EXPECT_EQ(6.0, m(1, 2));
    }

    TEST_CASE(TestEquality)
    {
        const Mat23d m1(Values);
        const Mat23d m2(Values);
        const Mat23d m3(42.0);

        EXPECT_TRUE(m1 == m2);
        EXPECT_FALSE(m1 == m3);
    }

    TEST_CASE(TestInequality)
    {
        const Mat23d m1(Values);
        const Mat23d m2(Values);
        const Mat23d m3(42.0);

        EXPECT_FALSE(m1 != m2);
        EXPECT_TRUE(m1 != m3);
    }

    TEST_CASE(TestApproximateEquality)
    {
        const Mat23d m1(Values);
        const Mat23d m2(Values);
        const Mat23d m3(42.0);

        EXPECT_TRUE(feq(m1, m2));
        EXPECT_FALSE(feq(m1, m3));
    }

    TEST_CASE(TestComparisonToZero)
    {
        const Mat23d m1(Values);
        const Mat23d m2(0.0);

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

        EXPECT_FEQ(Mat23d(ExpectedValues), Mat23d(Values) + Mat23d(OtherValues));
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

        EXPECT_FEQ(Mat23d(ExpectedValues), Mat23d(OtherValues) - Mat23d(Values));
    }

    TEST_CASE(TestNegation)
    {
        static const double ExpectedValues[] =
        {
            -1.0, -2.0, -3.0,
            -4.0, -5.0, -6.0
        };

        EXPECT_EQ(Mat23d(ExpectedValues), -Mat23d(Values));
    }

    TEST_CASE(TestRightMultiplicationByScalar)
    {
        static const double ExpectedValues[] =
        {
             2.0,  4.0,  6.0,
             8.0, 10.0, 12.0
        };

        EXPECT_FEQ(Mat23d(ExpectedValues), Mat23d(Values) * 2.0);
    }

    TEST_CASE(TestLeftMultiplicationByScalar)
    {
        static const double ExpectedValues[] =
        {
             2.0,  4.0,  6.0,
             8.0, 10.0, 12.0
        };

        EXPECT_FEQ(Mat23d(ExpectedValues), 2.0 * Mat23d(Values));
    }

    TEST_CASE(TestDivisionByScalar)
    {
        static const double ExpectedValues[] =
        {
             0.5, 1.0, 1.5,
             2.0, 2.5, 3.0
        };

        EXPECT_FEQ(Mat23d(ExpectedValues), Mat23d(Values) / 2.0);
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

        Mat23d m(Values);
        m += Mat23d(OtherValues);

        EXPECT_FEQ(Mat23d(ExpectedValues), m);
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

        Mat23d m(OtherValues);
        m -= Mat23d(Values);

        EXPECT_FEQ(Mat23d(ExpectedValues), m);
    }

    TEST_CASE(TestInPlaceMultiplicationByScalar)
    {
        static const double ExpectedValues[] =
        {
             2.0,  4.0,  6.0,
             8.0, 10.0, 12.0
        };

        Mat23d m(Values);
        m *= 2.0;

        EXPECT_FEQ(Mat23d(ExpectedValues), m);
    }

    TEST_CASE(TestInPlaceDivisionByScalar)
    {
        static const double ExpectedValues[] =
        {
             0.5, 1.0, 1.5,
             2.0, 2.5, 3.0
        };

        Mat23d m(Values);
        m /= 2.0;

        EXPECT_FEQ(Mat23d(ExpectedValues), m);
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

        const Mat32d result = transpose(Mat23d(Values));

        EXPECT_EQ(Mat32d(ExpectedValues), result);
    }
}

TEST_SUITE(Foundation_Math_MatrixNN)
{
    typedef Matrix<double, 5, 5> Mat55d;

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
        EXPECT_EQ(5, Mat55d::Rows);
        EXPECT_EQ(5, Mat55d::Columns);
        EXPECT_EQ(25, Mat55d::Components);
    }

    TEST_CASE(ConstructMatrixWithArrayOfValues)
    {
        const Mat55d m(Values);

        EXPECT_SEQUENCE_EQ(25, Values, &m[0]);
    }

    TEST_CASE(ConstructMatrixWithSingleValue)
    {
        const Mat55d m(42.0);

        for (size_t i = 0; i < 25; ++i)
            EXPECT_EQ(42.0, m[i]);
    }

    TEST_CASE(ConstructMatrixByTypeConversion)
    {
        const Mat55d m(Values);
        const Matrix<float, 5, 5> mf(m);

        for (size_t i = 0; i < 25; ++i)
            EXPECT_FEQ(static_cast<float>(Values[i]), mf[i]);
    }

    TEST_CASE(RetrieveIdentityMatrix)
    {
        static const double Values[] =
        {
             1.0, 0.0, 0.0, 0.0, 0.0,
             0.0, 1.0, 0.0, 0.0, 0.0,
             0.0, 0.0, 1.0, 0.0, 0.0,
             0.0, 0.0, 0.0, 1.0, 0.0,
             0.0, 0.0, 0.0, 0.0, 1.0
        };

        const Mat55d m(Mat55d::identity());

        EXPECT_SEQUENCE_EQ(25, Values, &m[0]);
    }

    TEST_CASE(TestFortranStyleSubscripting)
    {
        const Mat55d m(Values);

        EXPECT_EQ( 1.0, m(0, 0));
        EXPECT_EQ( 2.0, m(0, 1));
        EXPECT_EQ( 3.0, m(0, 2));
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

        const Matrix3d result = inverse(Matrix3d(Values));

        EXPECT_FEQ(Matrix3d(ExpectedValues), result);
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
         -7.0, -40.0, 42.0,
        -50.0,  23.0, 75.0,
        -92.0,   6.0, 74.0
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

#ifdef APPLESEED_ENABLE_IMATH_INTEROP

    static const double ValuesImathFormat[3][3] =
    {
        { Values[0], Values[1], Values[2] },
        { Values[3], Values[4], Values[5] },
        { Values[6], Values[7], Values[8] }
    };

    TEST_CASE(ImathMatrix33Roundtrip)
    {
        const Matrix3d source(Values);
        const Imath::M33d copy(source);
        const Matrix3d result(copy);

        EXPECT_EQ(source, result);
    }

    TEST_CASE(CompareImath33Rotation)
    {
        const double angle = deg_to_rad(21.0);

        const Matrix3d rot_z = Matrix3d::rotation_z(angle);

        const Imath::M33d imath_rot_z =
            Imath::M33d().setRotation(angle);

        const Matrix3d tmp(imath_rot_z);

        EXPECT_FEQ(rot_z, tmp);
    }

#endif

    TEST_CASE(RetrieveIdentityMatrix)
    {
        static const double ExpectedValues[] =
        {
             1.0, 0.0, 0.0,
             0.0, 1.0, 0.0,
             0.0, 0.0, 1.0
        };

        const Matrix3d m(Matrix3d::identity());

        EXPECT_SEQUENCE_EQ(9, ExpectedValues, &m[0]);
    }

    TEST_CASE(TestFortranStyleSubscripting)
    {
        const Matrix3d m(Values);

        EXPECT_EQ( -7.0, m(0, 0));
        EXPECT_EQ(-40.0, m(0, 1));
        EXPECT_EQ( 42.0, m(0, 2));
        EXPECT_EQ( 74.0, m(2, 2));
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

        const Matrix3d result = Matrix3d(LhsValues) * Matrix3d(RhsValues);

        EXPECT_FEQ(Matrix3d(ExpectedValues), result);
    }

    TEST_CASE(TestMatrixVectorMultiplication)
    {
        const Vector3d Vec(72.0, 37.0, -23.0);

        const Vector3d result = Matrix3d(Values) * Vec;

        EXPECT_FEQ(Vector3d(-2950.0, -4474.0, -8104.0), result);
    }

    TEST_CASE(TestVectorMatrixMultiplication)
    {
        const Vector3d Vec(72.0, 37.0, -23.0);

        const Vector3d result = Vec * Matrix3d(Values);

        EXPECT_FEQ(Vector3d(-238.0, -2167.0, 4097.0), result);
    }

    TEST_CASE(TestExtractScaling_GivenIdentityMatrix)
    {
        const Matrix3d m = Matrix3d::identity();

        const Vector3d s = m.extract_scaling();

        EXPECT_FEQ(Vector3d(1.0, 1.0, 1.0), s);
    }

    TEST_CASE(TestExtractScaling_GivenScalingMatrix)
    {
        const Matrix3d m = Matrix3d::scaling(Vector3d(2.0, 3.0, 0.5));

        const Vector3d s = m.extract_scaling();

        EXPECT_FEQ(Vector3d(2.0, 3.0, 0.5), s);
    }

    TEST_CASE(TestExtractScaling_GivenScalingFollowedByRotation)
    {
        const Matrix3d m =
              Matrix3d::rotation_x(Pi / 4.0)
            * Matrix3d::scaling(Vector3d(2.0, 3.0, 0.5));

        const Vector3d s = m.extract_scaling();

        EXPECT_FEQ(Vector3d(2.0, 3.0, 0.5), s);
    }

    TEST_CASE(TestExtractUnitQuaternion_GivenIdentityMatrix)
    {
        const Matrix3d m = Matrix3d::identity();

        const Quaterniond q = m.extract_unit_quaternion();

        EXPECT_FEQ(Quaterniond::rotation(Vector3d(1.0, 0.0, 0.0), 0.0), q);
    }

    TEST_CASE(TestExtractUnitQuaternion_GivenRotationMatrix)
    {
        const Matrix3d m = Matrix3d::rotation_x(Pi / 4.0);

        const Quaterniond q = m.extract_unit_quaternion();

        EXPECT_FEQ(Quaterniond::rotation(Vector3d(1.0, 0.0, 0.0), Pi / 4.0), q);
    }

    TEST_CASE(TestDecompose_GivenScalingFollowedByRotation)
    {
        const Matrix3d m =
              Matrix3d::rotation_x(Pi / 4.0)
            * Matrix3d::scaling(Vector3d(2.0, 3.0, 0.5));

        Vector3d s;
        Quaterniond q;
        m.decompose(s, q);

        EXPECT_FEQ(Vector3d(2.0, 3.0, 0.5), s);
        EXPECT_FEQ(Quaterniond::rotation(Vector3d(1.0, 0.0, 0.0), Pi / 4.0), q);
    }

    TEST_CASE(TestDecompose_GivenMirroring)
    {
        static const double Values[] =
        {
            0.0, 1.0, 0.0,
            1.0, 0.0, 0.0,
            0.0, 0.0, 1.0
        };

        const Matrix3d m(Values);

        Vector3d s;
        Quaterniond q;
        m.decompose(s, q);

        EXPECT_TRUE(is_normalized(q));
    }

    TEST_CASE(ConstructRotationMatrixGivenQuaternion)
    {
        const Vector3d from = normalize(Vector3d(1.0, 1.0, 0.0));
        const Vector3d to = normalize(Vector3d(1.0, 0.0, 1.0));

        const Quaterniond q = Quaterniond::rotation(from, to);
        const Matrix3d m = Matrix3d::rotation(q);
        const Vector3d result = m * from;

        EXPECT_FEQ(to, result);
    }
}

TEST_SUITE(Foundation_Math_Matrix44)
{
    static const double Values[] =
    {
        -38.0,  23.0, 14.0,  58.0,
        -92.0,  -8.0, 36.0, -90.0,
         96.0, -19.0, -4.0,  38.0,
        -99.0, -67.0, 68.0,  45.0
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

#ifdef APPLESEED_ENABLE_IMATH_INTEROP

    static const double ValuesImathFormat[4][4] =
    {
        { Values[ 0], Values[ 1], Values[ 2], Values[ 3] },
        { Values[ 4], Values[ 5], Values[ 6], Values[ 7] },
        { Values[ 8], Values[ 9], Values[10], Values[11] },
        { Values[12], Values[13], Values[14], Values[15] }
    };

    TEST_CASE(ImathMatrix44Roundtrip)
    {
        const Matrix4d source(Values);
        const Imath::M44d copy(source);
        const Matrix4d result(copy);

        EXPECT_EQ(source, result);
    }

    TEST_CASE(CompareImath44RotationX)
    {
        const double angle = deg_to_rad(30.0);

        const Matrix4d rot_x = Matrix4d::rotation_x(angle);

        const Imath::M44d imath_rot_x =
            Imath::M44d().setEulerAngles(Imath::V3d(angle, 0.0, 0.0));

        const Matrix4d tmp(imath_rot_x);

        EXPECT_FEQ(rot_x, tmp);
    }

#endif

    TEST_CASE(RetrieveIdentityMatrix)
    {
        static const double ExpectedValues[] =
        {
             1.0, 0.0, 0.0, 0.0,
             0.0, 1.0, 0.0, 0.0,
             0.0, 0.0, 1.0, 0.0,
             0.0, 0.0, 0.0, 1.0
        };

        const Matrix4d m(Matrix4d::identity());

        EXPECT_SEQUENCE_EQ(16, ExpectedValues, &m[0]);
    }

    TEST_CASE(TestFortranStyleSubscripting)
    {
        const Matrix4d m(Values);

        EXPECT_EQ(-38.0, m(0, 0));
        EXPECT_EQ( 23.0, m(0, 1));
        EXPECT_EQ( 14.0, m(0, 2));
        EXPECT_EQ( 45.0, m(3, 3));
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

        const Matrix4d result = Matrix4d(LhsValues) * Matrix4d(RhsValues);

        EXPECT_FEQ(Matrix4d(ExpectedValues), result);
    }

    TEST_CASE(TestMatrixVectorMultiplication)
    {
        const Vector4d Vec(73.0, 76.0, -68.0, 67.0);

        const Vector4d result = Matrix4d(Values) * Vec;

        EXPECT_FEQ(Vector4d(1908.0, -15802.0, 8382.0, -13928.0), result);
    }

    TEST_CASE(TestVectorMatrixMultiplication)
    {
        const Vector4d Vec(73.0, 76.0, -68.0, 67.0);

        const Vector4d result = Vec * Matrix4d(Values);

        EXPECT_FEQ(Vector4d(-22927.0, -2126.0, 8586.0, -2175.0), result);
    }

    TEST_CASE(TestExtractTranslation)
    {
        const Matrix4d m = Matrix4d::translation(Vector3d(-4.0, 5.0, 0.7));

        const Vector3d t = m.extract_translation();

        EXPECT_FEQ(Vector3d(-4.0, 5.0, 0.7), t);
    }

    TEST_CASE(TestDecompose_GivenScalingFollowedByRotationFollowedByTranslation)
    {
        const Matrix4d m =
              Matrix4d::translation(Vector3d(-4.0, 5.0, 0.7))
            * Matrix4d::rotation_x(Pi / 4.0)
            * Matrix4d::scaling(Vector3d(2.0, 3.0, 0.5));

        Vector3d s;
        Quaterniond q;
        Vector3d t;
        m.decompose(s, q, t);

        EXPECT_FEQ(Vector3d(2.0, 3.0, 0.5), s);
        EXPECT_FEQ(Quaterniond::rotation(Vector3d(1.0, 0.0, 0.0), Pi / 4.0), q);
        EXPECT_FEQ(Vector3d(-4.0, 5.0, 0.7), t);
    }
}
