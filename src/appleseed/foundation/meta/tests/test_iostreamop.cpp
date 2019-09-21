
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
#include "foundation/math/aabb.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <iomanip>
#include <sstream>

// Forward declarations.
namespace foundation    { class ExceptionStringConversionError; }

using namespace foundation;

TEST_SUITE(Foundation_Utility_IOStreamOp)
{
    TEST_CASE(ReadFloatArray_GivenEmptyStream_ReturnsEmptyArray)
    {
        std::stringstream sstr;

        FloatArray array;
        sstr >> array;

        EXPECT_TRUE(array.empty());
    }

    TEST_CASE(ReadFloatArray_GivenThreeFloatValues_ReturnsValues)
    {
        std::stringstream sstr;
        sstr << "1 -2.2 3e-1";

        FloatArray array;
        sstr >> array;

        const float Expected[] = { 1.0f, -2.2f, 3.0e-1f };

        ASSERT_EQ(3, array.size());
        EXPECT_SEQUENCE_FEQ(3, Expected, &array[0]);
    }

    TEST_CASE(ReadFloatArray_GivenInvalidValue_ThrowsExceptionStringConversionError)
    {
        std::stringstream sstr;
        sstr << "1.1 hello";

        FloatArray array;

        EXPECT_EXCEPTION(ExceptionStringConversionError,
        {
            sstr >> array;
        });
    }

    TEST_CASE(ReadFloatArray_GivenEmptyStream_DoNotSetFailBitOnStream)
    {
        std::stringstream sstr;

        FloatArray array;
        sstr >> array;

        EXPECT_FALSE(sstr.fail());
    }

    TEST_CASE(ReadFloatArray_GivenThreeFloatValues_DoNotSetFailBitOnStream)
    {
        std::stringstream sstr;
        sstr << "1 -2.2 3e-1";

        FloatArray array;
        sstr >> array;

        EXPECT_FALSE(sstr.fail());
    }

    TEST_CASE(ReadFloatArray_GivenInvalidValue_DoNotSetFailBitOnStream)
    {
        std::stringstream sstr;
        sstr << "1.1 hello";

        try
        {
            FloatArray array;
            sstr >> array;
        }
        catch (const ExceptionStringConversionError&)
        {
            // We were expecting this exception.
        }

        EXPECT_FALSE(sstr.fail());
    }

    TEST_CASE(ReadDoubleArray_GivenEmptyStream_ReturnsEmptyArray)
    {
        std::stringstream sstr;

        DoubleArray array;
        sstr >> array;

        EXPECT_TRUE(array.empty());
    }

    TEST_CASE(ReadDoubleArray_GivenThreeDoubleValues_ReturnsValues)
    {
        std::stringstream sstr;
        sstr << "1 -2.2 3e-1";

        DoubleArray array;
        sstr >> array;

        const double Expected[] = { 1.0, -2.2, 3.0e-1 };

        ASSERT_EQ(3, array.size());
        EXPECT_SEQUENCE_FEQ(3, Expected, &array[0]);
    }

    TEST_CASE(ReadDoubleArray_GivenInvalidValue_ThrowsExceptionStringConversionError)
    {
        std::stringstream sstr;
        sstr << "1.1 hello";

        DoubleArray array;

        EXPECT_EXCEPTION(ExceptionStringConversionError,
        {
            sstr >> array;
        });
    }

    TEST_CASE(Write3DAABBToStream)
    {
        std::stringstream sstr;
        sstr << std::fixed << std::setprecision(1);
        sstr << AABB3d(Vector3d(-1.0, -2.0, -3.0), Vector3d(1.0, 2.0, 3.0));

        EXPECT_EQ("-1.0 -2.0 -3.0 1.0 2.0 3.0", sstr.str());
    }

    TEST_CASE(Read3DAABBFromStream)
    {
        std::stringstream sstr;
        sstr << "-1.0 -2.0 -3.0 1.0 2.0 3.0";

        AABB3d aabb;
        sstr >> aabb;

        EXPECT_EQ(AABB3d(Vector3d(-1.0, -2.0, -3.0), Vector3d(1.0, 2.0, 3.0)), aabb);
    }
}
