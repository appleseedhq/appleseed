
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
#include "foundation/math/rng/distribution.h"
#include "foundation/platform/types.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <limits>

using namespace foundation;
using namespace std;

TEST_SUITE(Foundation_Math_Rng)
{
    struct FakeRNG
    {
        const uint32 m_value;

        explicit FakeRNG(const uint32 value)
          : m_value(value)
        {
        }

        uint32 rand_uint32() const
        {
            return m_value;
        }
    };

    //
    // rand_float31()
    //

    TEST_CASE(RandInt31_Given0x00000000_ReturnsZero)
    {
        const int32 value = rand_int31(FakeRNG(0x00000000));

        EXPECT_EQ(0, value);
    }

    TEST_CASE(RandInt31_Given0xFFFFFFFF_ReturnsZero)
    {
        const int32 value = rand_int31(FakeRNG(0xFFFFFFFF));

        EXPECT_EQ(0x7FFFFFFF, value);
    }

    //
    // rand_int1()
    //

    TEST_CASE(RandInt1_Given0x00000000_ReturnsLowBound)
    {
        const int32 value = rand_int1(FakeRNG(0x00000000), -12, 42);

        EXPECT_EQ(-12, value);
    }

    TEST_CASE(RandInt1_Given0xFFFFFFFF_ReturnsHighBound)
    {
        const int32 value = rand_int1(FakeRNG(0xFFFFFFFF), -12, 42);

        EXPECT_EQ(42, value);
    }

    //
    // rand_float1()
    //

    TEST_CASE(RandFloat1_Given0x00000000_ReturnsZero)
    {
        const float value = rand_float1(FakeRNG(0x00000000));

        EXPECT_EQ(0.0f, value);
    }

    TEST_CASE(RandFloat1_Given0xFFFFFFFF_ReturnsOne)
    {
        const float value = rand_float1(FakeRNG(0xFFFFFFFF));

        EXPECT_EQ(1.0f, value);
    }

    TEST_CASE(RandFloat1_Given0x00000000_ReturnsLowBound)
    {
        const float value = rand_float1(FakeRNG(0x00000000), -12.0f, 42.0f);

        EXPECT_EQ(-12.0f, value);
    }

    TEST_CASE(RandFloat1_Given0xFFFFFFFF_ReturnsHighBound)
    {
        const float value = rand_float1(FakeRNG(0xFFFFFFFF), -12.0f, 42.0f);

        EXPECT_EQ(42.0f, value);
    }

    //
    // rand_double1()
    //

    TEST_CASE(RandDouble1_Given0x00000000_ReturnsZero)
    {
        const double value = rand_double1(FakeRNG(0x00000000));

        EXPECT_EQ(0.0, value);
    }

    TEST_CASE(RandDouble1_Given0xFFFFFFFF_ReturnsOne)
    {
        const double value = rand_double1(FakeRNG(0xFFFFFFFF));

        EXPECT_EQ(1.0, value);
    }

    TEST_CASE(RandDouble1_Given0x00000000_ReturnsLowBound)
    {
        const double value = rand_double1(FakeRNG(0x00000000), -12.0, 42.0);

        EXPECT_EQ(-12.0, value);
    }

    TEST_CASE(RandDouble1_Given0xFFFFFFFF_ReturnsHighBound)
    {
        const double value = rand_double1(FakeRNG(0xFFFFFFFF), -12.0, 42.0);

        EXPECT_EQ(42.0, value);
    }

    //
    // rand_float2()
    //

    TEST_CASE(RandFloat2_Given0x00000000_ReturnsZero)
    {
        const float value = rand_float2(FakeRNG(0x00000000));

        EXPECT_EQ(0.0f, value);
    }

    TEST_CASE(RandFloat2_Given0xFFFFFFFF_ReturnsAlmostOne)
    {
        const float value = rand_float2(FakeRNG(0xFFFFFFFF));

        EXPECT_EQ(0.99999994f, value);
    }

    TEST_CASE(RandFloat2_Given0x00000000_ReturnsLowBound)
    {
        const float value = rand_float2(FakeRNG(0x00000000), -12.0f, 42.0f);

        EXPECT_EQ(-12.0f, value);
    }

    TEST_CASE(RandFloat2_Given0xFFFFFFFF_ReturnsAlmostHighBound)
    {
        const float value = rand_float2(FakeRNG(0xFFFFFFFF), -12.0f, 42.0f);

        EXPECT_EQ(41.999996f, value);
    }

    //
    // rand_double2()
    //

    TEST_CASE(RandDouble2_Given0x00000000_ReturnsZero)
    {
        const double value = rand_double2(FakeRNG(0x00000000));

        EXPECT_EQ(0.0, value);
    }

    TEST_CASE(RandDouble2_Given0xFFFFFFFF_ReturnsAlmostOne)
    {
        const double value = rand_double2(FakeRNG(0xFFFFFFFF));

        EXPECT_EQ(0.99999999976716936, value);
    }

    TEST_CASE(RandDouble2_Given0x00000000_ReturnsLowBound)
    {
        const double value = rand_double2(FakeRNG(0x00000000), -12.0, 42.0);

        EXPECT_EQ(-12.0, value);
    }

    TEST_CASE(RandDouble2_Given0xFFFFFFFF_ReturnsAlmostHighBound)
    {
        const double value = rand_double2(FakeRNG(0xFFFFFFFF), -12.0, 42.0);

        EXPECT_EQ(41.999999987427145, value);
    }

    //
    // rand_float3()
    //

    TEST_CASE(RandFloat3_Given0x00000000_ReturnsAlmostZero)
    {
        const float value = rand_float3(FakeRNG(0x00000000));

        EXPECT_EQ(numeric_limits<float>::epsilon(), value);
    }

    TEST_CASE(RandFloat3_Given0xFFFFFFFF_ReturnsAlmostOne)
    {
        const float value = rand_float3(FakeRNG(0xFFFFFFFF));

        EXPECT_EQ(0.99999994f, value);
    }

    TEST_CASE(RandFloat3_Given0x00000000_ReturnsAlmostLowBound)
    {
        const float value = rand_float3(FakeRNG(0x00000000), -12.0f, 42.0f);

        EXPECT_EQ(-11.999993f, value);
    }

    TEST_CASE(RandFloat3_Given0xFFFFFFFF_ReturnsAlmostHighBound)
    {
        const float value = rand_float3(FakeRNG(0xFFFFFFFF), -12.0f, 42.0f);

        EXPECT_EQ(41.999996f, value);
    }

    //
    // rand_double3()
    //

    TEST_CASE(RandDouble3_Given0x00000000_ReturnsAlmostZero)
    {
        const double value = rand_double3(FakeRNG(0x00000000));

        EXPECT_EQ(numeric_limits<double>::epsilon(), value);
    }

    TEST_CASE(RandDouble3_Given0xFFFFFFFF_ReturnsAlmostOne)
    {
        const double value = rand_double3(FakeRNG(0xFFFFFFFF));

        EXPECT_EQ(0.99999999999999978, value);
    }

    TEST_CASE(RandDouble3_Given0x00000000_ReturnsAlmostLowBound)
    {
        const double value = rand_double3(FakeRNG(0x00000000), -12.0, 42.0);

        EXPECT_EQ(-11.999999999999988, value);
    }

    TEST_CASE(RandDouble3_Given0xFFFFFFFF_ReturnsAlmostHighBound)
    {
        const double value = rand_double3(FakeRNG(0xFFFFFFFF), -12.0, 42.0);

        EXPECT_EQ(41.999999999999993, value);
    }
}
