
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2018 Francois Beaune, The appleseedhq Organization
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
#include "foundation/platform/arch.h"
#include "foundation/utility/poison.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstdint>

using namespace foundation;

#ifdef APPLESEED_DEBUG

TEST_SUITE(Foundation_Utility_Debug_Poison)
{
    TEST_CASE(Debug_Poison_Int8)
    {
        std::int8_t x = 0;

        debug_poison(x);

        EXPECT_EQ(std::int8_t(0xADU), x);
    }

    TEST_CASE(Debug_Poison_UInt8)
    {
        std::uint8_t x = 0;

        debug_poison(x);

        EXPECT_EQ(0xADU, x);
    }

    TEST_CASE(Debug_Poison_Int32)
    {
        std::int32_t x = 0;

        debug_poison(x);

        EXPECT_EQ(std::int32_t(0xADADADADU), x);
    }

    TEST_CASE(Debug_Poison_UInt32)
    {
        std::uint32_t x = 0;

        debug_poison(x);

        EXPECT_EQ(0xADADADADU, x);
    }

    TEST_CASE(Debug_Poison_VoidPointer)
    {
        void* p = nullptr;

        debug_poison(p);

#ifdef APPLESEED_ARCH32
        EXPECT_EQ(reinterpret_cast<void*>(0xDEADBEEFu), p);
#else
        EXPECT_EQ(reinterpret_cast<void*>(0xDEADBEEFDEADBEEFull), p);
#endif
    }

    TEST_CASE(Debug_Poison_IntPointer)
    {
        int* p = nullptr;

        debug_poison(p);

#ifdef APPLESEED_ARCH32
        EXPECT_EQ(reinterpret_cast<void*>(0xDEADBEEFu), p);
#else
        EXPECT_EQ(reinterpret_cast<void*>(0xDEADBEEFDEADBEEFull), p);
#endif
    }

    TEST_CASE(Debug_Poison_Float)
    {
        float x = 0.0f;

        debug_poison(x);

        EXPECT_FALSE(x == x);
    }

    TEST_CASE(Debug_Poison_Double)
    {
        double x = 0.0;

        debug_poison(x);

        EXPECT_FALSE(x == x);
    }

    TEST_CASE(Debug_Poison_Bool)
    {
        bool b = false;

        debug_poison(b);

        // We can't expect much here.
    }

    // Templates cannot take local types in C++03.
    enum Enum { A, B, C };

    TEST_CASE(Debug_Poison_Enum)
    {
        Enum actual = A;
        debug_poison(actual);

        int expected = 0;
        debug_poison(expected);

        EXPECT_EQ(expected, actual);
    }
}
#endif 

// always_poison should work in any build mode.

TEST_SUITE(Foundation_Utility_Always_Poison)
{
    TEST_CASE(Always_Poison_Int8)
    {
        std::int8_t x = 0;

        always_poison(x);

        EXPECT_EQ(std::int8_t(0xADU), x);
    }

    TEST_CASE(Always_Poison_UInt8)
    {
        std::uint8_t x = 0;

        always_poison(x);

        EXPECT_EQ(0xADU, x);
    }

    TEST_CASE(Always_Poison_Int32)
    {
        std::int32_t x = 0;

        always_poison(x);

        EXPECT_EQ(std::int32_t(0xADADADADU), x);
    }

    TEST_CASE(Always_Poison_UInt32)
    {
        std::uint32_t x = 0;

        always_poison(x);

        EXPECT_EQ(0xADADADADU, x);
    }

    TEST_CASE(Always_Poison_VoidPointer)
    {
        void* p = nullptr;

        always_poison(p);

#ifdef APPLESEED_ARCH32
        EXPECT_EQ(reinterpret_cast<void*>(0xDEADBEEFu), p);
#else
        EXPECT_EQ(reinterpret_cast<void*>(0xDEADBEEFDEADBEEFull), p);
#endif
    }

    TEST_CASE(Always_Poison_IntPointer)
    {
        int* p = nullptr;

        always_poison(p);

#ifdef APPLESEED_ARCH32
        EXPECT_EQ(reinterpret_cast<void*>(0xDEADBEEFu), p);
#else
        EXPECT_EQ(reinterpret_cast<void*>(0xDEADBEEFDEADBEEFull), p);
#endif
    }

    TEST_CASE(Always_Poison_Float)
    {
        float x = 0.0f;

        always_poison(x);

        EXPECT_FALSE(x == x);
    }

    TEST_CASE(Always_Poison_Double)
    {
        double x = 0.0;

        always_poison(x);

        EXPECT_FALSE(x == x);
    }

    TEST_CASE(Always_Poison_Bool)
    {
        bool b = false;

        always_poison(b);

        // We can't expect much here.
    }

    // Templates cannot take local types in C++03.
    enum Enum { A, B, C };

    TEST_CASE(Always_Poison_Enum)
    {
        Enum actual = A;
        always_poison(actual);

        int expected = 0;
        always_poison(expected);

        EXPECT_EQ(expected, actual);
    }
}

