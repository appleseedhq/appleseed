
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2017 Francois Beaune, The appleseedhq Organization
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
#include "foundation/platform/types.h"
#include "foundation/utility/poison.h"
#include "foundation/utility/test.h"

using namespace foundation;

TEST_SUITE(Foundation_Utility_Poison)
{
    TEST_CASE(Poison_Int8)
    {
        int8 x = 0;

        poison(x);

        EXPECT_EQ(int8(0xADU), x);
    }

    TEST_CASE(Poison_UInt8)
    {
        uint8 x = 0;

        poison(x);

        EXPECT_EQ(0xADU, x);
    }

    TEST_CASE(Poison_Int32)
    {
        int32 x = 0;

        poison(x);

        EXPECT_EQ(int32(0xADADADADU), x);
    }

    TEST_CASE(Poison_UInt32)
    {
        uint32 x = 0;

        poison(x);

        EXPECT_EQ(0xADADADADU, x);
    }

    TEST_CASE(Poison_VoidPointer)
    {
        void* p = 0;

        poison(p);

#ifdef APPLESEED_ARCH32
        EXPECT_EQ(reinterpret_cast<void*>(0xDEADBEEFUL), p);
#else
        EXPECT_EQ(reinterpret_cast<void*>(0xDEADBEEFDEADBEEFULL), p);
#endif
    }

    TEST_CASE(Poison_IntPointer)
    {
        int* p = 0;

        poison(p);

#ifdef APPLESEED_ARCH32
        EXPECT_EQ(reinterpret_cast<void*>(0xDEADBEEFUL), p);
#else
        EXPECT_EQ(reinterpret_cast<void*>(0xDEADBEEFDEADBEEFULL), p);
#endif
    }

    TEST_CASE(Poison_Float)
    {
        float x = 0.0f;

        poison(x);

        EXPECT_FALSE(x == x);
    }

    TEST_CASE(Poison_Double)
    {
        double x = 0.0;

        poison(x);

        EXPECT_FALSE(x == x);
    }

    TEST_CASE(Poison_Bool)
    {
        bool b = false;

        poison(b);

        // We can't expect much here.
    }

    // Templates cannot take local types in C++03.
    enum Enum { A, B, C };

    TEST_CASE(Poison_Enum)
    {
        Enum actual = A;
        poison(actual);

        int expected = 0;
        poison(expected);

        EXPECT_EQ(expected, actual);
    }
}
