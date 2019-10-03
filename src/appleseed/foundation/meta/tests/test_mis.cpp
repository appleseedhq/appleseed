
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
#include "foundation/math/mis.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <limits>

using namespace foundation;

TEST_SUITE(Foundation_Math_MIS)
{
#pragma warning (push)
#pragma warning (disable : 4723)    // potential division by 0
#pragma warning (disable : 4756)    // overflow in constant arithmetic)

    const float Huge = std::numeric_limits<float>::max();

    TEST_CASE(MisBalance)
    {
        EXPECT_FEQ(0.0f,        mis_balance(0.0f, 1.0f));
        EXPECT_FEQ(0.0f,        mis_balance(0.0f, 2.0f));

        EXPECT_FEQ(1.0f,        mis_balance(1.0f, 0.0f));
        EXPECT_FEQ(0.5f,        mis_balance(1.0f, 1.0f));
        EXPECT_FEQ(1.0f / 3,    mis_balance(1.0f, 2.0f));

        EXPECT_FEQ(1.0f,        mis_balance(2.0f, 0.0f));
        EXPECT_FEQ(2.0f / 3,    mis_balance(2.0f, 1.0f));
        EXPECT_FEQ(0.5f,        mis_balance(2.0f, 2.0f));

        EXPECT_FEQ(0.0f,        mis_balance(1.0f, Huge));
        EXPECT_FEQ(1.0f,        mis_balance(Huge, 1.0f));
        EXPECT_FEQ(0.5f,        mis_balance(Huge, Huge));
    }

    TEST_CASE(MisPower)
    {
        EXPECT_FEQ(0.0f,        mis_power(0.0f, 1.0f, 4.0f));
        EXPECT_FEQ(0.0f,        mis_power(0.0f, 2.0f, 4.0f));

        EXPECT_FEQ(1.0f,        mis_power(1.0f, 0.0f, 4.0f));
        EXPECT_FEQ(0.5f,        mis_power(1.0f, 1.0f, 4.0f));
        EXPECT_FEQ(1.0f / 17,   mis_power(1.0f, 2.0f, 4.0f));

        EXPECT_FEQ(1.0f,        mis_power(2.0f, 0.0f, 4.0f));
        EXPECT_FEQ(16.0f / 17,  mis_power(2.0f, 1.0f, 4.0f));
        EXPECT_FEQ(0.5f,        mis_power(2.0f, 2.0f, 4.0f));

        EXPECT_FEQ(0.0f,        mis_power(1.0f, Huge, 4.0f));
        EXPECT_FEQ(1.0f,        mis_power(Huge, 1.0f, 4.0f));
        EXPECT_FEQ(0.5f,        mis_power(Huge, Huge, 4.0f));
    }

    TEST_CASE(MisPower2)
    {
        EXPECT_FEQ(0.0f,        mis_power2(0.0f, 1.0f));
        EXPECT_FEQ(0.0f,        mis_power2(0.0f, 2.0f));

        EXPECT_FEQ(1.0f,        mis_power2(1.0f, 0.0f));
        EXPECT_FEQ(0.5f,        mis_power2(1.0f, 1.0f));
        EXPECT_FEQ(0.2f,        mis_power2(1.0f, 2.0f));

        EXPECT_FEQ(1.0f,        mis_power2(2.0f, 0.0f));
        EXPECT_FEQ(4.0f / 5,    mis_power2(2.0f, 1.0f));
        EXPECT_FEQ(0.5f,        mis_power2(2.0f, 2.0f));

        EXPECT_FEQ(0.0f,        mis_power2(1.0f, Huge));
        EXPECT_FEQ(1.0f,        mis_power2(Huge, 1.0f));
        EXPECT_FEQ(0.5f,        mis_power2(Huge, Huge));
    }

#pragma warning (pop)
}
