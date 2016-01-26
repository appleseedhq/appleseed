
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
#include "foundation/platform/types.h"
#include "foundation/utility/casts.h"
#include "foundation/utility/test.h"

using namespace foundation;

TEST_SUITE(Foundation_Utility_Casts)
{
    const uint32 OnePattern = 0x3F800000UL;

    TEST_CASE(TestBinaryCastFloatToUInt32)
    {
        float f = 1.0f;
        EXPECT_EQ(OnePattern, binary_cast<uint32>(f));
    }

    TEST_CASE(TestBinaryCastConstFloatToUInt32)
    {
        const float f = 1.0f;
        EXPECT_EQ(OnePattern, binary_cast<uint32>(f));
    }

    TEST_CASE(TestBinaryCastConstFloatRefToUInt32)
    {
        float f = 1.0f;
        const float& ref = f;
        EXPECT_EQ(OnePattern, binary_cast<uint32>(ref));
    }
}
