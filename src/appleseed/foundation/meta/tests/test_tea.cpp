
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Francois Beaune, The appleseedhq Organization
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
#include "foundation/utility/tea.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstdint>
#include <vector>

using namespace foundation;

TEST_SUITE(Foundation_Utility_TEA)
{
    TEST_CASE(PadPkcs7_EmptyVector)
    {
        std::vector<char> v = {};

        pad_pkcs7(v, 4);

        ASSERT_EQ(4, v.size());
        EXPECT_EQ(4, v[0]);
        EXPECT_EQ(4, v[1]);
        EXPECT_EQ(4, v[2]);
        EXPECT_EQ(4, v[3]);
    }

    TEST_CASE(PadPkcs7_VectorWithOneValue)
    {
        std::vector<char> v = { 'a' };

        pad_pkcs7(v, 4);

        ASSERT_EQ(4, v.size());
        EXPECT_EQ('a', v[0]);
        EXPECT_EQ( 3,  v[1]);
        EXPECT_EQ( 3,  v[2]);
        EXPECT_EQ( 3,  v[3]);
    }

    TEST_CASE(PadPkcs7_VectorWithTTwoValues)
    {
        std::vector<char> v = { 'a', 'b' };

        pad_pkcs7(v, 4);

        ASSERT_EQ(4, v.size());
        EXPECT_EQ('a', v[0]);
        EXPECT_EQ('b', v[1]);
        EXPECT_EQ( 2,  v[2]);
        EXPECT_EQ( 2,  v[3]);
    }

    TEST_CASE(PadPkcs7_VectorWithThreeValues)
    {
        std::vector<char> v = { 'a', 'b', 'c' };

        pad_pkcs7(v, 4);

        ASSERT_EQ(4, v.size());
        EXPECT_EQ('a', v[0]);
        EXPECT_EQ('b', v[1]);
        EXPECT_EQ('c', v[2]);
        EXPECT_EQ( 1,  v[3]);
    }

    TEST_CASE(PadPkcs7_VectorWithFourValues)
    {
        std::vector<char> v = { 'a', 'b', 'c', 'd' };

        pad_pkcs7(v, 4);

        ASSERT_EQ(8, v.size());
        EXPECT_EQ('a', v[0]);
        EXPECT_EQ('b', v[1]);
        EXPECT_EQ('c', v[2]);
        EXPECT_EQ('d', v[3]);
        EXPECT_EQ( 4,  v[4]);
        EXPECT_EQ( 4,  v[5]);
        EXPECT_EQ( 4,  v[6]);
        EXPECT_EQ( 4,  v[7]);
    }
    
    TEST_CASE(PadPkcs7_VectorWithFiveValues)
    {
        std::vector<char> v = { 'a', 'b', 'c', 'd', 'e' };

        pad_pkcs7(v, 4);

        ASSERT_EQ(8, v.size());
        EXPECT_EQ('a', v[0]);
        EXPECT_EQ('b', v[1]);
        EXPECT_EQ('c', v[2]);
        EXPECT_EQ('d', v[3]);
        EXPECT_EQ('e', v[4]);
        EXPECT_EQ( 3,  v[5]);
        EXPECT_EQ( 3,  v[6]);
        EXPECT_EQ( 3,  v[7]);
    }

    TEST_CASE(TeaEncrypt_TeaDecrypt_Roundtrip)
    {
        const std::uint32_t Key[4] = { 0xff99fa8eUL, 0xc16d82fcUL, 0xc0628de9UL, 0x2ee70132UL };
        const std::uint32_t Input[2] = { 0x5b3ae248UL, 0x90cfca7fUL };

        std::uint32_t data[2] = { Input[0], Input[1] };

        tea_encrypt(data, Key);
        tea_decrypt(data, Key);

        EXPECT_EQ(Input[0], data[0]);
        EXPECT_EQ(Input[1], data[1]);
    }
}
