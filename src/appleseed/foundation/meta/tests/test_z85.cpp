
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2019 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/utility/test.h"
#include "foundation/utility/z85.h"

// Standard headers.
#include <cstring>
#include <vector>

using namespace foundation;

TEST_SUITE(Foundation_Utility_Z85)
{
    TEST_CASE(Encode)
    {
        const size_t DataSize = 8;
        const unsigned char data[DataSize] = { 0x86, 0x4F, 0xD2, 0x6F, 0xB5, 0x59, 0xF7, 0x5B };

        std::vector<char> encoded;
        encoded.resize(z85_encoded_size(DataSize));

        z85_encode(data, DataSize, reinterpret_cast<char*>(encoded.data()));
        EXPECT_TRUE(memcmp("HelloWorld", encoded.data(), strlen("HelloWorld")) == 0);
    }

    TEST_CASE(RoundTrip)
    {
        const size_t DataSize = 32;
        const unsigned char data[DataSize] =
        {
            0x8E, 0x0B, 0xDD, 0x69, 0x76, 0x28, 0xB9, 0x1D,
            0x8F, 0x24, 0x55, 0x87, 0xEE, 0x95, 0xC5, 0xB0,
            0x4D, 0x48, 0x96, 0x3F, 0x79, 0x25, 0x98, 0x77,
            0xB4, 0x9C, 0xD9, 0x06, 0x3A, 0xEA, 0xD3, 0xB7
        };

        std::vector<char> encoded;
        encoded.resize(z85_encoded_size(DataSize));
        z85_encode(data, DataSize, reinterpret_cast<char*>(encoded.data()));

        std::vector<unsigned char> decoded;
        decoded.resize(z85_decoded_size(encoded.size()));
        EXPECT_EQ(DataSize, decoded.size());

        z85_decode(encoded.data(), encoded.size(), decoded.data());
        EXPECT_TRUE(memcmp(data, decoded.data(), DataSize) == 0);
    }
}
