
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
#include "foundation/platform/arch.h"
#include "foundation/platform/snprintf.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <string>

using namespace foundation;

TEST_SUITE(Foundation_Platform_Snprintf)
{
    TEST_CASE(PortableSnprintf_GivenResultSmallerThanBuffer_WritesTrailingZero_DoesNotWritePastBuffer_ReturnsNumberOfWrittenCharacters)
    {
        const size_t BufferSize = 3;
        char buf[BufferSize + 1] = { '!', '!', '!', '!' };

        const int result = portable_snprintf(buf, BufferSize, "AB");

        EXPECT_EQ('A',  buf[0]);
        EXPECT_EQ('B',  buf[1]);
        EXPECT_EQ('\0', buf[2]);
        EXPECT_EQ('!',  buf[3]);

        EXPECT_EQ(2, result);
    }

    TEST_CASE(PortableSnprintf_GivenResultSameLengthAsBuffer_WritesTrailingZero_DoesNotWritePastBuffer_ReturnsNumberOfCharactersThatWouldHaveBeenWritten)
    {
        const size_t BufferSize = 3;
        char buf[BufferSize + 1] = { '!', '!', '!', '!' };

        const int result = portable_snprintf(buf, BufferSize, "ABC");

        EXPECT_EQ('A',  buf[0]);
        EXPECT_EQ('B',  buf[1]);
        EXPECT_EQ('\0',  buf[2]);
        EXPECT_EQ('!',  buf[3]);

        EXPECT_EQ(3, result);
    }

    TEST_CASE(PortableSnprintf_GivenResultBiggerThanBuffer_WritesTrailingZero_DoesNotWritePastBuffer_ReturnsNumberOfCharactersThatWouldHaveBeenWritten)
    {
        const size_t BufferSize = 3;
        char buf[BufferSize + 1] = { '!', '!', '!', '!' };

        const int result = portable_snprintf(buf, BufferSize, "ABCD");

        EXPECT_EQ('A',  buf[0]);
        EXPECT_EQ('B',  buf[1]);
        EXPECT_EQ('\0',  buf[2]);
        EXPECT_EQ('!',  buf[3]);

        EXPECT_EQ(4, result);
    }

#if defined APPLESEED_ARCH32
    const size_t MaxSizeTValue = ~size_t(0);
    const char MaxSizeTValueString[] = "4294967295";
#elif defined APPLESEED_ARCH64
    const size_t MaxSizeTValue = ~size_t(0);
    const char MaxSizeTValueString[] = "18446744073709551615";
#else
    #error Cannot determine machine architecture.
#endif

    TEST_CASE(PortableSnprintf_GivenLargeSizeTValue_UsingC99FormatString_ReturnsProperlyFormattedString)
    {
        char buf[100];
        portable_snprintf(buf, sizeof(buf), "%zu", MaxSizeTValue);

        EXPECT_EQ(MaxSizeTValueString, std::string(buf));
    }

#ifdef _MSC_VER

    // This test is only enabled when building with Visual Studio, as other compilers
    // such as gcc will complain about %Iu being an invalid format string.

    TEST_CASE(PortableSnprintf_GivenLargeSizeTValue_UsingVisualStudioFormatString_ReturnsProperlyFormattedString)
    {
        char buf[100];
        const int result = portable_snprintf(buf, sizeof(buf), "%Iu", MaxSizeTValue);

        EXPECT_EQ(MaxSizeTValueString, std::string(buf));
    }

#endif
}
