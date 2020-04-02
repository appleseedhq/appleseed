
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
#include "foundation/platform/timers.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstdint>

using namespace foundation;

TEST_SUITE(Foundation_Platform_Timers)
{
#ifdef APPLESEED_X86

    TEST_CASE(TestX86TimerFrequency)
    {
        X86Timer timer;
        EXPECT_TRUE(timer.frequency() > 0);
    }

    TEST_CASE(TestX86TimerValues)
    {
        X86Timer timer;
        const std::uint64_t val1 = timer.read();
        const std::uint64_t val2 = timer.read();
        EXPECT_TRUE(val1 <= val2);
    }

#endif

    TEST_CASE(TestDefaultProcessorTimerFrequency)
    {
        DefaultProcessorTimer timer;
        EXPECT_TRUE(timer.frequency() > 0);
    }

    TEST_CASE(TestDefaultProcessorTimerValues)
    {
        DefaultProcessorTimer timer;
        const std::uint64_t val1 = timer.read();
        const std::uint64_t val2 = timer.read();
        EXPECT_TRUE(val1 <= val2);
    }

    TEST_CASE(TestDefaultWallclockTimerFrequency)
    {
        DefaultWallclockTimer timer;
        EXPECT_TRUE(timer.frequency() > 0);
    }

    TEST_CASE(TestDefaultWallclockTimerValues)
    {
        DefaultWallclockTimer timer;
        const std::uint64_t val1 = timer.read();
        const std::uint64_t val2 = timer.read();
        EXPECT_TRUE(val1 <= val2);
    }
}
