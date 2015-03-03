
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015 Francois Beaune, The appleseedhq Organization
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
#include "foundation/platform/defaulttimers.h"
#include "foundation/platform/thread.h"
#include "foundation/utility/job/abortswitch.h"
#include "foundation/utility/stopwatch.h"
#include "foundation/utility/test.h"

using namespace foundation;

TEST_SUITE(Foundation_Platform_Thread)
{
    TEST_CASE(InterruptibleSleep_AbortSwitchIsTriggered)
    {
        AbortSwitch abort_switch;
        abort_switch.abort();
        sleep(1000 * 3600, abort_switch);
    }

#ifdef EXPLORATION_TESTS

    TEST_CASE(Sleep_CheckElapsedTime)
    {
        Stopwatch<DefaultWallclockTimer> stopwatch;
        stopwatch.start();

        sleep(1000);

        stopwatch.measure();
        const double elapsed = stopwatch.get_seconds();
    }

    TEST_CASE(InterruptibleSleep_CheckElapsedTime)
    {
        AbortSwitch abort_switch;

        Stopwatch<DefaultWallclockTimer> stopwatch;
        stopwatch.start();

        sleep(1000, abort_switch);

        stopwatch.measure();
        const double elapsed = stopwatch.get_seconds();
    }

#endif
}
