
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Kevin Masson, The appleseedhq Organization
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
#include "foundation/utility/stopwatch.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstdint>

using namespace foundation;

TEST_SUITE(Foundation_Utility_Stopwatch)
{
    struct FakeTimer
    {
        std::uint64_t m_time;

        FakeTimer()
          : m_time(0)
        {
        }

        std::uint64_t frequency() const
        {
            return 1;
        }

        std::uint64_t read() const
        {
            return m_time;
        }

        std::uint64_t read_start() const
        {
            return read();
        }

        std::uint64_t read_end() const
        {
            return read();
        }
    };

    TEST_CASE(TestUnstartedStopwatch)
    {
        Stopwatch<FakeTimer> watch(0);

        EXPECT_EQ(watch.get_ticks(), 0);
    }

    TEST_CASE(TestMeasureStopwatch)
    {
        Stopwatch<FakeTimer> watch(0);
        FakeTimer& timer = watch.get_timer();

        watch.start();
        EXPECT_EQ(watch.get_ticks(), 0);

        timer.m_time += 500;
        EXPECT_EQ(watch.get_ticks(), 0);

        watch.measure();
        EXPECT_EQ(watch.get_ticks(), 500);

        timer.m_time += 100;
        watch.measure();
        EXPECT_EQ(watch.get_ticks(), 600);

        timer.m_time += 50;
        watch.measure();
        EXPECT_EQ(watch.get_ticks(), 650);
    }

    TEST_CASE(TestMeasureRestartedStopwatch)
    {
        Stopwatch<FakeTimer> watch(0);
        FakeTimer& timer = watch.get_timer();

        watch.start();
        timer.m_time += 400;
        watch.measure();
        EXPECT_EQ(watch.get_ticks(), 400);

        watch.start();
        timer.m_time += 600;
        EXPECT_EQ(watch.get_ticks(), 0);

        watch.measure();
        EXPECT_EQ(watch.get_ticks(), 600);
        EXPECT_EQ(timer.m_time, 1000);

        timer.m_time += 600;
        watch.measure();
        EXPECT_EQ(watch.get_ticks(), 1200);

        watch.start();
        watch.measure();
        EXPECT_EQ(watch.get_ticks(), 0);

        timer.m_time += 100;
        watch.measure();
        EXPECT_EQ(watch.get_ticks(), 100);
    }

    TEST_CASE(TestMeasureClearedStopwatch)
    {
        Stopwatch<FakeTimer> watch(0);
        FakeTimer& timer = watch.get_timer();

        watch.start();
        timer.m_time += 400;
        watch.measure();
        EXPECT_EQ(watch.get_ticks(), 400);

        watch.clear();
        EXPECT_EQ(watch.get_ticks(), 0);

        watch.measure();
        EXPECT_EQ(watch.get_ticks(), 400);

        timer.m_time += 10;
        watch.measure();
        EXPECT_EQ(watch.get_ticks(), 410);
    }

    TEST_CASE(TestMeasureClearedPausedStopwatch)
    {
        Stopwatch<FakeTimer> watch(0);
        FakeTimer& timer = watch.get_timer();

        watch.start();
        timer.m_time += 400;
        watch.measure();
        EXPECT_EQ(watch.get_ticks(), 400);

        watch.pause();
        EXPECT_EQ(watch.get_ticks(), 400);

        watch.clear();
        EXPECT_EQ(watch.get_ticks(), 0);

        watch.measure();
        EXPECT_EQ(watch.get_ticks(), 400);

        watch.clear();
        EXPECT_EQ(watch.get_ticks(), 0);

        watch.resume();
        EXPECT_EQ(watch.get_ticks(), 0);
        watch.measure();
        EXPECT_EQ(watch.get_ticks(), 400);
    }

    TEST_CASE(TestMeasurePausedStopwatch)
    {
        Stopwatch<FakeTimer> watch(0);
        FakeTimer& timer = watch.get_timer();

        watch.start();
        timer.m_time += 500;

        watch.measure();
        EXPECT_EQ(watch.get_ticks(), 500);

        timer.m_time += 100;

        watch.pause();
        timer.m_time += 400;
        EXPECT_EQ(watch.get_ticks(), 500);

        watch.measure();
        EXPECT_EQ(watch.get_ticks(), 600);

        watch.resume();
        timer.m_time += 150;
        watch.measure();
        EXPECT_EQ(watch.get_ticks(), 750);

        watch.pause();
        timer.m_time += 4000;

        watch.measure();
        EXPECT_EQ(watch.get_ticks(), 750);
    }

    TEST_CASE(TestMultipleMeasureOnRunningStopwatch)
    {
        Stopwatch<FakeTimer> watch(0);
        FakeTimer& timer = watch.get_timer();

        watch.start();
        timer.m_time += 400;
        watch.measure();
        watch.measure();
        watch.measure();
        watch.measure();
        EXPECT_EQ(watch.get_ticks(), 400);
    }

    TEST_CASE(TestMultipleMeasureOnPausedStopwatch)
    {
        Stopwatch<FakeTimer> watch(0);
        FakeTimer& timer = watch.get_timer();

        watch.start();
        timer.m_time += 5;

        watch.measure();
        watch.measure();
        watch.measure();
        watch.measure();
        EXPECT_EQ(watch.get_ticks(), 5);

        timer.m_time += 1;

        watch.pause();
        timer.m_time += 4;
        EXPECT_EQ(watch.get_ticks(), 5);

        watch.measure();
        watch.measure();
        watch.measure();
        watch.measure();
        EXPECT_EQ(watch.get_ticks(), 6);

        watch.resume();
        timer.m_time += 15;
        watch.measure();
        watch.measure();
        watch.measure();
        watch.measure();
        watch.measure();
        EXPECT_EQ(watch.get_ticks(), 21);

        watch.pause();
        timer.m_time += 4;

        watch.measure();
        watch.measure();
        watch.measure();
        watch.measure();
        watch.measure();
        EXPECT_EQ(watch.get_ticks(), 21);
    }
}
