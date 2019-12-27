
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
#include <ostream>

using namespace foundation;

namespace
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

    using StopwatchType = Stopwatch<FakeTimer>;
    using StateType = StopwatchType::State;
}

namespace foundation
{
    std::ostream& operator<<(std::ostream& s, const StateType state)
    {
        switch (state)
        {
          case StateType::Stopped: s << "stopped"; break;
          case StateType::Running: s << "running"; break;
          case StateType::Paused: s << "paused"; break;
          default: s << "unknown"; break;
        }

        return s;
    }
}

#define TIME_PASSES(n)                  \
    do                                  \
    {                                   \
        sw.get_timer().m_time += n;     \
    } while (false)

TEST_SUITE(Foundation_Utility_Stopwatch)
{
    //
    // Stopwatch is in initial state.
    //

    TEST_CASE(InitialState)
    {
        StopwatchType sw;

        EXPECT_EQ(StateType::Stopped, sw.get_state());
        EXPECT_EQ(0, sw.get_ticks());
    }

    TEST_CASE(Start_StopwatchIsInInitialState)
    {
        StopwatchType sw;

        // Action.
        sw.start();

        EXPECT_EQ(StateType::Running, sw.get_state());
        EXPECT_EQ(0, sw.get_ticks());
    }

    TEST_CASE(Stop_StopwatchIsInInitialState)
    {
        StopwatchType sw;

        // Action.
        sw.stop();

        EXPECT_EQ(StateType::Stopped, sw.get_state());
        EXPECT_EQ(0, sw.get_ticks());
    }

    // Can't pause while in initial state.

    // Can't resume while in initial state.

    TEST_CASE(Measure_StopwatchIsInInitialState)
    {
        StopwatchType sw;

        // Action.
        TIME_PASSES(100);
        sw.measure();

        EXPECT_EQ(StateType::Stopped, sw.get_state());
        EXPECT_EQ(0, sw.get_ticks());
    }

    //
    // Stopwatch was started.
    //

    TEST_CASE(Start_StopwatchWasStarted)
    {
        StopwatchType sw;

        sw.start();

        // Action.
        sw.start();

        EXPECT_EQ(StateType::Running, sw.get_state());
        EXPECT_EQ(0, sw.get_ticks());
    }

    TEST_CASE(Stop_StopwatchWasStarted)
    {
        StopwatchType sw;

        sw.start();

        // Action.
        sw.stop();

        EXPECT_EQ(StateType::Stopped, sw.get_state());
        EXPECT_EQ(0, sw.get_ticks());
    }

    TEST_CASE(Pause_StopwatchWasStarted)
    {
        StopwatchType sw;

        sw.start();

        // Action.
        sw.pause();

        EXPECT_EQ(StateType::Paused, sw.get_state());
        EXPECT_EQ(0, sw.get_ticks());
    }

    // Can't resume while running.

    TEST_CASE(Measure_StopwatchWasStarted)
    {
        StopwatchType sw;

        sw.start();

        // Action.
        TIME_PASSES(100);
        sw.measure();

        EXPECT_EQ(StateType::Running, sw.get_state());
        EXPECT_EQ(100, sw.get_ticks());
    }

    //
    // Stopwatch was started, then measured.
    //

    TEST_CASE(Start_StopwatchWasStartedThenMeasured)
    {
        StopwatchType sw;

        sw.start();

        TIME_PASSES(100);
        sw.measure();

        // Action.
        sw.start();

        EXPECT_EQ(StateType::Running, sw.get_state());
        EXPECT_EQ(0, sw.get_ticks());
    }

    TEST_CASE(Stop_StopwatchWasStartedThenMeasured)
    {
        StopwatchType sw;

        sw.start();

        TIME_PASSES(100);
        sw.measure();

        // Action.
        sw.stop();

        EXPECT_EQ(StateType::Stopped, sw.get_state());
        EXPECT_EQ(100, sw.get_ticks());
    }

    TEST_CASE(Pause_StopwatchWasStartedThenMeasured)
    {
        StopwatchType sw;

        sw.start();

        TIME_PASSES(100);
        sw.measure();

        // Action.
        sw.pause();

        EXPECT_EQ(StateType::Paused, sw.get_state());
        EXPECT_EQ(100, sw.get_ticks());
    }

    // Can't resume while running.

    TEST_CASE(Measure_StopwatchWasStartedThenMeasured)
    {
        StopwatchType sw;

        sw.start();

        TIME_PASSES(100);
        sw.measure();

        // Action.
        TIME_PASSES(10);
        sw.measure();

        EXPECT_EQ(StateType::Running, sw.get_state());
        EXPECT_EQ(110, sw.get_ticks());
    }

    //
    // Stopwatch was started, then measured, then stopped.
    //

    TEST_CASE(Start_StopwatchWasStartedThenMeasuredThenStopped)
    {
        StopwatchType sw;

        sw.start();

        TIME_PASSES(100);
        sw.measure();

        sw.stop();

        // Action.
        sw.start();

        EXPECT_EQ(StateType::Running, sw.get_state());
        EXPECT_EQ(0, sw.get_ticks());
    }

    TEST_CASE(Stop_StopwatchWasStartedThenMeasuredThenStopped)
    {
        StopwatchType sw;

        sw.start();

        TIME_PASSES(100);
        sw.measure();

        sw.stop();

        // Action.
        sw.stop();

        EXPECT_EQ(StateType::Stopped, sw.get_state());
        EXPECT_EQ(100, sw.get_ticks());
    }

    // Can't pause while stopped.

    // Can't resume while stopped.

    TEST_CASE(Measure_StopwatchWasStartedThenMeasuredThenStopped)
    {
        StopwatchType sw;

        sw.start();

        TIME_PASSES(100);
        sw.measure();

        sw.stop();

        // Action.
        TIME_PASSES(10);
        sw.measure();

        EXPECT_EQ(StateType::Stopped, sw.get_state());
        EXPECT_EQ(100, sw.get_ticks());
    }

    //
    // Stopwatch was started, then measured, then paused.
    //

    TEST_CASE(Start_StopwatchWasStartedThenMeasuredThenPaused)
    {
        StopwatchType sw;

        sw.start();

        TIME_PASSES(100);
        sw.measure();

        sw.pause();

        // Action.
        sw.start();

        EXPECT_EQ(StateType::Running, sw.get_state());
        EXPECT_EQ(0, sw.get_ticks());
    }

    TEST_CASE(Stop_StopwatchWasStartedThenMeasuredThenPaused)
    {
        StopwatchType sw;

        sw.start();

        TIME_PASSES(100);
        sw.measure();

        sw.pause();

        // Action.
        sw.stop();

        EXPECT_EQ(StateType::Stopped, sw.get_state());
        EXPECT_EQ(100, sw.get_ticks());
    }

    // Can't pause while paused.

    TEST_CASE(Resume_StopwatchWasStartedThenMeasuredThenPaused)
    {
        StopwatchType sw;

        sw.start();

        TIME_PASSES(100);
        sw.measure();

        sw.pause();

        // Action.
        sw.resume();

        EXPECT_EQ(StateType::Running, sw.get_state());
        EXPECT_EQ(100, sw.get_ticks());
    }

    TEST_CASE(Measure_StopwatchWasStartedThenMeasuredThenPaused)
    {
        StopwatchType sw;

        sw.start();

        TIME_PASSES(100);
        sw.measure();

        sw.pause();

        // Action.
        TIME_PASSES(10);
        sw.measure();

        EXPECT_EQ(StateType::Paused, sw.get_state());
        EXPECT_EQ(100, sw.get_ticks());
    }

    //
    // Stopwatch was paused, then measured, then resumed.
    //

    TEST_CASE(Start_StopwatchWasPausedThenMeasuredThenResumed)
    {
        StopwatchType sw;

        sw.start();

        TIME_PASSES(100);
        sw.measure();

        sw.pause();

        TIME_PASSES(10);
        sw.measure();

        sw.resume();

        TIME_PASSES(1);
        sw.measure();

        // Action.
        sw.start();

        EXPECT_EQ(StateType::Running, sw.get_state());
        EXPECT_EQ(0, sw.get_ticks());
    }

    TEST_CASE(Stop_StopwatchWasPausedThenMeasuredThenResumed)
    {
        StopwatchType sw;

        sw.start();

        TIME_PASSES(100);
        sw.measure();

        sw.pause();

        TIME_PASSES(10);
        sw.measure();

        sw.resume();

        TIME_PASSES(1);
        sw.measure();

        // Action.
        sw.stop();

        EXPECT_EQ(StateType::Stopped, sw.get_state());
        EXPECT_EQ(101, sw.get_ticks());
    }

    TEST_CASE(Pause_StopwatchWasPausedThenMeasuredThenResumed)
    {
        StopwatchType sw;

        sw.start();

        TIME_PASSES(100);
        sw.measure();

        sw.pause();

        TIME_PASSES(10);
        sw.measure();

        sw.resume();

        TIME_PASSES(1);
        sw.measure();

        // Action.
        sw.pause();

        EXPECT_EQ(StateType::Paused, sw.get_state());
        EXPECT_EQ(101, sw.get_ticks());
    }

    // Can't resume while resumed.

    TEST_CASE(Measure_StopwatchWasPausedThenMeasuredThenResumed)
    {
        StopwatchType sw;

        sw.start();

        TIME_PASSES(1000);
        sw.measure();

        sw.pause();

        TIME_PASSES(100);
        sw.measure();

        sw.resume();

        TIME_PASSES(10);
        sw.measure();

        // Action.
        TIME_PASSES(1);
        sw.measure();

        EXPECT_EQ(StateType::Running, sw.get_state());
        EXPECT_EQ(1011, sw.get_ticks());
    }
}
