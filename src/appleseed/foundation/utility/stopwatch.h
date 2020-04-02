
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

#pragma once

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/platform/timers.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <limits>

namespace foundation
{

//
// A stopwatch that allows to measure the amount of time elapsed between two instants.
//
// None of the methods of this class are thread-safe.
//

template <typename Timer>
class Stopwatch
  : public NonCopyable
{
  public:
    enum class State
    {
        Stopped,
        Running,
        Paused
    };

    // Constructor, optionally measures the stopwatch's overhead.
    explicit Stopwatch(const size_t overhead_measures = 0);

    // Return the internal timer.
    Timer& get_timer();
    const Timer& get_timer() const;

    // Return the state of the stopwatch.
    State get_state() const;

    // Start or restart the stopwatch.
    void start();

    // Stop the stopwatch.
    void stop();

    // Pause the stopwatch.
    void pause();

    // Resume the stopwatch.
    void resume();

    // Measure the time elapsed since the last call to start().
    // The stopwatch keeps running.
    Stopwatch& measure();

    // Clear the number of timer ticks recorded by measure().
    void clear();

    // Read the number of timer ticks recorded by the last call to measure().
    // Overhead is subtracted from the returned value.
    std::uint64_t get_ticks() const;

    // Read the number of seconds recorded by the last call to measure().
    // Overhead is subtracted from the returned value.
    double get_seconds() const;

  private:
    Timer           m_timer;        // internal timer
    std::uint64_t   m_timer_freq;   // frequency of the internal timer
    std::uint64_t   m_overhead;     // measured overhead of calling start() + measure()
    State           m_state;        // current state of the stopwatch
    std::uint64_t   m_start_time;   // timer value when start(), resume() or measure() is called
    std::uint64_t   m_pause_time;   // timer value when pause() is called
    std::uint64_t   m_elapsed;      // elapsed time when measure() is called
    std::uint64_t   m_committed;    // time spent before resume() was called

    // Measure the overhead of calling start() + measure().
    std::uint64_t measure_overhead(const size_t measures);
};


//
// Stopwatch class implementation.
//

template <typename Timer>
Stopwatch<Timer>::Stopwatch(const size_t overhead_measures)
  : m_overhead(0)
  , m_state(State::Stopped)
  , m_start_time(0)
  , m_pause_time(0)
  , m_elapsed(0)
  , m_committed(0)
{
    // Retrieve internal timer frequency.
    m_timer_freq = m_timer.frequency();

    // Optionally measure the overhead.
    if (overhead_measures > 0)
    {
        // Measure the overhead of calling start() + measure().
        m_overhead = measure_overhead(overhead_measures);
    }
}

template <typename Timer>
inline Timer& Stopwatch<Timer>::get_timer()
{
    return m_timer;
}

template <typename Timer>
inline const Timer& Stopwatch<Timer>::get_timer() const
{
    return m_timer;
}

template <typename Timer>
inline typename Stopwatch<Timer>::State Stopwatch<Timer>::get_state() const
{
    return m_state;
}

template <typename Timer>
inline void Stopwatch<Timer>::start()
{
    clear();

    m_state = State::Running;
    m_start_time = m_timer.read_start();
}

template <typename Timer>
inline void Stopwatch<Timer>::stop()
{
    m_state = State::Stopped;
}

template <typename Timer>
inline void Stopwatch<Timer>::pause()
{
    assert(m_state == State::Running);

    m_state = State::Paused;
    m_pause_time = m_timer.read_end();
}

template <typename Timer>
inline void Stopwatch<Timer>::resume()
{
    assert(m_state == State::Paused);

    // Update committed time.
    const std::uint64_t end = m_timer.read_end();
    m_committed += end >= m_start_time ? end - m_start_time : 0;
    m_committed -= end >= m_pause_time ? end - m_pause_time : 0;

    m_state = State::Running;
    m_start_time = m_timer.read_start();
}

template <typename Timer>
inline Stopwatch<Timer>& Stopwatch<Timer>::measure()
{
    if (m_state != State::Stopped)
    {
        // Compute and store elapsed time.
        const std::uint64_t end = m_timer.read_end();
        m_elapsed = end >= m_start_time ? end - m_start_time : 0;

        if (m_state == State::Paused)
            m_elapsed -= end >= m_pause_time ? end - m_pause_time : 0;

        m_elapsed += m_committed;
    }

    return *this;
}

template <typename Timer>
inline void Stopwatch<Timer>::clear()
{
    m_elapsed = 0;
    m_committed = 0;
}

template <typename Timer>
inline std::uint64_t Stopwatch<Timer>::get_ticks() const
{
    // Subtract known overhead from elapsed time.
    return m_elapsed >= m_overhead ? m_elapsed - m_overhead : 0;
}

template <typename Timer>
inline double Stopwatch<Timer>::get_seconds() const
{
    return static_cast<double>(get_ticks()) / m_timer_freq;
}

template <typename Timer>
std::uint64_t Stopwatch<Timer>::measure_overhead(const size_t measures)
{
    assert(m_overhead == 0);
    assert(measures > 0);

    std::uint64_t measured_overhead = std::numeric_limits<std::uint64_t>::max();

    // Measure the overhead of calling start() + measure() over a number of runs.
    for (size_t i = 0; i < measures; ++i)
    {
        // Perform a measurement cycle.
        start();
        measure();

        // Keep track of the smallest elapsed time over the runs.
        const std::uint64_t elapsed = get_ticks();
        if (measured_overhead > elapsed)
            measured_overhead = elapsed;
    }

    assert(measured_overhead < std::numeric_limits<std::uint64_t>::max());

    return measured_overhead;
}

}   // namespace foundation
