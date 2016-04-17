
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_UTILITY_STOPWATCH_H
#define APPLESEED_FOUNDATION_UTILITY_STOPWATCH_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/platform/timers.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <limits>

namespace foundation
{

//
// A stopwatch that allows to measure the amount of time elapsed between two instants.
//

template <typename Timer>
class Stopwatch
  : public NonCopyable
{
  public:
    // Constructor, calibrates the stopwatch.
    explicit Stopwatch(const size_t overhead_measures = 10);

    // Return the internal timer.
    Timer& get_timer();
    const Timer& get_timer() const;

    // Set the elapsed time to 0.
    void clear();

    // Start or restart the stopwatch.
    void start();

    // Measure the time elapsed since the last call to start().
    // The stopwatch keeps running.
    Stopwatch& measure();

    // Read the number of timer ticks elapsed since the last call to start().
    // measure() must have been called prior to calling this method.
    uint64 get_ticks() const;

    // Read the number of seconds elapsed since the last call to start().
    // measure() must have been called prior to calling this method.
    double get_seconds() const;

  private:
    Timer   m_timer;        // internal timer
    uint64  m_timer_freq;   // frequency of the internal timer
    uint64  m_overhead;     // measured overhead of calling start() + measure()
    uint64  m_start;        // timer value when start() is called
    uint64  m_elapsed;      // elapsed time when measure() is called, adjusted for overhead

    // Measure the overhead of calling start() + measure().
    uint64 measure_overhead(const size_t measures);
};


//
// Stopwatch class implementation.
//

template <typename Timer>
Stopwatch<Timer>::Stopwatch(const size_t overhead_measures)
{
    // Retrieve internal timer frequency.
    m_timer_freq = m_timer.frequency();

    // First, assume no overhead.
    m_overhead = 0;

    // Then, optionally measure the overhead.
    if (overhead_measures > 0)
    {
        // Measure the overhead of calling start() + measure().
        m_overhead = measure_overhead(overhead_measures);
    }

    m_start = 0;
    m_elapsed = 0;
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
inline void Stopwatch<Timer>::clear()
{
    m_elapsed = 0;
}

template <typename Timer>
inline void Stopwatch<Timer>::start()
{
    m_start = m_timer.read_start();
}

template <typename Timer>
inline Stopwatch<Timer>& Stopwatch<Timer>::measure()
{
    // Compute and store elapsed time.
    const uint64 end = m_timer.read_end();
    m_elapsed = end >= m_start ? end - m_start : 0;

    // Subtract known overhead from elapsed time.
    if (m_elapsed >= m_overhead)
        m_elapsed -= m_overhead;
    else m_elapsed = 0;

    return *this;
}

template <typename Timer>
inline uint64 Stopwatch<Timer>::get_ticks() const
{
    return m_elapsed;
}

template <typename Timer>
inline double Stopwatch<Timer>::get_seconds() const
{
    return static_cast<double>(get_ticks()) / m_timer_freq;
}

template <typename Timer>
uint64 Stopwatch<Timer>::measure_overhead(const size_t measures)
{
    assert(m_overhead == 0);
    assert(measures > 0);

    uint64 measured_overhead = std::numeric_limits<uint64>::max();

    // Measure the overhead of calling start() + measure() over a number of runs.
    for (size_t i = 0; i < measures; ++i)
    {
        // Perform a measurement cycle.
        start();
        measure();

        // Keep track of the smallest elapsed time over the runs.
        const uint64 elapsed = get_ticks();
        if (measured_overhead > elapsed)
            measured_overhead = elapsed;
    }

    assert(measured_overhead < std::numeric_limits<uint64>::max());

    return measured_overhead;
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_STOPWATCH_H
