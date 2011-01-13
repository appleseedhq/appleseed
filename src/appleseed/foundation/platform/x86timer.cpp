
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

// Interface header.
#include "x86timer.h"

// appleseed.foundation headers.
#include "foundation/platform/thread.h"

// Standard headers.
#include <cassert>

namespace foundation
{

//
// X86Timer class implementation.
//

namespace
{
    // Measure and return the frequency (in Hz) of a given timer.
    template <typename Timer>
    uint64 measure_timer_frequency(Timer& timer, const uint32 calibration_time_ms)
    {
        const uint64 begin = timer.read();
        sleep(calibration_time_ms);
        const uint64 end = timer.read();
        return ((end - begin) * 1000) / calibration_time_ms;
    }
}

// Constructor.
X86Timer::X86Timer(const uint32 calibration_time_ms)
  : m_frequency(measure_timer_frequency(*this, calibration_time_ms))
{
    assert(m_frequency > 0);
}

// Get the timer frequency, in Hz.
uint64 X86Timer::frequency()
{
    return m_frequency;
}

// Read the timer value.
uint64 X86Timer::read()
{
    uint32 h, l;

// Visual C++.
#if defined _MSC_VER

    __asm
    {
        cpuid       // force in-order execution of the RDTSC instruction
        rdtsc
        mov h, edx
        mov l, eax
    }

// gcc.
#elif defined __GNUC__

    asm volatile
    ("                                                                          \
        rdtsc;                                                                  \
        mov %%edx, %0;                                                          \
        mov %%eax, %1"
        : "=rm" (h), "=rm" (l)  // outputs
        :                       // inputs
        : "%eax", "%edx"        // clobbered registers
    );

// Unsupported platform.
#else
#error This timer is not supported on this platform.
#endif

    return (static_cast<uint64>(h) << 32) | l;
}

}   // namespace foundation
