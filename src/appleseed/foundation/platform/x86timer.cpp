
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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

// Platform headers.
#if defined _MSC_VER && defined _WIN64
#include <intrin.h>
#endif

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

X86Timer::X86Timer(const uint32 calibration_time_ms)
  : m_frequency(measure_timer_frequency(*this, calibration_time_ms))
{
    assert(m_frequency > 0);
}

uint64 X86Timer::frequency()
{
    return m_frequency;
}

uint64 X86Timer::read()
{
// Visual C++, 32-bit mode.
#if defined _MSC_VER && !defined _WIN64

    uint32 h, l;

    __asm
    {
        cpuid                   // force in-order execution of the RDTSC instruction
        rdtsc
        mov h, edx
        mov l, eax
    }

    return (static_cast<uint64>(h) << 32) | l;

// Visual C++, 64-bit mode.
#elif defined _MSC_VER && defined _WIN64

    int cpu_info[4];
    __cpuid(cpu_info, 0);       // force in-order execution of the RDTSC instruction

    return __rdtsc();

// gcc.
#elif defined __GNUC__

    uint32 h, l;

    asm volatile
    ("  rdtsc;                  \
        mov %%edx, %0;          \
        mov %%eax, %1"
        : "=rm" (h), "=rm" (l)  // outputs
        :                       // inputs
        : "%eax", "%edx"        // clobbered registers
    );

    return (static_cast<uint64>(h) << 32) | l;

// Other platforms.
#else

    #error The x86 timer is not supported on this platform.

#endif
}

}   // namespace foundation
