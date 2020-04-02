
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
// Reference:
//
//    How to Benchmark Code Execution Times on Intel IA-32 and IA-64 Instruction Set Architectures
//    http://www.intel.com/content/dam/www/public/us/en/documents/white-papers/ia-32-ia-64-benchmark-code-execution-paper.pdf
//

namespace
{
    // Measure and return the frequency (in Hz) of a given timer.
    template <typename Timer>
    std::uint64_t measure_timer_frequency(Timer& timer, const std::uint32_t calibration_time_ms)
    {
        const std::uint64_t begin = timer.read_start();
        sleep(calibration_time_ms);
        const std::uint64_t end = timer.read_end();
        return ((end - begin) * 1000) / calibration_time_ms;
    }
}

X86Timer::X86Timer(const std::uint32_t calibration_time_ms)
  : m_frequency(measure_timer_frequency(*this, calibration_time_ms))
{
    assert(m_frequency > 0);
}

std::uint64_t X86Timer::frequency()
{
    return m_frequency;
}

std::uint64_t X86Timer::read_start()
{
// Visual C++, 32-bit mode.
#if defined _MSC_VER && !defined _WIN64

    std::uint32_t h, l;

    __asm
    {
        cpuid                               // force in-order execution of the RDTSC instruction
        rdtsc
        mov h, edx
        mov l, eax
    }

    return (static_cast<std::uint64_t>(h) << 32) | l;

// Visual C++, 64-bit mode.
#elif defined _MSC_VER && defined _WIN64

    int cpu_info[4];
    __cpuid(cpu_info, 0);                   // force in-order execution of the RDTSC instruction

    return __rdtsc();

// gcc.
#elif defined __GNUC__

    std::uint32_t h, l, _dummy;

    // %ebx may be used to point to GOT for PIC on 32-bit x86, so it must be
    // preserved (cf. src/appleseed/foundation/platform/system.cpp).
    // We force in-order execution of the RDTSC instruction by calling CPUID
    // first.  Reference: "Using the RDTSC Instruction for Performance
    // Monitoring", Section 3.1, p. 3 [Intel 1997].
    asm volatile (
#if __x86_64__
        "movq %%rbx, %q2\n\t"
        "cpuid\n\t"
        "xchgq %%rbx, %q2\n\t"
#else
        "movl %%ebx, %2\n\t"
        "cpuid\n\t"
        "xchgl %%ebx, %2\n\t"
#endif
        "rdtsc"
        : "=d" (h), "=a" (l), "=r" (_dummy)
      : : "ecx"
    );

    return (static_cast<std::uint64_t>(h) << 32) | l;

// Other platforms.
#else

    #error The x86 timer is not supported on this platform.

#endif
}

std::uint64_t X86Timer::read_end()
{
// Visual C++, 32-bit mode.
#if defined _MSC_VER && !defined _WIN64

    std::uint32_t h, l;

    __asm
    {
        rdtscp
        mov h, edx
        mov l, eax
        cpuid                               // prevent instructions coming afterward from executing before the RDTSCP instruction
    }

    return (static_cast<std::uint64_t>(h) << 32) | l;

// Visual C++, 64-bit mode.
#elif defined _MSC_VER && defined _WIN64

    unsigned int aux;
    const std::uint64_t value = __rdtscp(&aux);

    int cpu_info[4];
    __cpuid(cpu_info, 0);                   // prevent instructions coming afterward from executing before the RDTSCP instruction

    return value;

// gcc.
#elif defined __GNUC__

    std::uint32_t h, l, _dummy;

    // Here we call CPUID to prevent instructions coming afterward from
    // executing before the RDTSCP instruction.  Reference: "How to
    // Benchmark Code Execution Times on Intel IA-32 and IA-64 Instruction
    // Set Architectures", Section 3.2.1, p. 16 [Intel 2010].
    asm volatile (
        "rdtscp\n\t"
        "movl %%edx, %0\n\t"
        "movl %%eax, %1\n\t"
#if __x86_64__
        "movq %%rbx, %q2\n\t"
        "cpuid\n\t"
        "xchgq %%rbx, %q2\n\t"
#else
        "movl %%ebx, %2\n\t"
        "cpuid\n\t"
        "xchgl %%ebx, %2\n\t"
#endif
        : "=m" (h), "=m" (l), "=r" (_dummy)
      : : "eax", "ecx", "edx"
    );

    return (static_cast<std::uint64_t>(h) << 32) | l;

// Other platforms.
#else

    #error The x86 timer is not supported on this platform.

#endif
}

}   // namespace foundation
