
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
#include "defaulttimers.h"

// appleseed.foundation headers.
#ifdef _WIN32
#include "foundation/platform/windows.h"
#endif

// Standard headers.
#include <ctime>
#if defined _WIN32
#include <sys/timeb.h>
#include <sys/types.h>
#elif defined __linux__ || defined __FreeBSD__
#include <time.h>
#include <sys/time.h>
#else
#include <sys/time.h>
#endif

namespace foundation
{

//
// DefaultProcessorTimer class implementation.
//

std::uint64_t DefaultProcessorTimer::frequency()
{
// Windows.
#if defined _WIN32

    // QueryPerformanceFrequency() never returns FALSE on Windows XP and later:
    // https://docs.microsoft.com/en-us/windows/desktop/sysinfo/acquiring-high-resolution-time-stamps#faq-about-programming-with-qpc-and-tsc
    LARGE_INTEGER frequency;
    QueryPerformanceFrequency(&frequency);
    return static_cast<std::uint64_t>(frequency.QuadPart);

// Linux and FreeBSD.
#elif defined __linux__ || defined __FreeBSD__

    return 1000000000ULL;

// Other platforms.
#else

    return static_cast<std::uint64_t>(CLOCKS_PER_SEC);

#endif
}

std::uint64_t DefaultProcessorTimer::read()
{
// Windows.
#if defined _WIN32

    LARGE_INTEGER count;
    QueryPerformanceCounter(&count);
    return static_cast<std::uint64_t>(count.QuadPart);

// Linux and FreeBSD.
#elif defined __linux__ || defined __FreeBSD__

    struct timespec ts;
    return
        clock_gettime(CLOCK_MONOTONIC, &ts) == 0
            ? static_cast<std::uint64_t>(ts.tv_sec) * 1000000000ULL + static_cast<std::uint64_t>(ts.tv_nsec)
            : 0;

// Other platforms.
#else

    return static_cast<std::uint64_t>(clock());

#endif
}


//
// DefaultWallclockTimer class implementation.
//

std::uint64_t DefaultWallclockTimer::frequency()
{
// POSIX platforms.
#if defined __GNUC__

    return 1000000;

// Windows.
#elif defined _WIN32

    return 1000;

// Other platforms.
#else

    return 1;

#endif
}

std::uint64_t DefaultWallclockTimer::read()
{
// POSIX platforms.
#if defined __GNUC__

    timeval tv;
    gettimeofday(&tv, nullptr);
    return static_cast<std::uint64_t>(tv.tv_sec) * 1000000 + static_cast<std::uint64_t>(tv.tv_usec);

// Windows.
#elif defined _WIN32

    __timeb64 tb;
    _ftime64(&tb);
    return static_cast<std::uint64_t>(tb.time) * 1000 + static_cast<std::uint64_t>(tb.millitm);

// Other platforms.
#else

    const time_t seconds = time(0);
    return static_cast<std::uint64_t>(seconds);

#endif
}

}   // namespace foundation
