
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#elif defined __GNUC__
#include <sys/time.h>
#endif

using namespace std;

namespace foundation
{

//
// DefaultProcessorTimer class implementation.
//

DefaultProcessorTimer::DefaultProcessorTimer()
{
// Windows.
#if defined _WIN32

    // Check whether QueryPerformanceCounter() is available.
    LARGE_INTEGER frequency;
    m_has_qpc = QueryPerformanceFrequency(&frequency) != 0;

#endif
}

uint64 DefaultProcessorTimer::frequency()
{
// Windows.
#if defined _WIN32

    if (m_has_qpc)
    {
        LARGE_INTEGER frequency;
        QueryPerformanceFrequency(&frequency);
        return static_cast<uint64>(frequency.QuadPart);
    }
    else
    {
        return static_cast<uint64>(CLOCKS_PER_SEC);
    }

// Other platforms.
#else

    return static_cast<uint64>(CLOCKS_PER_SEC);

#endif
}

uint64 DefaultProcessorTimer::read()
{
// Windows.
#if defined _WIN32

    if (m_has_qpc)
    {
        LARGE_INTEGER count;
        QueryPerformanceCounter(&count);
        return static_cast<uint64>(count.QuadPart);
    }
    else
    {
        return static_cast<uint64>(clock());
    }

// Other platforms.
#else

    //
    // On Linux, we might want to use clock_gettime(CLOCK_REALTIME).
    //
    //   Andrei Alexandrescu, Writing Fast Code
    //   https://youtu.be/vrfYLlR8X8k?t=1973
    //

    return static_cast<uint64>(clock());

#endif
}


//
// DefaultWallclockTimer class implementation.
//

uint64 DefaultWallclockTimer::frequency()
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

uint64 DefaultWallclockTimer::read()
{
// POSIX platforms.
#if defined __GNUC__

    timeval tv;
    gettimeofday(&tv, 0);
    return static_cast<uint64>(tv.tv_sec) * 1000000 + static_cast<uint64>(tv.tv_usec);

// Windows.
#elif defined _WIN32

    __timeb64 tb;
    _ftime64(&tb);
    return static_cast<uint64>(tb.time) * 1000 + static_cast<uint64>(tb.millitm);

// Other platforms.
#else

    const time_t seconds = time(0);
    return static_cast<uint64>(seconds);

#endif
}

}   // namespace foundation
