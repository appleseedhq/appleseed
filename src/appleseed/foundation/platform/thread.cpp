
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
#include "thread.h"

// appleseed.foundation headers.
#ifdef _WIN32
#include "foundation/platform/windows.h"
#endif

// boost headers.
#include "boost/date_time/posix_time/posix_time_types.hpp"

// Standard headers.
#include <cassert>

using namespace boost;

namespace foundation
{

// Suspend the current thread for a given number of milliseconds.
void sleep(const uint32 ms)
{
    this_thread::sleep(posix_time::milliseconds(ms));
}

// Give up the remainder of the current thread's time slice, to allow other threads to run.
void yield()
{
    this_thread::yield();
}


//
// BenchmarkingThreadContext class implementation (Windows).
//

#ifdef _WIN32

struct BenchmarkingThreadContext::Impl
{
    DWORD       m_process_priority_class;
    int         m_thread_priority;
    DWORD_PTR   m_thread_affinity_mask;
};

// Constructor.
BenchmarkingThreadContext::BenchmarkingThreadContext()
  : impl(new Impl())
{
    // Set the process priority class.
    impl->m_process_priority_class = GetPriorityClass(GetCurrentProcess());
    SetPriorityClass(GetCurrentProcess(), HIGH_PRIORITY_CLASS);

    // Set the thread priority.
    impl->m_thread_priority = GetThreadPriority(GetCurrentThread());
    SetThreadPriority(GetCurrentThread(), THREAD_PRIORITY_TIME_CRITICAL);

    // Set the thread affinity mask.
    impl->m_thread_affinity_mask = SetThreadAffinityMask(GetCurrentThread(), 1);
}

// Destructor.
BenchmarkingThreadContext::~BenchmarkingThreadContext()
{
    // Restore the previous settings.
    SetThreadAffinityMask(GetCurrentThread(), impl->m_thread_affinity_mask);
    SetThreadPriority(GetCurrentThread(), impl->m_thread_priority);
    SetPriorityClass(GetCurrentProcess(), impl->m_process_priority_class);
    delete impl;
}


//
// BenchmarkingThreadContext class implementation (other platforms).
//

#else

// Constructor.
BenchmarkingThreadContext::BenchmarkingThreadContext()
{
    // Do nothing on unsupported platforms.
}

// Destructor.
BenchmarkingThreadContext::~BenchmarkingThreadContext()
{
    // Do nothing on unsupported platforms.
}

#endif

}   // namespace foundation
