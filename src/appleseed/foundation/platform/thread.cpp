
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
#include "thread.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/platform/defaulttimers.h"
#ifdef _WIN32
#include "foundation/platform/windows.h"
#endif
#include "foundation/utility/job/iabortswitch.h"
#include "foundation/log/log.h"

// Boost headers.
#include "boost/chrono.hpp"

// Standard headers.
#include <cassert>

// Platform headers.
#if defined __APPLE__
#include <pthread.h>
#elif defined __FreeBSD__
#include <pthread.h>
#include <pthread_np.h>
#elif defined __linux__
#include <sys/prctl.h>
#endif

using namespace boost;

namespace foundation
{

//
// Utility functions implementation.
//

// Windows.
#if defined _WIN32

    //
    // Reference:
    //
    //   How to: Set a Thread Name in Native Code
    //   https://msdn.microsoft.com/en-us/library/xcb2z8hs.aspx
    //
    // Also:
    //
    //   Issue 2692213003: Use Windows 10 thread naming API, SetThreadDescription (Closed)
    //   https://codereview.chromium.org/2692213003
    //

#pragma pack(push, 8)
    struct ThreadNameInfo
    {
       DWORD    dwType;         // must be 0x1000
       LPCSTR   szName;         // pointer to name (in user address space)
       DWORD    dwThreadID;     // thread ID (-1 = caller thread)
       DWORD    dwFlags;        // reserved for future use, must be zero
    };
#pragma pack(pop)

    void set_current_thread_name(const char* name)
    {
        __try
        {
            ThreadNameInfo info;
            info.dwType = 0x1000;
            info.szName = name;
            info.dwThreadID = -1;
            info.dwFlags = 0;

            const DWORD VisualStudioException = 0x406D1388;
            RaiseException(VisualStudioException, 0, sizeof(info) / sizeof(ULONG_PTR), (ULONG_PTR*)&info);
        }
        __except (EXCEPTION_EXECUTE_HANDLER)
        {
        }
    }

// macOS.
#elif defined __APPLE__

    void set_current_thread_name(const char* name)
    {
        pthread_setname_np(name);
    }

// FreeBSD.
#elif defined __FreeBSD__

    void set_current_thread_name(const char* name)
    {
        pthread_set_name_np(pthread_self(), name);
    }

// Linux.
#elif defined __linux__

    void set_current_thread_name(const char* name)
    {
        prctl(PR_SET_NAME, (unsigned long)name, 0, 0, 0);
    }

// Other platforms.
#else

    void set_current_thread_name(const char* name)
    {
        // Do nothing.
    }

#endif

void sleep(const std::uint32_t ms)
{
    this_thread::sleep_for(chrono::milliseconds(ms));
}

void sleep(const std::uint32_t ms, IAbortSwitch& abort_switch)
{
    const chrono::milliseconds one_ms(1);

    DefaultWallclockTimer timer;

    const std::uint64_t freq = timer.frequency();
    const std::uint64_t start_time = timer.read_start();

    while (!abort_switch.is_aborted())
    {
        const std::uint64_t elapsed_ticks = timer.read_end() - start_time;
        const std::uint64_t elapsed_ms = (1000 * elapsed_ticks) / freq;

        if (elapsed_ms >= ms)
            break;

        this_thread::sleep_for(one_ms);
    }
}

void yield()
{
    this_thread::yield();
}


//
// ProcessPriorityContext class implementation.
//

#ifdef _WIN32

    //
    // Reference:
    //
    //   http://msdn.microsoft.com/en-us/library/windows/desktop/ms685100(v=vs.85).aspx
    //

    namespace
    {
        DWORD get_priority_class(const ProcessPriority priority)
        {
            switch (priority)
            {
              case ProcessPriorityLowest:   return IDLE_PRIORITY_CLASS;
              case ProcessPriorityLow:      return BELOW_NORMAL_PRIORITY_CLASS;
              case ProcessPriorityNormal:   return NORMAL_PRIORITY_CLASS;
              case ProcessPriorityHigh:     return ABOVE_NORMAL_PRIORITY_CLASS;
              case ProcessPriorityHighest:  return HIGH_PRIORITY_CLASS;
              default:
                APPLESEED_UNREACHABLE;
                return NORMAL_PRIORITY_CLASS;
            }
        }

        void set_current_process_priority_class(const DWORD priority_class, Logger* logger)
        {
            if (!SetPriorityClass(GetCurrentProcess(), priority_class))
            {
                if (logger)
                {
                    LOG_WARNING(
                        *logger,
                        "failed to set process priority class to %lu (%lu).",
                        priority_class,
                        GetLastError());
                }
            }
        }
    }

    struct ProcessPriorityContext::Impl
    {
        Logger* m_logger;
        DWORD   m_initial_priority_class;
    };

    ProcessPriorityContext::ProcessPriorityContext(
        const ProcessPriority   priority,
        Logger*                 logger)
      : impl(new Impl())
    {
        impl->m_logger = logger;
        impl->m_initial_priority_class = GetPriorityClass(GetCurrentProcess());

        set_current_process_priority_class(
            get_priority_class(priority),
            impl->m_logger);
    }

    ProcessPriorityContext::~ProcessPriorityContext()
    {
        set_current_process_priority_class(
            impl->m_initial_priority_class,
            impl->m_logger);

        delete impl;
    }

#else

    ProcessPriorityContext::ProcessPriorityContext(
        const ProcessPriority   priority,
        Logger*                 logger)
    {
        // todo: implement.
    }

    ProcessPriorityContext::~ProcessPriorityContext()
    {
    }

#endif


//
// ThreadPriorityContext class implementation.
//

#ifdef _WIN32

    //
    // Reference:
    //
    //   http://msdn.microsoft.com/en-us/library/windows/desktop/ms685100(v=vs.85).aspx
    //

    namespace
    {
        int get_thread_priority_level(const ProcessPriority priority)
        {
            switch (priority)
            {
              case ProcessPriorityLowest:   return THREAD_PRIORITY_LOWEST;
              case ProcessPriorityLow:      return THREAD_PRIORITY_BELOW_NORMAL;
              case ProcessPriorityNormal:   return THREAD_PRIORITY_NORMAL;
              case ProcessPriorityHigh:     return THREAD_PRIORITY_ABOVE_NORMAL;
              case ProcessPriorityHighest:  return THREAD_PRIORITY_HIGHEST;
              default:
                APPLESEED_UNREACHABLE;
                return THREAD_PRIORITY_NORMAL;
            }
        }

        void set_current_thread_priority_level(const int thread_priority, Logger* logger)
        {
            if (!SetThreadPriority(GetCurrentThread(), thread_priority))
            {
                if (logger)
                {
                    LOG_WARNING(
                        *logger,
                        "failed to set thread priority level to %d (%lu).",
                        thread_priority,
                        GetLastError());
                }
            }
        }
    }

    struct ThreadPriorityContext::Impl
    {
        Logger* m_logger;
        int     m_initial_priority_level;
    };

    ThreadPriorityContext::ThreadPriorityContext(
        const ProcessPriority   priority,
        Logger*                 logger)
      : impl(new Impl())
    {
        impl->m_logger = logger;
        impl->m_initial_priority_level = GetThreadPriority(GetCurrentThread());

        set_current_thread_priority_level(
            get_thread_priority_level(priority),
            impl->m_logger);
    }

    ThreadPriorityContext::~ThreadPriorityContext()
    {
        set_current_thread_priority_level(
            impl->m_initial_priority_level,
            impl->m_logger);

        delete impl;
    }

#else

    ThreadPriorityContext::ThreadPriorityContext(
        const ProcessPriority   priority,
        Logger*                 logger)
    {
        // todo: implement.
    }

    ThreadPriorityContext::~ThreadPriorityContext()
    {
    }

#endif


//
// BenchmarkingThreadContext class implementation.
//

#ifdef _WIN32

    struct BenchmarkingThreadContext::Impl
    {
        ProcessPriorityContext  m_process_priority_context;
        ThreadPriorityContext   m_thread_priority_context;
        std::uint64_t           m_thread_affinity_mask;

        explicit Impl(Logger* logger)
          : m_process_priority_context(ProcessPriorityHighest, logger)
          , m_thread_priority_context(ProcessPriorityHighest, logger)
          , m_thread_affinity_mask(SetThreadAffinityMask(GetCurrentThread(), 1))
        {
        }

        ~Impl()
        {
            SetThreadAffinityMask(GetCurrentThread(), m_thread_affinity_mask);
        }
    };

    BenchmarkingThreadContext::BenchmarkingThreadContext(Logger* logger)
      : impl(new Impl(logger))
    {
    }

    BenchmarkingThreadContext::~BenchmarkingThreadContext()
    {
        delete impl;
    }

#else

    BenchmarkingThreadContext::BenchmarkingThreadContext(Logger* logger)
    {
        // todo: implement.
    }

    BenchmarkingThreadContext::~BenchmarkingThreadContext()
    {
    }

#endif

}   // namespace foundation
