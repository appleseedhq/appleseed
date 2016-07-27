
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

#ifndef APPLESEED_FOUNDATION_PLATFORM_THREAD_H
#define APPLESEED_FOUNDATION_PLATFORM_THREAD_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/platform/atomic.h"
#include "foundation/platform/types.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Boost headers.
#include "boost/smart_ptr/detail/spinlock.hpp"
#include "boost/thread/mutex.hpp"
#include "boost/thread/thread.hpp"

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace foundation    { class Logger; }

namespace foundation
{

//
// A spinlock based on boost::detail::spinlock.
//

class Spinlock
  : public NonCopyable
{
  public:
    Spinlock();

    bool try_lock();

    void lock();

    void unlock();

    struct ScopedLock
      : public NonCopyable
    {
        boost::detail::spinlock::scoped_lock m_lock;

        explicit ScopedLock(Spinlock& spinlock);
    };

  private:
    boost::detail::spinlock m_sp;
};


//
// Process/thread priority levels.
//

enum ProcessPriority
{
    ProcessPriorityLowest,
    ProcessPriorityLow,
    ProcessPriorityNormal,
    ProcessPriorityHigh,
    ProcessPriorityHighest
};


//
// An object to set the priority for the current process.
//

class APPLESEED_DLLSYMBOL ProcessPriorityContext
  : public NonCopyable
{
  public:
    // The constructor sets the priority of the current process.
    ProcessPriorityContext(
        const ProcessPriority   priority,
        Logger*                 logger = 0);

    // The destructor restores previous settings.
    ~ProcessPriorityContext();

  private:
    struct Impl;
    Impl* impl;
};


//
// An object to set the priority for the current thread.
//

class APPLESEED_DLLSYMBOL ThreadPriorityContext
  : public NonCopyable
{
  public:
    // The constructor sets the priority of the current thread.
    ThreadPriorityContext(
        const ProcessPriority   priority,
        Logger*                 logger = 0);

    // The destructor restores previous settings.
    ~ThreadPriorityContext();

  private:
    struct Impl;
    Impl* impl;
};


//
// An object to configure the current process and thread for accurate microbenchmarking.
//

class APPLESEED_DLLSYMBOL BenchmarkingThreadContext
  : public NonCopyable
{
  public:
    // The constructor enables the benchmarking mode.
    explicit BenchmarkingThreadContext(Logger* logger = 0);

    // The destructor restores previous settings.
    ~BenchmarkingThreadContext();

  private:
    struct Impl;
    Impl* impl;
};


//
// Wraps a thread function pointer into a callable thread function object.
//

template <typename Function>
class ThreadFunctionWrapper
{
  public:
    explicit ThreadFunctionWrapper(Function* function)
      : m_function(function)
    {
    }

    ThreadFunctionWrapper(const ThreadFunctionWrapper& rhs)
      : m_function(rhs.m_function)
    {
    }

    void operator()()
    {
        (*m_function)();
    }

  private:
    Function* m_function;
};


//
// A cross-thread, cross-DLL boolean flag.
//

class APPLESEED_DLLSYMBOL ThreadFlag
{
  public:
    // Constructor, clears the flag.
    ThreadFlag();

    // Clear the flag.
    void clear();

    // Set the flag.
    void set();

    // Check the flag.
    bool is_clear() const;
    bool is_set() const;

  private:
    mutable volatile uint32 m_flag;
};


//
// Utility free functions.
//

// Set the name of the current thread.
// For portability, limit the name to 16 characters, including the terminating zero.
APPLESEED_DLLSYMBOL void set_current_thread_name(const char* name);

// Suspend the current thread for a given number of milliseconds.
APPLESEED_DLLSYMBOL void sleep(const uint32 ms);
APPLESEED_DLLSYMBOL void sleep(const uint32 ms, IAbortSwitch& abort_switch);

// Give up the remainder of the current thread's time slice, to allow other threads to run.
APPLESEED_DLLSYMBOL void yield();


//
// Spinlock class implementation.
//

inline bool Spinlock::try_lock()
{
    return m_sp.try_lock();
}

inline void Spinlock::lock()
{
    m_sp.lock();
}

inline void Spinlock::unlock()
{
    m_sp.unlock();
}

inline Spinlock::Spinlock()
{
    // todo: is there a simpler way to initialize m_sp in a platform-independent manner?
    boost::detail::spinlock initialized_sp = BOOST_DETAIL_SPINLOCK_INIT;
    m_sp = initialized_sp;
}

inline Spinlock::ScopedLock::ScopedLock(Spinlock& spinlock)
  : m_lock(spinlock.m_sp)
{
}


//
// ThreadFlag class implementation.
//

inline ThreadFlag::ThreadFlag()
{
    clear();
}

inline void ThreadFlag::clear()
{
    atomic_write(&m_flag, 0);
}

inline void ThreadFlag::set()
{
    atomic_write(&m_flag, 1);
}

inline bool ThreadFlag::is_clear() const
{
    return atomic_read(&m_flag) == 0;
}

inline bool ThreadFlag::is_set() const
{
    return !is_clear();
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_PLATFORM_THREAD_H
