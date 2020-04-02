
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
#include "foundation/platform/atomic.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Boost headers.
#include "boost/smart_ptr/detail/spinlock.hpp"
#include "boost/thread/locks.hpp"
#include "boost/thread/mutex.hpp"
#include "boost/thread/thread.hpp"

// Standard headers.
#include <cstdint>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace foundation    { class Logger; }

namespace foundation
{

//
// Utility functions.
//

// Set the name of the current thread.
// For portability, limit the name to 16 characters, including the terminating zero.
APPLESEED_DLLSYMBOL void set_current_thread_name(const char* name);

// Suspend the current thread for a given number of milliseconds.
APPLESEED_DLLSYMBOL void sleep(const std::uint32_t ms);
APPLESEED_DLLSYMBOL void sleep(const std::uint32_t ms, IAbortSwitch& abort_switch);

// Give up the remainder of the current thread's time slice, to allow other threads to run.
APPLESEED_DLLSYMBOL void yield();


//
// A simple spinlock.
//

class Spinlock
  : public NonCopyable
{
  public:
    Spinlock();

    bool try_lock();
    void lock();
    void unlock();

    class ScopedLock
      : public NonCopyable
    {
      public:
        explicit ScopedLock(Spinlock& spinlock);

      private:
        boost::detail::spinlock::scoped_lock m_lock;
    };

  private:
    boost::detail::spinlock m_sp;
};


//
// A read/write lock.
//

struct NoWaitPolicy
{
    static void pause(const std::uint32_t iteration) {}
};

struct YieldWaitPolicy
{
    static void pause(const std::uint32_t iteration) { yield(); }
};

template <std::uint32_t ms>
struct SleepWaitPolicy
{
    static void pause(const std::uint32_t iteration) { sleep(ms); }
};

template <typename WaitPolicy = NoWaitPolicy>
class ReadWriteLock
  : public NonCopyable
{
  public:
    ReadWriteLock();

    bool try_lock_read();
    void lock_read();
    void unlock_read();

    bool try_lock_write();
    void lock_write();
    void unlock_write();

    class ScopedReadLock
      : public NonCopyable
    {
      public:
        explicit ScopedReadLock(ReadWriteLock& lock);
        ~ScopedReadLock();

      private:
        ReadWriteLock& m_lock;
    };

    class ScopedWriteLock
      : public NonCopyable
    {
      public:
        explicit ScopedWriteLock(ReadWriteLock& lock);
        ~ScopedWriteLock();

      private:
        ReadWriteLock& m_lock;
    };

  private:
    boost::atomic<std::uint32_t>    m_readers;
    boost::mutex                    m_mutex;
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
        Logger*                 logger = nullptr);

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
        Logger*                 logger = nullptr);

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
    explicit BenchmarkingThreadContext(Logger* logger = nullptr);

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
    mutable volatile std::uint32_t m_flag;
};


//
// Spinlock class implementation.
//

inline Spinlock::Spinlock()
{
    boost::detail::spinlock initialized_sp = BOOST_DETAIL_SPINLOCK_INIT;
    std::memcpy(&m_sp, &initialized_sp, sizeof(initialized_sp));
}

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

inline Spinlock::ScopedLock::ScopedLock(Spinlock& spinlock)
  : m_lock(spinlock.m_sp)
{
}


//
// ReadWriteLock class implementation.
//

template <typename WaitPolicy>
inline ReadWriteLock<WaitPolicy>::ReadWriteLock()
  : m_readers(0)
{
}

template <typename WaitPolicy>
inline bool ReadWriteLock<WaitPolicy>::try_lock_read()
{
    // Make sure there are no writers active.
    if (!m_mutex.try_lock())
        return false;

    // A new reader is active.
    ++m_readers;

    // Let other readers start.
    m_mutex.unlock();

    return true;
}

template <typename WaitPolicy>
inline void ReadWriteLock<WaitPolicy>::lock_read()
{
    // Make sure there are no writers active.
    m_mutex.lock();

    // A new reader is active.
    ++m_readers;

    // Let other readers start.
    m_mutex.unlock();
}

template <typename WaitPolicy>
inline void ReadWriteLock<WaitPolicy>::unlock_read()
{
    --m_readers;
}

template <typename WaitPolicy>
inline bool ReadWriteLock<WaitPolicy>::try_lock_write()
{
    // Make sure no reader can start.
    if (!m_mutex.try_lock())
        return false;

    // Wait until active readers have terminated.
    for (std::uint32_t i = 0; m_readers > 0; ++i)
        WaitPolicy::pause(i);

    return true;
}

template <typename WaitPolicy>
inline void ReadWriteLock<WaitPolicy>::lock_write()
{
    // Make sure no reader can start.
    m_mutex.lock();

    // Wait until active readers have terminated.
    for (std::uint32_t i = 0; m_readers > 0; ++i)
        WaitPolicy::pause(i);
}

template <typename WaitPolicy>
inline void ReadWriteLock<WaitPolicy>::unlock_write()
{
    m_mutex.unlock();
}

template <typename WaitPolicy>
inline ReadWriteLock<WaitPolicy>::ScopedReadLock::ScopedReadLock(ReadWriteLock& lock)
  : m_lock(lock)
{
    m_lock.lock_read();
}

template <typename WaitPolicy>
inline ReadWriteLock<WaitPolicy>::ScopedReadLock::~ScopedReadLock()
{
    m_lock.unlock_read();
}

template <typename WaitPolicy>
inline ReadWriteLock<WaitPolicy>::ScopedWriteLock::ScopedWriteLock(ReadWriteLock& lock)
  : m_lock(lock)
{
    m_lock.lock_write();
}

template <typename WaitPolicy>
inline ReadWriteLock<WaitPolicy>::ScopedWriteLock::~ScopedWriteLock()
{
    m_lock.unlock_write();
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

}   // namespace foundation
