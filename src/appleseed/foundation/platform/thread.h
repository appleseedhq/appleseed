
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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
#include "foundation/platform/types.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// boost headers.
#include "boost/interprocess/detail/atomic.hpp"
#include "boost/smart_ptr/detail/spinlock.hpp"
#include "boost/thread/mutex.hpp"
#pragma warning (push)
#pragma warning (disable : 4244)    // conversion from '__int64' to 'long', possible loss of data
#include "boost/thread/thread.hpp"
#pragma warning (pop)
#include "boost/version.hpp"

// Forward declarations.
namespace foundation    { class Logger; }

// Starting with Boost 1.48, the atomic primitives are defined in boost::interprocess::ipcdetail.
#if BOOST_VERSION >= 104800
namespace boost_atomic = boost::interprocess::ipcdetail;
#else
namespace boost_atomic = boost::interprocess::detail;
#endif

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
// An object to configure the current process and thread for accurate microbenchmarking.
//

class DLLSYMBOL BenchmarkingThreadContext
  : public NonCopyable
{
  public:
    // The constructor enables the benchmarking mode.
    BenchmarkingThreadContext();

    // The destructor restores previous settings.
    ~BenchmarkingThreadContext();

  private:
    struct Impl;
    Impl* impl;
};


//
// An object to configure the current process for background operation.
//

class DLLSYMBOL BackgroundProcessContext
  : public NonCopyable
{
  public:
    // The constructor enables the background mode.
    explicit BackgroundProcessContext(Logger& logger);

    // The destructor restores previous settings.
    ~BackgroundProcessContext();

  private:
    Logger& m_logger;
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
// Utility free functions.
//

// Suspend the current thread for a given number of milliseconds.
DLLSYMBOL void sleep(const uint32 ms);

// Give up the remainder of the current thread's time slice, to allow other threads to run.
DLLSYMBOL void yield();


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

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_PLATFORM_THREAD_H
