
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

#ifndef APPLESEED_FOUNDATION_PLATFORM_THREAD_H
#define APPLESEED_FOUNDATION_PLATFORM_THREAD_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/platform/types.h"

// boost headers.
#include "boost/smart_ptr/detail/spinlock.hpp"
#include "boost/thread/mutex.hpp"
#include "boost/thread/thread.hpp"

//
// On Windows, define FOUNDATIONDLL to __declspec(dllexport) when building the DLL
// and to __declspec(dllimport) when building an application using the DLL.
// Other platforms don't use this export mechanism and the symbol FOUNDATIONDLL is
// defined to evaluate to nothing.
//

#ifndef FOUNDATIONDLL
#ifdef _WIN32
#ifdef APPLESEED_FOUNDATION_EXPORTS
#define FOUNDATIONDLL __declspec(dllexport)
#else
#define FOUNDATIONDLL __declspec(dllimport)
#endif
#else
#define FOUNDATIONDLL
#endif
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
// An object to configure a thread for accurate benchmarking.
//

class FOUNDATIONDLL BenchmarkingThreadContext
  : public NonCopyable
{
  public:
    // The constructor configures the current thread for benchmarking.
    BenchmarkingThreadContext();

    // The destructor restores previous thread settings.
    ~BenchmarkingThreadContext();

  private:
    // Private implementation.
    struct Impl;
    Impl* impl;
};


//
// Utility free functions.
//

// Suspend the current thread for a given number of milliseconds.
FOUNDATIONDLL void sleep(const uint32 ms);

// Give up the remainder of the current thread's time slice, to allow other threads to run.
FOUNDATIONDLL void yield();


//
// Spinlock class implementation.
//

inline Spinlock::Spinlock()
{
    boost::detail::spinlock initialized_sp = BOOST_DETAIL_SPINLOCK_INIT;
    m_sp = initialized_sp;
}

inline Spinlock::ScopedLock::ScopedLock(Spinlock& spinlock)
  : m_lock(spinlock.m_sp)
{
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_PLATFORM_THREAD_H
