
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
#include "foundation/platform/thread.h"
#include "foundation/utility/job/abortswitch.h"

// Boost headers.
#include "boost/thread/condition_variable.hpp"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace boost         { class thread; }
namespace foundation    { class IJob; }
namespace foundation    { class JobQueue; }
namespace foundation    { class Logger; }

namespace foundation
{

//
// Worker thread.
//

class WorkerThread
  : public NonCopyable
{
  public:
    // Constructor.
    WorkerThread(
        const size_t    index,
        Logger&         logger,
        JobQueue&       job_queue,
        const int       flags);     // see foundation::JobManager::Flags

    // Destructor.
    ~WorkerThread();

    // Start the worker thread.
    void start();

    // Stop the worker thread.
    void stop();

    // Pause the worker thread. See JobManager::pause() for details.
    void pause();

    // Resume the worker thread.
    void resume();

  private:
    // A helper class that encapsulates the run() method of the worker thread
    // into an object that can be passed to the constructor of boost::thread.
    struct ThreadFunc
    {
        WorkerThread&   m_parent;

        explicit ThreadFunc(WorkerThread& parent)
          : m_parent(parent)
        {
        }

        void operator()()
        {
            m_parent.run();
        }
    };

    const size_t                    m_index;
    Logger&                         m_logger;
    JobQueue&                       m_job_queue;
    const int                       m_flags;

    AbortSwitch                     m_abort_switch;

    ThreadFunc                      m_thread_func;
    boost::thread*                  m_thread;

    ThreadFlag                      m_pause_flag;
    boost::condition_variable_any   m_pause_event;
    boost::mutex                    m_pause_mutex;

    void set_thread_name();

    // Main line of the worker thread.
    void run();

    // Execute a job. Return true on success, false on failure.
    bool execute_job(IJob& job);
};

}   // namespace foundation
