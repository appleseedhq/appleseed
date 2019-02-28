
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

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class JobQueue; }
namespace foundation    { class Logger; }

namespace foundation
{

//
// A multithreaded job manager.
//
// The job manager itself is thread-local: none of its methods are thread-safe.
//

class APPLESEED_DLLSYMBOL JobManager
  : public NonCopyable
{
  public:
    enum Flags
    {
        KeepRunningOnEmptyQueue = 1UL << 0,     // the worker thread keeps running even if the job queue is empty
        KeepRunningOnJobFailure = 1UL << 1      // the worker thread keeps executing jobs from the work queue even if one or more jobs failed
    };

    // Constructor.
    JobManager(
        Logger&         logger,
        JobQueue&       job_queue,
        const size_t    thread_count,           // the number of simultaneous worker threads
        const int       flags = 0);

    // Destructor. Returns once currently running jobs are completed.
    ~JobManager();

    // Return the number of worker threads.
    size_t get_thread_count() const;

    // Start job execution. Returns immediately.
    void start();

    // Stop job execution. Returns once currently running jobs are completed.
    void stop();

    // Pause job execution by preventing worker threads from picking up new jobs.
    // Jobs already running will continue until completion.
    void pause();

    // Resume job execution.
    void resume();

  private:
    struct Impl;
    Impl* impl;
};

}   // namespace foundation
