
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
#include "workerthread.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"
#include "foundation/platform/thread.h"
#include "foundation/utility/job/ijob.h"
#include "foundation/utility/job/jobqueue.h"
#include "foundation/utility/log.h"

// Standard headers.
#include <exception>

using namespace boost;
using namespace std;

namespace foundation
{

//
// WorkerThread class implementation.
//

// Constructor.
WorkerThread::WorkerThread(
    const size_t    thread_index,
    Logger&         logger,
    JobQueue&       job_queue,
    const bool      keep_running)
  : m_thread_index(thread_index)
  , m_logger(logger)
  , m_job_queue(job_queue)
  , m_keep_running(keep_running)
  , m_terminate(false)
  , m_thread_func(*this)
  , m_thread(0)
{
}

// Destructor.
WorkerThread::~WorkerThread()
{
    stop();
}

// Start the worker thread.
void WorkerThread::start()
{
    // Don't do anything if the worker thread is already running.
    if (m_thread)
        return;

    m_terminate = false;

    m_thread = new thread(m_thread_func);
}

// Stop the worker thread.
void WorkerThread::stop()
{
    // Don't do anything if the worker thread is already stopped.
    if (!m_thread)
        return;

    m_terminate = true;
    m_thread->join();

    delete m_thread;
    m_thread = 0;
}

// Main line of the worker thread.
void WorkerThread::run()
{
    while (!m_terminate)
    {
        // Acquire a job.
        const JobQueue::JobInfo job_info =
            m_job_queue.acquire_scheduled_job();

        // Handle the case where the job queue is empty.
        if (job_info.first == 0)
        {
            if (m_keep_running)
            {
                // Give up time slice.
                yield();

                // Keep the thread running and checking for new jobs.
                continue;
            }
            else
            {
                // Terminate the thread.
                break;
            }
        }

        // Execute the job.
        execute_job(*job_info.first);

        // Retire the job.
        m_job_queue.retire_running_job(job_info);
    }
}

// Execute a job.
void WorkerThread::execute_job(IJob& job)
{
    try
    {
        job.execute(m_thread_index);
    }
    catch (const Exception& e)
    {
        LOG_ERROR(
            m_logger,
            "worker thread %u: job was terminated: %s",
            m_thread_index,
            e.what());
    }
    catch (const bad_alloc&)
    {
        LOG_ERROR(
            m_logger,
            "worker thread %u: job was terminated: memory allocation failure",
            m_thread_index);
    }
    catch (...)
    {
        LOG_ERROR(
            m_logger,
            "worker thread %u: job was terminated: unknown exception",
            m_thread_index);
    }
}

}   // namespace foundation
