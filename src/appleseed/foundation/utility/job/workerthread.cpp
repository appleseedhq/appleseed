
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
#include "workerthread.h"

// appleseed.foundation headers.
#include "foundation/platform/snprintf.h"
#ifdef APPLESEED_USE_SSE42
#include "foundation/platform/sse.h"
#endif
#include "foundation/platform/types.h"
#include "foundation/utility/job/ijob.h"
#include "foundation/utility/job/jobmanager.h"
#include "foundation/utility/job/jobqueue.h"
#include "foundation/log/log.h"

// Standard headers.
#include <exception>
#include <new>

using namespace boost;

namespace foundation
{

//
// WorkerThread class implementation.
//

WorkerThread::WorkerThread(
    const size_t    index,
    Logger&         logger,
    JobQueue&       job_queue,
    const int       flags)
  : m_index(index)
  , m_logger(logger)
  , m_job_queue(job_queue)
  , m_flags(flags)
  , m_thread_func(*this)
  , m_thread(nullptr)
{
}

WorkerThread::~WorkerThread()
{
    stop();
}

void WorkerThread::start()
{
    // Don't do anything if the worker thread is already running.
    if (m_thread)
        return;

    assert(m_pause_flag.is_clear());
    assert(!m_abort_switch.is_aborted());

    // Start the thread.
    m_thread = new boost::thread(m_thread_func);
}

void WorkerThread::stop()
{
    // Don't do anything if the worker thread is already stopped.
    if (!m_thread)
        return;

    // Resume the thread if it was paused.
    m_pause_flag.clear();
    m_pause_event.notify_all();

    // Ask the thread to stop, and wait until it has.
    m_abort_switch.abort();
    m_job_queue.signal_event();
    m_thread->join();
    m_abort_switch.clear();

    // Delete the thread object.
    delete m_thread;
    m_thread = nullptr;
}

void WorkerThread::pause()
{
    // Don't do anything if the worker thread is not running.
    if (!m_thread)
        return;

    boost::mutex::scoped_lock lock(m_pause_mutex);
    m_pause_flag.set();
}

void WorkerThread::resume()
{
    // Don't do anything if the worker thread is not running.
    if (!m_thread)
        return;

    boost::mutex::scoped_lock lock(m_pause_mutex);
    m_pause_flag.clear();
    m_pause_event.notify_all();
}

void WorkerThread::set_thread_name()
{
    char thread_name[16];
    portable_snprintf(thread_name, sizeof(thread_name), "worker_%03lu", (long unsigned int)m_index);
    set_current_thread_name(thread_name);
}

void WorkerThread::run()
{
    set_thread_name();

#if defined APPLESEED_WITH_EMBREE && defined APPLESEED_USE_SSE42

    //
    // When Embree support is enabled, enable 'Flush to Zero' and 'Denormals are Zero' modes.
    //
    // Note: On some platforms, the preprocessor symbols _MM_SET_DENORMALS_ZERO_MODE() and
    // _MM_DENORMALS_ZERO_ON are only defined if __SSE3__ is defined; executing these lines
    // with APPLESEED_USE_SSE42 should ensure they are always defined.
    //
    // Reference:
    //
    //   https://embree.github.io/api.html#mxcsr-control-and-status-register
    //

    _MM_SET_FLUSH_ZERO_MODE(_MM_FLUSH_ZERO_ON);
    _MM_SET_DENORMALS_ZERO_MODE(_MM_DENORMALS_ZERO_ON);

#endif

    while (!m_abort_switch.is_aborted())
    {
        if (m_pause_flag.is_set())
        {
            // Wait until the resume event.
            boost::mutex::scoped_lock lock(m_pause_mutex);
            while (m_pause_flag.is_set())
                m_pause_event.wait(lock);
        }

        // Acquire a job.
        const JobQueue::RunningJobInfo running_job_info =
            m_job_queue.wait_for_scheduled_job(m_abort_switch);

        // Handle the case where the job queue is empty.
        if (running_job_info.first.m_job == nullptr)
        {
            if (m_flags & JobManager::KeepRunningOnEmptyQueue)
            {
                // Keep the thread running and waiting for new jobs.
                continue;
            }
            else
            {
                // Terminate the thread.
                break;
            }
        }

        // Execute the job.
        const bool success = execute_job(*running_job_info.first.m_job);

        // Retire the job.
        m_job_queue.retire_running_job(running_job_info);

        // Handle job execution failures.
        if (!success && !(m_flags & JobManager::KeepRunningOnJobFailure))
        {
            m_job_queue.clear_scheduled_jobs();
            break;
        }
    }
}

bool WorkerThread::execute_job(IJob& job)
{
    try
    {
        job.execute(m_index);
    }
    catch (const std::bad_alloc&)
    {
        LOG_ERROR(
            m_logger,
            "worker thread " FMT_SIZE_T ": job was terminated (ran out of memory).",
            m_index);

        return false;
    }
#ifdef NDEBUG
    catch (const std::exception& e)     // namespace qualification required
    {
        LOG_ERROR(
            m_logger,
            "worker thread " FMT_SIZE_T ": job was terminated (%s).",
            m_index,
            e.what()[0] != '\0' ? e.what() : "no details available");

        return false;
    }
    catch (...)
    {
        LOG_ERROR(
            m_logger,
            "worker thread " FMT_SIZE_T ": job was terminated (unknown exception).",
            m_index);

        return false;
    }
#endif

    return true;
}

}   // namespace foundation
