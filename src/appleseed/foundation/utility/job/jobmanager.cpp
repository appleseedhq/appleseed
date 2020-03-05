
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
#include "jobmanager.h"

// appleseed.foundation headers.
#include "foundation/log/log.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/job/jobqueue.h"
#include "foundation/utility/job/workerthread.h"

// Standard headers.
#include <cassert>
#include <vector>

using namespace boost;

namespace foundation
{

//
// JobManager class implementation.
//

struct JobManager::Impl
{
    typedef std::vector<WorkerThread*> WorkerThreads;

    Logger&             m_logger;
    JobQueue&           m_job_queue;
    size_t              m_thread_count;
    const int           m_flags;
    WorkerThreads       m_worker_threads;

    // Constructor.
    Impl(
        Logger&         logger,
        JobQueue&       job_queue,
        const size_t    thread_count,
        const int       flags)
      : m_logger(logger)
      , m_job_queue(job_queue)
      , m_thread_count(thread_count)
      , m_flags(flags)
    {
    }
};

JobManager::JobManager(
    Logger&             logger,
    JobQueue&           job_queue,
    const size_t        thread_count,
    const int           flags)
  : impl(new Impl(logger, job_queue, thread_count, flags))
{
}

JobManager::~JobManager()
{
    stop();

    delete impl;
}

size_t JobManager::get_thread_count() const
{
    return impl->m_thread_count;
}

void JobManager::start()
{
    assert(impl->m_worker_threads.empty() ||
           impl->m_worker_threads.size() == impl->m_thread_count);

    // Create worker threads if they don't already exist.
    if (impl->m_worker_threads.empty())
    {
        for (size_t i = 0; i < impl->m_thread_count; ++i)
        {
            impl->m_worker_threads.push_back(
                new WorkerThread(
                    i,
                    impl->m_logger,
                    impl->m_job_queue,
                    impl->m_flags));
        }
    }

    // Start worker threads.
    for (each<Impl::WorkerThreads> i = impl->m_worker_threads; i; ++i)
        (*i)->start();
}

void JobManager::stop()
{
    // Stop and delete worker threads.
    for (each<Impl::WorkerThreads> i = impl->m_worker_threads; i; ++i)
        delete *i;
    impl->m_worker_threads.clear();
}

void JobManager::pause()
{
    for (each<Impl::WorkerThreads> i = impl->m_worker_threads; i; ++i)
        (*i)->pause();
}

void JobManager::resume()
{
    for (each<Impl::WorkerThreads> i = impl->m_worker_threads; i; ++i)
        (*i)->resume();
}

}   // namespace foundation
