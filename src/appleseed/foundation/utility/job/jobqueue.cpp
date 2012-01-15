
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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
#include "jobqueue.h"

// appleseed.foundation headers.
#include "foundation/platform/thread.h"
#include "foundation/utility/job/ijob.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/iterators.h"

// Standard headers.
#include <cassert>

using namespace boost;
using namespace std;

namespace foundation
{

//
// JobQueue class implementation.
//

struct JobQueue::Impl
{
    mutable Spinlock    m_spinlock;
    JobList             m_scheduled_jobs;
    JobList             m_running_jobs;

    static void delete_jobs(JobList& list)
    {
        for (each<JobList> i = list; i; ++i)
        {
            if (i->m_owned)
                delete i->m_job;
        }
 
        list.clear();
    }
};

JobQueue::JobQueue()
  : impl(new Impl())
{
}

JobQueue::~JobQueue()
{
    // We assume that worker threads are not running, so we don't lock.

    // At this point, no job must be running.
    assert(impl->m_running_jobs.empty());

    // Delete all scheduled jobs that the queue owns.
    Impl::delete_jobs(impl->m_scheduled_jobs);

    delete impl;
}

void JobQueue::clear_scheduled_jobs()
{
    Spinlock::ScopedLock lock(impl->m_spinlock);

    impl->delete_jobs(impl->m_scheduled_jobs);
}

bool JobQueue::has_scheduled_jobs() const
{
    Spinlock::ScopedLock lock(impl->m_spinlock);

    return !impl->m_scheduled_jobs.empty();
}

bool JobQueue::has_running_jobs() const
{
    Spinlock::ScopedLock lock(impl->m_spinlock);

    return !impl->m_running_jobs.empty();
}

bool JobQueue::has_scheduled_or_running_jobs() const
{
    Spinlock::ScopedLock lock(impl->m_spinlock);

    return !impl->m_scheduled_jobs.empty() || !impl->m_running_jobs.empty();
}

size_t JobQueue::get_scheduled_job_count() const
{
    Spinlock::ScopedLock lock(impl->m_spinlock);

    return impl->m_scheduled_jobs.size();
}

size_t JobQueue::get_running_job_count() const
{
    Spinlock::ScopedLock lock(impl->m_spinlock);

    return impl->m_running_jobs.size();
}

size_t JobQueue::get_total_job_count() const
{
    Spinlock::ScopedLock lock(impl->m_spinlock);

    return impl->m_scheduled_jobs.size() + impl->m_running_jobs.size();
}

void JobQueue::schedule(IJob* job, const bool transfer_ownership)
{
    assert(job);

    Spinlock::ScopedLock lock(impl->m_spinlock);

    impl->m_scheduled_jobs.push_back(JobInfo(job, transfer_ownership));
}

void JobQueue::wait_until_completion()
{
    while (true)
    {
        if (impl->m_spinlock.try_lock())
        {
            if (impl->m_scheduled_jobs.empty() && impl->m_running_jobs.empty())
            {
                impl->m_spinlock.unlock();
                return;
            }

            impl->m_spinlock.unlock();
        }

        yield();
    }
}

JobQueue::RunningJobInfo JobQueue::acquire_scheduled_job()
{
    Spinlock::ScopedLock lock(impl->m_spinlock);

    // Bail out if there is no scheduled job.
    if (impl->m_scheduled_jobs.empty())
        return RunningJobInfo(JobInfo(0, false), impl->m_running_jobs.end());

    // Move the next scheduled job to the end of the queue of running jobs.
    const JobInfo job_info = impl->m_scheduled_jobs.front();
    impl->m_scheduled_jobs.pop_front();
    impl->m_running_jobs.push_back(job_info);

    return RunningJobInfo(job_info, pred(impl->m_running_jobs.end()));
}

void JobQueue::retire_running_job(const RunningJobInfo& running_job_info)
{
    // Remove the job from the running list.
    impl->m_spinlock.lock();
    impl->m_running_jobs.erase(running_job_info.second);
    impl->m_spinlock.unlock();

    // Delete the job.
    if (running_job_info.first.m_owned)
        delete running_job_info.first.m_job;
}

}   // namespace foundation
