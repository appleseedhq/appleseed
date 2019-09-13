
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
#include "jobqueue.h"

// appleseed.foundation headers.
#include "foundation/platform/thread.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/iterators.h"
#include "foundation/utility/job/abortswitch.h"
#include "foundation/utility/job/ijob.h"

// Boost headers.
#include "boost/thread/condition_variable.hpp"

// Standard headers.
#include <cassert>

namespace foundation
{

//
// JobQueue class implementation.
//

struct JobQueue::Impl
{
    mutable boost::mutex            m_mutex;
    boost::condition_variable_any   m_event;
    JobList                         m_scheduled_jobs;
    JobList                         m_running_jobs;

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
    boost::mutex::scoped_lock lock(impl->m_mutex);

    impl->delete_jobs(impl->m_scheduled_jobs);

    // Notify worker threads that all scheduled jobs are gone.
    impl->m_event.notify_all();
}

bool JobQueue::has_scheduled_jobs() const
{
    boost::mutex::scoped_lock lock(impl->m_mutex);

    return !impl->m_scheduled_jobs.empty();
}

bool JobQueue::has_running_jobs() const
{
    boost::mutex::scoped_lock lock(impl->m_mutex);

    return !impl->m_running_jobs.empty();
}

bool JobQueue::has_scheduled_or_running_jobs() const
{
    boost::mutex::scoped_lock lock(impl->m_mutex);

    return !impl->m_scheduled_jobs.empty() || !impl->m_running_jobs.empty();
}

size_t JobQueue::get_scheduled_job_count() const
{
    boost::mutex::scoped_lock lock(impl->m_mutex);

    return impl->m_scheduled_jobs.size();
}

size_t JobQueue::get_running_job_count() const
{
    boost::mutex::scoped_lock lock(impl->m_mutex);

    return impl->m_running_jobs.size();
}

size_t JobQueue::get_total_job_count() const
{
    boost::mutex::scoped_lock lock(impl->m_mutex);

    return impl->m_scheduled_jobs.size() + impl->m_running_jobs.size();
}

void JobQueue::schedule(IJob* job, const bool transfer_ownership)
{
    assert(job);

    boost::mutex::scoped_lock lock(impl->m_mutex);

    impl->m_scheduled_jobs.push_back(JobInfo(job, transfer_ownership));

    // Notify worker threads that a new scheduled job is available.
    impl->m_event.notify_all();
}

void JobQueue::wait_until_completion()
{
    boost::mutex::scoped_lock lock(impl->m_mutex);

    // Wait until there is no more scheduled or running jobs.
    while (!impl->m_scheduled_jobs.empty() || !impl->m_running_jobs.empty())
        impl->m_event.wait(lock);
}

JobQueue::RunningJobInfo JobQueue::acquire_scheduled_job()
{
    boost::mutex::scoped_lock lock(impl->m_mutex);

    return acquire_scheduled_job_no_lock();
}

JobQueue::RunningJobInfo JobQueue::acquire_scheduled_job_no_lock()
{
    // Bail out if there is no scheduled job.
    if (impl->m_scheduled_jobs.empty())
        return RunningJobInfo(JobInfo(nullptr, false), impl->m_running_jobs.end());

    // Move the next scheduled job to the end of the queue of running jobs.
    const JobInfo job_info = impl->m_scheduled_jobs.front();
    impl->m_scheduled_jobs.pop_front();
    impl->m_running_jobs.push_back(job_info);

    // Notify worker threads that a job has moved from the scheduled to the running state.
    impl->m_event.notify_all();

    return RunningJobInfo(job_info, pred(impl->m_running_jobs.end()));
}

JobQueue::RunningJobInfo JobQueue::wait_for_scheduled_job(AbortSwitch& abort_switch)
{
    boost::mutex::scoped_lock lock(impl->m_mutex);

    // Wait for a scheduled job to be available.
    while (!abort_switch.is_aborted() && impl->m_scheduled_jobs.empty())    // order matters
        impl->m_event.wait(lock);

    return acquire_scheduled_job_no_lock();
}

void JobQueue::retire_running_job(const RunningJobInfo& running_job_info)
{
    boost::mutex::scoped_lock lock(impl->m_mutex);

    // Remove the job from the running list.
    impl->m_running_jobs.erase(running_job_info.second);

    // Delete the job.
    if (running_job_info.first.m_owned)
        delete running_job_info.first.m_job;

    // Notify worker threads that a scheduled job was retired.
    impl->m_event.notify_all();
}

void JobQueue::signal_event()
{
    boost::mutex::scoped_lock lock(impl->m_mutex);

    impl->m_event.notify_all();
}

}   // namespace foundation
