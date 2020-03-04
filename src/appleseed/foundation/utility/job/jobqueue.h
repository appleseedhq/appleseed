
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
#include "foundation/memory/poolallocator.h"
#include "foundation/utility/test.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>
#include <list>
#include <utility>

// Forward declarations.
namespace foundation    { class AbortSwitch; }
namespace foundation    { class IJob; }

// Unit test case declarations.
DECLARE_TEST_CASE(Foundation_Utility_Job_JobQueue, AcquireScheduledJobWorksOnEmptyJobQueue);
DECLARE_TEST_CASE(Foundation_Utility_Job_JobQueue, AcquireScheduledJobWorksOnNonEmptyJobQueue);
DECLARE_TEST_CASE(Foundation_Utility_Job_JobQueue, RetiringRunningJobWorks);
DECLARE_TEST_CASE(Foundation_Utility_Job_JobQueue, RunningJobOwnedByQueueIsDestructedWhenRetired);
DECLARE_TEST_CASE(Foundation_Utility_Job_JobQueue, RunningJobNotOwnedByQueueIsNotDestructedWhenRetired);

namespace foundation
{

//
// A job queue.
//
// All methods of this class, excepted the destructor, are thread-safe.
//
// Jobs in a job queue always are in either one of these states:
//
//   - scheduled: the job was inserted into the job queue, but hasn't yet been executed
//   - running: the job is currently being executed
//

class APPLESEED_DLLSYMBOL JobQueue
  : public NonCopyable
{
  public:
    // Constructor.
    JobQueue();

    // Destructor. All scheduled jobs are deleted. Not thread-safe.
    ~JobQueue();

    // Delete all scheduled jobs.
    void clear_scheduled_jobs();

    // Return whether the job queue contains scheduled jobs.
    bool has_scheduled_jobs() const;

    // Return whether the job queue contains running jobs.
    bool has_running_jobs() const;

    // Return whether the job queue contains scheduled or running jobs.
    bool has_scheduled_or_running_jobs() const;

    // Return the number of scheduled jobs in the job queue.
    size_t get_scheduled_job_count() const;

    // Return the number of running jobs in the job queue.
    size_t get_running_job_count() const;

    // Return the number of scheduled and running jobs in the job queue.
    size_t get_total_job_count() const;

    // Schedule a job for execution. Ownership of the job is transfered
    // to the job queue if and only if transfer_ownership is true.
    void schedule(IJob* job, const bool transfer_ownership = true);

    // Wait until all scheduled and running jobs are completed.
    void wait_until_completion();

  private:
    friend class WorkerThread;

    struct Impl;
    Impl* impl;

    GRANT_ACCESS_TO_TEST_CASE(Foundation_Utility_Job_JobQueue, AcquireScheduledJobWorksOnEmptyJobQueue);
    GRANT_ACCESS_TO_TEST_CASE(Foundation_Utility_Job_JobQueue, AcquireScheduledJobWorksOnNonEmptyJobQueue);
    GRANT_ACCESS_TO_TEST_CASE(Foundation_Utility_Job_JobQueue, RetiringRunningJobWorks);
    GRANT_ACCESS_TO_TEST_CASE(Foundation_Utility_Job_JobQueue, RunningJobOwnedByQueueIsDestructedWhenRetired);
    GRANT_ACCESS_TO_TEST_CASE(Foundation_Utility_Job_JobQueue, RunningJobNotOwnedByQueueIsNotDestructedWhenRetired);

    struct JobInfo
    {
        IJob*       m_job;
        const bool  m_owned;

        JobInfo(IJob* job, const bool owned)
          : m_job(job)
          , m_owned(owned)
        {
        }
    };

    typedef std::list<JobInfo, PoolAllocator<JobInfo, 64>> JobList;

    typedef std::pair<JobInfo, JobList::iterator> RunningJobInfo;

    // Acquire a scheduled job and change its state from 'scheduled' to 'running'.
    RunningJobInfo acquire_scheduled_job();

    // Acquire a scheduled job without any locking.
    RunningJobInfo acquire_scheduled_job_no_lock();

    // Wait for a scheduled job to be available.
    RunningJobInfo wait_for_scheduled_job(AbortSwitch& abort_switch);

    // Retire a running job. The job is deleted if it is owned by the queue.
    void retire_running_job(const RunningJobInfo& running_job_info);

    // Signal a queue event.
    void signal_event();
};

}   // namespace foundation
