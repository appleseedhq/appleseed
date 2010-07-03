
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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

#ifndef APPLESEED_FOUNDATION_UTILITY_JOB_JOBQUEUE_H
#define APPLESEED_FOUNDATION_UTILITY_JOB_JOBQUEUE_H

// appleseed.foundation headers.
#include "foundation/core/concepts.h"
#include "foundation/utility/poolallocator.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <list>

// Forward declarations.
namespace foundation    { class IJob; }

// Unit test case declarations.
FOUNDATION_DECLARE_TEST_CASE(Foundation_Utility_Job_JobQueue, AcquireScheduledJobWorksOnEmptyJobQueue);
FOUNDATION_DECLARE_TEST_CASE(Foundation_Utility_Job_JobQueue, AcquireScheduledJobWorksOnNonEmptyJobQueue);
FOUNDATION_DECLARE_TEST_CASE(Foundation_Utility_Job_JobQueue, RetiringRunningJobWorks);
FOUNDATION_DECLARE_TEST_CASE(Foundation_Utility_Job_JobQueue, RunningJobIsDestructedWhenRetired);

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
// A job queue.
//
// All methods of this class, excepted the destructor, are thread-safe.
//
// Jobs in a job queue always are in either one of these states:
//
//   - scheduled: the job was inserted into the job queue, but hasn't yet been executed
//   - running: the job is currently being executed
//

class FOUNDATIONDLL JobQueue
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

    // Schedule a job for execution. Ownership of the job is transfered to the job queue.
    void schedule(IJob* job);

    // Wait until all scheduled and running jobs are completed.
    void wait_until_completion();

  private:
    // Private implementation.
    struct Impl;
    Impl* impl;

    friend class WorkerThread;

    FOUNDATION_GRANT_ACCESS_TO_TEST_CASE(Foundation_Utility_Job_JobQueue, AcquireScheduledJobWorksOnEmptyJobQueue);
    FOUNDATION_GRANT_ACCESS_TO_TEST_CASE(Foundation_Utility_Job_JobQueue, AcquireScheduledJobWorksOnNonEmptyJobQueue);
    FOUNDATION_GRANT_ACCESS_TO_TEST_CASE(Foundation_Utility_Job_JobQueue, RetiringRunningJobWorks);
    FOUNDATION_GRANT_ACCESS_TO_TEST_CASE(Foundation_Utility_Job_JobQueue, RunningJobIsDestructedWhenRetired);

    typedef std::list<IJob*, PoolAllocator<IJob*> > JobList;
    typedef std::pair<IJob*, JobList::const_iterator> JobInfo;

    // Acquire a scheduled job for execution. Changes the job state from 'scheduled' to 'running'.
    JobInfo acquire_scheduled_job();

    // Retire a running job. The job is deleted.
    void retire_running_job(const JobInfo& job_info);
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_JOB_JOBQUEUE_H
