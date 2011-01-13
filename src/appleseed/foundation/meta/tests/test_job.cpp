
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

// appleseed.foundation headers.
#include "foundation/platform/timer.h"
#include "foundation/platform/types.h"
#include "foundation/utility/job/workerthread.h"
#include "foundation/utility/job.h"
#include "foundation/utility/log.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <exception>

TEST_SUITE(Foundation_Utility_Job_JobQueue)
{
    using namespace foundation;

    struct EmptyJob
      : public IJob
    {
        virtual void execute(const size_t thread_index)
        {
        }
    };

    class JobNotifyingAboutDestruction
      : public IJob
    {
      public:
        explicit JobNotifyingAboutDestruction(size_t& destruction_count)
          : m_destruction_count(destruction_count)
        {
        }

        ~JobNotifyingAboutDestruction()
        {
            ++m_destruction_count;
        }

        virtual void execute(const size_t thread_index)
        {
        }

      private:
        size_t& m_destruction_count;
    };

    TEST_CASE(InitialStateIsCorrect)
    {
        JobQueue job_queue;

        EXPECT_FALSE(job_queue.has_scheduled_jobs());
        EXPECT_FALSE(job_queue.has_running_jobs());
        EXPECT_FALSE(job_queue.has_scheduled_or_running_jobs());

        EXPECT_EQ(0, job_queue.get_scheduled_job_count());
        EXPECT_EQ(0, job_queue.get_running_job_count());
        EXPECT_EQ(0, job_queue.get_total_job_count());
    }

    TEST_CASE(SchedulingOfJobWorks)
    {
        JobQueue job_queue;
        job_queue.schedule(new EmptyJob());

        EXPECT_TRUE(job_queue.has_scheduled_jobs());
        EXPECT_FALSE(job_queue.has_running_jobs());
        EXPECT_TRUE(job_queue.has_scheduled_or_running_jobs());

        EXPECT_EQ(1, job_queue.get_scheduled_job_count());
        EXPECT_EQ(0, job_queue.get_running_job_count());
        EXPECT_EQ(1, job_queue.get_total_job_count());
    }

    TEST_CASE(ScheduledJobIsDestructedWhenJobQueueIsDestructed)
    {
        size_t destruction_count = 0;

        {
            JobQueue job_queue;
            job_queue.schedule(new JobNotifyingAboutDestruction(destruction_count));
        }

        EXPECT_EQ(1, destruction_count);
    }

    TEST_CASE(ClearingScheduledJobsWorks)
    {
        JobQueue job_queue;
        job_queue.schedule(new EmptyJob());
        job_queue.clear_scheduled_jobs();

        EXPECT_FALSE(job_queue.has_scheduled_jobs());
        EXPECT_FALSE(job_queue.has_running_jobs());
        EXPECT_FALSE(job_queue.has_scheduled_or_running_jobs());

        EXPECT_EQ(0, job_queue.get_scheduled_job_count());
        EXPECT_EQ(0, job_queue.get_running_job_count());
        EXPECT_EQ(0, job_queue.get_total_job_count());
    }

    TEST_CASE(ScheduledJobIsDestructedWhenJobQueueIsCleared)
    {
        size_t destruction_count = 0;

        JobQueue job_queue;
        job_queue.schedule(new JobNotifyingAboutDestruction(destruction_count));
        job_queue.clear_scheduled_jobs();

        EXPECT_EQ(1, destruction_count);
    }

    TEST_CASE(AcquireScheduledJobWorksOnEmptyJobQueue)
    {
        JobQueue job_queue;

        EXPECT_EQ(0, job_queue.acquire_scheduled_job().first);
    }

    TEST_CASE(AcquireScheduledJobWorksOnNonEmptyJobQueue)
    {
        IJob* job = new EmptyJob();

        JobQueue job_queue;
        job_queue.schedule(job);

        const JobQueue::JobInfo job_info = job_queue.acquire_scheduled_job();

        EXPECT_EQ(job, job_info.first);

        EXPECT_FALSE(job_queue.has_scheduled_jobs());
        EXPECT_TRUE(job_queue.has_running_jobs());
        EXPECT_TRUE(job_queue.has_scheduled_or_running_jobs());

        EXPECT_EQ(0, job_queue.get_scheduled_job_count());
        EXPECT_EQ(1, job_queue.get_running_job_count());
        EXPECT_EQ(1, job_queue.get_total_job_count());

        job_queue.retire_running_job(job_info);
    }

    TEST_CASE(RetiringRunningJobWorks)
    {
        JobQueue job_queue;
        job_queue.schedule(new EmptyJob());

        const JobQueue::JobInfo job_info = job_queue.acquire_scheduled_job();
        job_queue.retire_running_job(job_info);

        EXPECT_FALSE(job_queue.has_scheduled_jobs());
        EXPECT_FALSE(job_queue.has_running_jobs());
        EXPECT_FALSE(job_queue.has_scheduled_or_running_jobs());

        EXPECT_EQ(0, job_queue.get_scheduled_job_count());
        EXPECT_EQ(0, job_queue.get_running_job_count());
        EXPECT_EQ(0, job_queue.get_total_job_count());
    }

    TEST_CASE(RunningJobIsDestructedWhenRetired)
    {
        size_t destruction_count = 0;

        JobQueue job_queue;
        job_queue.schedule(new JobNotifyingAboutDestruction(destruction_count));

        const JobQueue::JobInfo job_info = job_queue.acquire_scheduled_job();
        job_queue.retire_running_job(job_info);

        EXPECT_EQ(1, destruction_count);
    }
}

TEST_SUITE(Foundation_Utility_Job_JobManager)
{
    using namespace foundation;

    struct FixtureJobManager
    {
        Logger      logger;
        JobQueue    job_queue;
        JobManager  job_manager;
        
        FixtureJobManager()
          : job_manager(logger, job_queue, 1)
        {
        }
    };

    struct EmptyJob
      : public IJob
    {
        virtual void execute(const size_t thread_index)
        {
        }
    };

    class JobNotifyingAboutExecution
      : public IJob
    {
      public:
        explicit JobNotifyingAboutExecution(volatile size_t& execution_count)
          : m_execution_count(execution_count)
        {
        }

        virtual void execute(const size_t thread_index)
        {
            ++m_execution_count;
        }

      private:
        volatile size_t&    m_execution_count;
    };

    class JobCreatingAnotherJob
      : public IJob
    {
      public:
        JobCreatingAnotherJob(
            JobQueue&           job_queue,
            volatile size_t&    execution_count)
          : m_job_queue(job_queue)
          , m_execution_count(execution_count)
        {
        }

        virtual void execute(const size_t thread_index)
        {
            m_job_queue.schedule(
                new JobNotifyingAboutExecution(m_execution_count));
        }

      private:
        JobQueue&           m_job_queue;
        volatile size_t&    m_execution_count;
    };

    TEST_CASE_WITH_FIXTURE(InitialStateIsCorrect, FixtureJobManager)
    {
        EXPECT_EQ(1, job_manager.get_thread_count());
    }

    TEST_CASE_WITH_FIXTURE(StateAfterJobExecutionIsCorrect, FixtureJobManager)
    {
        job_queue.schedule(new EmptyJob());

        job_manager.start();
        job_queue.wait_until_completion();

        EXPECT_FALSE(job_queue.has_scheduled_or_running_jobs());
    }

    TEST_CASE_WITH_FIXTURE(JobManagerExecutesJobs, FixtureJobManager)
    {
        volatile size_t execution_count = 0;

        job_queue.schedule(
            new JobNotifyingAboutExecution(execution_count));

        job_manager.start();
        job_queue.wait_until_completion();

        EXPECT_EQ(1, execution_count);
    }

    TEST_CASE_WITH_FIXTURE(JobManagerExecutesSubJobs, FixtureJobManager)
    {
        volatile size_t execution_count = 0;

        job_queue.schedule(
            new JobCreatingAnotherJob(job_queue, execution_count));

        job_manager.start();
        job_queue.wait_until_completion();

        EXPECT_EQ(1, execution_count);
    }
}

TEST_SUITE(Foundation_Utility_Job_WorkerThread)
{
    using namespace foundation;
    using namespace std;

    struct JobThrowingBadAllocException
      : public IJob
    {
        virtual void execute(const size_t thread_index)
        {
            throw bad_alloc();
        }
    };

    class TimeoutChecker
    {
      public:
        explicit TimeoutChecker(const double timeout_seconds)
          : m_timeout_seconds(timeout_seconds)
          , m_timer_frequency(m_timer.frequency())
          , m_start_ticks(m_timer.read())
        {
        }

        bool timeout() const
        {
            const uint64 elapsed_ticks = m_timer.read();

            const double elasped_seconds =
                static_cast<double>(elapsed_ticks - m_start_ticks) / m_timer_frequency;

            return elasped_seconds >= m_timeout_seconds;
        }

      private:
        const double                    m_timeout_seconds;
        mutable DefaultWallclockTimer   m_timer;
        const uint64                    m_timer_frequency;
        const uint64                    m_start_ticks;
    };

    TEST_CASE(Run_MemoryFailureDuingJobExecution_RetiresJob)
    {
        JobQueue job_queue;
        job_queue.schedule(new JobThrowingBadAllocException());

        Logger logger;
        WorkerThread worker(0, logger, job_queue, false);

        worker.start();

        TimeoutChecker timeout_checker(5.0);

        while (job_queue.has_scheduled_or_running_jobs())
        {
            if (timeout_checker.timeout())
                break;
        }

        EXPECT_FALSE(job_queue.has_scheduled_or_running_jobs());
    }
}
