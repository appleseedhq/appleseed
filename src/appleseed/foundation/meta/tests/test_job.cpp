
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

// appleseed.foundation headers.
#include "foundation/log/log.h"
#include "foundation/platform/atomic.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/timers.h"
#include "foundation/utility/job/abortswitch.h"
#include "foundation/utility/job/ijob.h"
#include "foundation/utility/job/jobmanager.h"
#include "foundation/utility/job/jobqueue.h"
#include "foundation/utility/job/workerthread.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>
#include <cstdint>
#include <new>
#include <utility>

using namespace foundation;

namespace
{
    struct EmptyJob
      : public IJob
    {
        void execute(const size_t thread_index) override
        {
        }
    };

    class JobNotifyingAboutExecution
      : public IJob
    {
      public:
        explicit JobNotifyingAboutExecution(volatile std::uint32_t* execution_count)
          : m_execution_count(execution_count)
        {
        }

        void execute(const size_t thread_index) override
        {
            atomic_inc(m_execution_count);
        }

      private:
        volatile std::uint32_t* m_execution_count;
    };
}

TEST_SUITE(Foundation_Utility_Job_AbortSwitch)
{
    TEST_CASE(Constructor_ClearsAbortFlag)
    {
        AbortSwitch s;

        EXPECT_FALSE(s.is_aborted());
    }

    TEST_CASE(Abort_SetsAbortFlag)
    {
        AbortSwitch s;

        s.abort();

        EXPECT_TRUE(s.is_aborted());
    }

    TEST_CASE(Clear_ClearsAbortFlag)
    {
        AbortSwitch s;
        s.abort();

        s.clear();

        EXPECT_FALSE(s.is_aborted());
    }
}

TEST_SUITE(Foundation_Utility_Job_JobQueue)
{
    class JobNotifyingAboutDestruction
      : public IJob
    {
      public:
        explicit JobNotifyingAboutDestruction(volatile std::uint32_t* destruction_count)
          : m_destruction_count(destruction_count)
        {
        }

        ~JobNotifyingAboutDestruction() override
        {
            atomic_inc(m_destruction_count);
        }

        void execute(const size_t thread_index) override
        {
        }

      private:
        volatile std::uint32_t* m_destruction_count;
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

    TEST_CASE(JobScheduledWithOwnershipTransferIsDestructedWhenJobQueueIsDestructed)
    {
        volatile std::uint32_t destruction_count = 0;

        {
            JobQueue job_queue;
            job_queue.schedule(new JobNotifyingAboutDestruction(&destruction_count), true);
        }

        EXPECT_EQ(1, destruction_count);
    }

    TEST_CASE(JobScheduledWithoutOwnershipTransferIsNotDestructedWhenJobQueueIsDestructed)
    {
        volatile std::uint32_t destruction_count = 0;

        {
            JobQueue job_queue;
            job_queue.schedule(new JobNotifyingAboutDestruction(&destruction_count), false);
        }

        EXPECT_EQ(0, destruction_count);
    }

    TEST_CASE(JobScheduledWithOwnershipTransferIsDestructedWhenJobQueueIsCleared)
    {
        volatile std::uint32_t destruction_count = 0;

        JobQueue job_queue;
        job_queue.schedule(new JobNotifyingAboutDestruction(&destruction_count), true);

        job_queue.clear_scheduled_jobs();

        EXPECT_EQ(1, destruction_count);
    }

    TEST_CASE(JobScheduledWithoutOwnershipTransferIsNotDestructedWhenJobQueueIsCleared)
    {
        volatile std::uint32_t destruction_count = 0;

        JobQueue job_queue;
        job_queue.schedule(new JobNotifyingAboutDestruction(&destruction_count), false);

        job_queue.clear_scheduled_jobs();

        EXPECT_EQ(0, destruction_count);
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

    TEST_CASE(AcquireScheduledJobWorksOnEmptyJobQueue)
    {
        JobQueue job_queue;

        EXPECT_EQ(0, job_queue.acquire_scheduled_job().first.m_job);
    }

    TEST_CASE(AcquireScheduledJobWorksOnNonEmptyJobQueue)
    {
        IJob* job = new EmptyJob();

        JobQueue job_queue;
        job_queue.schedule(job);

        const JobQueue::RunningJobInfo running_job_info =
            job_queue.acquire_scheduled_job();

        EXPECT_EQ(job, running_job_info.first.m_job);

        EXPECT_FALSE(job_queue.has_scheduled_jobs());
        EXPECT_TRUE(job_queue.has_running_jobs());
        EXPECT_TRUE(job_queue.has_scheduled_or_running_jobs());

        EXPECT_EQ(0, job_queue.get_scheduled_job_count());
        EXPECT_EQ(1, job_queue.get_running_job_count());
        EXPECT_EQ(1, job_queue.get_total_job_count());

        job_queue.retire_running_job(running_job_info);
    }

    TEST_CASE(RetiringRunningJobWorks)
    {
        JobQueue job_queue;
        job_queue.schedule(new EmptyJob());

        const JobQueue::RunningJobInfo running_job_info =
            job_queue.acquire_scheduled_job();
        job_queue.retire_running_job(running_job_info);

        EXPECT_FALSE(job_queue.has_scheduled_jobs());
        EXPECT_FALSE(job_queue.has_running_jobs());
        EXPECT_FALSE(job_queue.has_scheduled_or_running_jobs());

        EXPECT_EQ(0, job_queue.get_scheduled_job_count());
        EXPECT_EQ(0, job_queue.get_running_job_count());
        EXPECT_EQ(0, job_queue.get_total_job_count());
    }

    TEST_CASE(RunningJobOwnedByQueueIsDestructedWhenRetired)
    {
        volatile std::uint32_t destruction_count = 0;

        JobQueue job_queue;
        job_queue.schedule(new JobNotifyingAboutDestruction(&destruction_count), true);

        const JobQueue::RunningJobInfo running_job_info =
            job_queue.acquire_scheduled_job();
        job_queue.retire_running_job(running_job_info);

        EXPECT_EQ(1, destruction_count);
    }

    TEST_CASE(RunningJobNotOwnedByQueueIsNotDestructedWhenRetired)
    {
        volatile std::uint32_t destruction_count = 0;

        JobQueue job_queue;
        job_queue.schedule(new JobNotifyingAboutDestruction(&destruction_count), false);

        const JobQueue::RunningJobInfo running_job_info =
            job_queue.acquire_scheduled_job();
        job_queue.retire_running_job(running_job_info);

        EXPECT_EQ(0, destruction_count);
    }
}

TEST_SUITE(Foundation_Utility_Job_JobManager)
{
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

    class JobCreatingAnotherJob
      : public IJob
    {
      public:
        JobCreatingAnotherJob(
            JobQueue&           job_queue,
            volatile std::uint32_t*    execution_count)
          : m_job_queue(job_queue)
          , m_execution_count(execution_count)
        {
        }

        void execute(const size_t thread_index) override
        {
            m_job_queue.schedule(
                new JobNotifyingAboutExecution(m_execution_count));
        }

      private:
        JobQueue&           m_job_queue;
        volatile std::uint32_t*    m_execution_count;
    };

    TEST_CASE_F(InitialStateIsCorrect, FixtureJobManager)
    {
        EXPECT_EQ(1, job_manager.get_thread_count());
    }

    TEST_CASE_F(StateAfterJobExecutionIsCorrect, FixtureJobManager)
    {
        job_queue.schedule(new EmptyJob());

        job_manager.start();
        job_queue.wait_until_completion();

        EXPECT_FALSE(job_queue.has_scheduled_or_running_jobs());
    }

    TEST_CASE_F(JobManagerExecutesJobs, FixtureJobManager)
    {
        volatile std::uint32_t execution_count = 0;

        job_queue.schedule(
            new JobNotifyingAboutExecution(&execution_count));

        job_manager.start();
        job_queue.wait_until_completion();

        EXPECT_EQ(1, execution_count);
    }

    TEST_CASE_F(JobManagerExecutesSubJobs, FixtureJobManager)
    {
        volatile std::uint32_t execution_count = 0;

        job_queue.schedule(
            new JobCreatingAnotherJob(job_queue, &execution_count));

        job_manager.start();
        job_queue.wait_until_completion();

        EXPECT_EQ(1, execution_count);
    }
}

TEST_SUITE(Foundation_Utility_Job_WorkerThread)
{
    class TimeoutChecker
    {
      public:
        explicit TimeoutChecker(const double timeout_seconds)
          : m_timeout_seconds(timeout_seconds)
          , m_timer_frequency(m_timer.frequency())
          , m_start_ticks(m_timer.read_start())
        {
        }

        bool timeout() const
        {
            const std::uint64_t elapsed_ticks = m_timer.read_end();

            const double elasped_seconds =
                static_cast<double>(elapsed_ticks - m_start_ticks) / m_timer_frequency;

            return elasped_seconds >= m_timeout_seconds;
        }

      private:
        const double                    m_timeout_seconds;
        mutable DefaultWallclockTimer   m_timer;
        const std::uint64_t             m_timer_frequency;
        const std::uint64_t             m_start_ticks;
    };

    struct JobThrowingBadAllocException
      : public IJob
    {
        void execute(const size_t thread_index) override
        {
            throw std::bad_alloc();
        }
    };

    TEST_CASE(Run_MemoryFailureDuingJobExecution_RetiresFailedJob)
    {
        JobQueue job_queue;
        job_queue.schedule(new JobThrowingBadAllocException());

        Logger logger;
        WorkerThread worker(0, logger, job_queue, 0);

        worker.start();

        TimeoutChecker timeout_checker(5.0);

        while (job_queue.has_scheduled_or_running_jobs())
        {
            if (timeout_checker.timeout())
                break;
        }

        EXPECT_FALSE(job_queue.has_scheduled_or_running_jobs());
    }

    TEST_CASE(Run_MemoryFailureDuingJobExecution_FlagKeepRunningOnJobFailureIsCleared_DoesNotExecuteRemainingJobAndClearsJobQueue)
    {
        JobQueue job_queue;
        job_queue.schedule(new JobThrowingBadAllocException());

        volatile std::uint32_t execution_count = 0;
        job_queue.schedule(new JobNotifyingAboutExecution(&execution_count));

        Logger logger;
        WorkerThread worker(0, logger, job_queue, 0);

        worker.start();

        TimeoutChecker timeout_checker(5.0);

        while (job_queue.has_scheduled_or_running_jobs())
        {
            if (timeout_checker.timeout())
                break;
        }

        EXPECT_EQ(0, execution_count);
        EXPECT_FALSE(job_queue.has_scheduled_or_running_jobs());
    }

    TEST_CASE(Run_MemoryFailureDuingJobExecution_FlagKeepRunningOnJobFailureIsSet_ExecutesRemainingJob)
    {
        JobQueue job_queue;
        job_queue.schedule(new JobThrowingBadAllocException());

        volatile std::uint32_t execution_count = 0;
        job_queue.schedule(new JobNotifyingAboutExecution(&execution_count));

        Logger logger;
        WorkerThread worker(0, logger, job_queue, JobManager::KeepRunningOnJobFailure);

        worker.start();

        TimeoutChecker timeout_checker(5.0);

        while (job_queue.has_scheduled_or_running_jobs())
        {
            if (timeout_checker.timeout())
                break;
        }

        EXPECT_EQ(1, execution_count);
    }
}
