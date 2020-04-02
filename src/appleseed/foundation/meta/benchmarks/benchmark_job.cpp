
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
#include "foundation/utility/benchmark.h"
#include "foundation/utility/job.h"

// Standard headers.
#include <cstddef>

using namespace foundation;

BENCHMARK_SUITE(Foundation_Utility_Job)
{
    struct EmptyJob
      : public IJob
    {
        void execute(const size_t thread_index) override
        {
        }
    };

    template <size_t ThreadCount>
    struct Fixture
    {
        Logger      m_logger;
        JobQueue    m_job_queue;
        JobManager  m_job_manager;

        Fixture()
          : m_job_manager(m_logger, m_job_queue, ThreadCount, JobManager::KeepRunningOnEmptyQueue)
        {
            m_job_manager.start();
        }

        void payload()
        {
            const size_t JobCount = 256;
            EmptyJob jobs[JobCount];

            for (size_t i = 0; i < JobCount; ++i)
                m_job_queue.schedule(&jobs[i], false);

            m_job_queue.wait_until_completion();
        }
    };

    BENCHMARK_CASE_F(SingleThreadedJobExecution, Fixture<1>)
    {
        payload();
    }

    BENCHMARK_CASE_F(DoubleThreadedJobExecution, Fixture<2>)
    {
        payload();
    }
}
