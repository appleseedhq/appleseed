
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
#include "foundation/utility/benchmark.h"
#include "foundation/utility/job.h"
#include "foundation/utility/log.h"

// Standard headers.
#include <cstddef>

BENCHMARK_SUITE(Foundation_Utility_Job)
{
    using namespace foundation;
    using namespace std;

    struct EmptyJob
      : public IJob
    {
        virtual void execute(const size_t thread_index)
        {
        }

        // Prevent deletion of instances of this class.
        void operator delete(void*) {}
        void operator delete(void*, size_t) {}
        void operator delete[](void*, size_t) {}
    };

    template <size_t ThreadCount>
    struct Fixture
    {
        Logger      m_logger;
        JobQueue    m_job_queue;
        JobManager  m_job_manager;

        Fixture()
          : m_job_manager(m_logger, m_job_queue, ThreadCount, true)
        {
            m_job_manager.start();
        }

        void payload()
        {
            const size_t JobCount = 10;
            EmptyJob jobs[JobCount];

            for (size_t i = 0; i < JobCount; ++i)
                m_job_queue.schedule(&jobs[i]);

            m_job_queue.wait_until_completion();
        }
    };

    BENCHMARK_CASE_WITH_FIXTURE(SingleThreadedJobExecution, Fixture<1>)
    {
        payload();
    }

    BENCHMARK_CASE_WITH_FIXTURE(DoubleThreadedJobExecution, Fixture<2>)
    {
        payload();
    }
}
