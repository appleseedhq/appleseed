
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

// Interface header.
#include "benchmarksuiterepository.h"

// appleseed.foundation headers.
#include "foundation/utility/benchmark/benchmarkresult.h"
#include "foundation/utility/benchmark/benchmarksuite.h"
#include "foundation/utility/filter.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <vector>

using namespace std;

namespace foundation
{

//
// BenchmarkSuiteRepository class implementation.
//

struct BenchmarkSuiteRepository::Impl
{
    vector<BenchmarkSuite*> m_suites;
};

// Constructor.
BenchmarkSuiteRepository::BenchmarkSuiteRepository()
  : impl(new Impl())
{
}

// Destructor.
BenchmarkSuiteRepository::~BenchmarkSuiteRepository()
{
    delete impl;
}

// Register a benchmark suite.
void BenchmarkSuiteRepository::register_suite(BenchmarkSuite* suite)
{
    assert(suite);
    impl->m_suites.push_back(suite);
}

// Run all the registered benchmark suites.
void BenchmarkSuiteRepository::run(BenchmarkResult& result) const
{
    LetThroughFilter filter;
    run(filter, result);
}

// Run those benchmark suites whose name pass a given filter.
void BenchmarkSuiteRepository::run(
    const IFilter&      filter,
    BenchmarkResult&    result) const
{
    for (size_t i = 0; i < impl->m_suites.size(); ++i)
    {
        BenchmarkSuite& suite = *impl->m_suites[i];

        // Skip benchmark suites that aren't let through by the filter.
        if (!filter.accepts(suite.get_name()))
            continue;

        // Create a benchmark result for this benchmark suite.
        BenchmarkResult suite_result;
        suite_result.add_listeners(result);

        // Run the benchmark suite.
        suite_result.signal_suite_execution();
        impl->m_suites[i]->run(suite_result);

        // Report a benchmark suite failure if one or more benchmark cases failed.
        if (suite_result.get_case_failure_count() > 0)
            suite_result.signal_suite_failure();

        // Merge the benchmark suite result into the final benchmark result.
        result.merge(suite_result);
    }
}

}   // namespace foundation
