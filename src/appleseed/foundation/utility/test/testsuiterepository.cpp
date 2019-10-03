
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
#include "testsuiterepository.h"

// appleseed.foundation headers.
#include "foundation/utility/filter/ifilter.h"
#include "foundation/utility/filter/passthroughfilter.h"
#include "foundation/utility/test/testsuite.h"

// Standard headers.
#include <cassert>
#include <vector>

namespace foundation
{

//
// TestSuiteRepository class implementation.
//

struct TestSuiteRepository::Impl
{
    std::vector<TestSuite*> m_suites;
};

TestSuiteRepository::TestSuiteRepository()
  : impl(new Impl())
{
}

TestSuiteRepository::~TestSuiteRepository()
{
    delete impl;
}

void TestSuiteRepository::clear()
{
    impl->m_suites.clear();
}

void TestSuiteRepository::register_suite(TestSuite* suite)
{
    assert(suite);
    impl->m_suites.push_back(suite);
}

size_t TestSuiteRepository::get_suite_count() const
{
    return impl->m_suites.size();
}

TestSuite* TestSuiteRepository::get_suite(const size_t index) const
{
    assert(index < impl->m_suites.size());
    return impl->m_suites[index];
}

void TestSuiteRepository::run(
    ITestListener&  test_listener,
    TestResult&     cumulated_result) const
{
    PassThroughFilter filter;
    run(filter, test_listener, cumulated_result);
}

void TestSuiteRepository::run(
    const IFilter&  filter,
    ITestListener&  test_listener,
    TestResult&     cumulated_result) const
{
    for (size_t i = 0; i < impl->m_suites.size(); ++i)
    {
        TestSuite& test_suite = *impl->m_suites[i];

        if (filter.accepts(test_suite.get_name()))
            test_suite.run(test_listener, cumulated_result);
        else test_suite.run(filter, test_listener, cumulated_result);
    }
}

}   // namespace foundation
