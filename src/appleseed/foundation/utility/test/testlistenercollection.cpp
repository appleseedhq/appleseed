
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
#include "testlistenercollection.h"

// appleseed.foundation headers.
#include "foundation/utility/foreach.h"

// Standard headers.
#include <cassert>
#include <list>

namespace foundation
{

//
// TestListenerCollection class implementation.
//

struct TestListenerCollection::Impl
{
    typedef std::list<ITestListener*> TestListenerContainer;

    TestListenerContainer m_listeners;
};

TestListenerCollection::TestListenerCollection()
  : impl(new Impl())
{
}

TestListenerCollection::~TestListenerCollection()
{
    delete impl;
}

void TestListenerCollection::insert(ITestListener* listener)
{
    assert(listener);
    impl->m_listeners.push_back(listener);
}

void TestListenerCollection::begin_suite(
    const TestSuite&        test_suite)
{
    for (each<Impl::TestListenerContainer> i = impl->m_listeners; i; ++i)
        (*i)->begin_suite(test_suite);
}

void TestListenerCollection::end_suite(
    const TestSuite&        test_suite,
    const TestResult&       test_suite_result,
    const TestResult&       cumulated_result)
{
    for (each<Impl::TestListenerContainer> i = impl->m_listeners; i; ++i)
    {
        (*i)->end_suite(
            test_suite,
            test_suite_result,
            cumulated_result);
    }
}

void TestListenerCollection::begin_case(
    const TestSuite&        test_suite,
    const char*             test_case_name)
{
    for (each<Impl::TestListenerContainer> i = impl->m_listeners; i; ++i)
        (*i)->begin_case(test_suite, test_case_name);
}

void TestListenerCollection::end_case(
    const TestSuite&        test_suite,
    const char*             test_case_name,
    const TestResult&       test_suite_result,
    const TestResult&       test_case_result,
    const TestResult&       cumulated_result)
{
    for (each<Impl::TestListenerContainer> i = impl->m_listeners; i; ++i)
    {
        (*i)->end_case(
            test_suite,
            test_case_name,
            test_suite_result,
            test_case_result,
            cumulated_result);
    }
}

void TestListenerCollection::write(
    const TestSuite&        test_suite,
    const char*             test_case_name,
    const char*             file,
    const size_t            line,
    const TestMessage::Type message_type,
    const char*             message)
{
    for (each<Impl::TestListenerContainer> i = impl->m_listeners; i; ++i)
    {
        (*i)->write(
            test_suite,
            test_case_name,
            file,
            line,
            message_type,
            message);
    }
}

}   // namespace foundation
