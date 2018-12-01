
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
#include "foundation/core/concepts/singleton.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class IFilter; }
namespace foundation    { class ITestListener; }
namespace foundation    { class TestResult; }
namespace foundation    { class TestSuite; }

namespace foundation
{

//
// The (unique) test suite repository, as a collection of test suites.
//

class APPLESEED_DLLSYMBOL TestSuiteRepository
  : public Singleton<TestSuiteRepository>
{
  public:
    // Constructor (kept public for the unit tests).
    TestSuiteRepository();

    // Destructor (kept public for the unit tests).
    ~TestSuiteRepository() override;

    // Remove all test suites.
    void clear();

    // Register a test suite.
    void register_suite(TestSuite* suite);

    // Retrieve the number of registered test suites.
    size_t get_suite_count() const;

    // Retrieve a given registered test suite.
    TestSuite* get_suite(const size_t index) const;

    // Run all the registered test suites.
    void run(
        ITestListener&  test_listener,
        TestResult&     cumulated_result) const;

    // Run those test suites whose name pass a given filter.
    void run(
        const IFilter&  filter,
        ITestListener&  test_listener,
        TestResult&     cumulated_result) const;

  private:
    struct Impl;
    Impl* impl;
};

}   // namespace foundation
