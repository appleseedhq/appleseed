
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

#ifndef APPLESEED_FOUNDATION_UTILITY_TEST_TESTSUITEREPOSITORY_H
#define APPLESEED_FOUNDATION_UTILITY_TEST_TESTSUITEREPOSITORY_H

// appleseed.foundation headers.
#include "foundation/core/concepts/singleton.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class IFilter; }
namespace foundation    { class ITestListener; }
namespace foundation    { class TestResult; }
namespace foundation    { class TestSuite; }

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
// The (unique) test suite repository, as a collection of test suites.
//

class FOUNDATIONDLL TestSuiteRepository
  : public Singleton<TestSuiteRepository>
{
  public:
    // Constructor.
    TestSuiteRepository();

    // Destructor.
    ~TestSuiteRepository();

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
    // Private implementation.
    struct Impl;
    Impl* impl;
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_TEST_TESTSUITEREPOSITORY_H
