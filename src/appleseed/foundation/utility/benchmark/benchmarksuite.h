
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
#include "foundation/core/concepts/noncopyable.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class BenchmarkResult; }
namespace foundation    { class IBenchmarkCaseFactory; }
namespace foundation    { class IFilter; }

namespace foundation
{

//
// A benchmark suite, as a collection of benchmarks.
//

class APPLESEED_DLLSYMBOL BenchmarkSuite
  : public NonCopyable
{
  public:
    // Constructor.
    explicit BenchmarkSuite(const char* name);

    // Destructor.
    ~BenchmarkSuite();

    // Return the name of the benchmark suite.
    const char* get_name() const;

    // Register a benchmark case (via its factory function).
    void register_case(IBenchmarkCaseFactory* factory);

    // Run all the registered benchmark cases.
    void run(BenchmarkResult& suite_result) const;

    // Run those benchmark cases whose name pass a given filter.
    void run(
        const IFilter&      filter,
        BenchmarkResult&    suite_result) const;

  private:
    struct Impl;
    Impl* impl;
};

}   // namespace foundation
