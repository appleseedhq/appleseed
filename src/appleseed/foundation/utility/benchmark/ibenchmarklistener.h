
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
#include "foundation/core/concepts/iunknown.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class BenchmarkSuite; }
namespace foundation    { class IBenchmarkCase; }
namespace foundation    { class TimingResult; }

namespace foundation
{

//
// Interface of a benchmark listener.
//

class APPLESEED_DLLSYMBOL IBenchmarkListener
  : public IUnknown
{
  public:
    // Called before each benchmark suite is run.
    virtual void begin_suite(
        const BenchmarkSuite&   benchmark_suite) = 0;

    // Called after each benchmark suite is run.
    virtual void end_suite(
        const BenchmarkSuite&   benchmark_suite) = 0;

    // Called before each benchmark case is run.
    virtual void begin_case(
        const BenchmarkSuite&   benchmark_suite,
        const IBenchmarkCase&   benchmark_case) = 0;

    // Called after each benchmark case is run.
    virtual void end_case(
        const BenchmarkSuite&   benchmark_suite,
        const IBenchmarkCase&   benchmark_case) = 0;

    // Write a message.
    virtual void write(
        const BenchmarkSuite&   benchmark_suite,
        const IBenchmarkCase&   benchmark_case,
        const char*             file,
        const size_t            line,
        const char*             message) = 0;

    // Write a timing result.
    virtual void write(
        const BenchmarkSuite&   benchmark_suite,
        const IBenchmarkCase&   benchmark_case,
        const char*             file,
        const size_t            line,
        const TimingResult&     timing_result) = 0;
};

}   // namespace foundation
