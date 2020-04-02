
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

// Standard headers.
#include <cstdint>

namespace foundation
{

//
// Default processor-time timer, based on QueryPerformanceCounter()
// on Windows and std::clock() on other platforms.
//

class APPLESEED_DLLSYMBOL DefaultProcessorTimer
  : public NonCopyable
{
  public:
    // Get the timer frequency, in Hz.
    std::uint64_t frequency();

    // Read the timer value.
    std::uint64_t read();

    // For benchmarking, read the timer value before the benchmark starts.
    std::uint64_t read_start() { return read(); }

    // For benchmarking, read the timer value after the benchmark ends.
    std::uint64_t read_end() { return read(); }
};


//
// Default wallclock-time timer, based on gettimeofday() on POSIX
// platforms, _ftime() on Windows and std::time() on other platforms.
//

class APPLESEED_DLLSYMBOL DefaultWallclockTimer
  : public NonCopyable
{
  public:
    // Get the timer frequency, in Hz.
    std::uint64_t frequency();

    // Read the timer value.
    std::uint64_t read();

    // For benchmarking, read the timer value before the benchmark starts.
    std::uint64_t read_start() { return read(); }

    // For benchmarking, read the timer value after the benchmark ends.
    std::uint64_t read_end() { return read(); }
};

}   // namespace foundation
