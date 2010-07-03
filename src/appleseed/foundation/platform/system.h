
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

#ifndef APPLESEED_FOUNDATION_PLATFORM_SYSTEM_H
#define APPLESEED_FOUNDATION_PLATFORM_SYSTEM_H

// appleseed.foundation headers.
#include "foundation/core/concepts.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cstddef>

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
// The System class provides general low-level functionalities.
//

class FOUNDATIONDLL System
  : public NonCopyable
{
  public:
    //
    // CPUs.
    //

    // Return the number of CPUs installed in the system.
    static size_t get_cpu_count();

    // Return the frequency, in Hz, of the CPU at this instant.
    static uint64 get_cpu_frequency(const uint32 calibration_time_ms = 10);

    //
    // CPU caches.
    //

    // Return the size in bytes of the L1 data cache of a given CPU.
    static size_t get_l1_data_cache_size(size_t cpu_id);

    // Return the size in bytes of a L1 data cache line of a given CPU.
    static size_t get_l1_data_cache_line_size(size_t cpu_id);

    // Return the size in bytes of the L2 data cache of a given CPU.
    static size_t get_l2_data_cache_size(size_t cpu_id);

    // Return the size in bytes of a L2 data cache line of a given CPU.
    static size_t get_l2_data_cache_line_size(size_t cpu_id);

    //
    // Physical memory.
    //

    // Return the size in bytes of the total physical memory in the system.
    static size_t get_total_ram_size();

    // Return the size in bytes of the available physical memory in the system.
    static size_t get_available_ram_size();
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_PLATFORM_SYSTEM_H
