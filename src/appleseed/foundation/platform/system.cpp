
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
#include "system.h"

// appleseed.foundation headers.
#include "foundation/platform/x86timer.h"

namespace foundation
{

//
// CPUs.
//

// Return the number of CPUs installed in the system.
size_t System::get_cpu_count()
{
    return 1;
}

// Return the frequency, in Hz, of the CPU at this instant.
static uint64 get_cpu_frequency(const uint32 calibration_time_ms)
{
    return X86Timer(calibration_time_ms).frequency();
}


//
// CPU caches.
//

// Return the size in bytes of the L1 data cache of a given CPU.
size_t System::get_l1_data_cache_size(size_t /*cpu_id*/)
{
    // todo: implement.
    return 8 * 1024;
}

// Return the size in bytes of a L1 data cache line of a given CPU.
size_t System::get_l1_data_cache_line_size(size_t /*cpu_id*/)
{
    // todo: implement.
    return 64;
}

// Return the size in bytes of the L2 data cache of a given CPU.
size_t System::get_l2_data_cache_size(size_t /*cpu_id*/)
{
    // todo: implement.
    return 512 * 1024;
}

// Return the size in bytes of a L2 data cache line of a given CPU.
size_t System::get_l2_data_cache_line_size(size_t /*cpu_id*/)
{
    // todo: implement.
    return 64;
}


//
// Physical memory.
//

// Return the size in bytes of the total physical memory in the system.
size_t System::get_total_ram_size()
{
    // todo: implement.
    return 1571564 * 1024;
}

// Return the size in bytes of the available physical memory in the system.
size_t System::get_available_ram_size()
{
    // todo: implement.
    return 1571564 * 1024;
}

}   // namespace foundation
