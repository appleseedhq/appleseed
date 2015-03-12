
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "foundation/platform/thread.h"
#include "foundation/utility/log.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <string>

// Windows.
#if defined _WIN32

    // appleseed.foundation headers.
    #include "foundation/platform/windows.h"

    // Standard headers.
    #include <cassert>
    #include <cstdlib>

    // Platform headers.
    #include "psapi.h"

// OS X.
#elif defined __APPLE__

    // Platform headers.
    #include <mach/mach.h>
    #include <mach/task.h>
    #include <mach/task_info.h>
    #include <sys/mount.h>
    #include <sys/param.h>
    #include <sys/sysctl.h>
    #include <sys/types.h>

// Linux.
#elif defined __linux__

    // Standard headers.
    #include <cstdio>

    // Platform headers.
    #include <sys/sysinfo.h>
    #include <sys/types.h>
    #include <unistd.h>

// Other platforms.
#else

    #error Unsupported platform.

#endif

using namespace std;

namespace foundation
{

// ------------------------------------------------------------------------------------------------
// Common code.
// ------------------------------------------------------------------------------------------------

void System::print_information(Logger& logger)
{
    LOG_INFO(
        logger,
        "system information:\n"
        "  logical cores    %s\n"
        "  L1 data cache    size %s, line size %s\n"
        "  L2 cache         size %s, line size %s\n"
        "  L3 cache         size %s, line size %s\n"
        "  physical memory  size %s\n"
        "  virtual memory   size %s",
        pretty_uint(get_logical_cpu_core_count()).c_str(),
        pretty_size(get_l1_data_cache_size()).c_str(),
        pretty_size(get_l1_data_cache_line_size()).c_str(),
        pretty_size(get_l2_cache_size()).c_str(),
        pretty_size(get_l2_cache_line_size()).c_str(),
        pretty_size(get_l3_cache_size()).c_str(),
        pretty_size(get_l3_cache_line_size()).c_str(),
        pretty_size(get_total_physical_memory_size()).c_str(),
        pretty_size(get_total_virtual_memory_size()).c_str());
}

size_t System::get_logical_cpu_core_count()
{
    const size_t concurrency =
        static_cast<size_t>(boost::thread::hardware_concurrency());

    return concurrency > 1 ? concurrency : 1;
}

// ------------------------------------------------------------------------------------------------
// Windows.
// ------------------------------------------------------------------------------------------------

#if defined _WIN32

namespace
{
    // This code is based on a code snippet by Nick Strupat (http://stackoverflow.com/a/4049562).
    bool get_cache_descriptor(const size_t level, CACHE_DESCRIPTOR& result)
    {
        assert(level >= 1 && level <= 3);

        DWORD buffer_size = 0;
        BOOL success = GetLogicalProcessorInformation(0, &buffer_size);
        if (success == TRUE || GetLastError() != ERROR_INSUFFICIENT_BUFFER)
            return false;

        SYSTEM_LOGICAL_PROCESSOR_INFORMATION* buffer =
            (SYSTEM_LOGICAL_PROCESSOR_INFORMATION*)malloc(buffer_size);
        if (buffer == 0)
            return false;

        success = GetLogicalProcessorInformation(buffer, &buffer_size);
        if (success == FALSE)
        {
            free(buffer);
            return false;
        }

        bool found = false;

        for (size_t i = 0; i < buffer_size / sizeof(SYSTEM_LOGICAL_PROCESSOR_INFORMATION); ++i)
        {
            if (buffer[i].Relationship == RelationCache)
            {
                const CACHE_DESCRIPTOR& cache = buffer[i].Cache;

                if (cache.Level == level && (cache.Type == CacheData || cache.Type == CacheUnified))
                {
                    found = true;
                    result = cache;
                    break;
                }
            }
        }

        free(buffer);

        return found;
    }
}

size_t System::get_l1_data_cache_size()
{
    CACHE_DESCRIPTOR cache;
    return get_cache_descriptor(1, cache) ? cache.Size : 0;
}

size_t System::get_l1_data_cache_line_size()
{
    CACHE_DESCRIPTOR cache;
    return get_cache_descriptor(1, cache) ? cache.LineSize : 0;
}

size_t System::get_l2_cache_size()
{
    CACHE_DESCRIPTOR cache;
    return get_cache_descriptor(2, cache) ? cache.Size : 0;
}

size_t System::get_l2_cache_line_size()
{
    CACHE_DESCRIPTOR cache;
    return get_cache_descriptor(2, cache) ? cache.LineSize : 0;
}

size_t System::get_l3_cache_size()
{
    CACHE_DESCRIPTOR cache;
    return get_cache_descriptor(3, cache) ? cache.Size : 0;
}

size_t System::get_l3_cache_line_size()
{
    CACHE_DESCRIPTOR cache;
    return get_cache_descriptor(3, cache) ? cache.LineSize : 0;
}

uint64 System::get_total_physical_memory_size()
{
    // Reference: http://stackoverflow.com/questions/63166/how-to-determine-cpu-and-memory-consumption-from-inside-a-process

    MEMORYSTATUSEX mem_info;
    mem_info.dwLength = sizeof(mem_info);
    GlobalMemoryStatusEx(&mem_info);

    return static_cast<uint64>(mem_info.ullTotalPhys);
}

uint64 System::get_total_virtual_memory_size()
{
    // Reference: http://stackoverflow.com/questions/63166/how-to-determine-cpu-and-memory-consumption-from-inside-a-process

    MEMORYSTATUSEX mem_info;
    mem_info.dwLength = sizeof(mem_info);
    GlobalMemoryStatusEx(&mem_info);

    return static_cast<uint64>(mem_info.ullTotalPageFile);
}

uint64 System::get_process_virtual_memory_size()
{
    // Reference: http://stackoverflow.com/questions/63166/how-to-determine-cpu-and-memory-consumption-from-inside-a-process

    PROCESS_MEMORY_COUNTERS_EX pmc;
    GetProcessMemoryInfo(
        GetCurrentProcess(),
        reinterpret_cast<PROCESS_MEMORY_COUNTERS*>(&pmc),
        sizeof(pmc));

    return pmc.PrivateUsage;
}

// ------------------------------------------------------------------------------------------------
// OS X.
// ------------------------------------------------------------------------------------------------

#elif defined __APPLE__

namespace
{
    size_t get_system_value(const char* name)
    {
        size_t value = 0;
        size_t value_size = sizeof(value);
        return sysctlbyname(name, &value, &value_size, 0, 0) == 0 ? value : 0;
    }
}

size_t System::get_l1_data_cache_size()
{
    return get_system_value("hw.l1dcachesize");
}

size_t System::get_l1_data_cache_line_size()
{
    return get_l1_data_cache_size() > 0 ? get_system_value("hw.cachelinesize") : 0;
}

size_t System::get_l2_cache_size()
{
    return get_system_value("hw.l2cachesize");
}

size_t System::get_l2_cache_line_size()
{
    return get_l2_cache_size() > 0 ? get_system_value("hw.cachelinesize") : 0;
}

size_t System::get_l3_cache_size()
{
    return get_system_value("hw.l3cachesize");
}

size_t System::get_l3_cache_line_size()
{
    return get_l3_cache_size() > 0 ? get_system_value("hw.cachelinesize") : 0;
}

uint64 System::get_total_physical_memory_size()
{
    // Reference: http://stackoverflow.com/questions/63166/how-to-determine-cpu-and-memory-consumption-from-inside-a-process

    int name[2] = { CTL_HW, HW_MEMSIZE };
    uint64_t result;
    size_t result_length = sizeof(uint64_t);
    sysctl(name, 2, &result, &result_length, 0, 0);

    return static_cast<uint64>(result);
}

uint64 System::get_total_virtual_memory_size()
{
    // Reference: http://stackoverflow.com/questions/63166/how-to-determine-cpu-and-memory-consumption-from-inside-a-process

    struct statfs stats;
    if (statfs("/", &stats) == 0)
        return static_cast<uint64>(stats.f_bsize) * stats.f_bfree;
    else return 0;
}

uint64 System::get_process_virtual_memory_size()
{
    // Reference: http://nadeausoftware.com/articles/2012/07/c_c_tip_how_get_process_resident_set_size_physical_memory_use

#ifdef MACH_TASK_BASIC_INFO
    struct mach_task_basic_info info;
    mach_msg_type_number_t info_count = MACH_TASK_BASIC_INFO_COUNT;

    if (task_info(
            mach_task_self(),
            MACH_TASK_BASIC_INFO,
            (task_info_t)&info,
            &info_count) != KERN_SUCCESS)
        return 0;
#else
    struct task_basic_info info;
    mach_msg_type_number_t info_count = TASK_BASIC_INFO_COUNT;

    if (task_info(
            mach_task_self(),
            TASK_BASIC_INFO,
            (task_info_t)&info,
            &info_count) != KERN_SUCCESS)
        return 0;
#endif

    return info.resident_size;
}

// ------------------------------------------------------------------------------------------------
// Linux.
// ------------------------------------------------------------------------------------------------

#elif defined __linux__

size_t System::get_l1_data_cache_size()
{
    return sysconf(_SC_LEVEL1_DCACHE_SIZE);
}

size_t System::get_l1_data_cache_line_size()
{
    return sysconf(_SC_LEVEL1_DCACHE_LINESIZE);
}

size_t System::get_l2_cache_size()
{
    return sysconf(_SC_LEVEL2_CACHE_SIZE);
}

size_t System::get_l2_cache_line_size()
{
    return sysconf(_SC_LEVEL2_CACHE_LINESIZE);
}

size_t System::get_l3_cache_size()
{
    return sysconf(_SC_LEVEL3_CACHE_SIZE);
}

size_t System::get_l3_cache_line_size()
{
    return sysconf(_SC_LEVEL3_CACHE_LINESIZE);
}

uint64 System::get_total_physical_memory_size()
{
    // Reference: http://stackoverflow.com/questions/63166/how-to-determine-cpu-and-memory-consumption-from-inside-a-process

    struct sysinfo mem_info;
    sysinfo(&mem_info);

    return static_cast<uint64>(mem_info.totalram) * mem_info.mem_unit;
}

uint64 System::get_total_virtual_memory_size()
{
    // Reference: http://stackoverflow.com/questions/63166/how-to-determine-cpu-and-memory-consumption-from-inside-a-process

    struct sysinfo mem_info;
    sysinfo(&mem_info);

    uint64 result = mem_info.totalram;
    result += mem_info.totalswap;
    result *= mem_info.mem_unit;

    return result;
}

uint64 System::get_process_virtual_memory_size()
{
    // Reference: http://nadeausoftware.com/articles/2012/07/c_c_tip_how_get_process_resident_set_size_physical_memory_use

    FILE* fp = fopen("/proc/self/statm", "r");
    if (fp == 0)
        return 0;

    long rss = 0;
    if (fscanf(fp, "%*s%ld", &rss) != 1)
    {
        fclose(fp);
        return 0;
    }

    fclose(fp);

    return static_cast<uint64>(rss) * sysconf(_SC_PAGESIZE);
}

#endif

}   // namespace foundation
