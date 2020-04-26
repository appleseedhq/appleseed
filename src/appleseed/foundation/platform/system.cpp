
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
#include "system.h"

// appleseed.foundation headers.
#ifdef APPLESEED_WITH_GPU
#include "foundation/cuda/cudadevice.h"
#include "foundation/cuda/exception.h"
#endif
#include "foundation/platform/arch.h"
#include "foundation/platform/defaulttimers.h"
#include "foundation/platform/thread.h"
#include "foundation/platform/types.h"
#include "foundation/log/log.h"
#include "foundation/string/string.h"

// Standard headers.
#include <cstring>
#include <sstream>

// Windows.
#if defined _WIN32

    // appleseed.foundation headers.
    #include "foundation/platform/windows.h"

    // Standard headers.
    #include <cassert>
    #include <cstdlib>
    #include <string>

    // Platform headers.
    #include "psapi.h"

// macOS.
#elif defined __APPLE__

    // Platform headers.
    #include <mach/mach.h>
    #include <mach/task.h>
    #include <mach/task_info.h>
    #include <sys/mount.h>
    #include <sys/param.h>
    #include <sys/sysctl.h>
    #include <sys/types.h>
    #include <cpuid.h>

// Linux.
#elif defined __linux__

    // Standard headers.
    #include <cstdio>

    // Platform headers.
    #include <sys/sysinfo.h>
    #include <sys/types.h>
    #include <cpuid.h>
    #include <unistd.h>

// FreeBSD.
#elif defined __FreeBSD__

    #include <sys/types.h>
    #include <sys/resource.h>
    #include <sys/sysctl.h>
    #include <unistd.h>

// Other platforms.
#else

    #error Unsupported platform.

#endif

#ifdef APPLESEED_WITH_GPU

    #include <cuda.h>
    #include <optix.h>

#endif


namespace foundation
{

// ------------------------------------------------------------------------------------------------
// Windows.
// ------------------------------------------------------------------------------------------------

#if defined _WIN32

namespace
{
    // EAX is set to the value of `index`. Results are returned in `cpuinfo`:
    //   cpuinfo[0] == EAX
    //   cpuinfo[1] == EBX
    //   cpuinfo[2] == ECX
    //   cpuinfo[3] == EDX
    void cpuid(std::uint32_t cpuinfo[4], const std::uint32_t index)
    {
        __cpuidex(reinterpret_cast<int*>(cpuinfo), static_cast<int>(index), 0);
    }

    std::uint64_t xgetbv(const std::int32_t index)
    {
        return _xgetbv(index);
    }

    // This code is loosely based on a code snippet by Nick Strupat (http://stackoverflow.com/a/4049562).
    std::size_t get_cache_descriptor(const std::size_t level, CACHE_DESCRIPTOR& result)
    {
        assert(level >= 1 && level <= 3);

        DWORD buffer_size = 0;
        const BOOL success = GetLogicalProcessorInformation(nullptr, &buffer_size);
        if (success == TRUE || GetLastError() != ERROR_INSUFFICIENT_BUFFER)  // todo: success == TRUE?
            return false;

        std::vector<std::uint8_t> buffer_vec(buffer_size);
        auto buffer = reinterpret_cast<SYSTEM_LOGICAL_PROCESSOR_INFORMATION*>(buffer_vec.data());

        if (GetLogicalProcessorInformation(buffer, &buffer_size) == FALSE)
            return false;

        std::size_t cache_count = 0;

        for (std::size_t i = 0; i < buffer_size / sizeof(SYSTEM_LOGICAL_PROCESSOR_INFORMATION); ++i)
        {
            if (buffer[i].Relationship == RelationCache)
            {
                const CACHE_DESCRIPTOR& cache = buffer[i].Cache;

                if (cache.Level == level && (cache.Type == CacheData || cache.Type == CacheUnified))
                {
                    if (cache_count == 0)
                        result = cache;
                    ++cache_count;
                }
            }
        }

        return cache_count;
    }
}

std::size_t System::get_l1_data_cache_count()
{
    CACHE_DESCRIPTOR cache;
    return get_cache_descriptor(1, cache);
}

std::size_t System::get_l1_data_cache_size()
{
    CACHE_DESCRIPTOR cache;
    const std::size_t cache_count = get_cache_descriptor(1, cache);
    return cache_count > 0 ? cache.Size : 0;
}

std::size_t System::get_l1_data_cache_line_size()
{
    CACHE_DESCRIPTOR cache;
    const std::size_t cache_count = get_cache_descriptor(1, cache);
    return cache_count > 0 ? cache.LineSize : 0;
}

std::size_t System::get_l2_cache_count()
{
    CACHE_DESCRIPTOR cache;
    return get_cache_descriptor(2, cache);
}

std::size_t System::get_l2_cache_size()
{
    CACHE_DESCRIPTOR cache;
    const std::size_t cache_count = get_cache_descriptor(2, cache);
    return cache_count > 0 ? cache.Size : 0;
}

std::size_t System::get_l2_cache_line_size()
{
    CACHE_DESCRIPTOR cache;
    const std::size_t cache_count = get_cache_descriptor(2, cache);
    return cache_count > 0 ? cache.LineSize : 0;
}

std::size_t System::get_l3_cache_count()
{
    CACHE_DESCRIPTOR cache;
    return get_cache_descriptor(3, cache);
}

std::size_t System::get_l3_cache_size()
{
    CACHE_DESCRIPTOR cache;
    const std::size_t cache_count = get_cache_descriptor(3, cache);
    return cache_count > 0 ? cache.Size : 0;
}

std::size_t System::get_l3_cache_line_size()
{
    CACHE_DESCRIPTOR cache;
    const std::size_t cache_count = get_cache_descriptor(3, cache);
    return cache_count > 0 ? cache.LineSize : 0;
}

std::uint64_t System::get_total_physical_memory_size()
{
    // Reference: http://stackoverflow.com/questions/63166/how-to-determine-cpu-and-memory-consumption-from-inside-a-process

    MEMORYSTATUSEX mem_info;
    mem_info.dwLength = sizeof(mem_info);
    GlobalMemoryStatusEx(&mem_info);

    return static_cast<std::uint64_t>(mem_info.ullTotalPhys);
}

std::uint64_t System::get_total_virtual_memory_size()
{
    // Reference: http://stackoverflow.com/questions/63166/how-to-determine-cpu-and-memory-consumption-from-inside-a-process

    MEMORYSTATUSEX mem_info;
    mem_info.dwLength = sizeof(mem_info);
    GlobalMemoryStatusEx(&mem_info);

    return static_cast<std::uint64_t>(mem_info.ullTotalPageFile);
}

std::uint64_t System::get_process_virtual_memory_size()
{
    // Reference: http://stackoverflow.com/questions/63166/how-to-determine-cpu-and-memory-consumption-from-inside-a-process

    PROCESS_MEMORY_COUNTERS_EX pmc;
    GetProcessMemoryInfo(
        GetCurrentProcess(),
        reinterpret_cast<PROCESS_MEMORY_COUNTERS*>(&pmc),
        sizeof(pmc));

    return pmc.PrivateUsage;
}

std::uint64_t System::get_peak_process_virtual_memory_size()
{
    PROCESS_MEMORY_COUNTERS_EX pmc;
    GetProcessMemoryInfo(
        GetCurrentProcess(),
        reinterpret_cast<PROCESS_MEMORY_COUNTERS*>(&pmc),
        sizeof(pmc));

    return pmc.PeakPagefileUsage;
}

// ------------------------------------------------------------------------------------------------
// macOS.
// ------------------------------------------------------------------------------------------------

#elif defined __APPLE__

namespace
{
    // EAX is set to the value of `index`. Results are returned in `cpuinfo`:
    //   cpuinfo[0] == EAX
    //   cpuinfo[1] == EBX
    //   cpuinfo[2] == ECX
    //   cpuinfo[3] == EDX
    void cpuid(std::uint32_t cpuinfo[4], const std::uint32_t index)
    {
        __cpuid_count(
            index,
            0,
            cpuinfo[0],
            cpuinfo[1],
            cpuinfo[2],
            cpuinfo[3]);
    }

    std::uint64_t xgetbv(const std::int32_t index)
    {
        std::uint32_t eax, edx;
        __asm__ __volatile__("xgetbv" : "=a"(eax), "=d"(edx) : "c"(index));
        return (static_cast<std::uint64_t>(edx) << 32) | eax;
    }

    std::size_t get_system_value(const char* name)
    {
        std::size_t value = 0;
        std::size_t value_size = sizeof(value);
        return sysctlbyname(name, &value, &value_size, 0, 0) == 0 ? value : 0;
    }
}

std::size_t System::get_l1_data_cache_count()
{
    // todo
    return 0;
}

std::size_t System::get_l1_data_cache_size()
{
    return get_system_value("hw.l1dcachesize");
}

std::size_t System::get_l1_data_cache_line_size()
{
    return get_l1_data_cache_size() > 0 ? get_system_value("hw.cachelinesize") : 0;
}

std::size_t System::get_l2_cache_count()
{
    // todo
    return 0;
}

std::size_t System::get_l2_cache_size()
{
    return get_system_value("hw.l2cachesize");
}

std::size_t System::get_l2_cache_line_size()
{
    return get_l2_cache_size() > 0 ? get_system_value("hw.cachelinesize") : 0;
}

std::size_t System::get_l3_cache_count()
{
    // todo
    return 0;
}

std::size_t System::get_l3_cache_size()
{
    return get_system_value("hw.l3cachesize");
}

std::size_t System::get_l3_cache_line_size()
{
    return get_l3_cache_size() > 0 ? get_system_value("hw.cachelinesize") : 0;
}

std::uint64_t System::get_total_physical_memory_size()
{
    // Reference: http://stackoverflow.com/questions/63166/how-to-determine-cpu-and-memory-consumption-from-inside-a-process

    int name[2] = { CTL_HW, HW_MEMSIZE };
    std::uint64_t result;
    std::size_t result_length = sizeof(std::uint64_t);
    sysctl(name, 2, &result, &result_length, 0, 0);

    return static_cast<std::uint64_t>(result);
}

std::uint64_t System::get_total_virtual_memory_size()
{
    // Reference: http://stackoverflow.com/questions/63166/how-to-determine-cpu-and-memory-consumption-from-inside-a-process

    struct statfs stats;
    if (statfs("/", &stats) == 0)
        return static_cast<std::uint64_t>(stats.f_bsize) * stats.f_bfree;
    else return 0;
}

std::uint64_t System::get_process_virtual_memory_size()
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

    return info.virtual_size;
}

std::uint64_t System::get_peak_process_virtual_memory_size()
{
    // todo: implement.
    return 0;
}

// ------------------------------------------------------------------------------------------------
// Linux.
// ------------------------------------------------------------------------------------------------

#elif defined __linux__

namespace
{
    // EAX is set to the value of `index`. Results are returned in `cpuinfo`:
    //   cpuinfo[0] == EAX
    //   cpuinfo[1] == EBX
    //   cpuinfo[2] == ECX
    //   cpuinfo[3] == EDX
    void cpuid(std::uint32_t cpuinfo[4], const std::uint32_t index)
    {
        __cpuid_count(
            index,
            0,
            cpuinfo[0],
            cpuinfo[1],
            cpuinfo[2],
            cpuinfo[3]);
    }

    std::uint64_t xgetbv(const std::int32_t index)
    {
        std::uint32_t eax, edx;
        __asm__ __volatile__("xgetbv" : "=a"(eax), "=d"(edx) : "c"(index));
        return (static_cast<std::uint64_t>(edx) << 32) | eax;
    }
}

std::size_t System::get_l1_data_cache_count()
{
    return sysconf(_SC_LEVEL1_DCACHE_ASSOC);
}

std::size_t System::get_l1_data_cache_size()
{
    return sysconf(_SC_LEVEL1_DCACHE_SIZE);
}

std::size_t System::get_l1_data_cache_line_size()
{
    return sysconf(_SC_LEVEL1_DCACHE_LINESIZE);
}

std::size_t System::get_l2_cache_count()
{
    return sysconf(_SC_LEVEL2_CACHE_ASSOC);
}

std::size_t System::get_l2_cache_size()
{
    return sysconf(_SC_LEVEL2_CACHE_SIZE);
}

std::size_t System::get_l2_cache_line_size()
{
    return sysconf(_SC_LEVEL2_CACHE_LINESIZE);
}

std::size_t System::get_l3_cache_count()
{
    return sysconf(_SC_LEVEL3_CACHE_ASSOC);
}

std::size_t System::get_l3_cache_size()
{
    return sysconf(_SC_LEVEL3_CACHE_SIZE);
}

std::size_t System::get_l3_cache_line_size()
{
    return sysconf(_SC_LEVEL3_CACHE_LINESIZE);
}

std::uint64_t System::get_total_physical_memory_size()
{
    // Reference: http://stackoverflow.com/questions/63166/how-to-determine-cpu-and-memory-consumption-from-inside-a-process

    struct sysinfo mem_info;
    sysinfo(&mem_info);

    return static_cast<std::uint64_t>(mem_info.totalram) * mem_info.mem_unit;
}

std::uint64_t System::get_total_virtual_memory_size()
{
    // Reference: http://stackoverflow.com/questions/63166/how-to-determine-cpu-and-memory-consumption-from-inside-a-process

    struct sysinfo mem_info;
    sysinfo(&mem_info);

    std::uint64_t result = mem_info.totalram;
    result += mem_info.totalswap;
    result *= mem_info.mem_unit;

    return result;
}

std::uint64_t System::get_process_virtual_memory_size()
{
    // todo: this is wrong, it returns RSS instead of virtual memory.

    // Reference: http://nadeausoftware.com/articles/2012/07/c_c_tip_how_get_process_resident_set_size_physical_memory_use

    FILE* fp = fopen("/proc/self/statm", "r");
    if (fp == nullptr)
        return 0;

    long rss = 0;
    if (fscanf(fp, "%*s%ld", &rss) != 1)
    {
        fclose(fp);
        return 0;
    }

    fclose(fp);

    return static_cast<std::uint64_t>(rss) * sysconf(_SC_PAGESIZE);
}

std::uint64_t System::get_peak_process_virtual_memory_size()
{
    // todo: implement.
    return 0;
}

// ------------------------------------------------------------------------------------------------
// FreeBSD.
// ------------------------------------------------------------------------------------------------

#elif defined __FreeBSD__ && (__x86_64__ || __i386__)

// FreeBSD does not provide an API to query CPU cache information, so we'd
// have to ask for it by ourselves...

namespace
{
    // Since we only want to know cache size and line size, structure below
    // is very simplistic; initially all caches have zero size (i.e. absent).
    struct TrivialX86Cache
    {
        std::size_t m_associativity;
        std::size_t m_size;
        std::size_t m_line_size;
    } g_x86_caches[3];

    enum { L1D, L2, L3 };
    enum { eax, ebx, ecx, edx };

    // %ebx may be used to point to GOT (Global Offset Table) for PIC (Position
    // Independent Code) on 32-bit x86, so it must be preserved.  Normally
    // compilers handle this implicitly because %ebx is also callee saved, but
    // GCC before 5.0 forbids any use of %ebx with PIC, so it must be performed
    // explicitly.  Unfortunately, we need a separate implementation for x86-64
    // to preserve %rbx because 32-bit operations would set the upper 32 bits
    // to zero.
    void cpuid(uint32_t* regs)
    {
        asm(
#if __x86_64__
            "movq %%rbx, %q1\n\t"
            "cpuid\n\t"
            "xchgq %%rbx, %q1"
#else
            "movl %%ebx, %1\n\t"
            "cpuid\n\t"
            "xchgl %%ebx, %1"
#endif
          : "+a" (regs[eax]),
            "=r" (regs[ebx]),
            "+c" (regs[ecx]),
            "=d" (regs[edx]));
    }

#define BIT(n)              (1UL << (n))
#define BITMASK(h, l)       ((BIT(h) | (BIT(h) - 1)) & ~(BIT(l) - 1))
#define BITFIELD(x, h, l)   (((x) & BITMASK(h, l)) >> l)

    // For modern CPUs, we use Deterministic Cache Parameters (Function 04h)
    // to obtain cache information.
    void get_cache_info_deterministic(TrivialX86Cache* caches)
    {
        uint32_t regs[4];

        // Cycle up to ten possible caches to be extra sure.
        for (std::size_t i = 0; i < 10; i++)
        {
            regs[eax] = 4;
            regs[ecx] = i;
            cpuid(regs);

            const unsigned int type = BITFIELD(regs[eax], 4, 0);
            if (type == 0) break;       // no more caches, we're done
            if (type == 2) continue;    // ignore instruction caches

            const unsigned int level = BITFIELD(regs[eax], 7, 5);
            const unsigned int line_size = BITFIELD(regs[ebx], 11, 0) + 1;
            const unsigned int sets = BITFIELD(regs[ecx], 31, 0) + 1;
            const unsigned int associativity = BITFIELD(regs[ebx], 31, 22) + 1;

            assert(level > 0);

            caches[level - 1].m_associativity = associativity;
            caches[level - 1].m_size = line_size * sets;
            caches[level - 1].m_line_size = line_size;
        }
    }

    // On older CPUs we might have to rely on Cache Descriptors (Function 02h)
    // and Intel documentation (table of the known values).
    // todo: fill m_associativity field.
    void get_cache_info_from_table(TrivialX86Cache* caches)
    {
        uint32_t regs[4];
        int no_higher_level_cache = 0;

        regs[eax] = 2;
        cpuid(regs);

        // Doing only one call is technically wrong, but all CPUs up to Core i7
        // require a single call.  Since this is a fallback code path for really
        // old CPUs anyways (modern ones will provide Function 4), we should be
        // safe, but let's add an assert() on the lower 8 bits just in case.
        assert((regs[eax] & 0xFF) == 1);

        for (std::size_t i = 1; i < 4 * 4; i++)
        {
            // Check descriptor validity for every octet: if bit 31 is set,
            // skip to the next one.
            if (i % 4 == 0 && (regs[i / 4] >> 31) == 1)
            {
                i += 4;
                continue;
            }

            // Descriptor decode values from the Intel manual, Table 2-7.
            switch ((regs[i / 4] >> (i % 4) * 8) & 0xFF)
            {
              case 0x0A:
                caches[L1D].m_size = 8;
                caches[L1D].m_line_size = 32;
                break;
              case 0x0C:
                caches[L1D].m_size = 16;
                caches[L1D].m_line_size = 32;
                break;
              case 0x0D:
              case 0x60:
              case 0x67:
                caches[L1D].m_size = 16;
                caches[L1D].m_line_size = 64;
                break;
              case 0x21:
              case 0x3C:
              case 0x7A:
                caches[L2].m_size = 256;
                caches[L2].m_line_size = 64;
                break;
              case 0x22:
              case 0xD0:
                caches[L3].m_size = 512;
                caches[L3].m_line_size = 64;
                break;
              case 0x23:
              case 0xD1:
              case 0xD6:
                caches[L3].m_size = 1024;
                caches[L3].m_line_size = 64;
                break;
              case 0x25:
              case 0xD2:
              case 0xD7:
              case 0xE2:
                caches[L3].m_size = 2048;
                caches[L3].m_line_size = 64;
                break;
              case 0x29:
              case 0x46:
              case 0xD8:
              case 0xE3:
                caches[L3].m_size = 4096;
                caches[L3].m_line_size = 64;
                break;
              case 0x2C:
                caches[L1D].m_size = 32;
                caches[L1D].m_line_size = 64;
                break;
              case 0x39:
              case 0x3B:
              case 0x79:
                caches[L2].m_size = 128;
                caches[L2].m_line_size = 64;
                break;
              case 0x3A:
                caches[L2].m_size = 192;
                caches[L2].m_line_size = 64;
                break;
              case 0x3D:
                caches[L2].m_size = 384;
                caches[L2].m_line_size = 64;
                break;
              case 0x3E:
              case 0x7B:
              case 0x7F:
              case 0x86:
                caches[L2].m_size = 512;
                caches[L2].m_line_size = 64;
                break;
              case 0x40:
                no_higher_level_cache = 1;
                break;
              case 0x41:
                caches[L2].m_size = 128;
                caches[L2].m_line_size = 32;
                break;
              case 0x42:
              case 0x82:
                caches[L2].m_size = 256;
                caches[L2].m_line_size = 32;
                break;
              case 0x43:
              case 0x83:
                caches[L2].m_size = 512;
                caches[L2].m_line_size = 32;
                break;
              case 0x44:
                caches[L2].m_size = 1024;
                caches[L2].m_line_size = 32;
                break;
              case 0x45:
                caches[L2].m_size = 2048;
                caches[L2].m_line_size = 32;
                break;
              case 0x47:
              case 0x4B:
              case 0xE4:
                caches[L3].m_size = 8192;
                caches[L3].m_line_size = 64;
                break;
              case 0x48:
                caches[L2].m_size = 3072;
                caches[L2].m_line_size = 64;
                break;
              case 0x49:
                // todo: check for Intel Xeon processor MP, Family 0Fh,
                // Model 06h, because 0x49 means L3 cache (4MB, 16-way,
                // 64-byte linesize) for this CPU.
                caches[L2].m_size = 4096;
                caches[L2].m_line_size = 64;
                break;
              case 0x4A:
              case 0xDE:
                caches[L3].m_size = 6 * 1024;
                caches[L3].m_line_size = 64;
                break;
              case 0x4C:
              case 0xEA:
                caches[L3].m_size = 12 * 1024;
                caches[L3].m_line_size = 64;
                break;
              case 0x4D:
                caches[L3].m_size = 16 * 1024;
                caches[L3].m_line_size = 64;
                break;
              case 0x4E:
                caches[L2].m_size = 6 * 1024;
                caches[L2].m_line_size = 64;
                break;
              case 0x66:
                caches[L1D].m_size = 8;
                caches[L1D].m_line_size = 64;
                break;
              case 0x68:
                caches[L1D].m_size = 32;
                caches[L1D].m_line_size = 64;
                break;
              case 0x78:
              case 0x7C:
                caches[L2].m_size = 1024;
                caches[L2].m_line_size = 64;
                break;
              case 0x7D:
                caches[L2].m_size = 2048;
                caches[L2].m_line_size = 64;
                break;
              case 0x84:
                caches[L2].m_size = 1024;
                caches[L2].m_line_size = 32;
                break;
              case 0x85:
                caches[L2].m_size = 2048;
                caches[L2].m_line_size = 32;
                break;
              case 0x87:
                caches[L2].m_size = 1024;
                caches[L2].m_line_size = 64;
                break;
              case 0xDC:
                caches[L3].m_size = 1536;
                caches[L3].m_line_size = 64;
                break;
              case 0xDD:
                caches[L3].m_size = 3 * 1024;
                caches[L3].m_line_size = 64;
                break;
              case 0xEB:
                caches[L3].m_size = 18 * 1024;
                caches[L3].m_line_size = 64;
                break;
              case 0xEC:
                caches[L3].m_size = 24 * 1024;
                caches[L3].m_line_size = 64;
                break;
            }
        }

        // Convert Kbytes to bytes.
        caches[L1D].m_size *= 1024;
        caches[L2].m_size *= 1024;
        caches[L3].m_size *= 1024;
    }

    void x86_get_cache_basic_info(TrivialX86Cache* caches)
    {
        uint32_t regs[4];

        // Assume that all Intel CPUs support CPUID instruction.
        regs[eax] = 0;
        cpuid(regs);
        if (regs[eax] >= 4)
            get_cache_info_deterministic(caches);
        else if (regs[eax] >= 2)
            get_cache_info_from_table(caches);
    }
}

std::size_t System::get_l1_data_cache_size()
{
    // Here and below we'd check for L1D cache size: if it's initialized,
    // it means that x86_get_cache_basic_info() was already called and we
    // don't have to do it again.
    if (!g_x86_caches[L1D].m_size)
        x86_get_cache_basic_info(g_x86_caches);
    return g_x86_caches[L1D].m_size;
}

std::size_t System::get_l1_data_cache_line_size()
{
    if (!g_x86_caches[L1D].m_size)
        x86_get_cache_basic_info(g_x86_caches);
    return g_x86_caches[L1D].m_line_size;
}

std::size_t System::get_l2_cache_size()
{
    if (!g_x86_caches[L1D].m_size)
        x86_get_cache_basic_info(g_x86_caches);
    return g_x86_caches[L2].m_size;
}

std::size_t System::get_l2_cache_line_size()
{
    if (!g_x86_caches[L1D].m_size)
        x86_get_cache_basic_info(g_x86_caches);
    return g_x86_caches[L2].m_line_size;
}

std::size_t System::get_l3_cache_size()
{
    if (!g_x86_caches[L1D].m_size)
        x86_get_cache_basic_info(g_x86_caches);
    return g_x86_caches[L3].m_size;
}

std::size_t System::get_l3_cache_line_size()
{
    if (!g_x86_caches[L1D].m_size)
        x86_get_cache_basic_info(g_x86_caches);
    return g_x86_caches[L3].m_line_size;
}

std::uint64_t System::get_total_physical_memory_size()
{
    const long pagesize = sysconf(_SC_PAGESIZE);
    const long numpages = sysconf(_SC_PHYS_PAGES);

    return static_cast<std::uint64_t>(pagesize) * numpages;
}

std::uint64_t System::get_total_virtual_memory_size()
{
    quad_t swap;
    std::size_t len = sizeof(swap);

    const int result = sysctlbyname("vm.swap_total", &swap, &len, 0x0, 0);
    assert(result == 0);

    return get_total_physical_memory_size() + swap;
}

std::uint64_t System::get_process_virtual_memory_size()
{
    // todo: this is wrong, it returns peak RSS instead of virtual memory.

    // curproc->p_stats->p_ru is updated on statclock tick and may be not very
    // granular (if called early in program's life, it can even yield zeros).
    // Reference: https://lists.freebsd.org/pipermail/freebsd-stable/2006-March/023262.html

    struct rusage ru;

    const int result = getrusage(RUSAGE_SELF, &ru);
    assert(result == 0);

    return static_cast<std::uint64_t>(ru.ru_maxrss) * 1024;
}

std::uint64_t System::get_peak_process_virtual_memory_size()
{
    // todo: implement.
    return 0;
}

#endif

// ------------------------------------------------------------------------------------------------
// Common code.
// ------------------------------------------------------------------------------------------------

#ifdef APPLESEED_WITH_GPU

namespace
{
    void print_gpu_information(Logger& logger)
    {
        try
        {
            const CUDADeviceList& dev_list = CUDADeviceList::instance();
            if (dev_list.empty())
            {
                LOG_INFO(logger, "no GPU device found.\n");
                return;
            }

            LOG_INFO(logger, "GPU information:");

            // Devices.
            LOG_INFO(logger, "  device count                  " FMT_SIZE_T, dev_list.size());
            for (std::size_t i = 0, e = dev_list.size(); i < e; ++i)
            {
                const CUDADevice& dev = dev_list.get_device(i);

                LOG_INFO(
                    logger,
                    "    device #" FMT_SIZE_T ":\n"
                    "      name                      %s\n"
                    "      compute capability        %d.%d\n"
                    "      memory                    %s",
                    i,
                    dev.m_name.c_str(),
                    dev.m_compute_capability_major,
                    dev.m_compute_capability_minor,
                    pretty_size(dev.m_total_mem).c_str());
            }

            // Driver version.
            int cuda_version;
            check_cuda_result(cuDriverGetVersion(&cuda_version));
            const int major_cuda_version = cuda_version / 1000;
            const int minor_cuda_version = (cuda_version % 1000) / 10;
            LOG_INFO(logger, "  CUDA version                  %d.%d", major_cuda_version, minor_cuda_version);

            // OptiX version.
            unsigned int optix_version;
            if (rtGetVersion(&optix_version) == RT_SUCCESS)
            {
                const unsigned int major_optix_version = optix_version / 10000;
                const unsigned int minor_optix_version = (optix_version % 10000) / 100;
                const unsigned int micro_optix_version = optix_version % 100;
                LOG_INFO(
                    logger,
                    "  OptiX version                 %u.%u.%u",
                    major_optix_version,
                    minor_optix_version,
                    micro_optix_version);
            }
        }
        catch (const ExceptionCUDAError& e)
        {
            LOG_ERROR(logger, "%s", e.what());
            return;
        }
    }
}

#endif

void System::print_information(Logger& logger)
{
#ifdef APPLESEED_X86
    X86CPUFeatures features;
    detect_x86_cpu_features(features);
#endif

    // Can't use LOG_INFO() here because of the #ifdefs.
    logger.write(
        LogMessage::Info,
        __FILE__,
        __LINE__,
        "system information:\n"
        "  architecture                  %s\n"
#ifdef APPLESEED_X86
        "  vendor                        %s\n"
#endif
        "  logical cores                 %s\n"
        "  L1 data cache                 " FMT_SIZE_T " x %s, line size %s\n"
        "  L2 cache                      " FMT_SIZE_T " x %s, line size %s\n"
        "  L3 cache                      " FMT_SIZE_T " x %s, line size %s\n"
        "  instruction sets              %s\n"
        "  physical memory               %s\n"
        "  virtual memory                %s\n"
        "  default wallclock timer       %s Hz\n"
        "  default processor timer       %s Hz",
        get_cpu_architecture(),
#ifdef APPLESEED_X86
        features.m_vendor == X86CPUFeatures::Vendor::Intel ? "Intel" :
        features.m_vendor == X86CPUFeatures::Vendor::AMD ? "AMD" :
        "unknown",
#endif
        pretty_uint(get_logical_cpu_core_count()).c_str(),
        get_l1_data_cache_count(),
        pretty_size(get_l1_data_cache_size()).c_str(),
        pretty_size(get_l1_data_cache_line_size()).c_str(),
        get_l2_cache_count(),
        pretty_size(get_l2_cache_size()).c_str(),
        pretty_size(get_l2_cache_line_size()).c_str(),
        get_l3_cache_count(),
        pretty_size(get_l3_cache_size()).c_str(),
        pretty_size(get_l3_cache_line_size()).c_str(),
        get_cpu_features_string().c_str(),
        pretty_size(get_total_physical_memory_size()).c_str(),
        pretty_size(get_total_virtual_memory_size()).c_str(),
        pretty_uint(DefaultWallclockTimer().frequency()).c_str(),
        pretty_uint(DefaultProcessorTimer().frequency()).c_str());

#ifdef APPLESEED_WITH_GPU
    print_gpu_information(logger);
#endif
}

const char* System::get_cpu_architecture()
{
#ifdef APPLESEED_ARCH32
    #ifdef APPLESEED_X86
        return "x86 32-bit";
    #else
        return "unknown 32-bit";
    #endif
#else
    #ifdef APPLESEED_X86
        return "x86 64-bit";
    #else
        return "unknown 64-bit";
    #endif
#endif
}

std::size_t System::get_logical_cpu_core_count()
{
    const std::size_t concurrency =
        static_cast<std::size_t>(boost::thread::hardware_concurrency());

    return concurrency > 1 ? concurrency : 1;
}

#ifdef APPLESEED_X86

// This symbol is not defined by gcc (and potentially other compilers).
#ifndef _XCR_XFEATURE_ENABLED_MASK
#define _XCR_XFEATURE_ENABLED_MASK 0
#endif

namespace
{
    bool detect_os_avx()
    {
        // Reference: http://stackoverflow.com/a/22521619/922184

        std::uint32_t cpuinfo[4];
        cpuid(cpuinfo, 1);

        const bool os_uses_xsave_xrstor = (cpuinfo[2] & (1UL << 27)) != 0;
        const bool cpu_avx_support = (cpuinfo[2] & (1UL << 28)) != 0;

        if (os_uses_xsave_xrstor && cpu_avx_support)
        {
            const std::uint64_t xcr_feature_mask = xgetbv(_XCR_XFEATURE_ENABLED_MASK);
            return (xcr_feature_mask & 0x6) == 0x6;
        }

        return false;
    }

    bool detect_os_avx512()
    {
        if (!detect_os_avx())
            return false;

        const std::uint64_t xcr_feature_mask = xgetbv(_XCR_XFEATURE_ENABLED_MASK);
        return (xcr_feature_mask & 0xe6) == 0xe6;
    }

    std::string get_vendor_string()
    {
        char vendor[13];

        std::uint32_t cpuinfo[4];
        cpuid(cpuinfo, 0);

        memcpy(vendor + 0, &cpuinfo[1], 4);
        memcpy(vendor + 4, &cpuinfo[3], 4);
        memcpy(vendor + 8, &cpuinfo[2], 4);
        vendor[12] = '\0';

        return vendor;
    }
}

void System::detect_x86_cpu_features(X86CPUFeatures& features)
{
    //
    // Based on the excellent FeatureDetector project by Alexander J. Yee:
    // https://github.com/Mysticial/FeatureDetector
    //

    memset(&features, 0, sizeof(features));

    // CPU vendor.
    const std::string vendor = get_vendor_string();
    features.m_vendor =
        vendor == "GenuineIntel" ? X86CPUFeatures::Vendor::Intel :
        vendor == "AuthenticAMD" ? X86CPUFeatures::Vendor::AMD :
        X86CPUFeatures::Vendor::Unknown;

    // EAX=0: Get vendor ID.
    std::uint32_t cpuinfo[4];
    cpuid(cpuinfo, 0x00000000);
    const std::uint32_t highest_function_id = cpuinfo[0];

    if (highest_function_id >= 0x00000001)
    {
        // EAX=1: Processor Info and Feature Bits.
        cpuid(cpuinfo, 0x00000001);
        features.m_hw_mmx           = (cpuinfo[3] & (1UL << 23)) != 0;
        features.m_hw_sse           = (cpuinfo[3] & (1UL << 25)) != 0;
        features.m_hw_sse2          = (cpuinfo[3] & (1UL << 26)) != 0;
        features.m_hw_sse3          = (cpuinfo[2] & (1UL <<  0)) != 0;
        features.m_hw_ssse3         = (cpuinfo[2] & (1UL <<  9)) != 0;
        features.m_hw_sse41         = (cpuinfo[2] & (1UL << 19)) != 0;
        features.m_hw_sse42         = (cpuinfo[2] & (1UL << 20)) != 0;
        features.m_hw_aes           = (cpuinfo[2] & (1UL << 25)) != 0;
        features.m_hw_avx           = (cpuinfo[2] & (1UL << 28)) != 0;
        features.m_hw_fma3          = (cpuinfo[2] & (1UL << 12)) != 0;
        features.m_hw_rdrand        = (cpuinfo[2] & (1UL << 30)) != 0;
        features.m_hw_f16c          = (cpuinfo[2] & (1UL << 29)) != 0;
    }

    if (highest_function_id >= 0x00000007)
    {
        // EAX=7: Extended Features.
        cpuid(cpuinfo, 0x00000007);
        features.m_hw_avx2          = (cpuinfo[1] & (1UL <<  5)) != 0;
        features.m_hw_bmi1          = (cpuinfo[1] & (1UL <<  3)) != 0;
        features.m_hw_bmi2          = (cpuinfo[1] & (1UL <<  8)) != 0;
        features.m_hw_adx           = (cpuinfo[1] & (1UL << 19)) != 0;
        features.m_hw_mpx           = (cpuinfo[1] & (1UL << 14)) != 0;
        features.m_hw_sha           = (cpuinfo[1] & (1UL << 29)) != 0;
        features.m_hw_prefetchwt1   = (cpuinfo[2] & (1UL <<  0)) != 0;
        features.m_hw_avx512_f      = (cpuinfo[1] & (1UL << 16)) != 0;
        features.m_hw_avx512_cd     = (cpuinfo[1] & (1UL << 28)) != 0;
        features.m_hw_avx512_pf     = (cpuinfo[1] & (1UL << 26)) != 0;
        features.m_hw_avx512_er     = (cpuinfo[1] & (1UL << 27)) != 0;
        features.m_hw_avx512_vl     = (cpuinfo[1] & (1UL << 31)) != 0;
        features.m_hw_avx512_bw     = (cpuinfo[1] & (1UL << 30)) != 0;
        features.m_hw_avx512_dq     = (cpuinfo[1] & (1UL << 17)) != 0;
        features.m_hw_avx512_ifma   = (cpuinfo[1] & (1UL << 21)) != 0;
        features.m_hw_avx512_vbmi   = (cpuinfo[2] & (1UL <<  1)) != 0;
    }

    // EAX=0x80000000: Get Highest Extended Function Supported.
    cpuid(cpuinfo, 0x80000000);
    const std::uint32_t highest_ext_function_id = cpuinfo[0];

    if (highest_ext_function_id >= 0x80000001)
    {
        // EAX=0x80000001: Extended Processor Info and Feature Bits.
        cpuid(cpuinfo, 0x80000001);
        features.m_hw_x64           = (cpuinfo[3] & (1UL << 29)) != 0;
        features.m_hw_abm           = (cpuinfo[2] & (1UL <<  5)) != 0;
        features.m_hw_sse4a         = (cpuinfo[2] & (1UL <<  6)) != 0;
        features.m_hw_fma4          = (cpuinfo[2] & (1UL << 16)) != 0;
        features.m_hw_xop           = (cpuinfo[2] & (1UL << 11)) != 0;
    }

    // OS support for AVX and AVX-512 instruction sets.
    features.m_os_avx = detect_os_avx();
    features.m_os_avx512 = detect_os_avx512();
}

#endif

APIString System::get_cpu_features_string()
{
#ifdef APPLESEED_X86
    X86CPUFeatures features;
    detect_x86_cpu_features(features);

    std::stringstream isabuilder;
    if (features.m_hw_sse) isabuilder << "SSE ";
    if (features.m_hw_sse2) isabuilder << "SSE2 ";
    if (features.m_hw_sse3) isabuilder << "SSE3 ";
    if (features.m_hw_ssse3) isabuilder << "SSSE3 ";
    if (features.m_hw_sse41) isabuilder << "SSE4.1 ";
    if (features.m_hw_sse42) isabuilder << "SSE4.2 ";
    if (features.m_hw_sse4a) isabuilder << "SSE4a ";
    if (features.m_hw_avx) isabuilder << "AVX ";
    if (features.m_hw_avx2) isabuilder << "AVX2 ";
    if (features.m_hw_fma3) isabuilder << "FMA3 ";
    if (features.m_hw_f16c) isabuilder << "F16C ";

    return
        isabuilder.str().empty()
            ? "base instruction set"
            : trim_right(isabuilder.str());
#else
    return "base instruction set";
#endif
}

}   // namespace foundation
