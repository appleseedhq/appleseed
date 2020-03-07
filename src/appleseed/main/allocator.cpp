
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2020 Francois Beaune, The appleseedhq Organization
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
#include "allocator.h"

//
// This module must be enabled on Windows, and on Windows only. Windows is
// the only platform we support that doesn't natively provide 16-byte aligned
// allocations. macOS and Linux do.
//
// Moreover, on these platforms (especially on macOS) overriding the new and
// delete operators in a shared library such as appleseed creates all kinds
// of problems, as discussed in this thread on Stack Overflow:
//
// http://stackoverflow.com/questions/13793325/hiding-symbols-in-a-shared-library-on-mac-os-x
//

#ifdef _WIN32

// appleseed.foundation headers.
#include "foundation/memory/memory.h"
#include "foundation/platform/win32stackwalker.h"
#include "foundation/string/string.h"
#include "foundation/utility/foreach.h"

// Boost headers.
#include "boost/thread/recursive_mutex.hpp"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <ctime>
#include <exception>
#include <iomanip>
#include <map>
#include <new>
#include <sstream>
#include <string>
#include <utility>
#include <vector>

// Platform headers.
#include <sal.h>

using namespace boost;
using namespace foundation;


//
// Configuration of the memory allocation subsystem.
//

// This is the global enable/disable switch that conditions everything else.
#undef ENABLE_MEMORY_TRACKING

// Define this symbol to enable report of memory allocations.
#define LOG_MEMORY_ALLOCATIONS

// Define this symbol to enable report of memory allocation failures.
#define LOG_MEMORY_ALLOCATION_FAILURES

// Define this symbol to enable report of memory deallocations.
#undef LOG_MEMORY_DEALLOCATIONS

// Define this symbol to dump a (part of) the callstack when a block of memory is allocated.
#define DUMP_CALLSTACK_ON_ALLOCATION

// Define this symbol to force a call to std::fflush() after every write to the log file.
#define FLUSH_AFTER_WRITE

namespace
{
    // Name of the log file. It will be created in the current working directory.
    const char* MemoryLogFileName = "memory.log";

    // Shave off that many levels from the top of the stack when dumping it to the log file.
    const std::size_t NumberOfCallStackLevelsToSkip = 3;

    // Minimum alignment boundary of all memory allocations, in bytes.
    const std::size_t MemoryAlignment = 16;
}


//
// Memory tracker implementation.
//

#ifdef ENABLE_MEMORY_TRACKING

namespace
{
    // This mutex is used to serialize access to all static members of this file.
    recursive_mutex g_mutex;

    // Indicate whether memory tracking is currently enabled.
    bool g_tracking_enabled = false;

    // Statistics.
    std::uint64_t g_allocation_count = 0;           // total number of memory allocations
    std::uint64_t g_allocated_bytes = 0;            // number of bytes currently allocated
    std::uint64_t g_peak_allocated_bytes = 0;       // peak number of bytes ever allocated
    std::uint64_t g_total_allocated_bytes = 0;      // total number of bytes ever allocated
    std::uint64_t g_largest_allocation_bytes = 0;   // size in bytes of the largest allocation

    // File to which memory operations and leaks are logged.
    std::FILE* g_log_file = 0;

    typedef std::pair<const void*, std::size_t> MemoryBlock;
    typedef std::vector<MemoryBlock> MemoryBlockVector;
    typedef std::map<const void*, std::size_t> MemoryBlockMap;

    // Memory blocks currently allocated.
    MemoryBlockMap g_allocated_mem_blocks;

#ifdef DUMP_CALLSTACK_ON_ALLOCATION

    class StackWalkerOutputToFile
      : public StackWalker
    {
      public:
        StackWalkerOutputToFile()
          : m_file(nullptr)
          , m_level(0)
          , m_skip(false)
        {
        }

        void reset(std::FILE* file)
        {
            m_file = file;
            m_level = 0;
            m_skip = false;
        }

      private:
        std::FILE*  m_file;
        std::size_t m_level;
        bool        m_skip;

        void OnOutput(LPCSTR szText) override
        {
            ++m_level;

            if (m_level <= NumberOfCallStackLevelsToSkip)
                return;

            if (m_skip)
                return;

            if (m_file == nullptr)
                StackWalker::OnOutput(szText);
            else std::fprintf(m_file, "    %s", szText);

            static const char* MainFunctionSuffix = ": main\n";
            const std::size_t main_function_suffix_length = strlen(MainFunctionSuffix);
            const std::size_t text_length = strlen(szText);

            // Don't report stack frames below main().
            if (text_length >= main_function_suffix_length &&
                strcmp(szText + text_length - main_function_suffix_length, MainFunctionSuffix) == 0)
            {
                m_skip = true;
                return;
            }
        }

        void OnDbgHelpErr(LPCSTR szFuncName, DWORD gle, DWORD64 addr) override
        {
            // Eat all error messages.
        }
    };

    StackWalkerOutputToFile g_stack_walker;

#endif

    std::string get_timestamp_string()
    {
        std::time_t t;
        std::time(&t);

        const std::tm* local_time = localtime(&t);

        std::stringstream sstr;
        sstr << std::setfill('0');

        sstr << std::setw(2) << local_time->tm_hour << ':';
        sstr << std::setw(2) << local_time->tm_min << ':';
        sstr << std::setw(2) << local_time->tm_sec;

        return sstr.str();
    }

    void log_allocation_to_file(const void* unaligned_ptr, const void* aligned_ptr, const std::size_t size)
    {
        assert(!g_tracking_enabled);

        std::fprintf(
            g_log_file,
            "[%s] Allocated %s at %s (unaligned: %s)\n",
            get_timestamp_string().c_str(),
            pretty_size(size).c_str(),
            to_string(aligned_ptr).c_str(),
            to_string(unaligned_ptr).c_str());

#ifdef DUMP_CALLSTACK_ON_ALLOCATION
        std::fprintf(g_log_file, "  Call stack:\n");

        g_stack_walker.reset(g_log_file);
        g_stack_walker.ShowCallstack();

        std::fprintf(g_log_file, "\n");
#endif

#ifdef FLUSH_AFTER_WRITE
        std::fflush(g_log_file);
#endif
    }

    void log_allocation_failure_to_file(const std::size_t size)
    {
        assert(!g_tracking_enabled);

        std::fprintf(
            g_log_file,
            "[%s] FAILED to allocate %s\n",
            get_timestamp_string().c_str(),
            pretty_size(size).c_str());

#ifdef FLUSH_AFTER_WRITE
        std::fflush(g_log_file);
#endif
    }

    void log_deallocation_to_file(const void* unaligned_ptr, const void* aligned_ptr, const std::size_t size)
    {
        assert(!g_tracking_enabled);

        std::fprintf(
            g_log_file,
            "[%s] Deallocating %s at %s (unaligned: %s)...\n",
            get_timestamp_string().c_str(),
            pretty_size(size).c_str(),
            to_string(aligned_ptr).c_str(),
            to_string(unaligned_ptr).c_str());

#ifdef DUMP_CALLSTACK_ON_ALLOCATION
        std::fprintf(g_log_file, "\n");
#endif

#ifdef FLUSH_AFTER_WRITE
        std::fflush(g_log_file);
#endif
    }

    std::uint64_t compute_leaked_memory_size()
    {
        std::uint64_t total_size = 0;

        for (const_each<MemoryBlockMap> i = g_allocated_mem_blocks; i; ++i)
            total_size += static_cast<std::uint64_t>(i->second);

        return total_size;
    }

    struct SortByDecreasingSize
    {
        bool operator()(const MemoryBlock& lhs, const MemoryBlock& rhs) const
        {
            return lhs.second > rhs.second;
        }
    };

    void report_allocation_statistics()
    {
        assert(!g_tracking_enabled);

        std::fprintf(
            g_log_file,
            "\n"
            "Statistics:\n"
            "\n"
            "  Allocations:            %s\n"
            "  Total allocated:        %s (%s byte%s)\n"
            "  Peak allocated:         %s (%s byte%s)\n"
            "  Largest allocation:     %s (%s byte%s)\n"
            "\n",
            pretty_uint(g_allocation_count).c_str(),
            pretty_size(g_total_allocated_bytes).c_str(),
            pretty_uint(g_total_allocated_bytes).c_str(),
            g_total_allocated_bytes > 1 ? "s" : "",
            pretty_size(g_peak_allocated_bytes).c_str(),
            pretty_uint(g_peak_allocated_bytes).c_str(),
            g_peak_allocated_bytes > 1 ? "s" : "",
            pretty_size(g_largest_allocation_bytes).c_str(),
            pretty_uint(g_largest_allocation_bytes).c_str(),
            g_largest_allocation_bytes > 1 ? "s" : "");
    }

    void report_memory_leaks()
    {
        assert(!g_tracking_enabled);

        if (g_allocated_mem_blocks.empty())
        {
            std::fprintf(g_log_file, "No memory leak detected.\n");
        }
        else
        {
            const std::size_t leak_count = g_allocated_mem_blocks.size();
            const std::uint64_t leak_bytes = compute_leaked_memory_size();

            std::fprintf(
                g_log_file,
                "Detected %s potential memory leak%s (%s in total):\n\n",
                pretty_uint(leak_count).c_str(),
                leak_count > 1 ? "s" : "",
                pretty_size(leak_bytes).c_str());

            MemoryBlockVector sorted_blocks;
            sorted_blocks.reserve(g_allocated_mem_blocks.size());

            for (const_each<MemoryBlockMap> i = g_allocated_mem_blocks; i; ++i)
                sorted_blocks.push_back(*i);

            std::sort(sorted_blocks.begin(), sorted_blocks.end(), SortByDecreasingSize());

            for (const_each<MemoryBlockVector> i = sorted_blocks; i; ++i)
            {
                std::fprintf(
                    g_log_file,
                    "  %s at %s\n",
                    pretty_size(i->second).c_str(),
                    to_string(i->first).c_str());
            }
        }
    }
}


//
// Public entry points.
//

void log_allocation(const void* unaligned_ptr, const void* aligned_ptr, const std::size_t size)
{
    if (!g_tracking_enabled)
        return;

    recursive_mutex::scoped_lock lock(g_mutex);

    g_tracking_enabled = false;

    ++g_allocation_count;
    g_allocated_bytes += size;
    g_peak_allocated_bytes = std::max(g_peak_allocated_bytes, g_allocated_bytes);
    g_total_allocated_bytes += size;
    g_largest_allocation_bytes = std::max<std::uint64_t>(g_largest_allocation_bytes, size);

    g_allocated_mem_blocks[aligned_ptr] = size;

#ifdef LOG_MEMORY_ALLOCATIONS
    log_allocation_to_file(unaligned_ptr, aligned_ptr, size);
#endif

    g_tracking_enabled = true;
}

void log_allocation_failure(const std::size_t size)
{
#ifdef LOG_MEMORY_ALLOCATION_FAILURES
    if (!g_tracking_enabled)
        return;

    recursive_mutex::scoped_lock lock(g_mutex);

    g_tracking_enabled = false;

    log_allocation_failure_to_file(size);

    g_tracking_enabled = true;
#endif
}

void log_deallocation(const void* unaligned_ptr, const void* aligned_ptr)
{
    if (!g_tracking_enabled)
        return;

    recursive_mutex::scoped_lock lock(g_mutex);

    g_tracking_enabled = false;

    const MemoryBlockMap::iterator i = g_allocated_mem_blocks.find(aligned_ptr);

    if (i != g_allocated_mem_blocks.end())
    {
        assert(g_allocated_bytes >= i->second);
        g_allocated_bytes -= i->second;

#ifdef LOG_MEMORY_DEALLOCATIONS
        log_deallocation_to_file(unaligned_ptr, aligned_ptr, i->second);
#endif

        g_allocated_mem_blocks.erase(i);
    }

    g_tracking_enabled = true;
}

void start_memory_tracking()
{
    recursive_mutex::scoped_lock lock(g_mutex);

    if (g_tracking_enabled)
        return;

    g_tracking_enabled = false;

    atexit(stop_memory_tracking);

    g_log_file = fopen(MemoryLogFileName, "wt");

    if (g_log_file)
    {
#ifdef DUMP_CALLSTACK_ON_ALLOCATION
        g_stack_walker.LoadModules();
#endif

        g_tracking_enabled = true;
    }
}

void stop_memory_tracking()
{
    recursive_mutex::scoped_lock lock(g_mutex);

    if (!g_tracking_enabled)
        return;

    g_tracking_enabled = false;

    if (g_log_file)
    {
        report_allocation_statistics();
        report_memory_leaks();

        fclose(g_log_file);
    }
}

#else

void log_allocation(const void* unaligned_ptr, const void* aligned_ptr, const std::size_t size) {}
void log_allocation_failure(const std::size_t size) {}
void log_deallocation(const void* unaligned_ptr, const void* aligned_ptr) {}
void start_memory_tracking() {}
void stop_memory_tracking() {}

#endif


//
// Override all variants of the new and delete operators.
//

namespace
{
    void* new_impl(std::size_t size)
    {
        if (size < 1)
            size = 1;

        void* ptr = aligned_malloc(size, MemoryAlignment);

        if (!ptr)
            throw std::bad_alloc();

        return ptr;
    }

    void delete_impl(void* ptr)
    {
        if (!ptr)
            return;

        aligned_free(ptr);
    }
}

_Ret_notnull_ _Post_writable_byte_size_(size)
void* operator new(std::size_t size)
  throw(std::bad_alloc)
{
    return new_impl(size);
}

_Ret_notnull_ _Post_writable_byte_size_(size)
void* operator new[](std::size_t size)
  throw(std::bad_alloc)
{
    return new_impl(size);
}

_Ret_maybenull_ _Post_writable_byte_size_(size)
void* operator new(std::size_t size, const std::nothrow_t&)
  throw()
{
    return new_impl(size);
}

_Ret_maybenull_ _Post_writable_byte_size_(size)
void* operator new[](std::size_t size, const std::nothrow_t&)
  throw()
{
    return new_impl(size);
}

void operator delete(void* ptr)
{
    delete_impl(ptr);
}

void operator delete[](void* ptr)
{
    delete_impl(ptr);
}

void operator delete(void* ptr, const std::nothrow_t&)
{
    delete_impl(ptr);
}

void operator delete[](void* ptr, const std::nothrow_t&)
{
    delete_impl(ptr);
}

#else

void log_allocation(const void* unaligned_ptr, const void* aligned_ptr, const std::size_t size) {}
void log_allocation_failure(const std::size_t size) {}
void log_deallocation(const void* unaligned_ptr, const void* aligned_ptr) {}
void start_memory_tracking() {}
void stop_memory_tracking() {}

#endif  // _WIN32
