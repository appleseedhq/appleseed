
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
// allocations. OS X and Linux do.
//
// Moreover, on these platforms (especially on OS X) overriding the new and
// delete operators in a shared library such as appleseed creates all kinds
// of problems, as discussed in this thread on Stack Overflow:
//
// http://stackoverflow.com/questions/13793325/hiding-symbols-in-a-shared-library-on-mac-os-x
//
// Unfortunately, there is a bug in Visual Studio 2012's debug runtime on x64
// that will result in a crash when the application exits:
//
// http://connect.microsoft.com/VisualStudio/feedback/details/750951/std-locale-implementation-in-crt-assumes-all-facets-to-be-allocated-on-crt-heap-and-crashes-in-destructor-in-debug-mode-if-a-facet-was-allocated-by-a-custom-allocator
//

#ifdef _WIN32

// appleseed.foundation headers.
#include "foundation/platform/types.h"
#include "foundation/platform/win32stackwalker.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/memory.h"
#include "foundation/utility/string.h"

// Boost headers.
#include "boost/thread/recursive_mutex.hpp"

// Standard headers.
#include <algorithm>
#include <cassert>
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

#if _MSC_VER >= 1500
#include <sal.h>
#endif

using namespace boost;
using namespace foundation;
using namespace std;


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

namespace
{
    // Name of the log file. It will be created in the current working directory.
    const char* MemoryLogFileName = "memory.log";

    // Shave off that many levels from the top of the stack when dumping it to the log file.
    const size_t NumberOfCallStackLevelsToSkip = 3;

    // Minimum alignment boundary of all memory allocations, in bytes.
    const size_t MemoryAlignment = 16;
}


//
// Memory tracker implementation.
//

#ifdef ENABLE_MEMORY_TRACKING

namespace
{
    // This mutex is used to serialize access to all static members of this file.
    recursive_mutex s_mutex;

    // Indicate whether memory tracking is currently enabled.
    bool s_tracking_enabled = false;

    // Statistics.
    uint64 s_allocation_count = 0;          // total number of memory allocations
    uint64 s_allocated_bytes = 0;           // number of bytes currently allocated
    uint64 s_peak_allocated_bytes = 0;      // peak number of bytes ever allocated
    uint64 s_total_allocated_bytes = 0;     // total number of bytes ever allocated
    uint64 s_largest_allocation_bytes = 0;  // size in bytes of the largest allocation

    // File to which memory operations and leaks are logged.
    FILE* s_log_file = 0;

    typedef pair<const void*, size_t> MemoryBlock;
    typedef vector<MemoryBlock> MemoryBlockVector;
    typedef map<const void*, size_t> MemoryBlockMap;

    // Memory blocks currently allocated.
    MemoryBlockMap s_allocated_mem_blocks;

#ifdef DUMP_CALLSTACK_ON_ALLOCATION

    class StackWalkerOutputToFile
      : public StackWalker
    {
      public:
        StackWalkerOutputToFile()
          : m_file(0)
          , m_level(0)
          , m_skip(false)
        {
        }

        void reset(FILE* file)
        {
            m_file = file;
            m_level = 0;
            m_skip = false;
        }

      private:
        FILE*   m_file;
        size_t  m_level;
        bool    m_skip;

        virtual void OnOutput(LPCSTR szText)
        {
            ++m_level;

            if (m_level <= NumberOfCallStackLevelsToSkip)
                return;

            if (m_skip)
                return;

            if (m_file == 0)
                StackWalker::OnOutput(szText);
            else fprintf(m_file, "        %s", szText);

            static const char* MainFunctionSuffix = ": main\n";
            const size_t main_function_suffix_length = strlen(MainFunctionSuffix);
            const size_t text_length = strlen(szText);

            // Don't report stack frames below main().
            if (text_length >= main_function_suffix_length &&
                strcmp(
                    szText + text_length - main_function_suffix_length,
                    MainFunctionSuffix) == 0)
            {
                m_skip = true;
                return;
            }
        }
    };

    StackWalkerOutputToFile s_stack_walker;

#endif

    string get_timestamp_string()
    {
        time_t t;
        time(&t);

        const tm* local_time = localtime(&t);

        stringstream sstr;
        sstr << setfill('0');

        sstr << setw(2) << local_time->tm_hour << ':';
        sstr << setw(2) << local_time->tm_min << ':';
        sstr << setw(2) << local_time->tm_sec;

        return sstr.str();
    }

    void log_allocation_to_file(const void* ptr, const size_t size)
    {
        assert(!s_tracking_enabled);

        fprintf(
            s_log_file,
            "[%s] Allocated %s at %s\n",
            get_timestamp_string().c_str(),
            pretty_size(size).c_str(),
            to_string(ptr).c_str());

#ifdef DUMP_CALLSTACK_ON_ALLOCATION
        fprintf(s_log_file, "    Call stack:\n");

        s_stack_walker.reset(s_log_file);
        s_stack_walker.ShowCallstack();

        fprintf(s_log_file, "\n");
#endif
    }

    void log_allocation_failure_to_file(const size_t size)
    {
        assert(!s_tracking_enabled);

        fprintf(
            s_log_file,
            "[%s] FAILED to allocate %s\n",
            get_timestamp_string().c_str(),
            pretty_size(size).c_str());
    }

    void log_deallocation_to_file(const void* ptr, const size_t size)
    {
        assert(!s_tracking_enabled);

        fprintf(
            s_log_file,
            "[%s] Deallocated %s at %s\n\n",
            get_timestamp_string().c_str(),
            pretty_size(size).c_str(),
            to_string(ptr).c_str());
    }

    uint64 compute_leaked_memory_size()
    {
        uint64 total_size = 0;

        for (const_each<MemoryBlockMap> i = s_allocated_mem_blocks; i; ++i)
            total_size += static_cast<uint64>(i->second);

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
        assert(!s_tracking_enabled);

        fprintf(
            s_log_file,
            "\n"
            "Statistics:\n"
            "\n"
            "    Allocations:            %s\n"
            "    Total allocated:        %s (%s byte%s)\n"
            "    Peak allocated:         %s (%s byte%s)\n"
            "    Largest allocation:     %s (%s byte%s)\n"
            "\n",
            pretty_uint(s_allocation_count).c_str(),
            pretty_size(s_total_allocated_bytes).c_str(),
            pretty_uint(s_total_allocated_bytes).c_str(),
            s_total_allocated_bytes > 1 ? "s" : "",
            pretty_size(s_peak_allocated_bytes).c_str(),
            pretty_uint(s_peak_allocated_bytes).c_str(),
            s_peak_allocated_bytes > 1 ? "s" : "",
            pretty_size(s_largest_allocation_bytes).c_str(),
            pretty_uint(s_largest_allocation_bytes).c_str(),
            s_largest_allocation_bytes > 1 ? "s" : "");
    }

    void report_memory_leaks()
    {
        assert(!s_tracking_enabled);

        if (s_allocated_mem_blocks.empty())
        {
            fprintf(s_log_file, "No memory leak detected.\n");
        }
        else
        {
            const size_t leak_count = s_allocated_mem_blocks.size();
            const uint64 leak_bytes = compute_leaked_memory_size();

            fprintf(
                s_log_file,
                "Detected %s potential memory leak%s (%s in total):\n\n",
                pretty_uint(leak_count).c_str(),
                leak_count > 1 ? "s" : "",
                pretty_size(leak_bytes).c_str());

            MemoryBlockVector sorted_blocks;
            sorted_blocks.reserve(s_allocated_mem_blocks.size());

            for (const_each<MemoryBlockMap> i = s_allocated_mem_blocks; i; ++i)
                sorted_blocks.push_back(*i);

            sort(sorted_blocks.begin(), sorted_blocks.end(), SortByDecreasingSize());

            for (const_each<MemoryBlockVector> i = sorted_blocks; i; ++i)
            {
                fprintf(
                    s_log_file,
                    "    %s at %s\n",
                    pretty_size(i->second).c_str(),
                    to_string(i->first).c_str());
            }
        }
    }
}


//
// Public entry points.
//

void log_allocation(const void* ptr, const size_t size)
{
    if (!s_tracking_enabled)
        return;

    recursive_mutex::scoped_lock lock(s_mutex);

    s_tracking_enabled = false;

    ++s_allocation_count;
    s_allocated_bytes += size;
    s_peak_allocated_bytes = max(s_peak_allocated_bytes, s_allocated_bytes);
    s_total_allocated_bytes += size;
    s_largest_allocation_bytes = max<uint64>(s_largest_allocation_bytes, size);

    s_allocated_mem_blocks[ptr] = size;

#ifdef LOG_MEMORY_ALLOCATIONS
    log_allocation_to_file(ptr, size);
#endif

    s_tracking_enabled = true;
}

void log_allocation_failure(const size_t size)
{
#ifdef LOG_MEMORY_ALLOCATION_FAILURES
    if (!s_tracking_enabled)
        return;

    recursive_mutex::scoped_lock lock(s_mutex);

    s_tracking_enabled = false;

    log_allocation_failure_to_file(size);

    s_tracking_enabled = true;
#endif
}

void log_deallocation(const void* ptr)
{
    if (!s_tracking_enabled)
        return;

    recursive_mutex::scoped_lock lock(s_mutex);

    s_tracking_enabled = false;

    const MemoryBlockMap::iterator i = s_allocated_mem_blocks.find(ptr);

    if (i != s_allocated_mem_blocks.end())
    {
        assert(s_allocated_bytes >= i->second);
        s_allocated_bytes -= i->second;

#ifdef LOG_MEMORY_DEALLOCATIONS
        log_deallocation_to_file(ptr, i->second);
#endif

        s_allocated_mem_blocks.erase(i);
    }

    s_tracking_enabled = true;
}

void start_memory_tracking()
{
    recursive_mutex::scoped_lock lock(s_mutex);

    if (s_tracking_enabled)
        return;

    s_tracking_enabled = false;

    atexit(stop_memory_tracking);

    s_log_file = fopen(MemoryLogFileName, "wt");

    if (s_log_file)
    {
#ifdef DUMP_CALLSTACK_ON_ALLOCATION
        s_stack_walker.LoadModules();
#endif

        s_tracking_enabled = true;
    }
}

void stop_memory_tracking()
{
    recursive_mutex::scoped_lock lock(s_mutex);

    if (!s_tracking_enabled)
        return;

    s_tracking_enabled = false;

    if (s_log_file)
    {
        report_allocation_statistics();
        report_memory_leaks();

        fclose(s_log_file);
    }
}

#else

void log_allocation(const void* ptr, const size_t size) {}
void log_allocation_failure(const size_t size) {}
void log_deallocation(const void* ptr) {}
void start_memory_tracking() {}
void stop_memory_tracking() {}

#endif


//
// Override all variants of the new and delete operators.
//

namespace
{
    void* new_impl(size_t size)
    {
        if (size < 1)
            size = 1;

        void* ptr = aligned_malloc(size, MemoryAlignment);

        if (!ptr)
            throw bad_alloc();

        return ptr;
    }

    void delete_impl(void* ptr)
    {
        if (!ptr)
            return;

        aligned_free(ptr);
    }
}

#if _MSC_VER >= 1500
_Ret_notnull_ _Post_writable_byte_size_(size)
#endif
void* operator new(size_t size)
  throw(bad_alloc)
{
    return new_impl(size);
}

#if _MSC_VER >= 1500
_Ret_notnull_ _Post_writable_byte_size_(size)
#endif
void* operator new[](size_t size)
  throw(bad_alloc)
{
    return new_impl(size);
}

#if _MSC_VER >= 1500
_Ret_maybenull_ _Post_writable_byte_size_(size)
#endif
void* operator new(size_t size, const nothrow_t&)
  throw()
{
    return new_impl(size);
}

#if _MSC_VER >= 1500
_Ret_maybenull_ _Post_writable_byte_size_(size)
#endif
void* operator new[](size_t size, const nothrow_t&)
  throw()
{
    return new_impl(size);
}

void operator delete(void* ptr)
  throw()
{
    delete_impl(ptr);
}

void operator delete[](void* ptr)
  throw()
{
    delete_impl(ptr);
}

void operator delete(void* ptr, const nothrow_t&)
  throw()
{
    delete_impl(ptr);
}

void operator delete[](void* ptr, const nothrow_t&)
  throw()
{
    delete_impl(ptr);
}

#else

void log_allocation(const void* ptr, const size_t size) {}
void log_allocation_failure(const size_t size) {}
void log_deallocation(const void* ptr) {}
void start_memory_tracking() {}
void stop_memory_tracking() {}

#endif  // _WIN32
