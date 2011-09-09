
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

// Interface header.
#include "allocator.h"

// appleseed.foundation headers.
#include "foundation/platform/win32stackwalker.h"
#include "foundation/platform/types.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/memory.h"
#include "foundation/utility/string.h"

// boost headers.
#include "boost/thread/recursive_mutex.hpp"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstddef>
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

using namespace boost;
using namespace foundation;
using namespace std;


//
// Track memory allocations and deallocations and report memory leaks.
//

// Define this symbol to enable memory tracking.
#undef ENABLE_MEMORY_TRACKING

// Define this symbol to enable report of memory allocations.
#define REPORT_MEMORY_ALLOCATIONS

// Define this symbol to enable report of memory deallocations.
#undef REPORT_MEMORY_DEALLOCATIONS

#ifdef ENABLE_MEMORY_TRACKING

namespace
{
    const char* MemoryLogFileName = "memory.log";

    // Shave off that many levels from the top of the stack when dumping it to the log file.
    const size_t NumberOfCallStackLevelsToSkip = 3;

    // This mutex is used to serialize access to all static members of this file.
    recursive_mutex s_mutex;

    // Indicate whether memory tracking is currently enabled.
    bool s_tracking_enabled = false;

    // Total number of memory allocations.
    uint64 s_allocation_count = 0;

    // Total number of bytes allocated.
    uint64 s_total_allocated_bytes = 0;

    // File to which memory operations and leaks are logged.
    FILE* s_log_file = 0;

    typedef pair<const void*, size_t> MemoryBlock;
    typedef vector<MemoryBlock> MemoryBlockVector;
    typedef map<const void*, size_t> MemoryBlockMap;

    // Memory blocks currently allocated.
    MemoryBlockMap s_allocated_mem_blocks;

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

    string get_current_date_time_string()
    {
        time_t t;
        time(&t);

        const tm* local_time = localtime(&t);

        stringstream sstr;
        sstr << setfill('0');

        sstr << setw(4) << local_time->tm_year + 1900 << '-';
        sstr << setw(2) << local_time->tm_mon + 1 << '-';
        sstr << setw(2) << local_time->tm_mday << ' ';
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
            get_current_date_time_string().c_str(),
            pretty_size(size).c_str(),
            to_string(ptr).c_str());

        fprintf(s_log_file, "    Call stack:\n");

        s_stack_walker.reset(s_log_file);
        s_stack_walker.ShowCallstack();

        fprintf(s_log_file, "\n");
    }

    void log_allocation(const void* ptr, const size_t size)
    {
        recursive_mutex::scoped_lock lock(s_mutex);

        if (!s_tracking_enabled)
            return;

        s_tracking_enabled = false;

        ++s_allocation_count;
        s_total_allocated_bytes += static_cast<uint64>(size);

        s_allocated_mem_blocks[ptr] = size;

#ifdef REPORT_MEMORY_ALLOCATIONS
        log_allocation_to_file(ptr, size);
#endif

        s_tracking_enabled = true;
    }

    void log_deallocation_to_file(const void* ptr, const size_t size)
    {
        assert(!s_tracking_enabled);

        fprintf(
            s_log_file,
            "[%s] Deallocated %s at %s\n\n",
            get_current_date_time_string().c_str(),
            pretty_size(size).c_str(),
            to_string(ptr).c_str());
    }

    void log_deallocation(const void* ptr)
    {
        recursive_mutex::scoped_lock lock(s_mutex);

        if (!s_tracking_enabled)
            return;

        s_tracking_enabled = false;

        const MemoryBlockMap::const_iterator i = s_allocated_mem_blocks.find(ptr);

        if (i != s_allocated_mem_blocks.end())
        {
#ifdef REPORT_MEMORY_DEALLOCATIONS
            log_deallocation_to_file(ptr, i->second);
#endif

            s_allocated_mem_blocks.erase(i);
        }

        s_tracking_enabled = true;
    }

    void report_allocation_statistics()
    {
        assert(!s_tracking_enabled);

        fprintf(
            s_log_file,
            "%s memory allocation%s, %s (%s byte%s) allocated in total.\n\n",
            pretty_uint(s_allocation_count).c_str(),
            s_allocation_count > 1 ? "s" : "",
            pretty_size(s_total_allocated_bytes).c_str(),
            pretty_uint(s_total_allocated_bytes).c_str(),
            s_total_allocated_bytes > 1 ? "s" : "");
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
                "Detected %s possible memory leak%s (%s in total):\n\n",
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

        fprintf(s_log_file, "\n");
    }
}

void start_memory_tracking()
{
    recursive_mutex::scoped_lock lock(s_mutex);

    s_tracking_enabled = false;

    atexit(stop_memory_tracking);

    s_log_file = fopen(MemoryLogFileName, "wt");

    if (s_log_file)
    {
        s_stack_walker.LoadModules();

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

namespace
{
    void log_allocation(const void* ptr, const size_t size) {}
    void log_deallocation(const void* ptr) {}
}

void start_memory_tracking() {}
void stop_memory_tracking() {}

#endif


//
// Override all variants of the new and delete operators.
//

namespace
{
    // Minimum alignment boundary of all memory allocations, in bytes.
    const size_t Alignment = 16;

    void* new_impl(size_t size)
    {
        if (size < 1)
            size = 1;

        void* ptr = aligned_malloc(size, Alignment);

        if (!ptr)
            throw bad_alloc();

        log_allocation(ptr, size);

        return ptr;
    }

    void delete_impl(void* ptr)
    {
        if (!ptr)
            return;

        aligned_free(ptr);

        log_deallocation(ptr);
    }
}

void* operator new(size_t size)
  throw(bad_alloc)
{
    return new_impl(size);
}

void* operator new[](size_t size)
  throw(bad_alloc)
{
    return new_impl(size);
}

void* operator new(size_t size, const nothrow_t&)
  throw()
{
    return new_impl(size);
}

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
