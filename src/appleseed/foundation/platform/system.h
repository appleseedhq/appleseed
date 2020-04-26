
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
#include "foundation/utility/api/apistring.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>
#include <cstdint>

// Forward declarations.
namespace foundation    { class Logger; }

namespace foundation
{

//
// The System class provides information about the system.
//

class APPLESEED_DLLSYMBOL System
  : public NonCopyable
{
  public:
    // Print system information.
    static void print_information(Logger& logger);

    //
    // CPU architecture.
    //

    static const char* get_cpu_architecture();

    //
    // CPU cores.
    //

    // Return the number of logical CPU cores available in the system.
    static std::size_t get_logical_cpu_core_count();

    //
    // CPU caches.
    //

    // Return the number of L1 data caches on this CPU.
    static std::size_t get_l1_data_cache_count();

    // Return the size in bytes of a L1 data cache.
    static std::size_t get_l1_data_cache_size();

    // Return the size in bytes of a L1 data cache line.
    static std::size_t get_l1_data_cache_line_size();

    // Return the number of L2 data caches on this CPU.
    static std::size_t get_l2_cache_count();

    // Return the size in bytes of a L2 cache.
    static std::size_t get_l2_cache_size();

    // Return the size in bytes of a L2 cache line.
    static std::size_t get_l2_cache_line_size();

    // Return the number of L3 data caches on this CPU.
    static std::size_t get_l3_cache_count();

    // Return the size in bytes of a L3 cache, or 0 if there's no L3 cache.
    static std::size_t get_l3_cache_size();

    // Return the size in bytes of a L3 cache line, or 0 if there's no L3 cache.
    static std::size_t get_l3_cache_line_size();

    //
    // CPU features.
    //

#ifdef APPLESEED_X86

    struct X86CPUFeatures
    {
        // Vendor.
        enum class Vendor { Unknown, Intel, AMD };
        Vendor  m_vendor;

        // Miscellaneous instructions and instruction sets.
        bool    m_hw_mmx;
        bool    m_hw_x64;
        bool    m_hw_abm;
        bool    m_hw_rdrand;
        bool    m_hw_bmi1;
        bool    m_hw_bmi2;
        bool    m_hw_adx;
        bool    m_hw_prefetchwt1;
        bool    m_hw_mpx;

        // 128-bit SIMD.
        bool    m_hw_sse;
        bool    m_hw_sse2;
        bool    m_hw_sse3;
        bool    m_hw_ssse3;
        bool    m_hw_sse41;
        bool    m_hw_sse42;
        bool    m_hw_sse4a;
        bool    m_hw_aes;
        bool    m_hw_sha;
        bool    m_hw_f16c;

        // 256-bit SIMD.
        bool    m_hw_avx;
        bool    m_hw_xop;
        bool    m_hw_fma3;
        bool    m_hw_fma4;
        bool    m_hw_avx2;

        // 512-bit SIMD.
        bool    m_hw_avx512_f;
        bool    m_hw_avx512_pf;
        bool    m_hw_avx512_er;
        bool    m_hw_avx512_cd;
        bool    m_hw_avx512_vl;
        bool    m_hw_avx512_bw;
        bool    m_hw_avx512_dq;
        bool    m_hw_avx512_ifma;
        bool    m_hw_avx512_vbmi;

        // OS features.
        bool    m_os_avx;
        bool    m_os_avx512;
    };

    // Detect CPU features (x86 architecture only).
    static void detect_x86_cpu_features(X86CPUFeatures& features);

#endif

    // Return a string with the principal instruction sets available on this CPU.
    static APIString get_cpu_features_string();

    //
    // Physical memory.
    //

    // Return the total size in bytes of the physical memory.
    static std::uint64_t get_total_physical_memory_size();

    //
    // Virtual memory.
    //

    // Return the total size in bytes of the virtual memory.
    static std::uint64_t get_total_virtual_memory_size();

    // Return the amount in bytes of virtual memory used by the current process.
    static std::uint64_t get_process_virtual_memory_size();

    // Return the peak amount in bytes of virtual memory used by the current process.
    static std::uint64_t get_peak_process_virtual_memory_size();
};

}   // namespace foundation
