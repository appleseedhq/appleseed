// This code is taken from Facebook's Folly library,
// with minimal code changes.
// Original license follows:
/*
 * Copyright 2017-present Facebook, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef APPLESEED_FOUNDATION_UTILITY_STAMPEDPTR_H
#define APPLESEED_FOUNDATION_UTILITY_STAMPEDPTR_H

// Standard headers
#include <cassert>
#include <cstdint>

namespace foundation
{

//
// stamped_ptr packs both a pointer to T and a std::uint16_t into a 64-bit value,
// exploiting the fact that current addresses are limited to 48 bits on
// all current x86-64 and ARM64 processors.
//
// For both x86-64 and ARM64, 64-bit pointers have a canonical
// form in which the upper 16 bits are equal to bit 47.  Intel has
// announced a 57-bit addressing mode (see https://software.intel.com/
// sites/default/files/managed/2b/80/5-level_paging_white_paper.pdf),
// but it is not yet available.  The first problematic platform will
// probably be ARMv8.2, which supports 52-bit virtual addresses.
//
// This code works on all of the platforms I have available for test,
// and probably on all currently-shipping platforms that have a hope of
// compiling folly.  Rather than enumerating the supported platforms via
// ifdef, this code dynamically validates its packing assumption in debug
// builds on each call to a mutating function.  Presumably by the time we
// are running this process in an operating system image that can address
// more than 256TB of memory, RAM cost and the latency of 128-bit CAS
// will have improved enough that this optimization is no longer impactful.
//
// A common approach to this kind of packing seems to be to just assume
// the top 16 bits are zero, but https://github.com/LuaJIT/LuaJIT/issues/49
// indicates that ARM64 platforms in the wild are actually setting bit 47
// in their stack addresses.  That means that we need to extend bit 47 to
// do the right thing (it's not expensive, it compiles to one instruction
// on x86-64 and arm64).
//
// The name is taken from Java's AtomicStampedReference.
// This class acts like a pair whose elements are named ptr and stamp.
// It is guaranteed that a zero raw value gets decoded as a (ptr,stamp) of (nullptr,0).
//

template <typename T>
class stamped_ptr
{
  public:
    // Constructor.
    stamped_ptr()
    {
    }

    // Constructor.
    stamped_ptr(T* ptr, const std::uint16_t stamp = 0)
    {
        m_raw = pack(ptr, stamp);
    }

    T* get_ptr() const
    {
        return unpack_ptr(m_raw);
    }

    void set_ptr(T* ptr)
    {
        m_raw = pack(ptr, unpack_stamp(m_raw));
    }

    std::uint16_t get_stamp() const
    {
        return unpack_stamp(m_raw);
    }

    void set_stamp(const std::uint16_t stamp)
    {
        m_raw = pack(unpack_ptr(m_raw), stamp);
    }

    void set(T* ptr, const std::uint16_t stamp)
    {
        m_raw = pack(ptr, stamp);
    }

  private:
    std::uint64_t m_raw;

    static T* unpack_ptr(const std::uint64_t raw)
    {
        // Canonical form means we need to extend bit 47 of the pointer to
        // bits 48..63 (unless the operating system never hands those pointers
        // to us, which is difficult to prove).  Signed right-shift of a
        // negative number is implementation-defined in C++ (not undefined!),
        // but actually does the right thing on all the platforms I can find.
        auto extended = static_cast<std::int64_t>(raw) >> 16;
        return reinterpret_cast<T*>(static_cast<intptr_t>(extended));
    }

    static std::uint16_t unpack_stamp(const std::uint64_t raw)
    {
        return static_cast<std::uint16_t>(raw);
    }

    static std::uint64_t pack(T* ptr, const std::uint16_t stamp)
    {
        auto shifted =
            static_cast<std::uint64_t>(reinterpret_cast<uintptr_t>(ptr)) << 16;
        std::uint64_t raw = shifted | stamp;

        assert(unpack_ptr(raw) == ptr);
        assert(unpack_stamp(raw) == stamp);

        return raw;
    }
};

template <typename T>
stamped_ptr<T> make_stamped_ptr(T* ptr, const std::uint16_t stamp)
{
    return stamped_ptr<T>(ptr, stamp);
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_STAMPEDPTR_H
