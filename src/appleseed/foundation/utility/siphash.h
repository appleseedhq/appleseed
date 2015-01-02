
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
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

#ifndef APPLESEED_FOUNDATION_UTILITY_SIPHASH_H
#define APPLESEED_FOUNDATION_UTILITY_SIPHASH_H

// appleseed.foundation headers.
#include "foundation/platform/types.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

namespace foundation
{

//
// SipHash 2-4.
//
// Reference:
//
//   SipHash: a fast short-input PRF
//   Jean-Philippe Aumasson and Daniel J. Bernstein
//   https://131002.net/siphash/siphash.pdf
//

uint64 APPLESEED_DLLSYMBOL siphash24(
    const void*     bytes,
    const size_t    size,
    const uint64    k0,
    const uint64    k1);

inline uint64 siphash24(
    const void*     bytes,
    const size_t    size,
    const void*     key)
{
    return
        siphash24(
            bytes,
            size,
            *(static_cast<const uint64*>(key) + 0),
            *(static_cast<const uint64*>(key) + 1));
}

inline uint64 siphash24(
    const void*     bytes,
    const size_t    size)
{
    return siphash24(bytes, size, 0, 0);
}

template <typename T>
inline uint64 siphash24(const T& object)
{
    return siphash24(&object, sizeof(T));
}


//
// Helper function to combine two hashes.
//

inline uint64 siphash24(const uint64 a, const uint64 b)
{
    const uint64 pair[2] = { a, b };
    return siphash24(&pair, sizeof(pair));
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_SIPHASH_H
