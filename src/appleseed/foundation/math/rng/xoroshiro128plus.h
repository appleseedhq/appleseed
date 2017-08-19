
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_MATH_RNG_XOROSHIRO128PLUS_H
#define APPLESEED_FOUNDATION_MATH_RNG_XOROSHIRO128PLUS_H

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <cassert>

namespace foundation
{

//
// Xoroshiro128+ random number generator of period 2^128 - 1.
//
// Very fast and high quality.
//
// This RNG will generate zeros because it outputs the high 32 bits of its result.
//
// Reference:
//
//   http://xoroshiro.di.unimi.it/
//

class Xoroshiro128plus
{
  public:
    // Constructors, seed the generator.
    // The seed must not be zero everywhere.
    Xoroshiro128plus();
    explicit Xoroshiro128plus(const uint64 s0, const uint64 s1);

    // Generate a 32-bit random number.
    uint32 rand_uint32();

  private:
    uint64 m_s[2];
};


//
// Xoroshiro128plus class implementation.
//

inline Xoroshiro128plus::Xoroshiro128plus()
{
    m_s[0] = 0x46961B5E381BCE6EULL;
    m_s[1] = 0x55897310023CAE21ULL;
}

inline Xoroshiro128plus::Xoroshiro128plus(const uint64 s0, const uint64 s1)
{
    assert(s0 != 0 || s1 != 0);     // if the seed is 0 everywhere, all output values will be 0
    m_s[0] = s0;
    m_s[1] = s1;
}

inline uint32 Xoroshiro128plus::rand_uint32()
{
    const uint64 s0 = m_s[0];
    uint64 s1 = m_s[1];

    const uint64 result = s0 + s1;

    s1 ^= s0;
    m_s[0] = rotl64(s0, 55) ^ s1 ^ (s1 << 14);  // a, b
    m_s[1] = rotl64(s1, 36);                    // c

    return static_cast<uint32>(result >> 32);
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_RNG_XOROSHIRO128PLUS_H
