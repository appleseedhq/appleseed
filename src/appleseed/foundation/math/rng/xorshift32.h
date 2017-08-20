
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

#ifndef APPLESEED_FOUNDATION_MATH_RNG_XORSHIFT32_H
#define APPLESEED_FOUNDATION_MATH_RNG_XORSHIFT32_H

// appleseed.foundation headers.
#include "foundation/platform/types.h"

// Standard headers.
#include <cassert>

namespace foundation
{

//
// Xorshift random number generator of period 2^32 - 1.
//
// Very fast and reasonably high quality despite a relatively short period.
// Can be extended to much higher periods if necessary (see the paper).
//
// WARNING: will never generate zeros!
//
// References:
//
//   http://www.jstatsoft.org/v08/i14/paper
//   http://tbf.coe.wayne.edu/jmasm/vol2_no1.pdf
//
// Additional resource about xorshift generators:
//
//   http://xoroshiro.di.unimi.it/
//

class Xorshift32
{
  public:
    // Constructor, seeds the generator.
    // The seed must not be zero.
    explicit Xorshift32(const uint32 seed = 2463534242UL);

    // Generate a 32-bit random number.
    uint32 rand_uint32();

  private:
    uint32 m_s;
};


//
// Xorshift32 class implementation.
//

inline Xorshift32::Xorshift32(const uint32 seed)
  : m_s(seed)
{
    assert(seed != 0);  // if the seed is 0, all output values will be 0
}

inline uint32 Xorshift32::rand_uint32()
{
    m_s ^= m_s << 13;
    m_s ^= m_s >> 17;
    m_s ^= m_s << 5;
    return m_s;
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_RNG_XORSHIFT32_H
