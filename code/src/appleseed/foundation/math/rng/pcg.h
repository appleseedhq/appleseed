
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_MATH_RNG_PCG_H
#define APPLESEED_FOUNDATION_MATH_RNG_PCG_H

// appleseed.foundation headers.
#include "foundation/platform/types.h"

namespace foundation
{

//
// PCG random number generator.
//
// Reference:
//
//   http://www.pcg-random.org/
//

class PCG
{
  public:
    // Constructor, seeds the generator.
    PCG(
        const uint64 init_state = 0x853C49E6748FEA9BULL,
        const uint64 init_seq = 0xDA3E39CB94B95BDBULL);

    // Generate a 32-bit random number.
    uint32 rand_uint32();

  private:
    uint64  m_state;    // current state of the generator
    uint64  m_inc;      // controls which RNG sequence (stream) is selected -- must *always* be odd
};


//
// PCG class implementation.
//

inline PCG::PCG(const uint64 init_state, const uint64 init_seq)
{
    m_state = 0;
    m_inc = (init_seq << 1) | 1;
    rand_uint32();

    m_state += init_state;
    rand_uint32();
}

#pragma warning (push)
#pragma warning (disable : 4146)    // unary minus operator applied to unsigned type, result still unsigned

inline uint32 PCG::rand_uint32()
{
    const uint64 old_state = m_state;
    m_state = old_state * 6364136223846793005ULL + m_inc;

    const uint32 xorshifted = static_cast<uint32>(((old_state >> 18) ^ old_state) >> 27);
    const uint32 rot = static_cast<uint32>(old_state >> 59);
    return (xorshifted >> rot) | (xorshifted << ((-rot) & 31));
}

#pragma warning (pop)

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_RNG_PCG_H
