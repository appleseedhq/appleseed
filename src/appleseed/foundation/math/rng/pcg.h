
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2018 Francois Beaune, The appleseedhq Organization
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

// Standard headers.
#include <cstdint>

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
        const std::uint64_t init_state = 0x853C49E6748FEA9Bull,
        const std::uint64_t init_seq = 0xDA3E39CB94B95BDBull);

    // Generate a 32-bit random number.
    std::uint32_t rand_uint32();

  private:
    std::uint64_t   m_state;    // current state of the generator
    std::uint64_t   m_inc;      // controls which RNG sequence (stream) is selected -- must *always* be odd
};


//
// PCG class implementation.
//

inline PCG::PCG(const std::uint64_t init_state, const std::uint64_t init_seq)
{
    m_state = 0;
    m_inc = (init_seq << 1) | 1;
    rand_uint32();

    m_state += init_state;
    rand_uint32();
}

#pragma warning (push)
#pragma warning (disable : 4146)    // unary minus operator applied to unsigned type, result still unsigned

inline std::uint32_t PCG::rand_uint32()
{
    const std::uint64_t old_state = m_state;
    m_state = old_state * 6364136223846793005ull + m_inc;

    const std::uint32_t xorshifted = static_cast<std::uint32_t>(((old_state >> 18) ^ old_state) >> 27);
    const std::uint32_t rot = static_cast<std::uint32_t>(old_state >> 59);
    return (xorshifted >> rot) | (xorshifted << ((-rot) & 31));
}

#pragma warning (pop)

}   // namespace foundation
