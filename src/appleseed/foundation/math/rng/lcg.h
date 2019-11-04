
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

// Standard headers.
#include <cstdint>

namespace foundation
{

//
// Linear Congruential Generator, implementing a recursion of the form
//
//   x[n+1] = a * x[n] + b  (mod m)
//
// with the following parameters (used in the BCPL language):
//
//   multiplier  a = 2147001325
//   increment   b = 715136305
//   modulus     m = 2^32
//
// For all practical purposes, the Mersenne Twister RNG should be used.
// This generator is implemented for comparison and experimentation only.
//
// References:
//
//   http://random.mat.sbg.ac.at/
//   http://statistik.wu-wien.ac.at/prng/
//

class LCG
{
  public:
    // Constructor, seeds the generator.
    explicit LCG(const std::uint32_t seed = 0);

    // Generate a 32-bit random number.
    std::uint32_t rand_uint32();

  private:
    std::uint32_t m_s;
};


//
// LCG class implementation.
//

inline LCG::LCG(const std::uint32_t seed)
  : m_s(seed)
{
}

inline std::uint32_t LCG::rand_uint32()
{
    m_s = 2147001325u * m_s + 715136305u;
    return m_s;
}

}   // namespace foundation
