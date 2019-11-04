
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

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstdint>

namespace foundation
{

//
// Mersenne Twister random number generator.
//

class APPLESEED_DLLSYMBOL SerialMersenneTwister
{
  public:
    // Constructors, seed the generator.
    explicit SerialMersenneTwister(const std::uint32_t seed = 5489u);
    SerialMersenneTwister(const std::uint32_t init_key[], const int key_length);

    // Generate a 32-bit random number.
    std::uint32_t rand_uint32();

  private:
    // Period parameters.
    enum { N = 624, M = 397 };

    std::uint32_t   mt[N];  // state vector
    int             mti;    // current index in state vector

    // Initialize the state vector with a seed.
    void init_state(std::uint32_t seed);

    // Update the state vector.
    void update_state();
};


//
// SerialMersenneTwister class implementation.
//

inline std::uint32_t SerialMersenneTwister::rand_uint32()
{
    if (mti >= N)
    {
        update_state();
        mti = 0;
    }

    std::uint32_t y = mt[mti++];

    // Tempering.
    y ^= (y >> 11);
    y ^= (y << 7)  & 0x9d2c5680u;
    y ^= (y << 15) & 0xefc60000u;
    y ^= (y >> 18);

    return y;
}

}   // namespace foundation
