
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2017 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_FOUNDATION_MATH_RNG_SIMDMERSENNETWISTER_H
#define APPLESEED_FOUNDATION_MATH_RNG_SIMDMERSENNETWISTER_H

// appleseed.foundation headers.
#include "foundation/platform/sse.h"
#include "foundation/platform/types.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

namespace foundation
{

//
// Mersenne Twister random number generator.
//

class APPLESEED_DLLSYMBOL SimdMersenneTwister
{
  public:
    // Constructors, seed the generator.
    explicit SimdMersenneTwister(const uint32 seed = 5489UL);
    SimdMersenneTwister(const uint32 init_key[], const int key_length);

    // Generate a full-range 32-bit random number.
    uint32 rand_uint32();

  private:
    // Parameters.
    enum
    {
        MEXP = 19937,
        N = MEXP / 128 + 1,
        N32 = N * 4,
        POS1 = 122
    };

    union w128
    {
        uint32  u[4];
        uint64  u64[2];
        __m128i si;
    };

    w128    mt[N];
    int     mti;    // current index in state vector

    static const w128 m_sse2_param_mask;

    // Initialize the state vector with a seed.
    void init_state(uint32 seed);
    void init_array_state(const uint32 init_key[], const int key_length);

    // Update the state vector.
    void update_state();

    void period_certification();
};


//
// SimdMersenneTwister class implementation.
//

inline uint32 SimdMersenneTwister::rand_uint32()
{
    const uint32* psfmt32 = &mt[0].u[0];

    if (mti >= N32)
    {
        update_state();
        mti = 0;
    }

    return psfmt32[mti++];
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_RNG_MERSENNETWISTER_H
