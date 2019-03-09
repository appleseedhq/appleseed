
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
#include "foundation/platform/types.h"

namespace foundation
{

//
// Interesting resources about hashing:
//
//   http://www.azillionmonkeys.com/qed/hash.html
//   http://murmurhash.googlepages.com/
//   http://www.isthe.com/chongo/tech/comp/fnv/
//


//
// Integer hash functions.
//
// Reference:
//
//   https://gist.github.com/badboy/6267743
//

// Hash a 32-bit integer into a 32-bit integer.
uint32 hash_uint32(uint32 key);

// Hash a 64-bit integer into a 64-bit integer.
uint64 hash_uint64(uint64 key);

// Hash a 64-bit integer into a 32-bit integer.
uint32 hash_uint64_to_uint32(uint64 key);


//
// Mixing functions.
//

// Mix multiple 32-bit integers into one 32-bit integer.
uint32 mix_uint32(
    const uint32 a,
    const uint32 b);
uint32 mix_uint32(
    const uint32 a,
    const uint32 b,
    const uint32 c);
uint32 mix_uint32(
    const uint32 a,
    const uint32 b,
    const uint32 c,
    const uint32 d);

// Mix multiple 64-bit integers into one 64-bit integer.
uint64 mix_uint64(
    const uint64 a,
    const uint64 b);
uint64 mix_uint64(
    const uint64 a,
    const uint64 b,
    const uint64 c);
uint64 mix_uint64(
    const uint64 a,
    const uint64 b,
    const uint64 c,
    const uint64 d);


//
// Hash a 32-bit integer into a single-precision floating point value in [0,1).
//
// Reference:
//
//   Correlated Multi-Jittered Sampling
//   https://graphics.pixar.com/library/MultiJitteredSampling/paper.pdf
//

float hash_uint32_pixar(
    const uint32 value,
    const uint32 scramble);


//
// Integer hash functions implementation.
//

inline uint32 hash_uint32(uint32 key)
{
    key = (key ^ 61) ^ (key >> 16);
    key = key + (key << 3);
    key = key ^ (key >> 4);
    key = key * 0x27D4EB2Du;                    // a prime or an odd constant
    key = key ^ (key >> 15);
    return key;
}

inline uint64 hash_uint64(uint64 key)
{
    key = (~key) + (key << 21);                 // key = (key << 21) - key - 1;
    key = key ^ (key >> 24);
    key = (key + (key << 3)) + (key << 8);      // key * 265
    key = key ^ (key >> 14);
    key = (key + (key << 2)) + (key << 4);      // key * 21
    key = key ^ (key >> 28);
    key = key + (key << 31);
    return key;
}

inline uint32 hash_uint64_to_uint32(uint64 key)
{
    key = (~key) + (key << 18);                 // key = (key << 18) - key - 1;
    key = key ^ (key >> 31);
    key = key * 21;                             // key = (key + (key << 2)) + (key << 4);
    key = key ^ (key >> 11);
    key = key + (key << 6);
    key = key ^ (key >> 22);
    return static_cast<uint32>(key);
}

inline uint32 mix_uint32(
    const uint32 a,
    const uint32 b)
{
    const uint32 h0 = hash_uint32(     a);      // h0 =    h( a )
    const uint32 h1 = hash_uint32(h0 + b);      // h1 = h( h( a ) + b )
    return h1;
}

inline uint32 mix_uint32(
    const uint32 a,
    const uint32 b,
    const uint32 c)
{
    const uint32 h0 = hash_uint32(     a);      // h0 =       h( a )
    const uint32 h1 = hash_uint32(h0 + b);      // h1 =    h( h( a ) + b )
    const uint32 h2 = hash_uint32(h1 + c);      // h2 = h( h( h( a ) + b ) + c )
    return h2;
}

inline uint32 mix_uint32(
    const uint32 a,
    const uint32 b,
    const uint32 c,
    const uint32 d)
{
    const uint32 h0 = hash_uint32(     a);      // h0 =          h( a )
    const uint32 h1 = hash_uint32(h0 + b);      // h1 =       h( h( a ) + b )
    const uint32 h2 = hash_uint32(h1 + c);      // h2 =    h( h( h( a ) + b ) + c )
    const uint32 h3 = hash_uint32(h2 + d);      // h3 = h( h( h( h( a ) + b ) + c ) + d )
    return h3;
}

inline uint64 mix_uint64(
    const uint64 a,
    const uint64 b)
{
    const uint64 h0 = hash_uint64(     a);      // h0 =    h( a )
    const uint64 h1 = hash_uint64(h0 + b);      // h1 = h( h( a ) + b )
    return h1;
}

inline uint64 mix_uint64(
    const uint64 a,
    const uint64 b,
    const uint64 c)
{
    const uint64 h0 = hash_uint64(     a);      // h0 =       h( a )
    const uint64 h1 = hash_uint64(h0 + b);      // h1 =    h( h( a ) + b )
    const uint64 h2 = hash_uint64(h1 + c);      // h2 = h( h( h( a ) + b ) + c )
    return h2;
}

inline uint64 mix_uint64(
    const uint64 a,
    const uint64 b,
    const uint64 c,
    const uint64 d)
{
    const uint64 h0 = hash_uint64(     a);      // h0 =          h( a )
    const uint64 h1 = hash_uint64(h0 + b);      // h1 =       h( h( a ) + b )
    const uint64 h2 = hash_uint64(h1 + c);      // h2 =    h( h( h( a ) + b ) + c )
    const uint64 h3 = hash_uint64(h2 + d);      // h3 = h( h( h( h( a ) + b ) + c ) + d )
    return h3;
}

inline float hash_uint32_pixar(
    const uint32 value,
    const uint32 scramble)
{
    uint32 result = value;
    result ^= scramble;
    result ^= result >> 17;
    result ^= result >> 10;
    result *= 0xb36534e5;
    result ^= result >> 12;
    result ^= result >> 21;
    result *= 0x93fc4795;
    result ^= 0xdf6e307f;
    result ^= result >> 17;
    result *= 1 | scramble >> 18;
    return result * (1.0f / 4294967808.0f);
}

}   // namespace foundation
