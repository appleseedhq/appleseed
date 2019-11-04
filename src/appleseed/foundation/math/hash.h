
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
#include "foundation/math/scalar.h"

// Standard headers.
#include <cstdint>

namespace foundation
{

//
// Interesting resources about hashing:
//
//   Hash functions
//   http://www.azillionmonkeys.com/qed/hash.html
//
//   MurmurHash
//   http://murmurhash.googlepages.com/
//
//   FNV Hash
//   http://www.isthe.com/chongo/tech/comp/fnv/
//
//   SeaHash: Explained
//   http://ticki.github.io/blog/seahash-explained/
//
//   Designing a good non-cryptographic hash function
//   http://ticki.github.io/blog/designing-a-good-non-cryptographic-hash-function/
//
//   Table of basic reversible integer operations
//   http://marc-b-reynolds.github.io/math/2017/10/13/IntegerBijections.html
//


//
// Integer hash functions.
//
// References:
//
//   Integer Hash Function, Thomas Wang
//   https://gist.github.com/badboy/6267743
//
//   Correlated Multi-Jittered Sampling
//   https://graphics.pixar.com/library/MultiJitteredSampling/paper.pdf
//

// Hash a 32-bit integer into a 32-bit integer (Pixar version).
std::uint32_t hash_uint32(std::uint32_t value);

// Hash a 32-bit integer into a 32-bit integer (Thomas Wang version).
std::uint32_t hash_uint32_wang(std::uint32_t value);

// Hash a 64-bit integer into a 64-bit integer (Thomas Wang).
std::uint64_t hash_uint64(std::uint64_t value);

// Hash a 64-bit integer into a 32-bit integer (Thomas Wang).
std::uint32_t hash_uint64_to_uint32(std::uint64_t value);


//
// Integer mixing functions.
//

// Mix multiple 32-bit integers into one 32-bit integer.
std::uint32_t mix_uint32(
    const std::uint32_t     a,
    const std::uint32_t     b);
std::uint32_t mix_uint32(
    const std::uint32_t     a,
    const std::uint32_t     b,
    const std::uint32_t     c);
std::uint32_t mix_uint32(
    const std::uint32_t     a,
    const std::uint32_t     b,
    const std::uint32_t     c,
    const std::uint32_t     d);

// Mix multiple 64-bit integers into one 64-bit integer.
std::uint64_t mix_uint64(
    const std::uint64_t     a,
    const std::uint64_t     b);
std::uint64_t mix_uint64(
    const std::uint64_t     a,
    const std::uint64_t     b,
    const std::uint64_t     c);
std::uint64_t mix_uint64(
    const std::uint64_t     a,
    const std::uint64_t     b,
    const std::uint64_t     c,
    const std::uint64_t     d);


//
// Hash combination functions.
//

// Combine two 32-bit hashes.
std::uint32_t combine_hashes(
    const std::uint32_t     h1,
    const std::uint32_t     h2);

// Combine two 64-bit hashes.
std::uint64_t combine_hashes(
    const std::uint64_t     h1,
    const std::uint64_t     h2);


//
// Integer hash functions implementation.
//

inline std::uint32_t hash_uint32(std::uint32_t value)
{
    value ^= value >> 17;
    value ^= value >> 10;
    value *= 0xB36534E5u;
    value ^= value >> 12;
    value ^= value >> 21;
    value *= 0x93FC4795u;
    value ^= 0xDF6E307Fu;
    value ^= value >> 17;
    return value;
}

inline std::uint32_t hash_uint32_wang(std::uint32_t value)
{
    value = (value ^ 61) ^ (value >> 16);
    value += value << 3;                                // value *= 9
    value ^= value >> 4;
    value *= 0x27D4EB2Du;                               // a prime or an odd constant
    value ^= value >> 15;
    return value;
}

inline std::uint64_t hash_uint64(std::uint64_t value)
{
    value = (~value) + (value << 21);                   // value = (value << 21) - value - 1;
    value = value ^ (value >> 24);
    value = (value + (value << 3)) + (value << 8);      // value *= 265
    value = value ^ (value >> 14);
    value = (value + (value << 2)) + (value << 4);      // value *= 21
    value = value ^ (value >> 28);
    value = value + (value << 31);
    return value;
}

inline std::uint32_t hash_uint64_to_uint32(std::uint64_t value)
{
    value = (~value) + (value << 18);                   // value = (value << 18) - value - 1;
    value = value ^ (value >> 31);
    value = value * 21;                                 // value = (value + (value << 2)) + (value << 4);
    value = value ^ (value >> 11);
    value = value + (value << 6);
    value = value ^ (value >> 22);
    return static_cast<std::uint32_t>(value);
}


//
// Integer mixing functions implementation.
//

inline std::uint32_t mix_uint32(
    const std::uint32_t     a,
    const std::uint32_t     b)
{
    const std::uint32_t h0 = hash_uint32(     a);       // h0 =    h( a )
    const std::uint32_t h1 = hash_uint32(h0 + b);       // h1 = h( h( a ) + b )
    return h1;
}

inline std::uint32_t mix_uint32(
    const std::uint32_t     a,
    const std::uint32_t     b,
    const std::uint32_t     c)
{
    const std::uint32_t h0 = hash_uint32(     a);       // h0 =       h( a )
    const std::uint32_t h1 = hash_uint32(h0 + b);       // h1 =    h( h( a ) + b )
    const std::uint32_t h2 = hash_uint32(h1 + c);       // h2 = h( h( h( a ) + b ) + c )
    return h2;
}

inline std::uint32_t mix_uint32(
    const std::uint32_t     a,
    const std::uint32_t     b,
    const std::uint32_t     c,
    const std::uint32_t     d)
{
    const std::uint32_t h0 = hash_uint32(     a);       // h0 =          h( a )
    const std::uint32_t h1 = hash_uint32(h0 + b);       // h1 =       h( h( a ) + b )
    const std::uint32_t h2 = hash_uint32(h1 + c);       // h2 =    h( h( h( a ) + b ) + c )
    const std::uint32_t h3 = hash_uint32(h2 + d);       // h3 = h( h( h( h( a ) + b ) + c ) + d )
    return h3;
}

inline std::uint64_t mix_uint64(
    const std::uint64_t     a,
    const std::uint64_t     b)
{
    const std::uint64_t h0 = hash_uint64(     a);       // h0 =    h( a )
    const std::uint64_t h1 = hash_uint64(h0 + b);       // h1 = h( h( a ) + b )
    return h1;
}

inline std::uint64_t mix_uint64(
    const std::uint64_t     a,
    const std::uint64_t     b,
    const std::uint64_t     c)
{
    const std::uint64_t h0 = hash_uint64(     a);       // h0 =       h( a )
    const std::uint64_t h1 = hash_uint64(h0 + b);       // h1 =    h( h( a ) + b )
    const std::uint64_t h2 = hash_uint64(h1 + c);       // h2 = h( h( h( a ) + b ) + c )
    return h2;
}

inline std::uint64_t mix_uint64(
    const std::uint64_t     a,
    const std::uint64_t     b,
    const std::uint64_t     c,
    const std::uint64_t     d)
{
    const std::uint64_t h0 = hash_uint64(     a);       // h0 =          h( a )
    const std::uint64_t h1 = hash_uint64(h0 + b);       // h1 =       h( h( a ) + b )
    const std::uint64_t h2 = hash_uint64(h1 + c);       // h2 =    h( h( h( a ) + b ) + c )
    const std::uint64_t h3 = hash_uint64(h2 + d);       // h3 = h( h( h( h( a ) + b ) + c ) + d )
    return h3;
}


//
// Hash combination functions implementation.
//
// The six floating point constants below were determined with the following program:
//
//   #include <cmath>
//   #include <cstdint>
//   #include <ios>
//   #include <iostream>
//   #include <limits>
//   #include <sstream>
//   #include <string>
//
//   template <typename UInt>
//   UInt next_odd(const UInt x)
//   {
//       return (x % 2 == 0) ? x + 1 : x;
//   }
//
//   template <typename UInt>
//   UInt compute_constant(const long double x)
//   {
//       const long double y = std::pow(2.0L, std::numeric_limits<UInt>::digits) / x;
//       return next_odd(static_cast<UInt>(y));
//   }
//
//   template <typename UInt>
//   std::string hex(const UInt x)
//   {
//       std::stringstream sstr;
//       sstr << "0x" << std::hex << std::uppercase << x;
//       return sstr.str();
//   }
//
//   int main()
//   {
//       static_assert(sizeof(long double) > 8, "This program must be run on a platform where sizeof(double) > 8");
//
//       // Pi
//       std::cout << hex(compute_constant<std::uint32_t>(3.1415926535897932384626433832795)) << std::endl;
//       std::cout << hex(compute_constant<std::uint64_t>(3.1415926535897932384626433832795)) << std::endl;
//
//       // Phi = (1 + sqrt(5)) / 2
//       std::cout << hex(compute_constant<std::uint32_t>(1.6180339887498948482045868343656)) << std::endl;
//       std::cout << hex(compute_constant<std::uint64_t>(1.6180339887498948482045868343656)) << std::endl;
//
//       // sqrt(2)
//       std::cout << hex(compute_constant<std::uint32_t>(1.4142135623730950488016887242097)) << std::endl;
//       std::cout << hex(compute_constant<std::uint64_t>(1.4142135623730950488016887242097)) << std::endl;
//   }
//
// Run this code on Coliru:
//
//   http://coliru.stacked-crooked.com/a/16800f7061a54510
//
// Output:
//
//   0x517CC1B7
//   0x517CC1B727220B7B
//   0x9E3779B9
//   0x9E3779B97F4A7A97
//   0xB504F333
//   0xB504F333F9DE6109
//

inline std::uint32_t combine_hashes(
    const std::uint32_t     h1,
    const std::uint32_t     h2)
{
    const std::uint32_t k1 = 0x517CC1B7ul * h1;
    const std::uint32_t k2 = 0x9E3779B9ul * h2;
    return k1 + k2 + rotl32(k1, 17) + rotr32(k2, 13) + 0xB504F333ul;
}

inline std::uint64_t combine_hashes(
    const std::uint64_t     h1,
    const std::uint64_t     h2)
{
    const std::uint64_t k1 = 0x517CC1B727220B7Bull * h1;
    const std::uint64_t k2 = 0x9E3779B97F4A7A97ull * h2;
    return k1 + k2 + rotl64(k1, 17) + rotr64(k2, 13) + 0xB504F333F9DE6109ull;
}

}   // namespace foundation
