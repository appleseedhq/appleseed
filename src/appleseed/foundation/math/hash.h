
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_FOUNDATION_MATH_HASH_H
#define APPLESEED_FOUNDATION_MATH_HASH_H

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
//   http://www.concentric.net/~Ttwang/tech/inthash.htm
//

// Hash a 32-bit integer to a 32-bit integer.
uint32 hashint32(uint32 key);
uint32 hashint32alt(uint32 key);                // same as hashint32(), alternative algorithm

// Hash a 64-bit integer to a 64-bit integer.
uint64 hashint64(uint64 key);

// Hash a 64-bit integer to a 32-bit integer.
uint32 hashint64to32(uint64 key);

// Mix multiple 32-bit integers into one 32-bit integer.
uint32 mix32(
    const uint32 a,
    const uint32 b);
uint32 mix32(
    const uint32 a,
    const uint32 b,
    const uint32 c);
uint32 mix32(
    const uint32 a,
    const uint32 b,
    const uint32 c,
    const uint32 d);


//
// Integer hash functions implementation.
//

inline uint32 hashint32(uint32 key)
{
    key = ~key + (key << 15);                   // key = (key << 15) - key - 1;
    key = key ^ (key >> 12);
    key = key + (key << 2);
    key = key ^ (key >> 4);
    key = key * 2057;                           // key = (key + (key << 3)) + (key << 11);
    key = key ^ (key >> 16);
    return key;
}

inline uint32 hashint32alt(uint32 key)
{
    key = (key ^ 61) ^ (key >> 16);
    key = key + (key << 3);
    key = key ^ (key >> 4);
    key = key * 0x27D4EB2DUL;                   // a prime or an odd constant
    key = key ^ (key >> 15);
    return key;
}

inline uint64 hashint64(uint64 key)
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

inline uint32 hashint64to32(uint64 key)
{
    key = (~key) + (key << 18);                 // key = (key << 18) - key - 1;
    key = key ^ (key >> 31);
    key = key * 21;                             // key = (key + (key << 2)) + (key << 4);
    key = key ^ (key >> 11);
    key = key + (key << 6);
    key = key ^ (key >> 22);
    return static_cast<uint32>(key);
}

inline uint32 mix32(
    const uint32 a,
    const uint32 b)
{
    const uint32 h0 = hashint32(a);             // h0 = h( a )
    const uint32 h1 = h0 + b;                   // h1 = h( a ) + b
    return h1;
}

inline uint32 mix32(
    const uint32 a,
    const uint32 b,
    const uint32 c)
{
    const uint32 h0 = hashint32(a);             // h0 = h( a )
    const uint32 h1 = hashint32(b + h0);        // h1 = h( b + h( a ) )
    const uint32 h2 = h1 + c;                   // h2 = h( b + h( a ) ) + c
    return h2;
}

inline uint32 mix32(
    const uint32 a,
    const uint32 b,
    const uint32 c,
    const uint32 d)
{
    const uint32 h0 = hashint32(a);             // h0 = h( a )
    const uint32 h1 = hashint32(b + h0);        // h1 = h( b + h( a ) )
    const uint32 h2 = hashint32(c + h1);        // h2 = h( c + h( b + h( a ) ) )
    const uint32 h3 = h2 + d;                   // h3 = h( c + h( b + h( a ) ) ) + d
    return h3;
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_MATH_HASH_H
