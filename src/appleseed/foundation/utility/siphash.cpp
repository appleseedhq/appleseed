
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
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

// Interface header.
#include "siphash.h"

namespace foundation
{

/*
   SipHash reference C implementation

   Copyright (c) 2012 Jean-Philippe Aumasson <jeanphilippe.aumasson@gmail.com>
   Copyright (c) 2012 Daniel J. Bernstein <djb@cr.yp.to>

   To the extent possible under law, the author(s) have dedicated all copyright
   and related and neighboring rights to this software to the public domain
   worldwide. This software is distributed without any warranty.

   You should have received a copy of the CC0 Public Domain Dedication along with
   this software. If not, see <http://creativecommons.org/publicdomain/zero/1.0/>.
 */

#define cROUNDS 2
#define dROUNDS 4

#define ROTL(x,b) (std::uint64_t)( ((x) << (b)) | ( (x) >> (64 - (b))) )

#define U8TO64_LE(p)            \
    (((std::uint64_t)((p)[0])      ) | \
     ((std::uint64_t)((p)[1]) <<  8) | \
     ((std::uint64_t)((p)[2]) << 16) | \
     ((std::uint64_t)((p)[3]) << 24) | \
     ((std::uint64_t)((p)[4]) << 32) | \
     ((std::uint64_t)((p)[5]) << 40) | \
     ((std::uint64_t)((p)[6]) << 48) | \
     ((std::uint64_t)((p)[7]) << 56))

#define SIPROUND                                            \
    do {                                                    \
        v0 += v1; v1=ROTL(v1,13); v1 ^= v0; v0=ROTL(v0,32); \
        v2 += v3; v3=ROTL(v3,16); v3 ^= v2;                 \
        v0 += v3; v3=ROTL(v3,21); v3 ^= v0;                 \
        v2 += v1; v1=ROTL(v1,17); v1 ^= v2; v2=ROTL(v2,32); \
    } while(0)

std::uint64_t siphash24(
    const void*     bytes,
    const size_t    size,
    const std::uint64_t    k0,
    const std::uint64_t    k1)
{
    /* "somepseudorandomlygeneratedbytes" */
    std::uint64_t v0 = 0x736f6d6570736575ULL;
    std::uint64_t v1 = 0x646f72616e646f6dULL;
    std::uint64_t v2 = 0x6c7967656e657261ULL;
    std::uint64_t v3 = 0x7465646279746573ULL;
    std::uint64_t b;
    std::uint64_t m;
    int i;
    const std::uint8_t* in = reinterpret_cast<const std::uint8_t*>(bytes);
    const std::uint8_t* end = in + size - ( size % sizeof( std::uint64_t ) );
    const int left = size & 7;
    b = ( ( std::uint64_t )size ) << 56;
    v3 ^= k1;
    v2 ^= k0;
    v1 ^= k1;
    v0 ^= k0;

    for ( ; in != end; in += 8 )
    {
        m = U8TO64_LE( in );

        v3 ^= m;

        for( i=0; i<cROUNDS; ++i ) SIPROUND;

        v0 ^= m;
    }

    switch( left )
    {
      case 7: b |= ( ( std::uint64_t )in[ 6] )  << 48;
      case 6: b |= ( ( std::uint64_t )in[ 5] )  << 40;
      case 5: b |= ( ( std::uint64_t )in[ 4] )  << 32;
      case 4: b |= ( ( std::uint64_t )in[ 3] )  << 24;
      case 3: b |= ( ( std::uint64_t )in[ 2] )  << 16;
      case 2: b |= ( ( std::uint64_t )in[ 1] )  <<  8;
      case 1: b |= ( ( std::uint64_t )in[ 0] ); break;
      case 0: break;
    }

    v3 ^= b;

    for( i=0; i<cROUNDS; ++i ) SIPROUND;

    v0 ^= b;
    v2 ^= 0xff;

    for( i=0; i<dROUNDS; ++i ) SIPROUND;

    b = v0 ^ v1 ^ v2  ^ v3;

    return b;
}

}   // namespace foundation
