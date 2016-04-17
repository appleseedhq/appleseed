
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2016 Francois Beaune, The appleseedhq Organization
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

#define ROTL(x,b) (uint64)( ((x) << (b)) | ( (x) >> (64 - (b))) )

#define U8TO64_LE(p)            \
    (((uint64)((p)[0])      ) | \
     ((uint64)((p)[1]) <<  8) | \
     ((uint64)((p)[2]) << 16) | \
     ((uint64)((p)[3]) << 24) | \
     ((uint64)((p)[4]) << 32) | \
     ((uint64)((p)[5]) << 40) | \
     ((uint64)((p)[6]) << 48) | \
     ((uint64)((p)[7]) << 56))

#define SIPROUND                                            \
    do {                                                    \
        v0 += v1; v1=ROTL(v1,13); v1 ^= v0; v0=ROTL(v0,32); \
        v2 += v3; v3=ROTL(v3,16); v3 ^= v2;                 \
        v0 += v3; v3=ROTL(v3,21); v3 ^= v0;                 \
        v2 += v1; v1=ROTL(v1,17); v1 ^= v2; v2=ROTL(v2,32); \
    } while(0)

uint64 siphash24(
    const void*     bytes,
    const size_t    size,
    const uint64    k0,
    const uint64    k1)
{
    /* "somepseudorandomlygeneratedbytes" */
    uint64 v0 = 0x736f6d6570736575ULL;
    uint64 v1 = 0x646f72616e646f6dULL;
    uint64 v2 = 0x6c7967656e657261ULL;
    uint64 v3 = 0x7465646279746573ULL;
    uint64 b;
    uint64 m;
    int i;
    const uint8* in = reinterpret_cast<const uint8*>(bytes);
    const uint8* end = in + size - ( size % sizeof( uint64 ) );
    const int left = size & 7;
    b = ( ( uint64 )size ) << 56;
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
      case 7: b |= ( ( uint64 )in[ 6] )  << 48;
      case 6: b |= ( ( uint64 )in[ 5] )  << 40;
      case 5: b |= ( ( uint64 )in[ 4] )  << 32;
      case 4: b |= ( ( uint64 )in[ 3] )  << 24;
      case 3: b |= ( ( uint64 )in[ 2] )  << 16;
      case 2: b |= ( ( uint64 )in[ 1] )  <<  8;
      case 1: b |= ( ( uint64 )in[ 0] ); break;
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
