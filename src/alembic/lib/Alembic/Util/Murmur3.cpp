//-*****************************************************************************
//
// Copyright (c) 2009-2011, Industrial Light & Magic,
//   a division of Lucasfilm Entertainment Company Ltd.
//
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
// *       Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
// *       Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following disclaimer
// in the documentation and/or other materials provided with the
// distribution.
// *       Neither the name of Industrial Light & Magic nor the names of
// its contributors may be used to endorse or promote products derived
// from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//-*****************************************************************************

// MurmurHash3 was written by Austin Appleby, and is placed in the public
// domain. The author hereby disclaims copyright to this source code.

#include <boost/detail/endian.hpp>
#include <Alembic/Util/Murmur3.h>
#include <boost/cstdint.hpp>

namespace Alembic {
namespace Util {
namespace ALEMBIC_VERSION_NS {

using boost::uint8_t;
using boost::uint64_t;

//-*****************************************************************************
void MurmurHash3_x64_128 ( const void * key, const size_t len,
                           const size_t podSize, void * out )
{
    const uint8_t * data = (const uint8_t*)key;
    const size_t nblocks = len / 16;

    uint64_t h1 = 0;
    uint64_t h2 = 0;

#ifdef PLATFORM_WINDOWS
    uint64_t c1 = 0x87c37b91114253d5LL;
    uint64_t c2 = 0x4cf5ad432745937fLL;
#else
    uint64_t c1 = 0x87c37b91114253d5ULL;
    uint64_t c2 = 0x4cf5ad432745937fULL;
#endif

    //----------
    // body

    const uint64_t * blocks = (const uint64_t *)(data);

    for(size_t i = 0; i < nblocks; i++)
    {

        uint64_t k1 = blocks[i*2];
        uint64_t k2 = blocks[i*2+1];

#ifdef BOOST_BIG_ENDIAN
        if (podSize == 8)
        {
            k1 = (k1>>56) |
                ((k1<<40) & 0x00FF000000000000ULL) |
                ((k1<<24) & 0x0000FF0000000000ULL) |
                ((k1<<8)  & 0x000000FF00000000ULL) |
                ((k1>>8)  & 0x00000000FF000000ULL) |
                ((k1>>24) & 0x0000000000FF0000ULL) |
                ((k1>>40) & 0x000000000000FF00ULL) |
                (k1<<56);

            k2 = (k2>>56) |
                ((k2<<40) & 0x00FF000000000000ULL) |
                ((k2<<24) & 0x0000FF0000000000ULL) |
                ((k2<<8)  & 0x000000FF00000000ULL) |
                ((k2>>8)  & 0x00000000FF000000ULL) |
                ((k2>>24) & 0x0000000000FF0000ULL) |
                ((k2>>40) & 0x000000000000FF00ULL) |
                 (k2<<56);
        }
        else if (podSize == 4)
        {
            k1 =((k1<<24) & 0xFF00000000000000ULL) |
                ((k1<<8)  & 0x00FF000000000000ULL) |
                ((k1>>8)  & 0x0000FF0000000000ULL) |
                ((k1>>24) & 0x000000FF00000000ULL) |
                ((k1<<24) & 0x00000000FF000000ULL) |
                ((k1<<8)  & 0x0000000000FF0000ULL) |
                ((k1>>8)  & 0x000000000000FF00ULL) |
                ((k1>>24) & 0x00000000000000FFULL);

            k2 =((k2<<24) & 0xFF00000000000000ULL) |
                ((k2<<8)  & 0x00FF000000000000ULL) |
                ((k2>>8)  & 0x0000FF0000000000ULL) |
                ((k2>>24) & 0x000000FF00000000ULL) |
                ((k2<<24) & 0x00000000FF000000ULL) |
                ((k2<<8)  & 0x0000000000FF0000ULL) |
                ((k2>>8)  & 0x000000000000FF00ULL) |
                ((k2>>24) & 0x00000000000000FFULL);
        }
        else if (podSize == 2)
        {
            k1 =((k1<<8) & 0xFF00000000000000ULL) |
                ((k1>>8) & 0x00FF000000000000ULL) |
                ((k1<<8) & 0x0000FF0000000000ULL) |
                ((k1>>8) & 0x000000FF00000000ULL) |
                ((k1<<8) & 0x00000000FF000000ULL) |
                ((k1>>8) & 0x0000000000FF0000ULL) |
                ((k1<<8) & 0x000000000000FF00ULL) |
                ((k1>>8) & 0x00000000000000FFULL);

            k2 =((k2<<8) & 0xFF00000000000000ULL) |
                ((k2>>8) & 0x00FF000000000000ULL) |
                ((k2<<8) & 0x0000FF0000000000ULL) |
                ((k2>>8) & 0x000000FF00000000ULL) |
                ((k2<<8) & 0x00000000FF000000ULL) |
                ((k2>>8) & 0x0000000000FF0000ULL) |
                ((k2<<8) & 0x000000000000FF00ULL) |
                ((k2>>8) & 0x00000000000000FFULL);
        }
#endif

        k1 *= c1;
        k1  = (k1 << 31) | (k1 >> 33);
        k1 *= c2;
        h1 ^= k1;

        h1 = (h1 << 27) | (h1 >> 37);
        h1 += h2;
        h1 = h1*5+0x52dce729;

        k2 *= c2;
        k2  = (k2 << 33) | (k2 >> 31);
        k2 *= c1;
        h2 ^= k2;

        h2 = (h2 << 31) | (h2 >> 33);
        h2 += h1;
        h2 = h2*5+0x38495ab5;
    }

    //----------
    // tail

#ifdef BOOST_BIG_ENDIAN
    const uint8_t * unswappedTail = (const uint8_t*)(data + nblocks*16);
    uint8_t tail[16];
    size_t tailSize = len & 15;

    // no swapping needed
    if (podSize == 1)
    {
        memcpy(tail, unswappedTail, tailSize);
    }
    else
    {
        for (size_t j = 0; j < tailSize; ++j)
        {
            tail[j] = unswappedTail[j^(podSize-1)];
        }
    }
#else
    const uint8_t * tail = (const uint8_t*)(data + nblocks*16);
#endif

    uint64_t k1 = 0;
    uint64_t k2 = 0;

    switch(len & 15)
    {
        case 15: k2 ^= uint64_t(tail[14]) << 48;
        case 14: k2 ^= uint64_t(tail[13]) << 40;
        case 13: k2 ^= uint64_t(tail[12]) << 32;
        case 12: k2 ^= uint64_t(tail[11]) << 24;
        case 11: k2 ^= uint64_t(tail[10]) << 16;
        case 10: k2 ^= uint64_t(tail[ 9]) << 8;
        case  9: k2 ^= uint64_t(tail[ 8]) << 0;
        {
            k2 *= c2;
            k2  = (k2 << 33) | (k2 >> 31);
            k2 *= c1;
            h2 ^= k2;
        }

        case  8: k1 ^= uint64_t(tail[ 7]) << 56;
        case  7: k1 ^= uint64_t(tail[ 6]) << 48;
        case  6: k1 ^= uint64_t(tail[ 5]) << 40;
        case  5: k1 ^= uint64_t(tail[ 4]) << 32;
        case  4: k1 ^= uint64_t(tail[ 3]) << 24;
        case  3: k1 ^= uint64_t(tail[ 2]) << 16;
        case  2: k1 ^= uint64_t(tail[ 1]) << 8;
        case  1: k1 ^= uint64_t(tail[ 0]) << 0;
        {
            k1 *= c1;
            k1  = (k1 << 31) | (k1 >> 33);
            k1 *= c2;
            h1 ^= k1;
        }
    };

    //----------
    // finalization

    h1 ^= len;
    h2 ^= len;

    h1 += h2;
    h2 += h1;

#ifdef PLATFORM_WINDOWS
    h1 ^= h1 >> 33;
    h1 *= 0xff51afd7ed558ccdLL;
    h1 ^= h1 >> 33;
    h1 *= 0xc4ceb9fe1a85ec53LL;
    h1 ^= h1 >> 33;

    h2 ^= h2 >> 33;
    h2 *= 0xff51afd7ed558ccdLL;
    h2 ^= h2 >> 33;
    h2 *= 0xc4ceb9fe1a85ec53LL;
    h2 ^= h2 >> 33;
#else
    h1 ^= h1 >> 33;
    h1 *= 0xff51afd7ed558ccdLLU;
    h1 ^= h1 >> 33;
    h1 *= 0xc4ceb9fe1a85ec53LLU;
    h1 ^= h1 >> 33;

    h2 ^= h2 >> 33;
    h2 *= 0xff51afd7ed558ccdLLU;
    h2 ^= h2 >> 33;
    h2 *= 0xc4ceb9fe1a85ec53LLU;
    h2 ^= h2 >> 33;
#endif
    h1 += h2;
    h2 += h1;

    ((uint64_t*)out)[0] = h1;
    ((uint64_t*)out)[1] = h2;
}

} // End namespace ALEMBIC_VERSION_NS
} // End namespace Util
} // End namespace Alembic
