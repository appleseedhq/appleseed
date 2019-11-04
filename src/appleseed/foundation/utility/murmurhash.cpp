
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "murmurhash.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"

// Standard headers.
#include <cstring>

namespace foundation
{
namespace
{

std::uint64_t fmix(std::uint64_t k)
{
    k ^= k >> 33;
    k *= 0xff51afd7ed558ccd;
    k ^= k >> 33;
    k *= 0xc4ceb9fe1a85ec53;
    k ^= k >> 33;

    return k;
}

}

MurmurHash::MurmurHash()
  : m_h1(0)
  , m_h2(0)
{
}

void MurmurHash::append(const void* data, const size_t bytes)
{
    const int nBlocks = static_cast<int>(bytes) / 16;

    const std::uint64_t c1 = 0x87c37b91114253d5;
    const std::uint64_t c2 = 0x4cf5ad432745937f;

    // local copies of m_h1, and m_h2. we'll work
    // with these before copying back at the end.
    // this gives the optimiser more freedom to do
    // its thing.
    std::uint64_t h1 = m_h1;
    std::uint64_t h2 = m_h2;

    // body

    const std::uint64_t* blocks = reinterpret_cast<const std::uint64_t*>(data);
    for (int i = 0; i < nBlocks; i++)
    {
        std::uint64_t k1 = blocks[i*2];
        std::uint64_t k2 = blocks[i*2+1];

        k1 *= c1; k1  = rotl64(k1, 31); k1 *= c2; h1 ^= k1;

        h1 = rotl64(h1, 27); h1 += h2; h1 = h1*5 + 0x52dce729;

        k2 *= c2; k2  = rotl64(k2, 33); k2 *= c1; h2 ^= k2;

        h2 = rotl64(h2, 31); h2 += h1; h2 = h2*5 + 0x38495ab5;
    }

    // tail

    const std::uint8_t* tail = reinterpret_cast<const std::uint8_t*>(data) + nBlocks*16;

    std::uint64_t k1 = 0;
    std::uint64_t k2 = 0;

    switch(bytes & 15)
    {
    case 15: k2 ^= std::uint64_t(tail[14]) << 48;
    case 14: k2 ^= std::uint64_t(tail[13]) << 40;
    case 13: k2 ^= std::uint64_t(tail[12]) << 32;
    case 12: k2 ^= std::uint64_t(tail[11]) << 24;
    case 11: k2 ^= std::uint64_t(tail[10]) << 16;
    case 10: k2 ^= std::uint64_t(tail[ 9]) << 8;
    case  9: k2 ^= std::uint64_t(tail[ 8]) << 0;
           k2 *= c2; k2  = rotl64(k2,33); k2 *= c1; h2 ^= k2;

    case  8: k1 ^= std::uint64_t(tail[ 7]) << 56;
    case  7: k1 ^= std::uint64_t(tail[ 6]) << 48;
    case  6: k1 ^= std::uint64_t(tail[ 5]) << 40;
    case  5: k1 ^= std::uint64_t(tail[ 4]) << 32;
    case  4: k1 ^= std::uint64_t(tail[ 3]) << 24;
    case  3: k1 ^= std::uint64_t(tail[ 2]) << 16;
    case  2: k1 ^= std::uint64_t(tail[ 1]) << 8;
    case  1: k1 ^= std::uint64_t(tail[ 0]) << 0;
           k1 *= c1; k1  = rotl64(k1,31); k1 *= c2; h1 ^= k1;
    };

    // finalisation

    h1 ^= bytes; h2 ^= bytes;

    h1 += h2;
    h2 += h1;

    h1 = fmix(h1);
    h2 = fmix(h2);

    h1 += h2;
    h2 += h1;

    m_h1 = h1;
    m_h2 = h2;
}

MurmurHash& MurmurHash::append(const char* str)
{
    append(str, strlen(str));
    return *this;
}

bool MurmurHash::operator==(const MurmurHash& other) const
{
    return m_h1 == other.m_h1 && m_h2 == other.m_h2;
}

bool MurmurHash::operator!=(const MurmurHash& other) const
{
    return m_h1 != other.m_h1 || m_h2 != other.m_h2;
}

bool MurmurHash::operator<(const MurmurHash& other) const
{
    return m_h1 < other.m_h1 || (m_h1 == other.m_h1 && m_h2 < other.m_h2);
}

std::ostream& operator<<(std::ostream& o, const MurmurHash& hash)
{
    o << hash.to_string();
    return o;
}

}   // namespace foundation
