
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Francois Beaune, The appleseedhq Organization
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
#include <cassert>
#include <cstddef>
#include <cstdint>

namespace foundation
{

//
// References:
//
//   Padding (cryptography)
//   https://en.wikipedia.org/wiki/Padding_(cryptography)#PKCS#5_and_PKCS#7
//
//   TEA, a Tiny Encryption Algorithm
//   http://www.cix.co.uk/~klockstone/tea.pdf
//
//   Tiny Encryption Algorithm
//   https://en.wikipedia.org/wiki/Tiny_Encryption_Algorithm
//

// Pad an array such that its size is a multiple of `block_size`, using the PKCS#7 padding algorithm.
// This function works on std::vector<> and QByteArray.
template <typename Vec>
void pad_pkcs7(Vec& input, const int block_size);

// Encrypt a 64-bit data block using a 128-bit key using the TEA algorithm.
void tea_encrypt(std::uint32_t data[2], const std::uint32_t key[4]);

// Decrypt a 64-bit data block using a 128-bit key using the TEA algorithm.
void tea_decrypt(std::uint32_t data[2], const std::uint32_t key[4]);


//
// Implementation.
//

template <typename Vec>
inline void pad_pkcs7(Vec& input, const int block_size)
{
    assert(block_size >= 1);

    const auto initial_input_size = input.size();
    const auto padding_size = block_size - initial_input_size % block_size;
    const auto padding_byte = static_cast<char>(padding_size);

    input.resize(initial_input_size + padding_size);

    for (auto i = initial_input_size, e = input.size(); i < e; ++i)
        input[i] = padding_byte;

    assert(input.size() % block_size == 0);
}

inline void tea_encrypt(std::uint32_t data[2], const std::uint32_t key[4])
{
    const std::uint32_t Delta = 0x9e3779b9UL;   // a key schedule constant equal to 2^32 / golden ratio

    std::uint32_t y = data[0], z = data[1];
    std::uint32_t sum = 0;

    for (std::size_t i = 0; i < 32; ++i)
    {
        sum += Delta;
        y += ((z << 4) + key[0]) ^ (z + sum) ^ ((z >> 5) + key[1]);
        z += ((y << 4) + key[2]) ^ (y + sum) ^ ((y >> 5) + key[3]);
    }

    data[0] = y;
    data[1] = z;
}

inline void tea_decrypt(std::uint32_t data[2], const std::uint32_t key[4])
{
    const std::uint32_t Delta = 0x9e3779b9UL;

    std::uint32_t y = data[0], z = data[1];
    std::uint32_t sum = Delta << 5;

    for (std::size_t i = 0; i < 32; ++i)
    {
        z -= ((y << 4) + key[2]) ^ (y + sum) ^ ((y >> 5) + key[3]);
        y -= ((z << 4) + key[0]) ^ (z + sum) ^ ((z >> 5) + key[1]);
        sum -= Delta;
    }

    data[0] = y;
    data[1] = z;
}

}   // namespace foundation
