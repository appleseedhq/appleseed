
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

// Interface header.
#include "encryption.h"

// appleseed.foundation headers.
#include "foundation/utility/tea.h"

// Qt headers.
#include <QByteArray>

// Standard headers.
#include <cassert>
#include <cstdint>

using namespace foundation;

namespace appleseed {
namespace qtcommon {

constexpr int KeySize = 16;     // bytes
constexpr int BlockSize = 8;    // bytes

QByteArray encrypt(const QByteArray& input, const QByteArray& key)
{
    assert(key.size() == KeySize);

    QByteArray result = input;
    pad_pkcs7(result, BlockSize);

    for (int i = 0, e = result.size(); i < e; i += BlockSize)
    {
        tea_encrypt(
            reinterpret_cast<std::uint32_t*>(result.data() + i),
            reinterpret_cast<const std::uint32_t*>(key.data()));
    }

    assert(result.size() % BlockSize == 0);

    return result;
}

QByteArray decrypt(const QByteArray& input, const QByteArray& key)
{
    assert(input.size() >= BlockSize);
    assert(input.size() % BlockSize == 0);
    assert(key.size() == KeySize);

    QByteArray result = input;

    for (int i = 0, e = result.size(); i < e; i += BlockSize)
    {
        tea_decrypt(
            reinterpret_cast<std::uint32_t*>(result.data() + i),
            reinterpret_cast<const std::uint32_t*>(key.data()));
    }

    const int padding = result[result.size() - 1];  // back() is not available before Qt 5.10
    assert(padding > 0);

    result.resize(result.size() - padding);

    return result;
}

}   // namespace qtcommon
}   // namespace appleseed
