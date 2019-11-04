
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

#pragma once

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>
#include <cstdint>
#include <iomanip>
#include <sstream>
#include <string>

namespace foundation
{

//
// MurmurHash.
//
// A nice class for hashing arbitrary chunks of data, based on
// code available at https://github.com/aappleby/smhasher.
//
// From that page :
//
// "All MurmurHash versions are public domain software, and the
// author disclaims all copyright to their code."
//

class APPLESEED_DLLSYMBOL MurmurHash
{
  public:
    MurmurHash();

    bool operator==(const MurmurHash& other) const;
    bool operator!=(const MurmurHash& other) const;
    bool operator<(const MurmurHash& other) const;

    template <typename T>
    MurmurHash& append(const T& x);

    MurmurHash& append(const char* str);

    MurmurHash& append(const std::string& str);

    std::uint64_t h1() const;
    std::uint64_t h2() const;

    std::string to_string() const;

  private:
    void append(const void* data, const size_t bytes);

    std::uint64_t m_h1;
    std::uint64_t m_h2;
};

std::ostream& operator<<(std::ostream& o, const MurmurHash& hash);


//
// MurmurHash class implementation.
//

template <typename T>
inline MurmurHash& MurmurHash::append(const T& x)
{
    append(&x, sizeof(T));
    return *this;
}

inline MurmurHash& MurmurHash::append(const std::string& str)
{
    append(str.c_str(), str.size());
    return *this;
}

inline std::uint64_t MurmurHash::h1() const
{
    return m_h1;
}

inline std::uint64_t MurmurHash::h2() const
{
    return m_h2;
}

inline std::string MurmurHash::to_string() const
{
    std::stringstream s;
    s << std::hex << std::setfill('0')
      << std::setw(16) << m_h1
      << std::setw(16) << m_h2;
    return s.str();
}

}   // namespace foundation
