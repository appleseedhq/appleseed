
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2020 Esteban Tovagliari, The appleseedhq Organization
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
#include <ostream>

namespace foundation
{

//
// Interned string class.
//
// Interned strings have several advantages over standard strings:
//
//  - Only one copy of the characters is stored.
//  - Copying an interned string is the same as copying a pointer.
//  - Equality testing of two interned strings is equivalent to a pointer equality test.
//
// There are also some disadvantages:
//
//  - Constructing an interned string can be slower than a normal string.
//  - Memory for the string characters is never freed.
//
// This implementation relies internally on OpenImageIO's ustring class.
// InternedString can be very quickly converted to and from OIIO's (and OSL's) ustrings.
//

class APPLESEED_DLLSYMBOL InternedString final
{
  public:
    // Constructor.
    InternedString();

    // Constructor.
    explicit InternedString(const char* str);

    // Constructor.
    InternedString(const char* str, const std::size_t n);

    // Return a pointer to the characters in the string.
    const char* c_str() const noexcept { return m_cstr; }

  private:
    void init(const char* str, const std::size_t n);

    const char* m_cstr;
};

inline bool operator==(const InternedString& a, const InternedString& b) noexcept
{
    return a.c_str() == b.c_str();
}

inline bool operator!=(const InternedString& a, const InternedString& b) noexcept
{
    return a.c_str() != b.c_str();
}

APPLESEED_DLLSYMBOL bool operator<(const InternedString& a, const InternedString& b) noexcept;

APPLESEED_DLLSYMBOL std::ostream& operator<<(std::ostream& os, const InternedString& name);

namespace literals
{

// Construct an InternedString.
inline InternedString operator"" _istr(const char* s, std::size_t n)
{
    return InternedString(s, n);
}

}   // namespace literals

using namespace literals;

}   // namespace foundation
