
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

// Interface header.
#include "internedstring.h"

// OpenImageIO headers.
#include "foundation/platform/_beginoiioheaders.h"
#include "OpenImageIO/ustring.h"
#include "foundation/platform/_endoiioheaders.h"

// Standard headers.
#include <cassert>
#include <cstring>

namespace foundation
{

//
// InternedString class implementation.
//

InternedString::InternedString()
{
    m_cstr = nullptr;
}

InternedString::InternedString(const char* str)
{
    assert(str);
    init(str, std::strlen(str));
}

InternedString::InternedString(const char* str, const std::size_t n)
{
    assert(str);
    init(str, n);
}

void InternedString::init(const char* str, const std::size_t n)
{
    OIIO::ustring s(str, n);
    m_cstr = s.c_str();
}

bool operator<(const InternedString& a, const InternedString& b) noexcept
{
    return OIIO::ustring::from_unique(a.c_str()) < OIIO::ustring::from_unique(b.c_str());
}

std::ostream& operator<<(std::ostream& os, const InternedString& name)
{
    return os << name.c_str();
}

}   // namespace foundation
