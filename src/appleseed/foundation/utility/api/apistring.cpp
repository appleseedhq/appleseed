
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2018 Francois Beaune, The appleseedhq Organization
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
#include "apistring.h"

// Standard headers.
#include <cassert>
#include <cstring>


namespace foundation
{

//
// APIString class implementation.
//

APIString::APIString() APPLESEED_NOEXCEPT
  : m_s(nullptr)
{
}

APIString::APIString(const char* s)
  : m_s(duplicate_string(s))
{
}

APIString::APIString(const APIString& rhs)
  : m_s(rhs.m_s != nullptr ? duplicate_string(rhs.m_s) : nullptr)
{
}

APIString::APIString(APIString&& rhs) APPLESEED_NOEXCEPT
  : m_s(rhs.m_s)
{
    rhs.m_s = nullptr;
}

APIString::~APIString()
{
    free_string(m_s);
}

APIString& APIString::operator=(const APIString& rhs)
{
    free_string(m_s);
    m_s = rhs.m_s != nullptr ? duplicate_string(rhs.m_s) : nullptr;
    return *this;
}

APIString& APIString::operator=(APIString&& rhs) APPLESEED_NOEXCEPT
{
    free_string(m_s);
    m_s = rhs.m_s;
    rhs.m_s = nullptr;
    return *this;
}

bool APIString::operator==(const APIString& rhs) const APPLESEED_NOEXCEPT
{
    if (m_s == nullptr && rhs.m_s == nullptr)
        return true;

    if (m_s == nullptr || rhs.m_s == nullptr)
        return false;

    return strcmp(m_s, rhs.m_s) == 0;
}

bool APIString::operator!=(const APIString& rhs) const APPLESEED_NOEXCEPT
{
    return !(*this == rhs);
}

bool APIString::empty() const APPLESEED_NOEXCEPT
{
    return m_s != nullptr ? m_s[0] == '\0' : true;
}

const char* APIString::c_str() const APPLESEED_NOEXCEPT
{
    return m_s != nullptr ? m_s : "";
}

}   // namespace foundation
