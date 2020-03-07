
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

#pragma once

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/string/string.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <string>

namespace foundation
{

//
// A minimalist string class that can safely cross DLL boundaries.
//

class APPLESEED_DLLSYMBOL APIString
{
  public:
    // Constructors.
    APIString() APPLESEED_NOEXCEPT;
    APIString(const char* s);
    APIString(const std::string& s);
    APIString(const APIString& rhs);
    APIString(APIString&& rhs) APPLESEED_NOEXCEPT;

    // Destructor.
    ~APIString();

    // Assignment operators.
    APIString& operator=(const APIString& rhs);
    APIString& operator=(APIString&& rhs) APPLESEED_NOEXCEPT;

    // Comparison operators.
    bool operator==(const APIString& rhs) const APPLESEED_NOEXCEPT;
    bool operator!=(const APIString& rhs) const APPLESEED_NOEXCEPT;

    // Return true if the string is empty.
    bool empty() const APPLESEED_NOEXCEPT;

    // Access the string as a C string.
    const char* c_str() const APPLESEED_NOEXCEPT;

  private:
    const char* m_s;
};

template <>
std::string to_string(const APIString& value);


//
// APIString class implementation.
//

inline APIString::APIString(const std::string& s)
  : m_s(duplicate_string(s.c_str()))
{
}

template <>
inline std::string to_string(const APIString& value)
{
    return std::string(value.c_str());
}

}   // namespace foundation
