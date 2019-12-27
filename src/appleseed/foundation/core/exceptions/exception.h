
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstring>
#include <exception>

namespace foundation
{

//
// Base class for all appleseed exceptions.
//

class Exception
  : public std::exception
{
  public:
    // Constructors.
    Exception();
    explicit Exception(const char* what);

    // Returns a generic, implementation-defined description of the exception.
    const char* what() const throw() override;

  protected:
    // Copy a string to another, ensuring that no overflow occurs and that the
    // resulting string is always properly null-terminated.
    static void copy_string(
        char*           destination,
        const char*     source,
        const size_t    destination_size);

    char m_what[2048];

    void set_what(const char* what);
};


//
// Exception class implementation.
//

inline Exception::Exception()
{
    set_what("foundation::Exception");
}

inline Exception::Exception(const char* what)
{
    set_what(what);
}

inline const char* Exception::what() const throw()
{
    return m_what;
}

inline void Exception::copy_string(
    char*           destination,
    const char*     source,
    const size_t    destination_size)
{
    assert(destination);
    assert(source);
    assert(destination_size > 0);

    std::strncpy(destination, source, destination_size - 1);
    destination[destination_size - 1] = '\0';
}

inline void Exception::set_what(const char* what)
{
    copy_string(m_what, what, sizeof(m_what));
}

}   // namespace foundation
