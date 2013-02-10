
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_FOUNDATION_UTILITY_OTHERWISE_H
#define APPLESEED_FOUNDATION_UTILITY_OTHERWISE_H

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstring>

namespace foundation
{

//
// Utility macros to catch unhandled cases in switch () statements.
//

// Exception thrown by the 'throw_otherwise' macro.
struct SwitchException
  : public Exception
{
    SwitchException(const char* file, const size_t line)
      : Exception("unhandled case in switch statement")
      , m_line(line)
    {
        assert(file);
        std::strncpy(m_file, file, MaxFileNameLength);
        m_file[MaxFileNameLength] = '\0';
    }

  private:
    static const size_t MaxFileNameLength = 256;

    char            m_file[MaxFileNameLength + 1];
    const size_t    m_line;
};

// Call assert() when reaching an unhandled case.
#define assert_otherwise                                        \
    default:                                                    \
      assert(!"Unhandled case in switch statement.");           \
      break

// Throw a SwitchException when reaching an unhandled case.
#define throw_otherwise                                         \
    default:                                                    \
      throw foundation::SwitchException(__FILE__, __LINE__)

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_OTHERWISE_H
