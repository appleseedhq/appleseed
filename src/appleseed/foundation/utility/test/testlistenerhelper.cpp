
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

// Interface header.
#include "testlistenerhelper.h"

// appleseed.foundation headers.
#include "foundation/platform/snprintf.h"
#include "foundation/utility/test/itestlistener.h"

// Standard headers.
#include <cstdarg>

namespace foundation
{

//
// TestListenerHelper class implementation.
//

void TestListenerHelper::write(
    ITestListener&                      test_listener,
    const TestSuite&                    test_suite,
    const char*                         test_case_name,
    const char*                         file,
    const size_t                        line,
    const TestMessage::Type             message_type,
    APPLESEED_PRINTF_FMT const char*    format, ...)
{
    // Size in bytes of the temporary message buffer.
    const size_t BufferSize = 4096;

    // Print the formatted message into the temporary buffer.
    va_list argptr;
    va_start(argptr, format);
    char message[BufferSize];
    portable_vsnprintf(message, BufferSize, format, argptr);

    // Forward the message to the test listener.
    test_listener.write(
        test_suite,
        test_case_name,
        file,
        line,
        message_type,
        message);
}

}   // namespace foundation
