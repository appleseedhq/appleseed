
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
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

#ifdef _WIN32

// appleseed.foundation headers.
#include "foundation/platform/windows.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <string>

using namespace foundation;

TEST_SUITE(Foundation_Platform_Windows)
{
    TEST_CASE(GetLastWindowsErrorMessage_LastErrorCodeIsSuccess_MessageIsNotEmpty)
    {
        SetLastError(ERROR_SUCCESS);
        const std::string msg = get_last_windows_error_message();
        EXPECT_FALSE(msg.empty());
    }

    TEST_CASE(GetLastWindowsErrorMessageWide_LastErrorCodeIsSuccess_MessageIsNotEmpty)
    {
        SetLastError(ERROR_SUCCESS);
        const std::wstring msg = get_last_windows_error_message_wide();
        EXPECT_FALSE(msg.empty());
    }

    TEST_CASE(GetLastWindowsErrorMessage_LastErrorCodeIsFileNotFound_MessageIsNotEmpty)
    {
        SetLastError(ERROR_FILE_NOT_FOUND);
        const std::string msg = get_last_windows_error_message();
        EXPECT_FALSE(msg.empty());
    }

    TEST_CASE(GetLastWindowsErrorMessageWide_LastErrorCodeIsFileNotFound_MessageIsNotEmpty)
    {
        SetLastError(ERROR_FILE_NOT_FOUND);
        const std::wstring msg = get_last_windows_error_message_wide();
        EXPECT_FALSE(msg.empty());
    }
}

#endif
