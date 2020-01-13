
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

#ifndef _WIN32
    #error Unsupported platform.
#endif

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <string>

// Exclude rarely-used stuff from Windows headers.
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif

// Don't define the min and max macros.
#ifndef NOMINMAX
#define NOMINMAX
#endif

// Windows headers.
#include <Windows.h>

namespace foundation
{

// Disable all dialogs requiring user intervention, such as those popping up when assertions fail.
APPLESEED_DLLSYMBOL void disable_all_windows_abort_dialogs();

// Return the error message associated with the error code returned by GetLastError().
std::string get_last_windows_error_message();

// Same as get_last_windows_error_message() but returns a wide string.
std::wstring get_last_windows_error_message_wide();

// Return true if the current code page is UTF-8, which is what we ask for in the embedded manifests.
// We expect this function to return true starting with Windows 10 version 1903 (May update) and false
// for earlier Windows 10 versions and older Windows versions (8.1 and below).
bool does_windows_support_utf8_code_page();


//
// Inline implementations to expose these functions (that return std:: types) to plugins.
//

inline std::string get_last_windows_error_message()
{
    //
    // Relevant articles:
    //
    //   http://coolcowstudio.wordpress.com/2012/10/19/getlasterror-as-stdstring/
    //   http://blogs.msdn.com/b/oldnewthing/archive/2007/11/28/6564257.aspx
    //

    const DWORD error_code = GetLastError();

    char* buf;
    const DWORD buf_len =
        FormatMessageA(
            FORMAT_MESSAGE_ALLOCATE_BUFFER              // allocate the output buffer
                | FORMAT_MESSAGE_FROM_SYSTEM            // the message number is a system error code
                | FORMAT_MESSAGE_IGNORE_INSERTS,        // don't process %1... placeholders
            nullptr,                                    // format string (unused)
            error_code,                                 // message ID
            MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),  // use default language
            reinterpret_cast<char*>(&buf),              // output buffer
            0,                                          // size of the output buffer in TCHARs (unused)
            nullptr);                                   // argument list (unused)

    if (buf_len == 0)
        return "FormatMessageA() call failed.";

    const std::string result(buf, buf + buf_len);

    LocalFree(buf);

    return result;
}

inline std::wstring get_last_windows_error_message_wide()
{
    const DWORD error_code = GetLastError();

    wchar_t* buf;
    const DWORD buf_len =
        FormatMessageW(
            FORMAT_MESSAGE_ALLOCATE_BUFFER
                | FORMAT_MESSAGE_FROM_SYSTEM
                | FORMAT_MESSAGE_IGNORE_INSERTS,
            nullptr,
            error_code,
            MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
            reinterpret_cast<wchar_t*>(&buf),
            0,
            nullptr);

    if (buf_len == 0)
        return L"FormatMessageW() call failed.";

    const std::wstring result(buf, buf + buf_len);

    LocalFree(buf);

    return result;
}

}   // namespace foundation
