
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

// Interface header.
#include "windows.h"

// Windows headers.
#include <crtdbg.h>

using namespace std;

namespace foundation
{

void disable_all_windows_abort_dialogs()
{
    //
    // See comments at https://stackoverflow.com/q/53907689/393756 for explanations.
    //

    SetErrorMode(SEM_FAILCRITICALERRORS | SEM_NOGPFAULTERRORBOX | SEM_NOOPENFILEERRORBOX);

    _CrtSetReportMode(_CRT_WARN, _CRTDBG_MODE_FILE);
    _CrtSetReportMode(_CRT_ERROR, _CRTDBG_MODE_FILE);
    _CrtSetReportMode(_CRT_ASSERT, _CRTDBG_MODE_FILE);

    _CrtSetReportFile(_CRT_WARN, _CRTDBG_FILE_STDERR);
    _CrtSetReportFile(_CRT_ERROR, _CRTDBG_FILE_STDERR);
    _CrtSetReportFile(_CRT_ASSERT, _CRTDBG_FILE_STDERR);
}

string get_windows_last_error_message()
{
    //
    // Relevant articles:
    //
    //   http://coolcowstudio.wordpress.com/2012/10/19/getlasterror-as-stdstring/
    //   http://blogs.msdn.com/b/oldnewthing/archive/2007/11/28/6564257.aspx
    //

    LPVOID buf;

    const DWORD buf_len =
        FormatMessageA(
            FORMAT_MESSAGE_ALLOCATE_BUFFER              // allocate the output buffer
                | FORMAT_MESSAGE_FROM_SYSTEM            // the message number is a system error code
                | FORMAT_MESSAGE_IGNORE_INSERTS,        // don't process %1... placeholders
            0,                                          // format string (unused)
            GetLastError(),                             // error code
            MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),  // use default language
            reinterpret_cast<LPTSTR>(&buf),             // output buffer
            0,                                          // size of the output buffer in TCHARs (unused)
            0);                                         // argument list (unused)

    if (buf_len == 0)
        return "FormatMessageA() call failed.";

    LPCSTR msg = reinterpret_cast<LPCSTR>(buf);
    const string result(msg, msg + buf_len);

    LocalFree(buf);

    return result;
}

}   // namespace foundation

#endif
