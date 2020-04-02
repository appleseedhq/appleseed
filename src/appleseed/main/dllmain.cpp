
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

#ifdef _WIN32

// appleseed.foundation headers.
#include "foundation/platform/windows.h"

// Standard headers.
#include <ios>
#include <stdio.h>

// Platform headers.
#include <fcntl.h>
#include <io.h>

namespace
{
    void redirect(FILE* fp, const char* mode, const DWORD std_device)
    {
        const intptr_t handle = (intptr_t)GetStdHandle(std_device);
        const int fd = _open_osfhandle(handle, _O_TEXT);
        *fp = *_fdopen(fd, mode);
        setvbuf(fp, NULL, _IONBF, 0);
    }

    void open_console()
    {
        // Allocates a console for this process.
        AllocConsole();

        // Redirect stdout, stdin and stderr to the console.
        redirect(stdout, "w", STD_OUTPUT_HANDLE);
        redirect(stdin, "r", STD_INPUT_HANDLE);
        redirect(stderr, "w", STD_ERROR_HANDLE);

        // Make cout, wcout, cin, wcin, cerr, wcerr, clog and wclog point to the console as well.
        std::ios::sync_with_stdio();
    }
}


//
// DLL entry point.
//

BOOL APIENTRY DllMain(
    HINSTANCE   module,
    DWORD       reason,
    LPVOID      /*reserved*/)
{
    if (reason == DLL_PROCESS_ATTACH)
    {
#ifndef APPLESEED_SHIP
        // Open a console.
        // open_console();
#endif
    }

    return TRUE;
}

#endif  // _WIN32
