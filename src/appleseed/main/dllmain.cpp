
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

//
// Defines the entry point for the DLL application (Windows only).
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

using namespace std;

// Open a console.
void open_console()
{
    // Allocate a console.
    AllocConsole();

#pragma warning (push)
#pragma warning (disable : 4311)    // 'variable' : pointer truncation from 'type' to 'type'

    int hConHandle;
    long lStdHandle;
    FILE* fp;

    // Redirect stdout to the console.
    lStdHandle = (long)GetStdHandle(STD_OUTPUT_HANDLE);
    hConHandle = _open_osfhandle(lStdHandle, _O_TEXT);
    fp = _fdopen(hConHandle, "w");
    *stdout = *fp;
    setvbuf(stdout, NULL, _IONBF, 0);

    // Redirect stdin to the console.
    lStdHandle = (long)GetStdHandle(STD_INPUT_HANDLE);
    hConHandle = _open_osfhandle(lStdHandle, _O_TEXT);
    fp = _fdopen(hConHandle, "r");
    *stdin = *fp;
    setvbuf(stdin, NULL, _IONBF, 0);

    // Redirect stderr to the console.
    lStdHandle = (long)GetStdHandle(STD_ERROR_HANDLE);
    hConHandle = _open_osfhandle(lStdHandle, _O_TEXT);
    fp = _fdopen(hConHandle, "w");
    *stderr = *fp;
    setvbuf(stderr, NULL, _IONBF, 0);

#pragma warning (pop)

    // Make cout, wcout, cin, wcin, wcerr, cerr, wclog and clog point to the console as well.
    ios::sync_with_stdio();
}

// DLL entry point.
BOOL APIENTRY DllMain(
    HANDLE  /*module*/, 
    DWORD   reason_for_call, 
    LPVOID  /*reserved*/)
{
    switch (reason_for_call)
    {
      case DLL_PROCESS_ATTACH:
#ifndef SHIP
        // Open a console.
//      open_console();
#endif
        break;

      case DLL_THREAD_ATTACH:
        break;

      case DLL_THREAD_DETACH:
        break;

      case DLL_PROCESS_DETACH:
        break;
    }

    return TRUE;
}

#endif  // _WIN32
