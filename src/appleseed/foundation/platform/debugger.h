
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
#ifdef _MSC_VER
#include "foundation/platform/windows.h"
#endif

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#ifndef _MSC_VER
#include <csignal>
#endif

namespace foundation
{

//
// Check whether a debugger is attached to the process.
//

APPLESEED_DLLSYMBOL bool is_debugger_attached();


//
// Break the execution of the program into the debugger.
//

// Visual C++.
#if defined _MSC_VER

    // Reference: http://msdn.microsoft.com/en-us/library/windows/desktop/ms679297(v=vs.85).aspx.
    #define BREAKPOINT() DebugBreak()

// Other platforms.
#else

    // Reference: http://stackoverflow.com/questions/4326414/set-breakpoint-in-c-or-c-code-programmatically-for-gdb-on-linux.
    #define BREAKPOINT() std::raise(SIGINT)

#endif

}   // namespace foundation
