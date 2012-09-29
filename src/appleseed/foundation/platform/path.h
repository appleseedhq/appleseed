
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_FOUNDATION_PLATFORM_PATH_H
#define APPLESEED_FOUNDATION_PLATFORM_PATH_H

// appleseed.main headers.
#include "main/dllsymbol.h"

// appleseed.foundation headers.
#ifdef _WIN32
#include "foundation/platform/windows.h"
#endif

// Platform headers.
#if defined __APPLE__
#include <sys/param.h>
#elif defined __linux__
#include <linux/limits.h> 
#endif

namespace foundation
{

//
// The maximum length of a filesystem path.
//

// Windows.
#if defined _WIN32
#define FOUNDATION_MAX_PATH_LENGTH  MAX_PATH

// Mac OS X.
#elif defined __APPLE__
#define FOUNDATION_MAX_PATH_LENGTH  MAXPATHLEN

// Linux.
#elif defined __linux__
#define FOUNDATION_MAX_PATH_LENGTH  PATH_MAX

// Unsupported platform.
#else
#error Unsupported platform.
#endif


//
// Path handling.
//

class DLLSYMBOL Path
{
  public:
    // Return the path to the application's executable. NOT thread-safe.
    static const char* get_executable_path();

    // Return the path to the directory containing the application's executable. NOT thread-safe.
    static const char* get_executable_directory();
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_PLATFORM_PATH_H
