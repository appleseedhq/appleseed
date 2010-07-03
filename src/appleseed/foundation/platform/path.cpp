
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "path.h"

// appleseed.foundation headers.
#ifdef __APPLE__
#include "foundation/platform/types.h"
#endif

// boost headers.
#include "boost/filesystem/operations.hpp"

// Platform headers.
#ifdef __APPLE__
#include <mach-o/dyld.h>
#endif

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstring>

using namespace boost;
using namespace std;

namespace foundation
{

//
// Path class implementation.
//

// Return the path to the application's executable.
const char* Path::get_executable_path()
{
// Windows.
#if defined _WIN32

    static char path[MAX_PATH + 1];
    static bool path_initialized = false;

    if (!path_initialized)
    {
	    const DWORD result =
            GetModuleFileName(
		        GetModuleHandle(NULL),
		        path,
		        sizeof(path));
        assert(result);
        path_initialized = true;
    }

    return path;

// Mac OS X.
#elif defined __APPLE__

    static char path[MAXPATHLEN + 1];
    static bool path_initialized = false;

    if (!path_initialized)
    {
        uint32 path_len = MAXPATHLEN;
        const int result = _NSGetExecutablePath(path, &path_len);
        assert(result == 0);
        path_initialized = true;
    }

    return path;

#endif
}

// Return the path to the directory containing the application's executable.
const char* Path::get_executable_directory()
{
    static char path[FOUNDATION_MAX_PATH_LENGTH + 1];
    static bool path_initialized = false;

    if (!path_initialized)
    {
        filesystem::path executable_path(get_executable_path());

        assert(executable_path.has_filename());
        executable_path.remove_filename();

        assert(executable_path.directory_string().size() <= FOUNDATION_MAX_PATH_LENGTH);
        strcpy(path, executable_path.directory_string().c_str());

        path_initialized = true;
    }

    return path;
}

}   // namespace foundation
