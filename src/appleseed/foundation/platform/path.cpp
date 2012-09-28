
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

// Interface header.
#include "path.h"

// appleseed.foundation headers.
#ifdef __APPLE__
#include "foundation/platform/types.h"
#endif

// boost headers.
#include "boost/filesystem/operations.hpp"

// Platform headers.
#if defined __APPLE__
#include <mach-o/dyld.h>
#elif defined __linux__
#include <unistd.h>
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

const char* Path::get_executable_path()
{
    static char path[FOUNDATION_MAX_PATH_LENGTH + 1];
    static bool path_initialized = false;

    if (!path_initialized)
    {
// Windows.
#if defined _WIN32

	    const DWORD result =
            GetModuleFileName(
		        GetModuleHandle(NULL),
		        path,
		        sizeof(path));
        assert(result);

// Mac OS X.
#elif defined __APPLE__

        uint32 path_len = MAXPATHLEN;
        const int result = _NSGetExecutablePath(path, &path_len);
        assert(result == 0);

// Linux.
#elif defined __linux__

        ssize_t result = readlink("/proc/self/exe", path, sizeof(path));
        assert(result > 0);
        path[result] = '\0';

#endif

        path_initialized = true;
    }

    return path;
}

const char* Path::get_executable_directory()
{
    static char path[FOUNDATION_MAX_PATH_LENGTH + 1];
    static bool path_initialized = false;

    if (!path_initialized)
    {
        filesystem::path executable_path(get_executable_path());

        assert(executable_path.has_filename());
        executable_path.remove_filename();

        assert(executable_path.string().size() <= FOUNDATION_MAX_PATH_LENGTH);
        strcpy(path, executable_path.string().c_str());

        path_initialized = true;
    }

    return path;
}

}   // namespace foundation
