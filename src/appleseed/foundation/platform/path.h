
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
#ifdef _WIN32
#include "foundation/platform/windows.h"
#endif
#include "foundation/utility/iterators.h"
#include "foundation/string/string.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Boost headers.
#include "boost/filesystem/exception.hpp"
#include "boost/filesystem/operations.hpp"
#include "boost/filesystem/path.hpp"
#include "boost/filesystem/exception.hpp"

// Standard headers.
#include <cassert>

// Platform headers.
#if defined __APPLE__ || defined __FreeBSD__
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

// macOS.
#elif defined __APPLE__ || defined __FreeBSD__
    #define FOUNDATION_MAX_PATH_LENGTH  MAXPATHLEN

// Linux.
#elif defined __linux__
    #define FOUNDATION_MAX_PATH_LENGTH  PATH_MAX

// Unsupported platform.
#else
    #error Unsupported platform.
#endif


//
// Common paths.
//

// Return the path to the application's executable. NOT thread-safe.
APPLESEED_DLLSYMBOL const char* get_executable_path();

// Return the path to the directory containing the application's executable. NOT thread-safe.
APPLESEED_DLLSYMBOL const char* get_executable_directory();

// Return the path to the user's home directory.
APPLESEED_DLLSYMBOL const char* get_home_directory();


//
// Operations on boost::filesystem::path objects.
//

// Try to call boost::filesystem::weakly_canonical() on a given path such that it won't
// fail if the path does not exist, then call boost::filesystem::path::make_preferred()
// on the result.
boost::filesystem::path safe_weakly_canonical(const boost::filesystem::path& p);

// Return true if a path has a non-empty extension.
bool has_extension(const boost::filesystem::path& p);

// Split two paths into a common base path and two relative paths.
void split_paths(
    const boost::filesystem::path&  p1,
    const boost::filesystem::path&  p2,
    boost::filesystem::path&        common,
    boost::filesystem::path&        r1,
    boost::filesystem::path&        r2);

// Find the next available file path by searching for the first path that does not
// refer to an existing file on disk, and where the path follows the given pattern
// where consecutive '#' characters have been replaced by increasing integer values
// starting with 1.
boost::filesystem::path find_next_available_path(const boost::filesystem::path& p);


//
// Inline implementation of functions using boost::filesystem to allow using them
// outside of appleseed's shared library (for instance in appleseed.studio).
//

inline boost::filesystem::path safe_weakly_canonical(const boost::filesystem::path& p)
{
    auto result = p;

    try
    {
        result = boost::filesystem::weakly_canonical(result);
    }
    catch (const boost::filesystem::filesystem_error&)
    {
    }

    return result.make_preferred();
}

inline bool has_extension(const boost::filesystem::path& p)
{
    const auto ext = p.extension();
    return !ext.empty() && ext != ".";
}

inline void split_paths(
    const boost::filesystem::path&  p1,
    const boost::filesystem::path&  p2,
    boost::filesystem::path&        common,
    boost::filesystem::path&        r1,
    boost::filesystem::path&        r2)
{
    assert(common.empty());
    assert(r1.empty());
    assert(r2.empty());

    auto i1 = p1.begin();
    auto i2 = p2.begin();

    while (i1 != p1.end() && i2 != p2.end())
    {
        if (*i1 != *i2)
            break;

        if ((p1.has_filename() && succ(i1) == p1.end()) ||
            (p2.has_filename() && succ(i2) == p2.end()))
            break;

        common /= *i1;

        ++i1, ++i2;
    }

    while (i1 != p1.end())
        r1 /= *i1++;

    while (i2 != p2.end())
        r2 /= *i2++;
}

inline boost::filesystem::path find_next_available_path(const boost::filesystem::path& p)
{
    const auto pattern = p.string();
    const auto max_value = get_numbered_string_max_value(pattern);

    for (size_t value = 1; value <= max_value; ++value)
    {
        const boost::filesystem::path candidate(get_numbered_string(pattern, value));

        if (!boost::filesystem::exists(candidate))
            return candidate;
    }

    return boost::filesystem::path(get_numbered_string(pattern, 1));
}

}   // namespace foundation
