
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

// Interface header.
#include "appleseed.h"

// Standard headers.
#include <cstdio>

using namespace std;

namespace foundation
{

//
// Appleseed class implementation.
//

const char* Appleseed::get_lib_name()
{
// Windows.
#if defined _WIN32

    return "appleseed.dll";

// Mac OS X.
#elif defined __APPLE__

    return "libappleseed.dylib";

// Other operating system.
#else

    return "appleseed.so";

#endif
}

const char* Appleseed::get_lib_version()
{
    static const char* LibVersionToken = "1.1.0-UNOFFICIAL";
    return LibVersionToken;
}

const char* Appleseed::get_lib_maturity_level()
{
    static const char* LibMaturityLevelToken = "alpha-4";
    return LibMaturityLevelToken;
}

size_t Appleseed::get_lib_build_number()
{
    static const size_t LibBuildNumberToken = 7618;
    return LibBuildNumberToken;
}

const char* Appleseed::get_lib_configuration()
{
#ifdef DEBUG
    return "Debug";
#else
    return "Release";
#endif
}

const char* Appleseed::get_lib_compilation_date()
{
    return __DATE__;
}

const char* Appleseed::get_lib_compilation_time()
{
    return __TIME__;
}

namespace
{
    struct SyntheticVersionString
    {
        char m_value[1024];

        SyntheticVersionString()
        {
            sprintf(
                m_value, 
                "%s version %s %s (build " FMT_SIZE_T ")",
                Appleseed::get_lib_name(),
                Appleseed::get_lib_version(),
                Appleseed::get_lib_maturity_level(),
                Appleseed::get_lib_build_number());
        }

    };

    SyntheticVersionString s_synthetic_version_string;
}

const char* Appleseed::get_synthetic_version_string()
{
    return s_synthetic_version_string.m_value;
}

}   // namespace foundation
