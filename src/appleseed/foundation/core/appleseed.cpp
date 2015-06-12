
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/core/version.h"

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

// OS X.
#elif defined __APPLE__

    return "libappleseed.dylib";

// Other platforms.
#else

    return "appleseed.so";

#endif
}

const char* Appleseed::get_lib_version()
{
#if defined APPLESEED_USE_SSE42
    return APPLESEED_VERSION_STRING " (SSE 4.2)";
#elif defined APPLESEED_USE_SSE
    return APPLESEED_VERSION_STRING " (SSE 2)";
#else
    return APPLESEED_VERSION_STRING;
#endif
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
                "%s version %s",
                Appleseed::get_lib_name(),
                Appleseed::get_lib_version());
        }
    };

    SyntheticVersionString s_synthetic_version_string;
}

const char* Appleseed::get_synthetic_version_string()
{
    return s_synthetic_version_string.m_value;
}

}   // namespace foundation
