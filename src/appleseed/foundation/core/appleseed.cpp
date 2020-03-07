
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

// Interface header.
#include "appleseed.h"

// appleseed.foundation headers.
#include "foundation/core/version.h"
#include "foundation/string/string.h"

// Standard headers.
#include <sstream>
#include <string>

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

// macOS.
#elif defined __APPLE__

    return "libappleseed.dylib";

// Other platforms.
#else

    return "appleseed.so";

#endif
}

const char* Appleseed::get_lib_version()
{
    return APPLESEED_VERSION_STRING;
}

const char* Appleseed::get_lib_configuration()
{
#if defined APPLESEED_DEBUG
    return "Debug";
#elif defined APPLESEED_RELEASE
    return "Release";
#elif defined APPLESEED_PROFILE
    return "Profile";
#elif defined APPLESEED_SHIP
    return "Ship";
#else
    return "Unknown";
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
    struct LibCPUFeaturesString
    {
        std::string m_value;

        LibCPUFeaturesString()
        {
            std::stringstream sstr;

#ifdef APPLESEED_USE_SSE
            sstr << "SSE SSE2 ";
#endif

#ifdef APPLESEED_USE_SSE42
            sstr << "SSE3 SSSE3 SSE4.1 SSE4.2 ";
#endif

#ifdef APPLESEED_USE_AVX
            sstr << "AVX ";
#endif

#ifdef APPLESEED_USE_F16C
            sstr << "F16C ";
#endif

            m_value =
                sstr.str().empty()
                    ? "base instruction set"
                    : trim_right(sstr.str());
        }
    };

    LibCPUFeaturesString g_lib_cpu_features_string;
}

const char* Appleseed::get_lib_cpu_features()
{
    return g_lib_cpu_features_string.m_value.c_str();
}

namespace
{
    struct SyntheticVersionString
    {
        std::string m_value;

        SyntheticVersionString()
        {
            std::stringstream sstr;

            sstr << Appleseed::get_lib_name();
            sstr << " version ";
            sstr << Appleseed::get_lib_version();

            if (Appleseed::get_lib_cpu_features()[0] != '\0')
            {
                sstr << " (";
                sstr << Appleseed::get_lib_cpu_features();
                sstr << ")";
            }

            m_value = sstr.str();
        }
    };

    SyntheticVersionString g_synthetic_version_string;
}

const char* Appleseed::get_synthetic_version_string()
{
    return g_synthetic_version_string.m_value.c_str();
}

}   // namespace foundation
