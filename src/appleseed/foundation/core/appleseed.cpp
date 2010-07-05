
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
#include "appleseed.h"

namespace foundation
{

//
// Library information.
//

// Return the name of the library.
const char* Appleseed::get_lib_name()
{
// Windows.
#if defined _WIN32

    return "appleseed.dll";

// Mac OS X.
#elif defined __APPLE__

    return "appleseed.dylib";

// Other operating system.
#else

    return "appleseed.so";

#endif
}

// Return the version string of the library.
const char* Appleseed::get_lib_version()
{
    return "1.1.0-UNOFFICIAL";
}

// Return the maturity level of the library.
const char* Appleseed::get_lib_maturity_level()
{
    return "alpha";
}

// Return the build number of the library.
size_t Appleseed::get_lib_build_number()
{
    // This works as follow: the build number is stored in a static variable
    // whose name is unique (the text after build_number_ is a GUID).
    // Before compilation begins, a tool searches for this variable declaration
    // (in this file) and increases the build number by 1.
    static const size_t build_number_335A07D9_68C1_4E21_86FA_60C64BD6FE6C = 7605;
    return build_number_335A07D9_68C1_4E21_86FA_60C64BD6FE6C;
}

// Return the configuration of the library.
const char* Appleseed::get_lib_configuration()
{
#ifdef DEBUG
    return "Debug";
#else
    return "Release";
#endif
}

// Return the compilation date of the library.
const char* Appleseed::get_lib_compilation_date()
{
    return __DATE__;
}

// Return the compilation time of the library.
const char* Appleseed::get_lib_compilation_time()
{
    return __TIME__;
}

}   // namespace foundation
