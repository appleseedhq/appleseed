
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

#ifndef APPLESEED_FOUNDATION_CORE_APPLESEED_H
#define APPLESEED_FOUNDATION_CORE_APPLESEED_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// Standard headers.
#include <cstddef>

//
// On Windows, define FOUNDATIONDLL to __declspec(dllexport) when building the DLL
// and to __declspec(dllimport) when building an application using the DLL.
// Other platforms don't use this export mechanism and the symbol FOUNDATIONDLL is
// defined to evaluate to nothing.
//

#ifndef FOUNDATIONDLL
#ifdef _WIN32
#ifdef APPLESEED_FOUNDATION_EXPORTS
#define FOUNDATIONDLL __declspec(dllexport)
#else
#define FOUNDATIONDLL __declspec(dllimport)
#endif
#else
#define FOUNDATIONDLL
#endif
#endif

namespace foundation
{

//
// The Appleseed class provides information about the appleseed library,
// such as the version or build numbers.
//

class FOUNDATIONDLL Appleseed
  : public NonCopyable
{
  public:
    // Return the name of the library.
    static const char* get_lib_name();

    // Return the version string of the library.
    // The version string strictly follows the pattern generation.release.patch
    // where generation is the generation number, release is the release number
    // withing that generation, and patch is the patch number or bug fix within
    // this release. generation and release numbers start at 1, patch number
    // starts at 0. The initial version is thus 1.1.0.
    static const char* get_lib_version();

    // Return the maturity level of the library.
    // Amongst possible values are "pre-alpha", "alpha", "beta", etc.
    static const char* get_lib_maturity_level();

    // Return the build number of the library.
    // Build numbers are guaranteed to be strictly increasing over time.
    // The first build number is 1.
    static size_t get_lib_build_number();

    // Return the configuration of the library.
    // Amongst possible values are "Debug", "Release", etc.
    static const char* get_lib_configuration();

    // Return the compilation date of the library.
    static const char* get_lib_compilation_date();

    // Return the compilation time of the library.
    static const char* get_lib_compilation_time();
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_CORE_APPLESEED_H
