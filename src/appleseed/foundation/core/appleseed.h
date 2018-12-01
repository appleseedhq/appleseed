
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
#include "foundation/core/concepts/noncopyable.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

namespace foundation
{

//
// The Appleseed class provides information about the appleseed library.
//

class APPLESEED_DLLSYMBOL Appleseed
  : public NonCopyable
{
  public:
    // Return the name of the library.
    static const char* get_lib_name();

    // Return the version string of the library.
    static const char* get_lib_version();

    // Return the configuration of the library, e.g. "Debug" or "Release".
    static const char* get_lib_configuration();

    // Return the compilation date of the library.
    static const char* get_lib_compilation_date();

    // Return the compilation time of the library.
    static const char* get_lib_compilation_time();

    // Return a string listing the CPU instruction sets that are potentially taken advantage of, e.g. "SSE SSE2".
    static const char* get_lib_cpu_features();

    // Return a synthetic version string.
    static const char* get_synthetic_version_string();
};

}   // namespace foundation
