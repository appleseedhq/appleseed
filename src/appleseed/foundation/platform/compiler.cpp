
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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
#include "compiler.h"

namespace foundation
{

//
// Compiler class implementation.
//

const char* Compiler::get_compiler_name()
{
// Visual C++.
#if defined _MSC_VER
    return "Microsoft Visual C++";

// Clang.
#elif defined __clang__
    return "clang";

// gcc.
#elif defined __GNUC__
    return "gcc";

// Other compilers.
#else
    return "(Unknown Compiler)";
#endif
}

const char* Compiler::get_compiler_version()
{
// Visual C++.
#if defined _MSC_VER
    return APPLESEED_TO_STRING_EVAL(_MSC_VER);

// Clang.
#elif defined __clang__
    return APPLESEED_TO_STRING_EVAL(__clang_major__) "."
           APPLESEED_TO_STRING_EVAL(__clang_minor__) "."
           APPLESEED_TO_STRING_EVAL(__clang_patchlevel__);

// gcc.
#elif defined __GNUC__
    return APPLESEED_TO_STRING_EVAL(__GNUC__) "."
           APPLESEED_TO_STRING_EVAL(__GNUC_MINOR__) "."
           APPLESEED_TO_STRING_EVAL(__GNUC_PATCHLEVEL__);

// Other compilers.
#else
    return "(Unknown Compiler Version)";
#endif
}

}   // namespace foundation
