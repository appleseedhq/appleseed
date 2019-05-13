
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
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

//
// Make sure APPLESEED_ARCH32 and APPLESEED_ARCH64 are not both defined at the same time.
//

#if defined(APPLESEED_ARCH32) && defined(APPLESEED_ARCH64)
    #error Only one of APPLESEED_ARCH32 and APPLESEED_ARCH64 must be defined.
#endif


//
// Define APPLESEED_ARCH32 or APPLESEED_ARCH64 appropriately, if necessary.
//
// This cannot be done in the CMake scripts because we might be compiling
// in 32-bit mode on a 64-bit machine.
//
// This code is inspired by the LZ4 implementation.
//

#if !defined(APPLESEED_ARCH32) && !defined(APPLESEED_ARCH64)
    #if defined __x86_64__                      || \
        defined _M_X64                          || \
        defined _WIN64                          || \
        defined __powerpc64__                   || \
        defined __ppc64__                       || \
        defined __PPC64__                       || \
        defined __64BIT__                       || \
        defined _LP64                           || \
        defined __LP64__                        || \
        defined __ia64                          || \
        defined __itanium__                     || \
        defined _M_IA64                         || \
        defined APPLESEED_DEVICE_COMPILATION
        #define APPLESEED_ARCH64
    #else
        #define APPLESEED_ARCH32
    #endif
#endif
