
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/platform/sharedlibrary.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/test.h"

// Standard headers.
#include <cstddef>

TEST_SUITE(Foundation_Platform_SharedLibrary)
{
    using namespace foundation;

#ifdef _WIN32
        // Not sure how to test this...
#else
    TEST_CASE(LoadSystemLibAndGetSymbol)
    {
        auto_release_ptr<SharedLibrary> sh_lib(new SharedLibrary("libdl.so"));
        void* symbol = sh_lib->get_symbol("dlopen", false);
        EXPECT_TRUE(symbol != 0);
    }

    TEST_CASE(CannotLoadSharedLibrary)
    {
        EXPECT_EXCEPTION(
            ExceptionCannotLoadSharedLib,
            auto_release_ptr<SharedLibrary>(new SharedLibrary("libdlXX.so")));
    }

    TEST_CASE(CannotGetSymbol)
    {
        auto_release_ptr<SharedLibrary> sh_lib(new SharedLibrary("libdl.so"));
        void* symbol = sh_lib->get_symbol("XdlopenXX");
        EXPECT_TRUE(symbol == 0);

        EXPECT_EXCEPTION(
            ExceptionSharedLibCannotGetSymbol,
            sh_lib->get_symbol("XdlopenXX", false));
    }
#endif
}
