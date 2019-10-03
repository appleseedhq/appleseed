
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "foundation/core/exceptions/exception.h"
#ifdef _WIN32
#include "foundation/platform/windows.h"
#endif

namespace foundation
{

//
// Exception thrown when a shared library cannot be loaded.
//

class ExceptionCannotLoadSharedLib
  : public Exception
{
  public:
    // Constructor.
    ExceptionCannotLoadSharedLib(
        const char* path,
        const char* error_msg);
};


//
// Exception thrown when a shared library symbol is not found.
//

class ExceptionSharedLibCannotGetSymbol
  : public Exception
{
  public:
    // Constructor.
    ExceptionSharedLibCannotGetSymbol(
        const char* symbol_name,
        const char* error_msg);
};


//
// SharedLibrary class.
//
// todo: use Pimpl idiom to avoid exposing Windows headers.
//

class SharedLibrary
  : public NonCopyable
{
  public:
    // Return the OS' default file extension for shared libraries.
    static const char* get_default_file_extension();

    // Constructor.
    explicit SharedLibrary(const char* path);

    // Destructor.
    ~SharedLibrary();

    // Get a symbol from the shared library.
    void* get_symbol(const char* name, const bool no_throw = true) const;

  private:
#ifdef _WIN32
    HMODULE m_handle;
#else
    void*   m_handle;
#endif
};

}   // namespace foundation
