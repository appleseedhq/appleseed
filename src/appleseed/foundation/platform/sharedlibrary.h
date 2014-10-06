
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

#ifndef APPLESEED_FOUNDATION_PLATFORM_SHARED_LIBRARY_H
#define APPLESEED_FOUNDATION_PLATFORM_SHARED_LIBRARY_H

// appleseed.foundation headers.
#include "foundation/core/concepts/iunknown.h"
#include "foundation/core/exceptions/exception.h"
#include "foundation/platform/compiler.h"

// appleseed.main headers.
#include "main/dllsymbol.h"


namespace foundation
{

//
// Cannot load shared lib exception.
//

class DLLSYMBOL ExceptionCannotLoadSharedLib
  : public Exception
{
  public:
    // Constructor.
    ExceptionCannotLoadSharedLib(
        const char* path,
        const char* error_msg);
};


//
// Shared library cannot get symbol exception.
//

class DLLSYMBOL ExceptionSharedLibCannotGetSymbol
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

class DLLSYMBOL SharedLibrary
  : public IUnknown
{
  public:
    // Constructor.
    explicit SharedLibrary(const char* path);

    // Delete this instance.
    virtual void release() OVERRIDE;

    // Get a symbol from the shared library
    void* get_symbol(const char* name, bool no_throw = true) const;

  private:
    struct Impl;
    Impl* impl;

    // Destructor.
    ~SharedLibrary();
};

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_PLATFORM_SHARED_LIBRARY_H
