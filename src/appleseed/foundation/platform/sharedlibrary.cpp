
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

// Interface header.
#include "sharedlibrary.h"

// Standard headers.
#include <string>

// Platform headers.
#ifndef _WIN32
#include <dlfcn.h>
#endif

namespace foundation
{

//
// ExceptionCannotLoadSharedLib class implementation.
//

ExceptionCannotLoadSharedLib::ExceptionCannotLoadSharedLib(
    const char* path,
    const char* error_msg)
{
    std::string err("Cannot load shared library ");
    err += path;
    err += ": ";
    err += error_msg;
    set_what(err.c_str());
}


//
// ExceptionSharedLibCannotGetSymbol class implementation.
//

ExceptionSharedLibCannotGetSymbol::ExceptionSharedLibCannotGetSymbol(
    const char* symbol_name,
    const char* error_msg)
{
    std::string err("Cannot get symbol ");
    err += symbol_name;
    err += ": ";
    err += error_msg;
    set_what(err.c_str());
}


//
// SharedLibrary class implementation.
//

namespace
{
    std::string get_last_error_message()
    {
#ifdef _WIN32
        return get_last_windows_error_message();
#else
        return dlerror();
#endif
    }
}

const char* SharedLibrary::get_default_file_extension()
{
    // This method must return lower case strings.
#ifdef _WIN32
    return ".dll";
#else
    return ".so";
#endif
}

SharedLibrary::SharedLibrary(const char* path)
{
#ifdef _WIN32
    m_handle = LoadLibraryA(path);
#else
    m_handle = dlopen(path, RTLD_NOW | RTLD_GLOBAL);
#endif

    if (m_handle == nullptr)
        throw ExceptionCannotLoadSharedLib(path, get_last_error_message().c_str());
}

SharedLibrary::~SharedLibrary()
{
#ifdef _WIN32
    FreeLibrary(m_handle);
#else
    dlclose(m_handle);
#endif
}

void* SharedLibrary::get_symbol(const char* name, const bool no_throw) const
{
#ifdef _WIN32
    void* symbol = GetProcAddress(m_handle, name);
#else
    void* symbol = dlsym(m_handle, name);
#endif

    if (symbol == nullptr && !no_throw)
        throw ExceptionSharedLibCannotGetSymbol(name, get_last_error_message().c_str());

    return symbol;
}

}   // namespace foundation
