
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2012-2013 Esteban Tovagliari, Jupiter Jazz Limited
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

#if defined _MSC_VER
    // C4244: conversion from 'Py_ssize_t' to 'unsigned int', possible loss of data
    __pragma(warning(push))             \
    __pragma(warning(disable: 4244))

    // Redefined in pyconfig.h.
    #undef copysign
#endif

// Boost headers.
#include "boost/python/detail/wrap_python.hpp"  // has to be first, to avoid redefinition warnings
#include "boost/python.hpp"

#if defined _MSC_VER
    __pragma(warning(pop))
#endif

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"

namespace foundation
{

// This is needed for Boost.Python to consider foundation::auto_release_ptr<> as a smart pointer.
// When defined, you can use foundation::auto_release_ptr<> as a holder in bpy::class<>.
template <typename T>
T* get_pointer(const auto_release_ptr<T>& p)
{
    return p.get();
}

}   // namespace foundation
