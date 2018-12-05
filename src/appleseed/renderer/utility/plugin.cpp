
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "plugin.h"

// appleseed.foundation headers.
#include "foundation/platform/sharedlibrary.h"

// Standard headers.
#include <memory>
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// Plugin class implementation.
//

struct Plugin::Impl
{
    const string                m_filepath;
    unique_ptr<SharedLibrary>   m_shared_library;

    explicit Impl(const char* filepath)
      : m_filepath(filepath)
      , m_shared_library(new SharedLibrary(filepath))
    {
    }
};

Plugin::Plugin(const char* filepath)
  : impl(new Impl(filepath))
{
}

Plugin::~Plugin()
{
    delete impl;
}

const char* Plugin::get_filepath() const
{
    return impl->m_filepath.c_str();
}

void* Plugin::get_symbol(const char* name, const bool no_throw) const
{
    return impl->m_shared_library->get_symbol(name, no_throw);
}

}   // namespace renderer
