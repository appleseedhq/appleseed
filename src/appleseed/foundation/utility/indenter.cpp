
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

// Interface header.
#include "indenter.h"

// Standard headers.
#include <cassert>
#include <string>

namespace foundation
{

struct Indenter::Impl
{
    size_t               m_tab_size;
    char                 m_tab_char;
    std::string          m_str;
};

Indenter::Indenter(
    const size_t    tab_size,
    const char      tab_char)
  : impl(new Impl())
{
    impl->m_tab_size = tab_size;
    impl->m_tab_char = tab_char;
}

Indenter::~Indenter()
{
    delete impl;
}

void Indenter::operator++()
{
    impl->m_str.resize(impl->m_str.size() + impl->m_tab_size, impl->m_tab_char);
}

void Indenter::operator--()
{
    assert(impl->m_str.size() >= impl->m_tab_size);

    if (impl->m_str.size() >= impl->m_tab_size)
        impl->m_str.resize(impl->m_str.size() - impl->m_tab_size, impl->m_tab_char);
    else impl->m_str.resize(0);
}

const char* Indenter::c_str() const
{
    return impl->m_str.c_str();
}

}   // namespace foundation
