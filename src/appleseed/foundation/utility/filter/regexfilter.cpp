
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "regexfilter.h"

// Boost headers.
#include "boost/regex.hpp"

// Standard headers.
#include <cassert>

using namespace boost;

namespace foundation
{

struct RegExFilter::Impl
{
    regex   m_expression;
    bool    m_valid;
};

RegExFilter::RegExFilter()
  : impl(new Impl())
{
    impl->m_valid = false;
}

RegExFilter::RegExFilter(const char* pattern, const CaseSensivity case_sensivity)
  : impl(new Impl())
{
    set_pattern(pattern, case_sensivity);
}

RegExFilter::~RegExFilter()
{
    delete impl;
}

void RegExFilter::set_pattern(const char* pattern, const CaseSensivity case_sensivity)
{
    assert(pattern);

    try
    {
        if (case_sensivity == CaseSensitive)
            impl->m_expression.assign(pattern);
        else impl->m_expression.assign(pattern, regex_constants::icase);

        impl->m_valid = !impl->m_expression.empty();
    }
    catch (const regex_error&)
    {
        impl->m_valid = false;
    }
}

bool RegExFilter::is_valid() const
{
    return impl->m_valid;
}

bool RegExFilter::accepts(const char* name) const
{
    assert(impl->m_valid);
    assert(name);

    return regex_search(name, impl->m_expression);
}

}   // namespace foundation
