
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
#include "stringlogtarget.h"

// appleseed.foundation headers.
#include "foundation/string/string.h"
#include "foundation/utility/foreach.h"

// Standard headers.
#include <string>
#include <vector>

namespace foundation
{

struct StringLogTarget::Impl
{
    std::string m_str;
};

StringLogTarget::StringLogTarget()
  : impl(new Impl())
{
}

StringLogTarget::~StringLogTarget()
{
    delete impl;
}

void StringLogTarget::release()
{
    delete this;
}

void StringLogTarget::write(
    const LogMessage::Category  category,
    const char*                 file,
    const size_t                line,
    const char*                 header,
    const char*                 message)
{
    std::vector<std::string> lines;
    split(message, "\n", lines);

    for (const_each<std::vector<std::string>> i = lines; i; ++i)
    {
        impl->m_str.append(header);
        impl->m_str.append(*i);
        impl->m_str.append("\n");
    }
}

const char* StringLogTarget::get_string() const
{
    return impl->m_str.c_str();
}

StringLogTarget* create_string_log_target()
{
    return new StringLogTarget();
}

}   // namespace foundation
