
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
#include "filelogtarget.h"

// Standard headers.
#include <cassert>

namespace foundation
{

//
// FileLogTarget class implementation.
//

FileLogTarget::FileLogTarget(const int options)
  : m_options(options)
  , m_file(nullptr)
{
}

FileLogTarget::~FileLogTarget()
{
    close();
}

void FileLogTarget::release()
{
    delete this;
}

void FileLogTarget::write(
    const LogMessage::Category  category,
    const char*                 file,
    const size_t                line,
    const char*                 header,
    const char*                 message)
{
    if (m_file != nullptr)
    {
        write_message(m_file, category, header, message);

        if (m_options & FlushAfterEveryMessage)
            fflush(m_file);
    }
}

bool FileLogTarget::open(const char* filename)
{
    assert(filename);

    close();

    m_file = fopen(filename, "wt");

    return m_file != nullptr;
}

void FileLogTarget::close()
{
    if (m_file)
    {
        fclose(m_file);
        m_file = nullptr;
    }
}

bool FileLogTarget::is_open() const
{
    return m_file != nullptr;
}

FileLogTarget* create_file_log_target(const int options)
{
    return new FileLogTarget(options);
}

}   // namespace foundation
