
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

#pragma once

// appleseed.foundation headers.
#include "foundation/log/filelogtargetbase.h"
#include "foundation/log/logmessage.h"
#include "foundation/platform/compiler.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>
#include <cstdio>

namespace foundation
{

//
// A log target that outputs to a file.
//

class APPLESEED_DLLSYMBOL FileLogTarget
  : public FileLogTargetBase
{
  public:
    enum Options
    {
        Default                     = 0,            // none of the flags below
        FlushAfterEveryMessage      = 1UL << 0      // call fflush() on the file after every message
    };

    // Delete this instance.
    void release() override;

    // Write a message.
    void write(
        const LogMessage::Category  category,
        const char*                 file,
        const size_t                line,
        const char*                 header,
        const char*                 message) override;

    bool open(const char* filename);

    void close();

    bool is_open() const;

  private:
    friend APPLESEED_DLLSYMBOL FileLogTarget* create_file_log_target(const int options);

    const int   m_options;
    std::FILE*  m_file;

    // Constructor.
    explicit FileLogTarget(const int options);

    // Destructor.
    ~FileLogTarget() override;
};

// Create an instance of a log target that outputs to a file.
APPLESEED_DLLSYMBOL FileLogTarget* create_file_log_target(
    const int options = FileLogTarget::Options::Default);

}   // namespace foundation
