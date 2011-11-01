
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

#ifndef MAKETILEDEXR_COMMANDLINE_H
#define MAKETILEDEXR_COMMANDLINE_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/utility/commandlineparser.h"

// Standard headers.
#include <string>

// Forward declarations.
namespace foundation    { class Logger; }

//
// Command line handler.
//

class CommandLine
  : public foundation::NonCopyable
{
  public:
    foundation::FlagOptionHandler                   m_help;
    foundation::FlagOptionHandler                   m_message_coloring;
    foundation::FlagOptionHandler                   m_progress_messages;
    foundation::ValueOptionHandler<std::string>     m_filenames;
    foundation::ValueOptionHandler<int>             m_tile_size;

    // Constructor.
    CommandLine();

    // Parse program command line.
    void parse(const int argc, const char* argv[]);

  private:
    foundation::CommandLineParser m_parser;

    // Print program usage.
    void print_program_usage(
        const char*         program_name,
        foundation::Logger& logger) const;
};

#endif  // !MAKETILEDEXR_COMMANDLINE_H
