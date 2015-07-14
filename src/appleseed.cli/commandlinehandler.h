
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

#ifndef APPLESEED_CLI_COMMANDLINEHANDLER_H
#define APPLESEED_CLI_COMMANDLINEHANDLER_H

// appleseed.foundation headers.
#include "foundation/utility/commandlineparser.h"

// appleseed.shared headers.
#include "application/commandlinehandlerbase.h"

// Standard headers.
#include <string>

// Forward declarations.
namespace appleseed { namespace shared { class SuperLogger; } }

namespace appleseed {
namespace cli {

//
// Command line handler.
//

class CommandLineHandler
  : public shared::CommandLineHandlerBase
{
  public:
    // Input file.
    foundation::ValueOptionHandler<std::string>     m_filename;

    // General options.
    foundation::ValueOptionHandler<std::string>     m_configuration;
    foundation::ValueOptionHandler<std::string>     m_params;
#if defined __APPLE__ || defined _WIN32
    foundation::FlagOptionHandler                   m_display_output;
#endif
    foundation::FlagOptionHandler                   m_disable_autosave;

    // Aliases for rendering options.
    foundation::ValueOptionHandler<std::string>     m_threads;  // std::string because we need to handle 'auto'
    foundation::ValueOptionHandler<std::string>     m_output;
    foundation::FlagOptionHandler                   m_continuous_saving;
    foundation::ValueOptionHandler<int>             m_resolution;
    foundation::ValueOptionHandler<int>             m_window;
    foundation::ValueOptionHandler<int>             m_samples;
    foundation::ValueOptionHandler<int>             m_passes;
    foundation::ValueOptionHandler<std::string>     m_override_shading;
    foundation::ValueOptionHandler<std::string>     m_select_object_instances;

    // Houdini-related options.
    foundation::FlagOptionHandler                   m_mplay_display;
    foundation::ValueOptionHandler<int>             m_hrmanpipe_display;

    // Developer-oriented options.
    foundation::ValueOptionHandler<std::string>     m_run_unit_tests;
    foundation::ValueOptionHandler<std::string>     m_run_unit_benchmarks;
    foundation::FlagOptionHandler                   m_verbose_unit_tests;
    foundation::FlagOptionHandler                   m_benchmark_mode;

    // Constructor.
    CommandLineHandler();

  private:
    // Emit usage instructions to the logger.
    virtual void print_program_usage(
        const char*             program_name,
        shared::SuperLogger&    logger) const;
};

}       // namespace cli
}       // namespace appleseed

#endif  // !APPLESEED_CLI_COMMANDLINEHANDLER_H
