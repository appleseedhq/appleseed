
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
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

// appleseed.common headers.
#include "application/commandlinehandlerbase.h"

// appleseed.foundation headers.
#include "foundation/utility/commandlineparser.h"

// Standard headers.
#include <string>

// Forward declarations.
namespace appleseed { namespace common { class SuperLogger; } }

namespace appleseed {
namespace dumpmetadata {

//
// Command line handler.
//

class CommandLineHandler
  : public common::CommandLineHandlerBase
{
  public:
    foundation::ValueOptionHandler<std::string> m_format;

    // Constructor.
    CommandLineHandler();

  private:
    // Emit usage instructions to the logger.
    void print_program_usage(
        const char*             executable_name,
        common::SuperLogger&    logger) const override;
};

}   // namespace dumpmetadata
}   // namespace appleseed
