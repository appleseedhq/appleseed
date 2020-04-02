
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
#include "foundation/log/log.h"

// Forward declarations.
namespace foundation { class Dictionary; }

namespace appleseed {
namespace common {

class SuperLogger
  : public foundation::Logger
{
  public:
    // Constructor.
    SuperLogger();

    // Destructor.
    ~SuperLogger() override;

    // Retrieve the current log target.
    foundation::ILogTarget& get_log_target() const;

    // Replace the current log target.
    void set_log_target(foundation::ILogTarget* log_target);

    // Replace the current log target by one that supports message coloring.
    void enable_message_coloring();

    // Set the verbosity level.
    void set_verbosity_level_from_string(const char* level_name, const bool warn_if_invalid = true);

    // Apply a collection of settings to this logger.
    void configure_from_settings(const foundation::Dictionary& settings);

  private:
    foundation::ILogTarget* m_log_target;
};

}   // namespace common
}   // namespace appleseed
