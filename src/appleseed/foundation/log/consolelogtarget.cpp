
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
#include "consolelogtarget.h"

// appleseed.foundation headers.
#include "foundation/log/filelogtargetbase.h"
#include "foundation/log/logmessage.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/console.h"

// Standard headers.
#include <cstddef>

namespace foundation
{

namespace
{
    //
    // A log target that outputs to the console.
    //

    class ConsoleLogTarget
      : public FileLogTargetBase
    {
      public:
        // Constructor.
        explicit ConsoleLogTarget(FILE* file)
          : m_file(file)
        {
        }

        // Delete this instance.
        void release() override
        {
            delete this;
        }

        // Write a message.
        void write(
            const LogMessage::Category  category,
            const char*                 file,
            const size_t                line,
            const char*                 header,
            const char*                 message) override
        {
            set_text_color(category);
            write_message(m_file, category, header, message);
            reset_text_color();
        }

      private:
        FILE* m_file;

        // Set the text color, if colors are enabled.
        void set_text_color(const LogMessage::Category category) const
        {
            switch (category)
            {
              default:
              case LogMessage::Info:
                console().set_text_color(Console::StdErr, Console::DefaultColor);
                break;

              case LogMessage::Debug:
                console().set_text_color(Console::StdErr, Console::LightGreen);
                break;

              case LogMessage::Warning:
                console().set_text_color(Console::StdErr, Console::LightMagenta);
                break;

              case LogMessage::Error:
              case LogMessage::Fatal:
                console().set_text_color(Console::StdErr, Console::LightRed);
                break;
            }
        }

        // Reset the text color, if colors are enabled.
        void reset_text_color() const
        {
            Console::instance().reset_text_color(Console::StdErr);
        }
    };
}


//
// Create an instance of a log target that outputs to the console and use colors.
//

ILogTarget* create_console_log_target(FILE* file)
{
    return new ConsoleLogTarget(file);
}

}   // namespace foundation
