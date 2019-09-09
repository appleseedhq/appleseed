
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
#include "console.h"

// appleseed.foundation headers.
#include "foundation/platform/thread.h"
#ifdef _WIN32
#include "foundation/platform/windows.h"
#endif

// Standard headers.
#include <cassert>
#include <cstdio>
#include <iostream>


namespace foundation
{

// ------------------------------------------------------------------------------------------------
// Common code.
// ------------------------------------------------------------------------------------------------

Console& console()
{
    return Console::instance();
}

void Console::wait_for_enter_keypress()
{
    while (fgetc(stdin) != '\n')
        yield();
}

void Console::pause()
{
    std::cout << "Press Enter to continue..." << std::endl;
    wait_for_enter_keypress();
}

// ------------------------------------------------------------------------------------------------
// Windows.
// ------------------------------------------------------------------------------------------------

#ifdef _WIN32

namespace
{
    HANDLE get_device_handle(const Console::Device device)
    {
        switch (device)
        {
          default:
          case Console::StdOut:         return GetStdHandle(STD_OUTPUT_HANDLE);
          case Console::StdErr:         return GetStdHandle(STD_ERROR_HANDLE);
        }
    }

    WORD get_color_code(const Console::Color color)
    {
        switch (color)
        {
          case Console::Black:          return 0;
          case Console::White:          return FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE | FOREGROUND_INTENSITY;
          case Console::DarkGray:       return                                                       FOREGROUND_INTENSITY;
          case Console::DarkRed:        return FOREGROUND_RED;
          case Console::DarkGreen:      return                  FOREGROUND_GREEN;
          case Console::DarkYellow:     return FOREGROUND_RED | FOREGROUND_GREEN;
          case Console::DarkBlue:       return                                     FOREGROUND_BLUE;
          case Console::DarkMagenta:    return FOREGROUND_RED                    | FOREGROUND_BLUE;
          case Console::DarkCyan:       return                  FOREGROUND_GREEN | FOREGROUND_BLUE;
          case Console::LightGray:      return FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE;
          case Console::LightRed:       return FOREGROUND_RED                                      | FOREGROUND_INTENSITY;
          case Console::LightGreen:     return                  FOREGROUND_GREEN                   | FOREGROUND_INTENSITY;
          case Console::LightYellow:    return FOREGROUND_RED | FOREGROUND_GREEN                   | FOREGROUND_INTENSITY;
          case Console::LightBlue:      return                                     FOREGROUND_BLUE | FOREGROUND_INTENSITY;
          case Console::LightMagenta:   return FOREGROUND_RED                    | FOREGROUND_BLUE | FOREGROUND_INTENSITY;
          case Console::LightCyan:      return                  FOREGROUND_GREEN | FOREGROUND_BLUE | FOREGROUND_INTENSITY;
          default:                      return 0;
        }
    }
}

struct Console::Impl
{
    WORD m_default_attributes[NumDevices];
};

Console::Console()
  : impl(new Impl())
{
    CONSOLE_SCREEN_BUFFER_INFO info;

    // Save current stdout text attributes.
    GetConsoleScreenBufferInfo(get_device_handle(StdOut), &info);
    impl->m_default_attributes[StdOut] = info.wAttributes;

    // Save current stderr text attributes.
    GetConsoleScreenBufferInfo(get_device_handle(StdErr), &info);
    impl->m_default_attributes[StdErr] = info.wAttributes;
}

Console::~Console()
{
    delete impl;
}

void Console::set_text_color(
    const Device    device,
    const Color     color)
{
    assert(device < NumDevices);
    const WORD default_attributes = impl->m_default_attributes[device];

    if (color == DefaultColor)
    {
        SetConsoleTextAttribute(
            get_device_handle(device),
            default_attributes);
    }
    else
    {
        // Only set the foreground color, preserve the background color.
        // https://blogs.msdn.microsoft.com/joshpoley/2011/07/26/console-output-with-a-transparent-background-color/
        SetConsoleTextAttribute(
            get_device_handle(device),
            get_color_code(color) | (default_attributes & 0x00F0));
    }
}

void Console::reset_text_color(
    const Device    device)
{
    set_text_color(device, DefaultColor);
}

// ------------------------------------------------------------------------------------------------
// Other platforms.
// ------------------------------------------------------------------------------------------------

#else

namespace
{
    FILE* get_device_file(const Console::Device device)
    {
        switch (device)
        {
          default:
          case Console::StdOut:         return stdout;
          case Console::StdErr:         return stderr;
        }
    }

    const char* get_ansi_color_code(const Console::Color color)
    {
        switch (color)
        {
          case Console::Black:          return "\x1b[0;30m";
          case Console::White:          return "\x1b[1;37m";
          case Console::DarkGray:       return "\x1b[1;30m";
          case Console::DarkRed:        return "\x1b[1;31m";
          case Console::DarkGreen:      return "\x1b[0;32m";
          case Console::DarkYellow:     return "\x1b[0;33m";
          case Console::DarkBlue:       return "\x1b[0;34m";
          case Console::DarkMagenta:    return "\x1b[0;35m";
          case Console::DarkCyan:       return "\x1b[0;36m";
          case Console::LightGray:      return "\x1b[0;37m";
          case Console::LightRed:       return "\x1b[1;31m";
          case Console::LightGreen:     return "\x1b[1;32m";
          case Console::LightYellow:    return "\x1b[1;33m";
          case Console::LightBlue:      return "\x1b[1;34m";
          case Console::LightMagenta:   return "\x1b[1;35m";
          case Console::LightCyan:      return "\x1b[1;36m";
          default:                      return "";
        }
    }
}

Console::Console()
  : impl(nullptr)
{
}

Console::~Console()
{
}

void Console::set_text_color(
    const Device    device,
    const Color     color)
{
    if (color == DefaultColor)
         fprintf(get_device_file(device), "\x1b[0m");
    else fprintf(get_device_file(device), "%s", get_ansi_color_code(color));
}

void Console::reset_text_color(
    const Device    device)
{
    set_text_color(device, DefaultColor);
}

#endif

}   // namespace foundation
