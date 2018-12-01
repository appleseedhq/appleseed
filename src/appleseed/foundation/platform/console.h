
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
#include "foundation/core/concepts/singleton.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

namespace foundation
{

//
// Console class.
//

class APPLESEED_DLLSYMBOL Console
  : public Singleton<Console>
{
  public:
    // Output devices.
    enum Device
    {
        StdOut,
        StdErr,
        NumDevices      // keep at the end
    };

    // Colors.
    enum Color
    {
        DefaultColor,
        Black,
        White,
        DarkGray,
        DarkRed,
        DarkGreen,
        DarkYellow,
        DarkBlue,
        DarkMagenta,
        DarkCyan,
        LightGray,
        LightRed,
        LightGreen,
        LightYellow,
        LightBlue,
        LightMagenta,
        LightCyan
    };

    // Set the text color.
    void set_text_color(
        const Device    device,
        const Color     color);

    // Reset the text color to the default value.
    void reset_text_color(
        const Device    device);

    // Wait until the Enter key is pressed.
    static void wait_for_enter_keypress();

    // Print a message to stdout and wait until Enter is pressed.
    static void pause();

  private:
    friend class Singleton<Console>;

    struct Impl;
    Impl* impl;

    // Constructor.
    Console();

    // Destructor.
    ~Console() override;
};

APPLESEED_DLLSYMBOL Console& console();

}   // namespace foundation
