
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

// Interface header.
#include "maplefile.h"

// appleseed.foundation headers.
#include "foundation/platform/snprintf.h"

// Standard headers.
#include <cstdarg>

using namespace std;

namespace foundation
{

//
// MapleFile class implementation.
//

// Constructor, opens the file for writing.
MapleFile::MapleFile(
    const string&   filename)
{
    m_file = fopen(filename.c_str(), "wt");
    if (m_file == 0)
        throw ExceptionIOError();
}

// Destructor, closes the file.
MapleFile::~MapleFile()
{
    fclose(m_file);
}

// Print an arbitrary string with formatting.
void MapleFile::print(
    const char*     format, ...)
{
    // Size in bytes of the temporary text buffer.
    static const size_t BufferSize = 4096;

    // Print the formatted message into a temporary C string.
    va_list argptr;
    va_start(argptr, format);
    char text[BufferSize];
    portable_vsnprintf(text, BufferSize, format, argptr);

    // Print the result into the file.
    fprintf(m_file, "%s", text);
}

// Issue a restart command.
void MapleFile::restart()
{
    print("restart:\n");
}

// Issue a with() command to load a package.
void MapleFile::with(
    const string&   package)
{
    print("with(%s):\n", package.c_str());
}

// Issue a plot() command to plot a function.
void MapleFile::plot(
    const string&   variable1,
    const string&   color1,
    const string&   legend1)
{
    print(
        "plot([%s],color=[%s],legend=[\"%s\"]);\n",
        variable1.c_str(),
        color1.c_str(),
        legend1.c_str());
}
void MapleFile::plot(
    const string&   variable1,
    const string&   color1,
    const string&   legend1,
    const string&   variable2,
    const string&   color2,
    const string&   legend2)
{
    print(
        "plot([%s,%s],color=[%s,%s],legend=[\"%s\",\"%s\"]);\n",
        variable1.c_str(),
        variable2.c_str(),
        color1.c_str(),
        color2.c_str(),
        legend1.c_str(),
        legend2.c_str());
}
void MapleFile::plot(
    const string&   variable1,
    const string&   color1,
    const string&   legend1,
    const string&   variable2,
    const string&   color2,
    const string&   legend2,
    const string&   variable3,
    const string&   color3,
    const string&   legend3)
{
    print(
        "plot([%s,%s,%s],color=[%s,%s,%s],legend=[\"%s\",\"%s\",\"%s\"]);\n",
        variable1.c_str(),
        variable2.c_str(),
        variable3.c_str(),
        color1.c_str(),
        color2.c_str(),
        color3.c_str(),
        legend1.c_str(),
        legend2.c_str(),
        legend3.c_str());
}

}   // namespace foundation
