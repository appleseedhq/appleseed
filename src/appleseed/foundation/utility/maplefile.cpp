
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
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/platform/snprintf.h"

// Standard headers.
#include <cstdarg>

using namespace std;

namespace foundation
{

//
// MaplePlotDef class implementation.
//

MaplePlotDef::MaplePlotDef(const string& variable)
  : m_variable(variable)
  , m_style("line")
  , m_color("black")
{
}

MaplePlotDef::MaplePlotDef(const MaplePlotDef& rhs)
  : m_variable(rhs.m_variable)
  , m_legend(rhs.m_legend)
  , m_style(rhs.m_style)
  , m_color(rhs.m_color)
{
}

MaplePlotDef& MaplePlotDef::set_legend(const string& legend)
{
    m_legend = legend;
    return *this;
}

MaplePlotDef& MaplePlotDef::set_style(const string& style)
{
    m_style = style;
    return *this;
}

MaplePlotDef& MaplePlotDef::set_color(const string& color)
{
    m_color = color;
    return *this;
}


//
// MapleFile class implementation.
//

MapleFile::MapleFile(const string& filename)
{
    m_file = fopen(filename.c_str(), "wt");

    if (m_file == 0)
        throw ExceptionIOError();
}

MapleFile::~MapleFile()
{
    fclose(m_file);
}

void MapleFile::print(const char* format, ...)
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

void MapleFile::restart()
{
    fprintf(m_file, "restart:\n");
}

void MapleFile::with(const string& package)
{
    fprintf(m_file, "with(%s):\n", package.c_str());
}

void MapleFile::plot(
    const string&   variable,
    const string&   legend,
    const string&   style,
    const string&   color)
{
    fprintf(
        m_file,
        "plot([%s],legend=[\"%s\"],style=[%s],color=[%s]);\n",
        variable.c_str(),
        legend.c_str(),
        style.c_str(),
        color.c_str());
}

void MapleFile::plot(const vector<MaplePlotDef>& plots)
{
    stringstream sstr;

    sstr << "plot([";

    for (size_t i = 0; i < plots.size(); ++i)
    {
        if (i > 0)
            sstr << ",";
        sstr << plots[i].m_variable;
    }

    sstr << "],legend=[";

    for (size_t i = 0; i < plots.size(); ++i)
    {
        if (i > 0)
            sstr << ",";
        sstr << "\"" << plots[i].m_legend << "\"";
    }

    sstr << "],style=[";

    for (size_t i = 0; i < plots.size(); ++i)
    {
        if (i > 0)
            sstr << ",";
        sstr << plots[i].m_style;
    }

    sstr << "],color=[";

    for (size_t i = 0; i < plots.size(); ++i)
    {
        if (i > 0)
            sstr << ",";
        sstr << plots[i].m_color;
    }

    sstr << "]);" << endl;

    fprintf(m_file, "%s", sstr.str().c_str());
}

}   // namespace foundation
