
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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

#ifndef APPLESEED_FOUNDATION_UTILITY_MAPLEFILE_H
#define APPLESEED_FOUNDATION_UTILITY_MAPLEFILE_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdio>
#include <sstream>
#include <string>
#include <vector>

namespace foundation
{

//
// Maple plot definition.
//

class MaplePlotDef
{
  public:
    explicit MaplePlotDef(const std::string& variable);

    MaplePlotDef(const MaplePlotDef& rhs);

    MaplePlotDef& set_legend(const std::string& legend);
    MaplePlotDef& set_style(const std::string& style);
    MaplePlotDef& set_color(const std::string& color);

  private:
    friend class MapleFile;

    std::string m_variable;
    std::string m_legend;
    std::string m_style;
    std::string m_color;
};


//
// Maple input file (textual).
//

class MapleFile
  : public NonCopyable
{
  public:
    // Constructor, opens the file for writing.
    explicit MapleFile(const std::string& filename);

    // Destructor, closes the file.
    ~MapleFile();

    // Print an arbitrary string with formatting.
    void print(const char* format, ...);

    // Issue a restart command.
    void restart();

    // Issue a with() command to load a package.
    void with(const std::string& package);

    // Define an array of 2D vectors.
    template <typename T>
    void define(
        const std::string&      variable,
        const size_t            size,
        const T                 x[],
        const T                 y[]);
    template <typename T>
    void define(
        const std::string&      variable,
        const std::vector<T>&   x,
        const std::vector<T>&   y);

    // Issue a plot() command to plot a single function.
    void plot(
        const std::string&      variable,
        const std::string&      legend,
        const std::string&      style = "line",
        const std::string&      color = "black");

    // Issue a plot() command to plot several functions on the same canvas.
    void plot(const std::vector<MaplePlotDef>& plots);

  private:
    std::FILE* m_file;
};


//
// MapleFile class implementation.
//

template <typename T>
void MapleFile::define(
    const std::string&      variable,
    const size_t            size,
    const T                 x[],
    const T                 y[])
{
    assert(size > 0);

    std::stringstream sstr;

    sstr << variable << ":=[";

    for (size_t i = 0; i < size; ++i)
    {
        if (i > 0)
            sstr << ',';
        sstr << "[" << x[i] << "," << y[i] << "]";
    }

    sstr << "]:" << std::endl;

    std::fprintf(m_file, "%s", sstr.str().c_str());
}

template <typename T>
void MapleFile::define(
    const std::string&      variable,
    const std::vector<T>&   x,
    const std::vector<T>&   y)
{
    assert(!x.empty());
    assert(x.size() == y.size());

    define(variable, x.size(), &x[0], &y[0]);
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_MAPLEFILE_H
