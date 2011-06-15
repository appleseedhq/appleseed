
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

#ifndef APPLESEED_FOUNDATION_UTILITY_MAPLEFILE_H
#define APPLESEED_FOUNDATION_UTILITY_MAPLEFILE_H

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/core/exceptions/exception.h"

// Standard headers.
#include <cstddef>
#include <cstdio>
#include <string>

namespace foundation
{

//
// Maple input file (textual).
//

class MapleFile
  : public NonCopyable
{
  public:
    // Exception thrown when an I/O error occur.
    struct ExceptionIOError
      : public Exception
    {
    };

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
        const std::string&  variable,
        const size_t        size,
        const T             x[],
        const T             y[]);

    // Issue a plot() command to plot a function.
    void plot(
        const std::string&  variable1,
        const std::string&  color1,
        const std::string&  legend1);
    void plot(
        const std::string&  variable1,
        const std::string&  color1,
        const std::string&  legend1,
        const std::string&  variable2,
        const std::string&  color2,
        const std::string&  legend2);
    void plot(
        const std::string&  variable1,
        const std::string&  color1,
        const std::string&  legend1,
        const std::string&  variable2,
        const std::string&  color2,
        const std::string&  legend2,
        const std::string&  variable3,
        const std::string&  color3,
        const std::string&  legend3);

  private:
    std::FILE*  m_file;
    bool        m_io_error;
};


//
// MapleFile class implementation.
//

// Define an array of 2D vectors.
template <typename T>
void MapleFile::define(
    const std::string&      variable,
    const size_t            size,
    const T                 x[],
    const T                 y[])
{
    print("%s:=[", variable.c_str());

    for (size_t i = 0; i < size; ++i)
    {
        if (i > 0)
            print(",");
        print("[%f,%f]", x[i], y[i]);
    }

    print("]:\n");
}

}       // namespace foundation

#endif  // !APPLESEED_FOUNDATION_UTILITY_MAPLEFILE_H
