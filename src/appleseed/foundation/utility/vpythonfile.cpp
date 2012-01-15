
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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
#include "vpythonfile.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <sstream>

using namespace std;

namespace foundation
{

//
// VPythonFile class implementation.
//

VPythonFile::VPythonFile(const string& filename)
{
    m_file = fopen(filename.c_str(), "wt");

    if (m_file == 0)
        throw ExceptionIOError();

    fprintf(m_file, "from __future__ import division\n");
    fprintf(m_file, "from visual import *\n");
}

VPythonFile::~VPythonFile()
{
    fclose(m_file);
}

namespace
{
    string points_to_string(const size_t point_count, const Vector3d points[])
    {
        stringstream sstr;

        for (size_t i = 0; i < point_count; ++i)
        {
            const Vector3d& point = points[i];

            if (i > 0)
                sstr << ",";
            
            sstr << "(" << point.x << "," << point.y << "," << point.z << ")";
        }

        return sstr.str();
    }
}

void VPythonFile::draw_point(
    const Vector3d&     point,
    const string&       color,
    const size_t        size)
{
    fprintf(
        m_file,
        "points(pos=[(%f,%f,%f)], size=" FMT_SIZE_T ", color=color.%s)\n",
        point.x, point.y, point.z,
        size,
        color.c_str());
}

void VPythonFile::draw_points(
    const size_t        point_count,
    const Vector3d      points[],
    const string&       color,
    const size_t        size)
{
    fprintf(
        m_file,
        "points(pos=[%s], size=" FMT_SIZE_T ", color=color.%s)\n",
        points_to_string(point_count, points).c_str(),
        size,
        color.c_str());
}

void VPythonFile::draw_unit_square(const double thickness)
{
    fprintf(
        m_file,
        "curve(pos=[(0,0,0),(0,0,1),(1,0,1),(1,0,0),(0,0,0)], radius=%f)\n",
        thickness);
}

void VPythonFile::draw_arrow(
    const Vector3d&     from,
    const Vector3d&     to,
    const double        shaft_width)
{
    const Vector3d axis = to - from;

    fprintf(
        m_file,
        "arrow(pos=(%f,%f,%f), axis=(%f,%f,%f), shaftwidth=%f)\n",
        from.x, from.y, from.z,
        axis.x, axis.y, axis.z,
        shaft_width);
}

}   // namespace foundation
