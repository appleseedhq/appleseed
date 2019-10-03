
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
#include "vpythonfile.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/platform/types.h"

// Standard headers.
#include <sstream>

namespace foundation
{

//
// VPythonFile class implementation.
//

VPythonFile::VPythonFile(const std::string& filename)
{
    m_file = fopen(filename.c_str(), "wt");

    if (m_file == nullptr)
        throw ExceptionIOError();

    fprintf(m_file, "from __future__ import division\n");
    fprintf(m_file, "from vpython import *\n");
}

VPythonFile::~VPythonFile()
{
    close();
}

void VPythonFile::close()
{
    if (m_file)
    {
        fclose(m_file);
        m_file = nullptr;
    }
}

namespace
{
    std::string points_to_string(const size_t point_count, const Vector3d points[])
    {
        std::stringstream sstr;

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
    const char*         color,
    const size_t        size)
{
    fprintf(
        m_file,
        "points(pos=[(%f,%f,%f)], radius=" FMT_SIZE_T ", color=%s)\n",
        point.x, point.y, point.z,
        size,
        color);
}

void VPythonFile::draw_points(
    const size_t        point_count,
    const Vector3d      points[],
    const char*         color,
    const size_t        size)
{
    fprintf(
        m_file,
        "points(pos=[%s], radius=" FMT_SIZE_T ", color=%s)\n",
        points_to_string(point_count, points).c_str(),
        size,
        color);
}

void VPythonFile::draw_polyline(
    const size_t        point_count,
    const Vector3d      points[],
    const char*         color,
    const double        thickness)
{
    fprintf(
        m_file,
        "curve(pos=[%s], radius=%f, color=%s)\n",
        points_to_string(point_count, points).c_str(),
        thickness,
        color);
}

void VPythonFile::draw_unit_square(
    const char*         color,
    const double        thickness)
{
    fprintf(
        m_file,
        "curve(pos=[(0,0,0),(0,0,1),(1,0,1),(1,0,0),(0,0,0)], radius=%f, color=%s)\n",
        thickness,
        color);
}

void VPythonFile::draw_arrow(
    const Vector3d&     from,
    const Vector3d&     to,
    const char*         color,
    const double        shaft_width)
{
    const Vector3d axis = to - from;

    fprintf(
        m_file,
        "arrow(pos=vec(%f,%f,%f), axis=vec(%f,%f,%f), shaftwidth=%f, fixedwidth=True, color=%s)\n",
        from.x, from.y, from.z,
        axis.x, axis.y, axis.z,
        shaft_width,
        color);
}

void VPythonFile::draw_axes(const double shaft_width)
{
    draw_arrow(Vector3d(0.0), Vector3d(1.0, 0.0, 0.0), "color.red", shaft_width);
    draw_arrow(Vector3d(0.0), Vector3d(0.0, 1.0, 0.0), "color.green", shaft_width);
    draw_arrow(Vector3d(0.0), Vector3d(0.0, 0.0, 1.0), "color.blue", shaft_width);
}

void VPythonFile::draw_aabb(
    const AABB3d&       bbox,
    const char*         color,
    const double        thickness)
{
    fprintf(
        m_file,
        "curve(pos=[(%f,%f,%f),(%f,%f,%f),(%f,%f,%f),(%f,%f,%f),(%f,%f,%f)], radius=%f, color=%s)\n",
        bbox.min.x, bbox.min.y, bbox.min.z,
        bbox.max.x, bbox.min.y, bbox.min.z,
        bbox.max.x, bbox.max.y, bbox.min.z,
        bbox.min.x, bbox.max.y, bbox.min.z,
        bbox.min.x, bbox.min.y, bbox.min.z,
        thickness,
        color);

    fprintf(
        m_file,
        "curve(pos=[(%f,%f,%f),(%f,%f,%f),(%f,%f,%f),(%f,%f,%f),(%f,%f,%f)], radius=%f, color=%s)\n",
        bbox.min.x, bbox.min.y, bbox.max.z,
        bbox.max.x, bbox.min.y, bbox.max.z,
        bbox.max.x, bbox.max.y, bbox.max.z,
        bbox.min.x, bbox.max.y, bbox.max.z,
        bbox.min.x, bbox.min.y, bbox.max.z,
        thickness,
        color);

    fprintf(
        m_file,
        "curve(pos=[(%f,%f,%f),(%f,%f,%f)], radius=%f, color=%s)\n",
        bbox.min.x, bbox.min.y, bbox.min.z,
        bbox.min.x, bbox.min.y, bbox.max.z,
        thickness,
        color);

    fprintf(
        m_file,
        "curve(pos=[(%f,%f,%f),(%f,%f,%f)], radius=%f, color=%s)\n",
        bbox.max.x, bbox.min.y, bbox.min.z,
        bbox.max.x, bbox.min.y, bbox.max.z,
        thickness,
        color);

    fprintf(
        m_file,
        "curve(pos=[(%f,%f,%f),(%f,%f,%f)], radius=%f, color=%s)\n",
        bbox.max.x, bbox.max.y, bbox.min.z,
        bbox.max.x, bbox.max.y, bbox.max.z,
        thickness,
        color);

    fprintf(
        m_file,
        "curve(pos=[(%f,%f,%f),(%f,%f,%f)], radius=%f, color=%s)\n",
        bbox.min.x, bbox.max.y, bbox.min.z,
        bbox.min.x, bbox.max.y, bbox.max.z,
        thickness,
        color);
}

void VPythonFile::draw_triangle(
    const Vector3d&     v0,
    const Vector3d&     v1,
    const Vector3d&     v2,
    const char*         color)
{
    fprintf(
        m_file,
        "triangle(vs=[vertex(pos=vec(%f,%f,%f)),vertex(pos=vec(%f,%f,%f)),vertex(pos=vec(%f,%f,%f))], color=%s)\n",
        v0.x, v0.y, v0.z,
        v1.x, v1.y, v1.z,
        v2.x, v2.y, v2.z,
        color);
}

}   // namespace foundation
