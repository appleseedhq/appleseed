
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
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/aabb.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>
#include <cstdio>
#include <string>

namespace foundation
{

//
// VPython program file.
//
// See http://vpython.org/ for more information about VPython.
//

class VPythonFile
  : public NonCopyable
{
  public:
    // Constructor, opens the file for writing.
    explicit VPythonFile(const std::string& filename);

    // Destructor, closes the file.
    ~VPythonFile();

    // Close the file.
    void close();

    // Output a single point.
    void draw_point(
        const Vector3d&     point,
        const char*         color = "color.white",
        const size_t        size = 5);

    // Output a collection of points.
    void draw_points(
        const size_t        point_count,
        const Vector3d      points[],
        const char*         color = "color.white",
        const size_t        size = 5);

    // Output a polyline.
    void draw_polyline(
        const size_t        point_count,
        const Vector3d      points[],
        const char*         color = "color.white",
        const double        thickness = 0.001);

    // Output the unit square in the XZ plane.
    void draw_unit_square(
        const char*         color = "color.white",
        const double        thickness = 0.001);

    // Output an arrow.
    void draw_arrow(
        const Vector3d&     from,
        const Vector3d&     to,
        const char*         color = "color.white",
        const double        shaft_width = 0.001);

    // Output the canonical axes.
    void draw_axes(const double shaft_width = 0.001);

    // Output a bounding box.
    void draw_aabb(
        const AABB3d&       bbox,
        const char*         color = "color.white",
        const double        thickness = 0.001);

    // Output a triangle.
    void draw_triangle(
        const Vector3d&     v0,
        const Vector3d&     v1,
        const Vector3d&     v2,
        const char*         color = "color.white");

  private:
    std::FILE* m_file;
};

}   // namespace foundation
