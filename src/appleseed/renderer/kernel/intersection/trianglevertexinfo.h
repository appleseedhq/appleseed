
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

// Standard headers.
#include <cstddef>
#include <cstdint>

namespace renderer
{

//
// A helper structure to locate all vertices of a triangle.
//

class TriangleVertexInfo
{
  public:
    size_t          m_vertex_index;             // index of the first vertex in the vertex array
    size_t          m_motion_segment_count;     // number of motion segments for this triangle
    std::uint32_t   m_vis_flags;                // visibility flags of this triangle

    TriangleVertexInfo() {}

    TriangleVertexInfo(
        const size_t            vertex_index,
        const size_t            motion_segment_count,
        const std::uint32_t     vis_flags)
      : m_vertex_index(vertex_index)
      , m_motion_segment_count(motion_segment_count)
      , m_vis_flags(vis_flags)
    {
    }
};

}   // namespace renderer
