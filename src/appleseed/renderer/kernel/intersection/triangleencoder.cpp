
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
#include "triangleencoder.h"

// appleseed.renderer headers.
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/kernel/intersection/trianglevertexinfo.h"

// appleseed.foundation headers.
#include "foundation/memory/memory.h"

// Standard headers.
#include <cstdint>

using namespace foundation;

namespace renderer
{

size_t TriangleEncoder::compute_size(
    const std::vector<TriangleVertexInfo>&  triangle_vertex_infos,
    const std::vector<size_t>&              triangle_indices,
    const size_t                            item_begin,
    const size_t                            item_count)
{
    size_t size = 0;

    for (size_t i = 0; i < item_count; ++i)
    {
        const size_t triangle_index = triangle_indices[item_begin + i];
        const TriangleVertexInfo& vertex_info = triangle_vertex_infos[triangle_index];

        size += sizeof(std::uint32_t);      // visibility flags
        size += sizeof(std::uint32_t);      // motion segment count

        if (vertex_info.m_motion_segment_count == 0)
            size += sizeof(GTriangleType);
        else size += (vertex_info.m_motion_segment_count + 1) * 3 * sizeof(GVector3);
    }

    return size;
}

void TriangleEncoder::encode(
    const std::vector<TriangleVertexInfo>&  triangle_vertex_infos,
    const std::vector<GVector3>&            triangle_vertices,
    const std::vector<size_t>&              triangle_indices,
    const size_t                            item_begin,
    const size_t                            item_count,
    MemoryWriter&                           writer)
{
    for (size_t i = 0; i < item_count; ++i)
    {
        const size_t triangle_index = triangle_indices[item_begin + i];
        const TriangleVertexInfo& vertex_info = triangle_vertex_infos[triangle_index];

        writer.write(vertex_info.m_vis_flags);
        writer.write(static_cast<std::uint32_t>(vertex_info.m_motion_segment_count));

        if (vertex_info.m_motion_segment_count == 0)
        {
            writer.write(
                GTriangleType(
                    triangle_vertices[vertex_info.m_vertex_index + 0],
                    triangle_vertices[vertex_info.m_vertex_index + 1],
                    triangle_vertices[vertex_info.m_vertex_index + 2]));
        }
        else
        {
            writer.write(
                &triangle_vertices[vertex_info.m_vertex_index],
                (vertex_info.m_motion_segment_count + 1) * 3 * sizeof(GVector3));
        }
    }
}

}   // namespace renderer
