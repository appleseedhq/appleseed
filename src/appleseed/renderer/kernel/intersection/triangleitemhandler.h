
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

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"

// appleseed.foundation headers.
#include "foundation/math/aabb.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>
#include <vector>

// Forward declarations.
namespace renderer      { class TriangleVertexInfo; }

namespace renderer
{

class TriangleItemHandler
{
  public:
    TriangleItemHandler(
        const std::vector<TriangleVertexInfo>&  triangle_vertex_infos,
        const std::vector<GVector3>&            triangle_vertices,
        const std::vector<foundation::AABB3d>&  triangle_bboxes);

    double get_bbox_grow_eps() const;

    foundation::AABB3d clip(
        const size_t                            item_index,
        const size_t                            dimension,
        const double                            slab_min,
        const double                            slab_max) const;

    bool intersect(
        const size_t                            item_index,
        const foundation::AABB3d&               bbox) const;

  private:
    const std::vector<TriangleVertexInfo>&      m_triangle_vertex_infos;
    const std::vector<GVector3>&                m_triangle_vertices;
    const std::vector<foundation::AABB3d>&      m_triangle_bboxes;

    static foundation::Vector3d segment_plane_intersection(
        const foundation::Vector3d&             a,
        const foundation::Vector3d&             b,
        const size_t                            d,
        const double                            x);
};

}   // namespace renderer
