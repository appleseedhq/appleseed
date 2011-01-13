
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

#ifndef APPLESEED_RENDERER_KERNEL_TESSELLATION_STATICTESSELLATION_H
#define APPLESEED_RENDERER_KERNEL_TESSELLATION_STATICTESSELLATION_H

// appleseed.renderer headers.
#include "renderer/global/global.h"
#include "renderer/modeling/object/triangle.h"

// appleseed.foundation headers.
#include "foundation/utility/attributeset.h"
#include "foundation/utility/lazy.h"

// Standard headers.
#include <vector>

namespace renderer
{

//
// A tessellation, as a collection of polygonal primitives.
//

template <typename Primitive>
class StaticTessellation
  : public foundation::NonCopyable
{
  public:
    // Primitive type.
    typedef Primitive PrimitiveType;

    // Vertex and primitive array types.
    // todo: use paged arrays?
    typedef std::vector<GVector3> VectorArray;
    typedef std::vector<PrimitiveType> PrimitiveArray;

    VectorArray                 m_vertices;                 // vertex array
    VectorArray                 m_vertex_normals;           // vertex normal array
    PrimitiveArray              m_primitives;               // primitive array
    foundation::AttributeSet    m_vertex_attributes;        // vertex attributes
    foundation::AttributeSet    m_primitive_attributes;     // primitive attributes
};

// Specialization of the StaticTessellation class for triangles.
typedef StaticTessellation<Triangle> StaticTriangleTess;


//
// Static triangle tessellation access cache.
//

typedef foundation::AccessCache<
            StaticTriangleTess,
            16
        > StaticTriangleTessAccessCache;

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_TESSELLATION_STATICTESSELLATION_H
