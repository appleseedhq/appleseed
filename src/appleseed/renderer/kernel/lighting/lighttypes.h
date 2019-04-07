
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Petra Gospodnetic, The appleseedhq Organization
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
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer  { class AssemblyInstance; }
namespace renderer  { class Light; }
namespace renderer  { class Material; }

namespace renderer
{

enum LightType
{
    NonPhysicalLightType = 0,
    EmittingShapeType = 1
};

//
// A non-physical light.
//

class NonPhysicalLightInfo
{
  public:
    TransformSequence           m_transform_sequence;           // assembly instance (parent of the light) space to world space
    const Light*                m_light;
};


//
// A light-emitting shape.
//

class EmittingShape
{
  public:
    const AssemblyInstance*     m_assembly_instance;
    size_t                      m_object_instance_index;
    size_t                      m_shape_index;
    size_t                      m_light_tree_node_index;
    foundation::Vector3d        m_v0, m_v1, m_v2;               // world space vertices of the triangle
    foundation::Vector3d        m_n0, m_n1, m_n2;               // world space vertex normals
    foundation::Vector3d        m_geometric_normal;             // world space geometric normal, unit-length
    TriangleSupportPlaneType    m_shape_support_plane;          // support plane of the shape in assembly space
    float                       m_area;                         // world space shape area
    float                       m_rcp_area;                     // world space shape area reciprocal
    float                       m_shape_prob;                   // probability density of this shape
    const Material*             m_material;
};

}   // namespace renderer
