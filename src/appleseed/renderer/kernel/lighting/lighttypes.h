
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Petra Gospodnetic, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTTYPES_H
#define APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTTYPES_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/utility/transformsequence.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/math/aabb.h"
#include "foundation/math/vector.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer  { class AssemblyInstance; }
namespace renderer  { class Light; }
namespace renderer  { class Material; }

namespace renderer
{

//
// Non-physical light.
//

class NonPhysicalLightInfo
{
  public:
    TransformSequence           m_transform_sequence;           // assembly instance (parent of the light) space to world space
    const Light*                m_light;
};


//
// Light-emitting triangle.
//

class EmittingTriangle
{
  public:
    const AssemblyInstance*     m_assembly_instance;
    size_t                      m_object_instance_index;
    size_t                      m_region_index;
    size_t                      m_triangle_index;
    foundation::Vector3d        m_v0, m_v1, m_v2;               // world space vertices of the triangle
    foundation::Vector3d        m_n0, m_n1, m_n2;               // world space vertex normals
    foundation::Vector3d        m_geometric_normal;             // world space geometric normal, unit-length
    TriangleSupportPlaneType    m_triangle_support_plane;       // support plane of the triangle in assembly space
    float                       m_area;                         // world space triangle area
    float                       m_rcp_area;                     // world space triangle area reciprocal
    // TODO: remove static triangle probability.
    float                       m_triangle_prob;                // probability density of this triangle
    const Material*             m_material;
};


//
// Any kind of light source. Both non-physical light and emitting triangle.
//

class LightSource
  : public foundation::NonCopyable
{
  public:
    // Destructor.
    virtual ~LightSource() {}

    // Get the light source position.
    virtual foundation::Vector3d get_position() const = 0;

    // Get the light source bounding box.
    virtual foundation::AABB3d get_bbox() const = 0;

    // Get the light intensity.
    // NOTE: currently works only for point lights!
    virtual Spectrum get_intensity() const = 0;
};


//
// Non-physical light source.
//

class NonPhysicalLightSource
  : public LightSource
{
  public:
    explicit NonPhysicalLightSource(const NonPhysicalLightInfo* light);

    virtual foundation::Vector3d get_position() const override;
    virtual foundation::AABB3d get_bbox() const override;
    virtual Spectrum get_intensity() const override;

  private:
    // Reference to the actual source.
    const NonPhysicalLightInfo* m_light_info;
};


//
// Emitting triangle light source.
//

class EmittingTriangleLightSource
  : public LightSource
{
  public:
    explicit EmittingTriangleLightSource(const EmittingTriangle* light);

    virtual foundation::Vector3d get_position() const override;
    virtual foundation::AABB3d get_bbox() const override;
    virtual Spectrum get_intensity() const override;

  private:
    // Reference to the actual source.
    const EmittingTriangle* m_light;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_LIGHTING_LIGHTTYPES_H
