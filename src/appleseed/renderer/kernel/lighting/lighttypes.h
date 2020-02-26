
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
#include "foundation/math/aabb.h"
#include "foundation/math/vector.h"
#include "foundation/memory/stampedptr.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer  { class AssemblyInstance; }
namespace renderer  { class BackwardLightSampler; }
namespace renderer  { class Intersector; }
namespace renderer  { class Light; }
namespace renderer  { class LightSample; }
namespace renderer  { class Material; }
namespace renderer  { class ShadingPoint; }

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
    TransformSequence   m_transform_sequence;   // assembly instance (parent of the light) space to world space
    const Light*        m_light;
};


//
// A light-emitting shape.
//

class EmittingShape
{
  public:
    enum ShapeType
    {
        TriangleShape = 0,
        RectangleShape,
        SphereShape,
        DiskShape
    };

    static EmittingShape create_triangle_shape(
        const AssemblyInstance*     assembly_instance,
        const size_t                object_instance_index,
        const size_t                primitive_index,
        const Material*             material,
        const double                area,
        const foundation::Vector3d& v0,
        const foundation::Vector3d& v1,
        const foundation::Vector3d& v2,
        const foundation::Vector3d& n0,
        const foundation::Vector3d& n1,
        const foundation::Vector3d& n2,
        const foundation::Vector3d& geometric_normal);

    static EmittingShape create_rectangle_shape(
        const AssemblyInstance*     assembly_instance,
        const size_t                object_instance_index,
        const Material*             material,
        const double                area,
        const foundation::Vector3d& o,
        const foundation::Vector3d& x,
        const foundation::Vector3d& y,
        const foundation::Vector3d& n);

    static EmittingShape create_sphere_shape(
        const AssemblyInstance*     assembly_instance,
        const size_t                object_instance_index,
        const Material*             material,
        const double                area,
        const foundation::Vector3d& center,
        const double                radius);

    static EmittingShape create_disk_shape(
        const AssemblyInstance*     assembly_instance,
        const size_t                object_instance_index,
        const Material*             material,
        const double                area,
        const foundation::Vector3d& c,
        const double                r,
        const foundation::Vector3d& n,
        const foundation::Vector3d& x,
        const foundation::Vector3d& y);

    ShapeType get_shape_type() const;

    const AssemblyInstance* get_assembly_instance() const;

    size_t get_object_instance_index() const;

    size_t get_primitive_index() const;

    float get_area() const;
    float get_rcp_area() const;

    float get_shape_prob() const;
    void set_shape_prob(const float prob);

    const Material* get_material() const;

    const foundation::AABB3d& get_bbox() const;

    const foundation::Vector3d& get_centroid() const;

    void sample_uniform(
        const foundation::Vector2f& s,
        const float                 shape_prob,
        LightSample&                light_sample) const;

    float evaluate_pdf_uniform() const;

    void make_shading_point(
        ShadingPoint&               shading_point,
        const foundation::Vector3d& point,
        const foundation::Vector3d& direction,
        const foundation::Vector2f& bary,
        const Intersector&          intersector) const;

    // Estimate average and maximum radiant flux emitted by this shape.
    void estimate_flux();

    // Return estimated average and maximum radiant flux in W emitted by this shape.
    float get_average_flux() const;
    float get_max_flux() const;

  private:
    friend class LightSamplerBase;
    friend class BackwardLightSampler;

    struct Triangle
    {
        foundation::Vector3d    m_v0, m_v1, m_v2;               // world space vertices of the shape
        foundation::Vector3d    m_n0, m_n1, m_n2;               // world space vertex normals
        foundation::Vector3d    m_geometric_normal;             // world space geometric normal, unit-length
        double  m_plane_dist;
    };

    struct Rectangle
    {
        foundation::Vector3d    m_origin;                       // world space position of the bottom left corner of the rectangle
        foundation::Vector3d    m_x, m_y;                       // world space x and y axes
        double                  m_width;                        // rectangle width
        double                  m_height;                       // rectangle height
        foundation::Vector3d    m_geometric_normal;             // world space geometric normal, unit-length
        double                  m_plane_dist;
    };

    struct Sphere
    {
        foundation::Vector3d    m_center;                       // world space center of the sphere
        double                  m_radius;                       // sphere radius
    };

    struct Disk
    {
        foundation::Vector3d    m_center;               // world space center of the disk
        foundation::Vector3d    m_geometric_normal;     // world space geometric normal, unit-length
        double                  m_radius;               // world space disk radius
        foundation::Vector3d    m_x, m_y;               // world space x and y axes
    };

    union Geom
    {
        Triangle    m_triangle;
        Rectangle   m_rectangle;
        Sphere      m_sphere;
        Disk        m_disk;
    };

    typedef foundation::stamped_ptr<const AssemblyInstance> AssemblyInstanceAndType;

    AssemblyInstanceAndType     m_assembly_instance_and_type;
    size_t                      m_object_instance_index;
    size_t                      m_light_tree_node_index;
    size_t                      m_primitive_index;
    Geom                        m_geom;
    TriangleSupportPlaneType    m_shape_support_plane;          // support plane of the shape in assembly space
    float                       m_area;                         // world space shape area
    float                       m_rcp_area;                     // world space shape area reciprocal
    float                       m_shape_prob;                   // probability density of this shape
    float                       m_average_flux;                 // estimated average radiant flux in W emitted by this shape
    float                       m_max_flux;                     // estimated maximum radiant flux in W emitted by this shape
    const Material*             m_material;
    foundation::AABB3d          m_bbox;
    foundation::Vector3d        m_centroid;

    // Constructor.
    EmittingShape(
        const ShapeType             shape_type,
        const AssemblyInstance*     assembly_instance,
        const size_t                object_instance_index,
        const size_t                primitive_index,
        const Material*             material);
};


//
// EmittingShape class implementation.
//

inline EmittingShape::ShapeType EmittingShape::get_shape_type() const
{
    return static_cast<ShapeType>(m_assembly_instance_and_type.get_stamp());
}

inline const AssemblyInstance* EmittingShape::get_assembly_instance() const
{
    return m_assembly_instance_and_type.get_ptr();
}

inline size_t EmittingShape::get_primitive_index() const
{
    return m_primitive_index;
}

inline size_t EmittingShape::get_object_instance_index() const
{
    return m_object_instance_index;
}

inline float EmittingShape::get_area() const
{
    return m_area;
}

inline float EmittingShape::get_rcp_area() const
{
    return m_rcp_area;
}

inline float EmittingShape::get_shape_prob() const
{
    return m_shape_prob;
}

inline void EmittingShape::set_shape_prob(const float prob)
{
    m_shape_prob = prob;
}

inline const Material* EmittingShape::get_material() const
{
    return m_material;
}

inline const foundation::AABB3d& EmittingShape::get_bbox() const
{
    return m_bbox;
}

inline const foundation::Vector3d& EmittingShape::get_centroid() const
{
    return m_centroid;
}

inline float EmittingShape::evaluate_pdf_uniform() const
{
    return m_shape_prob * m_rcp_area;
}

inline float EmittingShape::get_average_flux() const
{
    return m_average_flux;
}

inline float EmittingShape::get_max_flux() const
{
    return m_max_flux;
}

}   // namespace renderer
