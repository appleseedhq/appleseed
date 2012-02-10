
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

#ifndef APPLESEED_RENDERER_KERNEL_SHADING_SHADINGPOINT_H
#define APPLESEED_RENDERER_KERNEL_SHADING_SHADINGPOINT_H

// appleseed.renderer headers.
#include "renderer/global/global.h"
#include "renderer/kernel/intersection/triangletree.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/kernel/tessellation/statictessellation.h"
#include "renderer/modeling/input/inputparams.h"
#include "renderer/modeling/object/regionkit.h"
#include "renderer/modeling/object/triangle.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/transform.h"

// Forward declarations.
namespace renderer  { class Material; }

namespace renderer
{

//
// A shading point. 
//

class ShadingPoint
  : public foundation::NonCopyable
{
  public:
    // Constructor, calls clear().
    ShadingPoint();

    // Reset the shading point to its initial state (no intersection).
    void clear();

    // Return the scene that was tested for intersection.
    const Scene& get_scene() const;

    // Return the world space ray that was cast through the scene.
    const ShadingRay& get_ray() const;

    // Return true if an intersection was found, false otherwise.
    bool hit() const;

    // Return the distance from the ray origin to the intersection point.
    double get_distance() const;

    // Return the barycentric coordinates of the intersection point.
    const foundation::Vector2d& get_bary() const;

    // Return parameters for input evaluation.
    const InputParams& get_input_params() const;

    // Return the texture coordinates from a given UV set at the intersection point.
    const foundation::Vector2d& get_uv(const size_t uvset) const;

    // Return the intersection point in world space.
    const foundation::Vector3d& get_point() const;

    // Return the intersection point in world space, properly offset to avoid self-intersections.
    const foundation::Vector3d& get_offset_point(const foundation::Vector3d& direction) const;

    // Return the world space geometric normal at the intersection point.
    const foundation::Vector3d& get_geometric_normal() const;

    // Return the world space shading normal at the intersection point.
    const foundation::Vector3d& get_shading_normal() const;

    // Return a world space orthonormal basis around the shading normal.
    const foundation::Basis3d& get_shading_basis() const;

    // Return the side of the surface that was hit.
    ObjectInstance::Side get_side() const;

    // Return the i'th world space vertex of the hit triangle.
    const foundation::Vector3d& get_vertex(const size_t i) const;

    // Return the world space normal at the i'th vertex of the hit triangle.
    const foundation::Vector3d& get_vertex_normal(const size_t i) const;

    // Return the material at the intersection point, or 0 if there is none.
    const Material* get_material() const;

    // Return the assembly instance that was hit.
    const AssemblyInstance& get_assembly_instance() const;

    // Return the assembly that was hit.
    const Assembly& get_assembly() const;

    // Return the object instance that was hit.
    const ObjectInstance& get_object_instance() const;

    // Return the object that was hit.
    const Object& get_object() const;

    // Return the unique ID of the assembly instance that was hit.
    foundation::UniqueID get_assembly_instance_uid() const;

    // Return the index, within the assembly, of the object instance that was hit.
    size_t get_object_instance_index() const;

    // Return the index, within the object, of the region containing the hit triangle.
    size_t get_region_index() const;

    // Return the index, within the region, of the hit triangle.
    size_t get_triangle_index() const;

    // Return the index, within the region, of the primitive attribute of the hit triangle.
    size_t get_primitive_attribute_index() const;

  private:
    friend class AssemblyLeafProbeVisitor;
    friend class AssemblyLeafVisitor;
    friend class Intersector;
    friend class RegionLeafVisitor;
    friend class TriangleLeafVisitor;
    friend class ShadingPointBuilder;

    RegionKitAccessCache*           m_region_kit_cache;
    StaticTriangleTessAccessCache*  m_tess_cache;
    const Scene*                    m_scene;                    // scene
    ShadingRay                      m_ray;                      // world space ray (m_tmax = distance to intersection)

    // Intersection results.
    bool                            m_hit;                      // intersection?
    foundation::Vector2d            m_bary;                     // barycentric coordinates of intersection point
    foundation::UniqueID            m_asm_instance_uid;         // unique ID of the assembly instance that was hit
    size_t                          m_object_instance_index;    // index of the object instance that was hit
    size_t                          m_region_index;             // index of the region containing the hit triangle
    size_t                          m_triangle_index;           // index of the hit triangle
    TriangleSupportPlaneType        m_triangle_support_plane;   // support plane of the hit triangle

    // Additional intersection results, computed on demand.
    enum Members
    {
        HasSourceGeometry           = 1 << 0,                   // object instance space triangle vertices and vertex normals
        HasUV0                      = 1 << 1,                   // texture coordinates from UV set #0 at intersection point
        HasPoint                    = 1 << 2,                   // world space intersection point
        HasGeometricNormal          = 1 << 3,                   // world space geometric normal at intersection point
        HasShadingNormal            = 1 << 4,                   // world space shading normal at intersection point
        HasShadingBasis             = 1 << 5,                   // world space orthonormal basis around shading normal
        HasWorldSpaceVertices       = 1 << 6,                   // world space triangle vertices
        HasWorldSpaceVertexNormals  = 1 << 7,                   // world space vertex normals
        HasMaterial                 = 1 << 8,                   // material at intersection point
        HasRefinedPoints            = 1 << 9                    // refined intersection points
    };
    mutable foundation::uint32      m_members;                  // which members have already been computed
    mutable const AssemblyInstance* m_assembly_instance;        // hit assembly instance
    mutable const Assembly*         m_assembly;                 // hit assembly
    mutable const ObjectInstance*   m_object_instance;          // hit object instance
    mutable Object*                 m_object;                   // hit object
    mutable foundation::uint32      m_triangle_pa;              // hit triangle attribute index
    mutable GVector2                m_v0_uv, m_v1_uv, m_v2_uv;  // texture coordinates from UV set #0 at triangle vertices
    mutable GVector3                m_v0, m_v1, m_v2;           // object instance space triangle vertices
    mutable GVector3                m_n0, m_n1, m_n2;           // object instance space triangle vertex normals
    mutable InputParams             m_input_params;             // parameters for input evaluation
    mutable ObjectInstance::Side    m_side;                     // side of the surface that was hit
    mutable foundation::Basis3d     m_shading_basis;            // world space orthonormal basis around shading normal
    mutable foundation::Vector3d    m_v0_w, m_v1_w, m_v2_w;     // world space triangle vertices
    mutable foundation::Vector3d    m_n0_w, m_n1_w, m_n2_w;     // world space triangle vertex normals
    mutable const Material*         m_material;                 // material at intersection point

    // Data required to avoid self-intersections.
    mutable foundation::Vector3d    m_asm_geo_normal;           // assembly instance space geometric normal to hit triangle
    mutable foundation::Vector3d    m_front_point;              // hit point refined to front, in assembly instance space
    mutable foundation::Vector3d    m_back_point;               // hit point refined to back, in assembly instance space

    // Cache the source geometry, fetching it if necessary.
    void cache_source_geometry() const;

    // Fetch the source geometry.
    void fetch_source_geometry() const;

    // Refine and offset the intersection point.
    void refine_and_offset() const;
};


//
// ShadingPoint class implementation.
//

FORCE_INLINE ShadingPoint::ShadingPoint()
{
    clear();
}

FORCE_INLINE void ShadingPoint::clear()
{
    m_region_kit_cache = 0;
    m_tess_cache = 0;
    m_scene = 0;
    m_hit = false;
    m_members = 0;
}

inline const Scene& ShadingPoint::get_scene() const
{
    assert(m_scene);
    return *m_scene;
}

inline const ShadingRay& ShadingPoint::get_ray() const
{
    return m_ray;
}

inline bool ShadingPoint::hit() const
{
    return m_hit;
}

inline double ShadingPoint::get_distance() const
{
    assert(hit());
    return m_ray.m_tmax;
}

inline const foundation::Vector2d& ShadingPoint::get_bary() const
{
    assert(hit());
    return m_bary;
}

inline const InputParams& ShadingPoint::get_input_params() const
{
    assert(hit());

    get_uv(0);

/*
    At the moment, only UV coordinates are needed by the input sources
    (see renderer/input/source.h and derivates).

    get_point();
    get_geometric_normal();
    get_shading_normal();
*/

    return m_input_params;
}

inline const foundation::Vector2d& ShadingPoint::get_uv(const size_t uvset) const
{
    assert(hit());
    assert(uvset == 0);     // todo: support multiple UV sets

    if (!(m_members & HasUV0))
    {
        cache_source_geometry();

        // Compute the texture coordinates.
        const foundation::Vector2d v0_uv = foundation::Vector2d(m_v0_uv);
        const foundation::Vector2d v1_uv = foundation::Vector2d(m_v1_uv);
        const foundation::Vector2d v2_uv = foundation::Vector2d(m_v2_uv);
        const double w = 1.0 - m_bary[0] - m_bary[1];
        m_input_params.m_uv =
              v0_uv * w
            + v1_uv * m_bary[0]
            + v2_uv * m_bary[1];

        // Texture coordinates from UV set #0 are now available.
        m_members |= HasUV0;
    }

    return m_input_params.m_uv;
}

inline const foundation::Vector3d& ShadingPoint::get_point() const
{
    assert(hit());

    if (!(m_members & HasPoint))
    {
        // Compute the intersection point.
        m_input_params.m_point = m_ray.point_at(m_ray.m_tmax);

        // The intersection point is now available.
        m_members |= HasPoint;
    }

    return m_input_params.m_point;
}

inline const foundation::Vector3d& ShadingPoint::get_offset_point(const foundation::Vector3d& direction) const
{
    assert(hit());
    assert(m_members & HasRefinedPoints);

    return dot(m_asm_geo_normal, direction) > 0.0 ? m_front_point : m_back_point;
}

inline const foundation::Vector3d& ShadingPoint::get_geometric_normal() const
{
    assert(hit());

    if (!(m_members & HasGeometricNormal))
    {
        if (m_members & HasWorldSpaceVertices)
        {
            // We already have the world space vertices of the hit triangle.
            // Use them to compute the geometric normal directly in world space.
            m_input_params.m_geometric_normal =
                foundation::cross(m_v1_w - m_v0_w, m_v2_w - m_v0_w);
        }
        else
        {
            cache_source_geometry();

            // Retrieve object instance space to assembly instance space transform.
            const foundation::Transformd& obj_instance_transform =
                m_object_instance->get_transform();

            // Retrieve assembly instance space to world space transform.
            const foundation::Transformd& asm_instance_transform =
                m_assembly_instance->get_transform();

            // Compute the object instance space geometric normal.
            const foundation::Vector3d v0 = foundation::Vector3d(m_v0);
            const foundation::Vector3d v1 = foundation::Vector3d(m_v1);
            const foundation::Vector3d v2 = foundation::Vector3d(m_v2);
            m_input_params.m_geometric_normal = foundation::cross(v1 - v0, v2 - v0);

            // Transform the geometric normal to world space.
            m_input_params.m_geometric_normal =
                asm_instance_transform.transform_normal_to_parent(
                    obj_instance_transform.transform_normal_to_parent(m_input_params.m_geometric_normal));
        }

        // Normalize the geometric normal.
        m_input_params.m_geometric_normal =
            foundation::normalize(m_input_params.m_geometric_normal);

        // Make the geometric normal face the direction of the incoming ray.
        m_input_params.m_geometric_normal =
            foundation::faceforward(m_input_params.m_geometric_normal, m_ray.m_dir);

        // The geometric normal is now available.
        m_members |= HasGeometricNormal;
    }

    return m_input_params.m_geometric_normal;
}

inline const foundation::Vector3d& ShadingPoint::get_shading_normal() const
{
    assert(hit());

    if (!(m_members & HasShadingNormal))
    {
        if (m_members & HasWorldSpaceVertexNormals)
        {
            // We already have the world space vertex normals of the hit triangle.
            // Use them to compute the shading normal directly in world space.
            const double w = 1.0 - m_bary[0] - m_bary[1];
            m_input_params.m_shading_normal =
                  m_n0_w * w
                + m_n1_w * m_bary[0]
                + m_n2_w * m_bary[1];
        }
        else
        {
            cache_source_geometry();

            // Retrieve object instance space to assembly instance space transform.
            const foundation::Transformd& obj_instance_transform =
                m_object_instance->get_transform();

            // Retrieve assembly instance space to world space transform.
            const foundation::Transformd& asm_instance_transform =
                m_assembly_instance->get_transform();

            // Compute the object instance space shading normal.
            const foundation::Vector3d n0 = foundation::Vector3d(m_n0);
            const foundation::Vector3d n1 = foundation::Vector3d(m_n1);
            const foundation::Vector3d n2 = foundation::Vector3d(m_n2);
            const double w = 1.0 - m_bary[0] - m_bary[1];
            m_input_params.m_shading_normal =
                  n0 * w
                + n1 * m_bary[0]
                + n2 * m_bary[1];

            // Transform the shading normal to world space.
            m_input_params.m_shading_normal =
                asm_instance_transform.transform_normal_to_parent(
                    obj_instance_transform.transform_normal_to_parent(m_input_params.m_shading_normal));
        }

        // Remember which side of the surface we hit.
        m_side =
            dot(m_ray.m_dir, m_input_params.m_shading_normal) > 0.0
                ? ObjectInstance::BackSide
                : ObjectInstance::FrontSide;

        // Normalize the shading normal.
        m_input_params.m_shading_normal = foundation::normalize(m_input_params.m_shading_normal);

        // Make the shading normal face the direction of the incoming ray.
        if (m_side == ObjectInstance::BackSide)
            m_input_params.m_shading_normal = -m_input_params.m_shading_normal;

        // The shading normal is now available.
        m_members |= HasShadingNormal;
    }

    return m_input_params.m_shading_normal;
}

inline const foundation::Basis3d& ShadingPoint::get_shading_basis() const
{
    assert(hit());

    if (!(m_members & HasShadingBasis))
    {
        // Construct the orthonormal basis.
        m_shading_basis.build(get_shading_normal());

        // The orthonormal basis is now available.
        m_members |= HasShadingBasis;
    }

    return m_shading_basis;
}

inline ObjectInstance::Side ShadingPoint::get_side() const
{
    get_shading_normal();

    return m_side;
}

inline const foundation::Vector3d& ShadingPoint::get_vertex(const size_t i) const
{
    assert(hit());
    assert(i < 3);

    if (!(m_members & HasWorldSpaceVertices))
    {
        cache_source_geometry();

        // Retrieve object instance space to assembly instance space transform.
        const foundation::Transformd& obj_instance_transform =
            m_object_instance->get_transform();

        // Retrieve assembly instance space to world space transform.
        const foundation::Transformd& asm_instance_transform =
            m_assembly_instance->get_transform();

        // Transform triangle vertices to world space.
        const foundation::Vector3d v0 = foundation::Vector3d(m_v0);
        const foundation::Vector3d v1 = foundation::Vector3d(m_v1);
        const foundation::Vector3d v2 = foundation::Vector3d(m_v2);
        m_v0_w = obj_instance_transform.transform_point_to_parent(v0);
        m_v1_w = obj_instance_transform.transform_point_to_parent(v1);
        m_v2_w = obj_instance_transform.transform_point_to_parent(v2);
        m_v0_w = asm_instance_transform.transform_point_to_parent(m_v0_w);
        m_v1_w = asm_instance_transform.transform_point_to_parent(m_v1_w);
        m_v2_w = asm_instance_transform.transform_point_to_parent(m_v2_w);

        // World space triangle vertices are now available.
        m_members |= HasWorldSpaceVertices;
    }

    return (&m_v0_w)[i];
}

inline const foundation::Vector3d& ShadingPoint::get_vertex_normal(const size_t i) const
{
    assert(hit());
    assert(i < 3);

    if (!(m_members & HasWorldSpaceVertexNormals))
    {
        cache_source_geometry();

        // Retrieve instance space to assembly instance space transform.
        const foundation::Transformd& obj_instance_transform =
            m_object_instance->get_transform();

        // Retrieve assembly instance space to world space transform.
        const foundation::Transformd& asm_instance_transform =
            m_assembly_instance->get_transform();

        // Transform vertex normals to world space.
        const foundation::Vector3d n0 = foundation::Vector3d(m_n0);
        const foundation::Vector3d n1 = foundation::Vector3d(m_n1);
        const foundation::Vector3d n2 = foundation::Vector3d(m_n2);
        m_n0_w = obj_instance_transform.transform_normal_to_parent(n0);
        m_n1_w = obj_instance_transform.transform_normal_to_parent(n1);
        m_n2_w = obj_instance_transform.transform_normal_to_parent(n2);
        m_n0_w = asm_instance_transform.transform_normal_to_parent(m_n0_w);
        m_n1_w = asm_instance_transform.transform_normal_to_parent(m_n1_w);
        m_n2_w = asm_instance_transform.transform_normal_to_parent(m_n2_w);

        // World space vertex normals are now available.
        m_members |= HasWorldSpaceVertexNormals;
    }

    return (&m_n0_w)[i];
}

inline const Material* ShadingPoint::get_material() const
{
    assert(hit());

    if (!(m_members & HasMaterial))
    {
        cache_source_geometry();

        m_material = 0;

        // Proceed with retrieving the material only if the hit triangle has one.
        if (m_triangle_pa != Triangle::None)
        {
            // Retrieve material indices from the object instance.
            const MaterialArray& materials =
                get_side() == ObjectInstance::BackSide
                    ? m_object_instance->get_back_materials()
                    : m_object_instance->get_front_materials();

            // Fetch the material.
            if (static_cast<size_t>(m_triangle_pa) < materials.size())
                m_material = materials[m_triangle_pa];
        }

        // The material at the intersection point is now available.
        m_members |= HasMaterial;
    }

    return m_material;
}

inline const AssemblyInstance& ShadingPoint::get_assembly_instance() const
{
    assert(hit());

    cache_source_geometry();

    return *m_assembly_instance;
}

inline const Assembly& ShadingPoint::get_assembly() const
{
    assert(hit());

    cache_source_geometry();

    return *m_assembly;
}

inline const ObjectInstance& ShadingPoint::get_object_instance() const
{
    assert(hit());

    cache_source_geometry();

    return *m_object_instance;
}

inline const Object& ShadingPoint::get_object() const
{
    assert(hit());

    cache_source_geometry();

    return *m_object;
}

inline foundation::UniqueID ShadingPoint::get_assembly_instance_uid() const
{
    assert(hit());
    return m_asm_instance_uid;
}

inline size_t ShadingPoint::get_object_instance_index() const
{
    assert(hit());
    return m_object_instance_index;
}

inline size_t ShadingPoint::get_region_index() const
{
    assert(hit());
    return m_region_index;
}

inline size_t ShadingPoint::get_triangle_index() const
{
    assert(hit());
    return m_triangle_index;
}

inline size_t ShadingPoint::get_primitive_attribute_index() const
{
    assert(hit());

    cache_source_geometry();

    return static_cast<size_t>(m_triangle_pa);
}

inline void ShadingPoint::cache_source_geometry() const
{
    if (!(m_members & HasSourceGeometry))
    {
        fetch_source_geometry();

        // Source geometry is now available.
        m_members |= HasSourceGeometry;
    }
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_SHADING_SHADINGPOINT_H
