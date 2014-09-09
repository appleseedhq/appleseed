
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "renderer/global/globaltypes.h"
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/kernel/tessellation/statictessellation.h"
#include "renderer/modeling/material/inormalmodifier.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/object/curveobject.h"
#include "renderer/modeling/object/regionkit.h"
#include "renderer/modeling/object/triangle.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/objectinstance.h"

// appleseed.foundation headers.
#include "foundation/math/basis.h"
#include "foundation/math/beziercurve.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/platform/types.h"

// OSL headers.
#ifdef WITH_OSL
#include "foundation/platform/oslheaderguards.h"
BEGIN_OSL_INCLUDES
#include "OSL/oslexec.h"
#include "OSL/shaderglobals.h"
END_OSL_INCLUDES
#endif

// Standard headers.
#include <cassert>
#include <cstddef>

// Forward declarations.
namespace renderer  { class Object; }
#ifdef WITH_OSL
namespace renderer  { class OSLShaderGroupExec; }
namespace renderer  { class ShaderGroup; }
#endif
namespace renderer  { class Scene; }
namespace renderer  { class TextureCache; }

namespace renderer
{

//
// A shading point.
//

class ShadingPoint
{
  public:
    // The type of the primitive at the shading point.
    enum PrimitiveType
    {
        PrimitiveNone,
        PrimitiveTriangle,
        PrimitiveCurve
    };

    // Constructor, calls clear().
    ShadingPoint();

    // Copy constructor.
    explicit ShadingPoint(const ShadingPoint& rhs);

    // Reset the shading point to its initial state (no intersection).
    void clear();

    // Replace the ray stored in the shading point.
    void set_ray(const ShadingRay& ray);

    // Return the scene that was tested for intersection.
    const Scene& get_scene() const;

    // Return the world space ray that was cast through the scene.
    const ShadingRay& get_ray() const;

    // Return the time stored in the ray.
    double get_time() const;

    // Return the time differential stored in the ray.
    double get_dtime() const;

    // Return true if an intersection was found, false otherwise.
    bool hit() const;

    // Return the type of the hit primitive.
    PrimitiveType get_primitive_type() const;

    // Return the distance from the ray origin to the intersection point.
    double get_distance() const;

    // Return the barycentric coordinates of the intersection point.
    const foundation::Vector2d& get_bary() const;

    // Return the texture coordinates from a given UV set at the intersection point.
    const foundation::Vector2d& get_uv(const size_t uvset) const;

    // Return the intersection point in world space.
    const foundation::Vector3d& get_point() const;

    // Return the intersection point in world space, with per-object-instance ray bias applied.
    foundation::Vector3d get_biased_point(const foundation::Vector3d& direction) const;

    // Return the intersection point in world space, properly offset to avoid self-intersections.
    const foundation::Vector3d& get_offset_point(const foundation::Vector3d& direction) const;

    // Return the world space partial derivatives of the intersection point wrt. a given UV set.
    const foundation::Vector3d& get_dpdu(const size_t uvset) const;
    const foundation::Vector3d& get_dpdv(const size_t uvset) const;

    // Return the world space geometric normal at the intersection point. The geometric normal
    // always faces the incoming ray, i.e. dot(ray_dir, geometric_normal) is always positive or null.
    const foundation::Vector3d& get_geometric_normal() const;

    // Return the world space (possibly modified) shading normal at the intersection point.
    // The shading normal is always in the same hemisphere as the geometric normal, but it is
    // not always facing the incoming ray, i.e. dot(ray_dir, shading_normal) may be negative.
    const foundation::Vector3d& get_shading_normal() const;

    // Return the original world space shading normal at the intersection point.
    const foundation::Vector3d& get_original_shading_normal() const;

    // Return a world space orthonormal basis around the (possibly modified) shading normal.
    const foundation::Basis3d& get_shading_basis() const;

    // Return the side of the surface that was hit.
    ObjectInstance::Side get_side() const;

    // Return the i'th world space vertex of the hit triangle.
    const foundation::Vector3d& get_vertex(const size_t i) const;

    // Return the world space point velocity.
    const foundation::Vector3d& get_point_velocity() const;

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

    // Return the index, within the assembly, of the object instance that was hit.
    size_t get_object_instance_index() const;

    // Return the index, within the object, of the region containing the hit triangle.
    size_t get_region_index() const;

    // Return the index of the hit primitive.
    size_t get_primitive_index() const;

    // Return the index of the primitive attribute.
    size_t get_primitive_attribute_index() const;

#ifdef WITH_OSL
    struct OSLObjectTransformInfo
    {
        bool is_animated() const;

        OSL::Matrix44 get_transform() const;
        OSL::Matrix44 get_transform(float t) const;

        OSL::Matrix44 get_inverse_transform() const;
        OSL::Matrix44 get_inverse_transform(float t) const;

        const TransformSequence*        m_assembly_instance_transform;
        const foundation::Transformd*   m_object_instance_transform;
    };

    struct OSLTraceData
    {
        bool        m_traced;
        bool        m_hit;
        float       m_hit_distance;
        OSL::Vec3   m_P;
        OSL::Vec3   m_N;
        OSL::Vec3   m_Ng;
        float       m_u;
        float       m_v;
    };

    OSL::ShaderGlobals& get_osl_shader_globals() const;
#endif

  private:
    friend class AssemblyLeafProbeVisitor;
    friend class AssemblyLeafVisitor;
    friend class CurveLeafVisitor;
    friend class Intersector;
#ifdef WITH_OSL
    friend class OSLShaderGroupExec;
#endif
    friend class RegionLeafVisitor;
    friend class ShadingPointBuilder;
    friend class TriangleLeafVisitor;

    RegionKitAccessCache*               m_region_kit_cache;
    StaticTriangleTessAccessCache*      m_tess_cache;
    TextureCache*                       m_texture_cache;

    const Scene*                        m_scene;
    mutable ShadingRay                  m_ray;                              // world space ray (m_tmax = distance to intersection)

    // Intersection results.
    PrimitiveType                       m_primitive_type;                   // type of the hit primitive
    foundation::Vector2d                m_bary;                             // barycentric coordinates of intersection point
    const AssemblyInstance*             m_assembly_instance;                // hit assembly instance
    foundation::Transformd              m_assembly_instance_transform;      // transform of the hit assembly instance at ray time
    const TransformSequence*            m_assembly_instance_transform_seq;  // transform sequence of the hit assembly instance.
    size_t                              m_object_instance_index;            // index of the object instance that was hit
    size_t                              m_region_index;                     // index of the region containing the hit triangle
    size_t                              m_primitive_index;                  // index of the hit primitive
    TriangleSupportPlaneType            m_triangle_support_plane;           // support plane of the hit triangle

    // Flags to keep track of which on-demand results have been computed and cached.
    enum Members
    {
        HasSourceGeometry               = 1 << 0,
        HasUV0                          = 1 << 1,
        HasPoint                        = 1 << 2,
        HasBiasedPoint                  = 1 << 3,
        HasRefinedPoints                = 1 << 4,
        HasPartialDerivatives           = 1 << 5,
        HasGeometricNormal              = 1 << 6,
        HasShadingNormal                = 1 << 7,
        HasOriginalShadingNormal        = 1 << 8,
        HasShadingBasis                 = 1 << 9,
        HasWorldSpaceTriangleVertices   = 1 << 10,
        HasMaterial                     = 1 << 11,
        HasTriangleVertexTangents       = 1 << 12,
        HasPointVelocity                = 1 << 13
#ifdef WITH_OSL
        , HasOSLShaderGlobals           = 1 << 14
#endif
    };
    mutable foundation::uint32          m_members;

    // Source geometry, fetched on demand.
    mutable const Assembly*             m_assembly;                     // hit assembly
    mutable const ObjectInstance*       m_object_instance;              // hit object instance
    mutable Object*                     m_object;                       // hit object
    mutable foundation::uint32          m_primitive_pa;                 // hit primitive attribute index
    mutable GVector2                    m_v0_uv, m_v1_uv, m_v2_uv;      // texture coordinates from UV set #0 at triangle vertices
    mutable GVector3                    m_v0, m_v1, m_v2;               // object instance space triangle vertices
    mutable GVector3                    m_n0, m_n1, m_n2;               // object instance space triangle vertex normals
    mutable GVector3                    m_t0, m_t1, m_t2;               // object instance space triangle vertex tangents

    // Additional intersection results, computed on demand.
    mutable foundation::Vector2d        m_uv;                           // texture coordinates from UV set #0
    mutable foundation::Vector3d        m_point;                        // world space intersection point
    mutable foundation::Vector3d        m_biased_point;                 // world space intersection point with per-object-instance bias applied
    mutable foundation::Vector3d        m_dpdu;                         // world space partial derivative of the intersection point wrt. U
    mutable foundation::Vector3d        m_dpdv;                         // world space partial derivative of the intersection point wrt. V
    mutable foundation::Vector3d        m_geometric_normal;             // world space geometric normal, unit-length
    mutable foundation::Vector3d        m_shading_normal;               // world space (possibly modified) shading normal, unit-length
    mutable foundation::Vector3d        m_original_shading_normal;      // original world space shading normal, unit-length
    mutable ObjectInstance::Side        m_side;                         // side of the surface that was hit
    mutable foundation::Basis3d         m_shading_basis;                // world space orthonormal basis around shading normal
    mutable foundation::Vector3d        m_v0_w, m_v1_w, m_v2_w;         // world space triangle vertices
    mutable foundation::Vector3d        m_point_velocity;               // world space point velocity
    mutable const Material*             m_material;                     // material at intersection point

    // Data required to avoid self-intersections.
    mutable foundation::Vector3d        m_asm_geo_normal;               // assembly instance space geometric normal to hit triangle
    mutable foundation::Vector3d        m_front_point;                  // hit point refined to front, in assembly instance space
    mutable foundation::Vector3d        m_back_point;                   // hit point refined to back, in assembly instance space

#ifdef WITH_OSL
    mutable OSLObjectTransformInfo      m_obj_transform_info;
    mutable OSLTraceData                m_osl_trace_data;
    mutable OSL::ShaderGlobals          m_shader_globals;
#endif

    // Fetch and cache the source geometry.
    void cache_source_geometry() const;

    // Fetch the source geometry.
    void fetch_source_geometry() const;
    void fetch_triangle_source_geometry() const;
    void fetch_curve_source_geometry() const;

    // Refine and offset the intersection point.
    void refine_and_offset() const;

    void compute_partial_derivatives() const;
    void compute_geometric_normal() const;
    void compute_shading_normal() const;
    void compute_original_shading_normal() const;
    void compute_shading_basis() const;
    void compute_world_space_triangle_vertices() const;
    void compute_point_velocity() const;

#ifdef WITH_OSL
    void initialize_osl_shader_globals(
        const ShaderGroup&      sg,
        OSL::RendererServices*  renderer,
        const float             surface_area = 0.0f) const;

    void initialize_osl_shader_globals(
        const ShaderGroup&          sg,
        const ShadingRay::TypeType  ray_type,
        OSL::RendererServices*      renderer,
        const float                 surface_area = 0.0f) const;
#endif
};


//
// ShadingPoint class implementation.
//

FORCE_INLINE ShadingPoint::ShadingPoint()
{
    clear();
}

inline ShadingPoint::ShadingPoint(const ShadingPoint& rhs)
  : m_region_kit_cache(rhs.m_region_kit_cache)
  , m_tess_cache(rhs.m_tess_cache)
  , m_texture_cache(rhs.m_texture_cache)
  , m_scene(rhs.m_scene)
  , m_ray(rhs.m_ray)
  , m_primitive_type(rhs.m_primitive_type)
  , m_bary(rhs.m_bary)
  , m_assembly_instance(rhs.m_assembly_instance)
  , m_assembly_instance_transform(rhs.m_assembly_instance_transform)
  , m_assembly_instance_transform_seq(rhs.m_assembly_instance_transform_seq)
  , m_object_instance_index(rhs.m_object_instance_index)
  , m_region_index(rhs.m_region_index)
  , m_primitive_index(rhs.m_primitive_index)
  , m_triangle_support_plane(rhs.m_triangle_support_plane)
  , m_members(0)
{
}

FORCE_INLINE void ShadingPoint::clear()
{
    m_region_kit_cache = 0;
    m_tess_cache = 0;
    m_texture_cache = 0;
    m_scene = 0;
    m_primitive_type = PrimitiveNone;
    m_members = 0;
}

inline void ShadingPoint::set_ray(const ShadingRay& ray)
{
    m_ray = ray;
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

inline double ShadingPoint::get_time() const
{
    return m_ray.m_time;
}

inline double ShadingPoint::get_dtime() const
{
    return m_ray.m_dtime;
}

inline bool ShadingPoint::hit() const
{
    return m_primitive_type != PrimitiveNone;
}

inline ShadingPoint::PrimitiveType ShadingPoint::get_primitive_type() const
{
    return m_primitive_type;
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

inline const foundation::Vector2d& ShadingPoint::get_uv(const size_t uvset) const
{
    assert(hit());
    assert(uvset == 0);     // todo: support multiple UV sets

    if (!(m_members & HasUV0))
    {
        cache_source_geometry();

        if (m_primitive_type == PrimitiveTriangle)
        {
            // Compute the texture coordinates.
            const foundation::Vector2d v0_uv(m_v0_uv);
            const foundation::Vector2d v1_uv(m_v1_uv);
            const foundation::Vector2d v2_uv(m_v2_uv);
            m_uv =
                  v0_uv * (1.0 - m_bary[0] - m_bary[1])
                + v1_uv * m_bary[0]
                + v2_uv * m_bary[1];
        }
        else
        {
            assert(m_primitive_type == PrimitiveCurve);
            m_uv = m_bary;
        }

        // Texture coordinates from UV set #0 are now available.
        m_members |= HasUV0;
    }

    return m_uv;
}

inline const foundation::Vector3d& ShadingPoint::get_point() const
{
    assert(hit());

    if (!(m_members & HasPoint))
    {
        m_point = m_ray.point_at(m_ray.m_tmax);
        m_members |= HasPoint;
    }

    return m_point;
}

inline const foundation::Vector3d& ShadingPoint::get_offset_point(const foundation::Vector3d& direction) const
{
    assert(hit());
    assert(m_members & HasRefinedPoints);

    return
        foundation::dot(m_asm_geo_normal, direction) > 0.0
            ? m_front_point
            : m_back_point;
}

inline const foundation::Vector3d& ShadingPoint::get_dpdu(const size_t uvset) const
{
    assert(hit());
    assert(uvset == 0);     // todo: support multiple UV sets

    if (!(m_members & HasPartialDerivatives))
    {
        compute_partial_derivatives();
        m_members |= HasPartialDerivatives;
    }

    return m_dpdu;
}

inline const foundation::Vector3d& ShadingPoint::get_dpdv(const size_t uvset) const
{
    assert(hit());
    assert(uvset == 0);     // todo: support multiple UV sets

    if (!(m_members & HasPartialDerivatives))
    {
        compute_partial_derivatives();
        m_members |= HasPartialDerivatives;
    }

    return m_dpdv;
}

inline const foundation::Vector3d& ShadingPoint::get_geometric_normal() const
{
    assert(hit());

    if (!(m_members & HasGeometricNormal))
    {
        compute_geometric_normal();
        m_members |= HasGeometricNormal;
    }

    return m_geometric_normal;
}

inline const foundation::Vector3d& ShadingPoint::get_shading_normal() const
{
    assert(hit());

    if (!(m_members & HasShadingNormal))
    {
        compute_shading_normal();
        m_members |= HasShadingNormal;
    }

    return m_shading_normal;
}

inline const foundation::Vector3d& ShadingPoint::get_original_shading_normal() const
{
    assert(hit());

    if (!(m_members & HasOriginalShadingNormal))
    {
        compute_original_shading_normal();
        m_members |= HasOriginalShadingNormal;
    }

    return m_original_shading_normal;
}

inline const foundation::Basis3d& ShadingPoint::get_shading_basis() const
{
    assert(hit());

    if (!(m_members & HasShadingBasis))
    {
        compute_shading_basis();
        m_members |= HasShadingBasis;
    }

    return m_shading_basis;
}

inline ObjectInstance::Side ShadingPoint::get_side() const
{
    assert(hit());
    get_geometric_normal();
    return m_side;
}

inline const foundation::Vector3d& ShadingPoint::get_vertex(const size_t i) const
{
    assert(hit());
    assert(m_primitive_type == PrimitiveTriangle);
    assert(i < 3);

    if (!(m_members & HasWorldSpaceTriangleVertices))
    {
        compute_world_space_triangle_vertices();
        m_members |= HasWorldSpaceTriangleVertices;
    }

    return (&m_v0_w)[i];
}

inline const foundation::Vector3d& ShadingPoint::get_point_velocity() const
{
    assert(hit());

    if (!(m_members & HasPointVelocity))
    {
        compute_point_velocity();
        m_members |= HasPointVelocity;
    }

    return m_point_velocity;
}

inline const Material* ShadingPoint::get_material() const
{
    assert(hit());

    if (!(m_members & HasMaterial))
    {
        cache_source_geometry();

        m_material = 0;

        // Proceed with retrieving the material only if the hit primitive has one.
        if (m_primitive_pa != Triangle::None)
        {
            // Retrieve material indices from the object instance.
            const MaterialArray& materials =
                get_side() == ObjectInstance::BackSide
                    ? m_object_instance->get_back_materials()
                    : m_object_instance->get_front_materials();

            // Fetch the material.
            if (static_cast<size_t>(m_primitive_pa) < materials.size())
                m_material = materials[m_primitive_pa];
        }

        // The material at the intersection point is now available.
        m_members |= HasMaterial;
    }

    return m_material;
}

inline const AssemblyInstance& ShadingPoint::get_assembly_instance() const
{
    assert(hit());
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

inline size_t ShadingPoint::get_primitive_index() const
{
    assert(hit());
    return m_primitive_index;
}

inline size_t ShadingPoint::get_primitive_attribute_index() const
{
    assert(hit());
    cache_source_geometry();
    return static_cast<size_t>(m_primitive_pa);
}

inline void ShadingPoint::cache_source_geometry() const
{
    if (!(m_members & HasSourceGeometry))
    {
        fetch_source_geometry();
        m_members |= HasSourceGeometry;
    }
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_KERNEL_SHADING_SHADINGPOINT_H
