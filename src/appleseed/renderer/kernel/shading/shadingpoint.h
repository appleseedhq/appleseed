
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
#include "renderer/kernel/intersection/intersectionsettings.h"
#include "renderer/kernel/shading/shadingray.h"
#include "renderer/kernel/tessellation/statictessellation.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/object/curveobject.h"
#include "renderer/modeling/object/triangle.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/visibilityflags.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/math/basis.h"
#include "foundation/math/beziercurve.h"
#include "foundation/math/transform.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/poison.h"

// OSL headers.
#include "foundation/platform/_beginoslheaders.h"
#include "OSL/oslexec.h"
#include "OSL/shaderglobals.h"
#include "foundation/platform/_endoslheaders.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdint>

// Forward declarations.
namespace renderer  { class Object; }
namespace renderer  { class OSLShaderGroupExec; }
namespace renderer  { class ShaderGroup; }
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
        PrimitiveNone               = 0,

        PrimitiveTriangle           = 1UL << 1,
        PrimitiveProceduralSurface  = 1UL << 2,

        PrimitiveCurve              = 1UL << 3,
        PrimitiveCurve1             = PrimitiveCurve | 0,
        PrimitiveCurve3             = PrimitiveCurve | 1,

        PrimitiveVolume             = 1UL << 4
    };

    // Constructor, calls clear().
    ShadingPoint();

    // Copy constructor.
    explicit ShadingPoint(const ShadingPoint& rhs);

    // Assignment.
    ShadingPoint& operator=(const ShadingPoint& rhs);

    // Reset the shading point to its initial state (no intersection).
    void clear();

    // Initialize this shading point as a point in participating media,
    // given a ray through volume and a distance from origin.
    void create_volume_shading_point(
        const ShadingPoint&     prev_shading_point,
        const ShadingRay&       volume_ray,
        const float             distance);

    // Return the scene that was tested for intersection.
    const Scene& get_scene() const;

    // Set/get the world space ray that was cast through the scene.
    void set_ray(const ShadingRay& ray);
    const ShadingRay& get_ray() const;

    // Return the time stored in the ray.
    const ShadingRay::Time& get_time() const;

    // Return true if the shading point is either
    // a surface point or a volume point and can be shaded.
    bool is_valid() const;

    // Return true if an intersection with some surface was found, false otherwise.
    bool hit_surface() const;

    // Return true if the shading point is located inside a participating medium.
    bool hit_volume() const;

    // Return the type of the hit primitive.
    PrimitiveType get_primitive_type() const;
    bool is_triangle_primitive() const;
    bool is_curve_primitive() const;

    // Return the distance from the ray origin to the intersection point.
    double get_distance() const;

    // Return the barycentric coordinates of the intersection point.
    const foundation::Vector2f& get_bary() const;

    // Return the texture coordinates from a given UV set at the intersection point.
    const foundation::Vector2f& get_uv(const size_t uvset) const;

    // Return the screen space partial derivatives of the texture coordinates from a given UV set.
    const foundation::Vector2f& get_duvdx(const size_t uvset) const;
    const foundation::Vector2f& get_duvdy(const size_t uvset) const;

    // Return the intersection point in world space.
    const foundation::Vector3d& get_point() const;

    // Return the intersection point, properly offset to avoid self-intersections.
    // The intersection point and direction space depends on the primitive type.
    const foundation::Vector3d& get_offset_point(const foundation::Vector3d& direction) const;

    // Return the world space partial derivatives of the intersection point wrt. a given UV set.
    const foundation::Vector3d& get_dpdu(const size_t uvset) const;
    const foundation::Vector3d& get_dpdv(const size_t uvset) const;

    // Return the world space partial derivatives of the intersection normal wrt. a given UV set.
    const foundation::Vector3d& get_dndu(const size_t uvset) const;
    const foundation::Vector3d& get_dndv(const size_t uvset) const;

    // Return the screen space partial derivatives of the intersection point.
    const foundation::Vector3d& get_dpdx() const;
    const foundation::Vector3d& get_dpdy() const;

    // Return the world space geometric normal at the intersection point. The geometric normal
    // always faces the incoming ray, i.e. dot(ray_dir, geometric_normal) is always positive or null.
    const foundation::Vector3d& get_geometric_normal() const;

    // Return the original world space shading normal at the intersection point.
    const foundation::Vector3d& get_original_shading_normal() const;

    // Return the (possibly modified) world space shading normal at the intersection point.
    // The shading normal is always in the same hemisphere as the geometric normal but it is
    // not necessarily facing the incoming ray, i.e. dot(ray_dir, shading_normal) may be negative.
    const foundation::Vector3d& get_shading_normal() const;

    // Set/get the world space orthonormal basis around the (possibly modified) shading normal.
    void set_shading_basis(const foundation::Basis3d& basis) const;
    const foundation::Basis3d& get_shading_basis() const;

    // Return the side of the surface that was hit.
    ObjectInstance::Side get_side() const;

    // Flip the side on which this shading point lies.
    void flip_side();

    // Return true if the ray is entering/leaving an object.
    bool is_entering() const;
    bool is_leaving() const;

    // Return the i'th world space vertex of the hit triangle.
    const foundation::Vector3d& get_vertex(const size_t i) const;

    // Return the world space point velocity.
    const foundation::Vector3d& get_world_space_point_velocity() const;

    // Return the material of the side (front or back) that was hit, at the intersection point, or 0 if there is none.
    const Material* get_material() const;

    // Return the material of the opposite side to the side that was hit, at the intersection point, or 0 if there is none.
    const Material* get_opposite_material() const;

    // Return the assembly instance that was hit.
    const AssemblyInstance& get_assembly_instance() const;

    // Return the transform at ray time of the assembly instance that was hit.
    const foundation::Transformd& get_assembly_instance_transform() const;

    // Return the assembly that was hit.
    const Assembly& get_assembly() const;

    // Return the object instance that was hit.
    ObjectInstance& get_object_instance() const;

    // Return the object that was hit.
    const Object& get_object() const;

    // Return the index, within the assembly, of the object instance that was hit.
    size_t get_object_instance_index() const;

    // Return the index of the hit primitive.
    size_t get_primitive_index() const;

    // Return the index of the primitive attribute.
    size_t get_primitive_attribute_index() const;

    // Return the opacity at the intersection point.
    const Alpha& get_alpha() const;

    // Return the interpolated per-vertex color at the intersection point.
    const foundation::Color3f& get_per_vertex_color() const;

    // Access the data structure that exposes this shading point to OSL.
    // ShadingSystem::execute() takes a mutable OSL::ShaderGlobals reference.
    OSL::ShaderGlobals& get_osl_shader_globals() const;

    struct OSLObjectTransformInfo
    {
        bool is_animated() const;

        OSL::Matrix44 get_transform() const;
        OSL::Matrix44 get_transform(const float t) const;

        OSL::Matrix44 get_inverse_transform() const;
        OSL::Matrix44 get_inverse_transform(const float t) const;

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

  private:
    friend class AssemblyLeafProbeVisitor;
    friend class AssemblyLeafVisitor;
    friend class CurveLeafVisitor;
    friend class EmbreeScene;
    friend class Intersector;
    friend class NPRSurfaceShaderHelper;
    friend class OSLShaderGroupExec;
    friend class RendererServices;
    friend class ShadingPointBuilder;
    friend class TriangleLeafVisitor;
    friend class foundation::PoisonImpl<ShadingPoint>;

    //
    // Make sure to update `PoisonImpl<>::do_poison()` in shadinpoint.cpp when adding new data members.
    //

    // Context.
    TextureCache*                       m_texture_cache;
    const Scene*                        m_scene;
    mutable ShadingRay                  m_ray;                              // world space ray (m_tmax = distance to intersection)

    // Primary intersection results.
    PrimitiveType                       m_primitive_type;                   // type of the hit primitive
    foundation::Vector2f                m_bary;                             // barycentric coordinates of intersection point
    const AssemblyInstance*             m_assembly_instance;                // hit assembly instance
    foundation::Transformd              m_assembly_instance_transform;      // transform of the hit assembly instance at ray time
    const TransformSequence*            m_assembly_instance_transform_seq;  // transform sequence of the hit assembly instance.
    size_t                              m_object_instance_index;            // index of the object instance that was hit
    size_t                              m_primitive_index;                  // index of the hit primitive
    TriangleSupportPlaneType            m_triangle_support_plane;           // support plane of the hit triangle

    // Flags to keep track of which on-demand results have been computed and cached.
    enum Members
    {
        HasSourceGeometry               = 1UL << 0,
        HasTriangleVertexNormals        = 1UL << 1,
        HasTriangleVertexTangents       = 1UL << 2,
        HasUV0                          = 1UL << 3,
        HasPoint                        = 1UL << 4,
        HasRefinedPoints                = 1UL << 5,
        HasWorldSpaceDerivatives        = 1UL << 6,
        HasGeometricNormal              = 1UL << 7,
        HasOriginalShadingNormal        = 1UL << 8,
        HasShadingBasis                 = 1UL << 9,
        HasWorldSpaceTriangleVertices   = 1UL << 10,
        HasMaterials                    = 1UL << 11,
        HasWorldSpacePointVelocity      = 1UL << 12,
        HasAlpha                        = 1UL << 13,
        HasPerVertexColor               = 1UL << 14,
        HasScreenSpaceDerivatives       = 1UL << 15,
        HasOSLShaderGlobals             = 1UL << 16
    };
    mutable std::uint32_t               m_members;

    // Source geometry (derived from primary intersection results).
    mutable const Assembly*             m_assembly;                     // hit assembly
    mutable ObjectInstance*             m_object_instance;              // hit object instance
    mutable Object*                     m_object;                       // hit object
    mutable std::uint32_t               m_primitive_pa;                 // hit primitive attribute index
    mutable GVector2                    m_v0_uv, m_v1_uv, m_v2_uv;      // texture coordinates from UV set #0 at triangle vertices
    mutable GVector3                    m_v0, m_v1, m_v2;               // object instance space triangle vertices
    mutable GVector3                    m_n0, m_n1, m_n2;               // object instance space triangle vertex normals
    mutable GVector3                    m_t0, m_t1, m_t2;               // object instance space triangle vertex tangents

    // On-demand intersection results (derived from primary intersection results).
    mutable foundation::Vector2f        m_uv;                           // texture coordinates from UV set #0
    mutable foundation::Vector2f        m_duvdx;                        // screen space partial derivative of the texture coords wrt. X
    mutable foundation::Vector2f        m_duvdy;                        // screen space partial derivative of the texture coords wrt. Y
    mutable foundation::Vector3d        m_point;                        // world space intersection point
    mutable foundation::Vector3d        m_dpdu;                         // world space partial derivative of the intersection point wrt. U
    mutable foundation::Vector3d        m_dpdv;                         // world space partial derivative of the intersection point wrt. V
    mutable foundation::Vector3d        m_dndu;                         // world space partial derivative of the intersection normal wrt. U
    mutable foundation::Vector3d        m_dndv;                         // world space partial derivative of the intersection normal wrt. V
    mutable foundation::Vector3d        m_dpdx;                         // screen space partial derivative of the intersection point wrt. X
    mutable foundation::Vector3d        m_dpdy;                         // screen space partial derivative of the intersection point wrt. Y
    mutable foundation::Vector3d        m_geometric_normal;             // world space geometric normal, unit-length
    mutable foundation::Vector3d        m_original_shading_normal;      // original world space shading normal, unit-length
    mutable foundation::Basis3d         m_shading_basis;                // world space orthonormal basis around shading normal
    mutable ObjectInstance::Side        m_side;                         // side of the surface that was hit
    mutable foundation::Vector3d        m_v0_w, m_v1_w, m_v2_w;         // world space triangle vertices
    mutable foundation::Vector3d        m_point_velocity;               // world space point velocity
    mutable const Material*             m_material;                     // material at intersection point
    mutable const Material*             m_opposite_material;            // opposite material at intersection point
    mutable Alpha                       m_alpha;                        // opacity at intersection point
    mutable foundation::Color3f         m_color;                        // per-vertex interpolated color at intersection point

    // Data required to avoid self-intersections.
    mutable foundation::Vector3d        m_refine_space_geo_normal;       // primitive-specific space geometric normal to hit primitive (non unit-length)
    mutable foundation::Vector3d        m_refine_space_front_point;      // hit point refined to front, in primitive specific space
    mutable foundation::Vector3d        m_refine_space_back_point;       // hit point refined to back, in primitive specific space

    // OSL-related data.
    mutable OSLObjectTransformInfo      m_obj_transform_info;
    mutable OSLTraceData                m_osl_trace_data;
    mutable OSL::ShaderGlobals          m_shader_globals;

    // NPR-related data.
    mutable foundation::Color3f         m_surface_shader_diffuse;
    mutable foundation::Color3f         m_surface_shader_glossy;
    mutable foundation::Color3f         m_surface_shader_emission;

    // Fetch and cache the source geometry.
    void cache_source_geometry() const;

    // Fetch the source geometry.
    void fetch_source_geometry() const;
    void fetch_triangle_source_geometry() const;
    void fetch_curve_source_geometry() const;

    // Refine and offset the intersection point.
    void refine_and_offset() const;

    void compute_world_space_partial_derivatives() const;
    void compute_screen_space_partial_derivatives() const;
    void compute_normals() const;
    void compute_triangle_normals() const;
    void compute_curve_normals() const;
    void compute_geometric_normal() const;
    void compute_shading_normal() const;
    void compute_original_shading_normal() const;
    void compute_shading_basis() const;
    void compute_world_space_triangle_vertices() const;
    void compute_world_space_point_velocity() const;

    void compute_alpha() const;
    void compute_per_vertex_color() const;

    void fetch_materials() const;

    void initialize_osl_shader_globals(
        const ShaderGroup&              sg,
        const VisibilityFlags::Type     ray_flags,
        OSL::RendererServices*          renderer) const;
};


//
// ShadingPoint class implementation.
//

APPLESEED_FORCE_INLINE ShadingPoint::ShadingPoint()
{
    foundation::debug_poison(*this);

    clear();
}

inline ShadingPoint::ShadingPoint(const ShadingPoint& rhs)
  : m_texture_cache(rhs.m_texture_cache)
  , m_scene(rhs.m_scene)
  , m_ray(rhs.m_ray)
  , m_primitive_type(rhs.m_primitive_type)
  , m_bary(rhs.m_bary)
  , m_assembly_instance(rhs.m_assembly_instance)
  , m_assembly_instance_transform(rhs.m_assembly_instance_transform)
  , m_assembly_instance_transform_seq(rhs.m_assembly_instance_transform_seq)
  , m_object_instance_index(rhs.m_object_instance_index)
  , m_primitive_index(rhs.m_primitive_index)
  , m_triangle_support_plane(rhs.m_triangle_support_plane)
  , m_members(0)
{
}

inline ShadingPoint& ShadingPoint::operator=(const ShadingPoint& rhs)
{
    m_texture_cache = rhs.m_texture_cache;
    m_scene = rhs.m_scene;
    m_ray = rhs.m_ray;
    m_primitive_type = rhs.m_primitive_type;
    m_bary = rhs.m_bary;
    m_assembly_instance = rhs.m_assembly_instance;
    m_assembly_instance_transform = rhs.m_assembly_instance_transform;
    m_assembly_instance_transform_seq = rhs.m_assembly_instance_transform_seq;
    m_object_instance_index = rhs.m_object_instance_index;
    m_primitive_index = rhs.m_primitive_index;
    m_triangle_support_plane = rhs.m_triangle_support_plane;
    m_members = 0;
    return *this;
}

APPLESEED_FORCE_INLINE void ShadingPoint::clear()
{
    m_texture_cache = nullptr;
    m_scene = nullptr;
    m_primitive_type = PrimitiveNone;
    m_members = 0;
}

inline const Scene& ShadingPoint::get_scene() const
{
    assert(m_scene);
    return *m_scene;
}

inline void ShadingPoint::set_ray(const ShadingRay& ray)
{
    assert(foundation::is_normalized(ray.m_dir));
    m_ray = ray;
}

inline const ShadingRay& ShadingPoint::get_ray() const
{
    return m_ray;
}

inline const ShadingRay::Time& ShadingPoint::get_time() const
{
    return m_ray.m_time;
}

inline bool ShadingPoint::is_valid() const
{
    return m_primitive_type != PrimitiveNone;
}

inline bool ShadingPoint::hit_surface() const
{
    return is_valid() && !hit_volume();
}

inline bool ShadingPoint::hit_volume() const
{
    return m_primitive_type == PrimitiveVolume;
}

inline ShadingPoint::PrimitiveType ShadingPoint::get_primitive_type() const
{
    return m_primitive_type;
}

inline bool ShadingPoint::is_triangle_primitive() const
{
    return (m_primitive_type & PrimitiveTriangle) != 0;
}

inline bool ShadingPoint::is_curve_primitive() const
{
    return (m_primitive_type & PrimitiveCurve) != 0;
}

inline double ShadingPoint::get_distance() const
{
    assert(is_valid());
    return m_ray.m_tmax;
}

inline const foundation::Vector2f& ShadingPoint::get_bary() const
{
    assert(hit_surface());
    return m_bary;
}

inline const foundation::Vector2f& ShadingPoint::get_uv(const size_t uvset) const
{
    assert(hit_surface());
    assert(uvset == 0);     // todo: support multiple UV sets

    if (!(m_members & HasUV0))
    {
        cache_source_geometry();

        switch (m_primitive_type)
        {
          case PrimitiveTriangle:
            // Compute texture coordinates.
            m_uv =
                  m_v0_uv * (1.0f - m_bary[0] - m_bary[1])
                + m_v1_uv * m_bary[0]
                + m_v2_uv * m_bary[1];
            break;

          case PrimitiveProceduralSurface:
            // Nothing to do.
            break;

          case PrimitiveCurve1:
          case PrimitiveCurve3:
            m_uv = m_bary;
            break;

          assert_otherwise;
        }

        // Texture coordinates from UV set #0 are now available.
        m_members |= HasUV0;
    }

    return m_uv;
}

inline const foundation::Vector2f& ShadingPoint::get_duvdx(const size_t uvset) const
{
    assert(hit_surface());
    assert(uvset == 0);     // todo: support multiple UV sets

    if (!(m_members & HasScreenSpaceDerivatives))
    {
        compute_screen_space_partial_derivatives();
        m_members |= HasScreenSpaceDerivatives;
    }

    return m_duvdx;
}

inline const foundation::Vector2f& ShadingPoint::get_duvdy(const size_t uvset) const
{
    assert(hit_surface());
    assert(uvset == 0);     // todo: support multiple UV sets

    if (!(m_members & HasScreenSpaceDerivatives))
    {
        compute_screen_space_partial_derivatives();
        m_members |= HasScreenSpaceDerivatives;
    }

    return m_duvdy;
}

inline const foundation::Vector3d& ShadingPoint::get_point() const
{
    assert(is_valid());

    if (!(m_members & HasPoint))
    {
        m_point = m_ray.point_at(m_ray.m_tmax);
        m_members |= HasPoint;
    }

    return m_point;
}

inline const foundation::Vector3d& ShadingPoint::get_offset_point(const foundation::Vector3d& direction) const
{
    assert(hit_surface());
    assert(m_members & HasRefinedPoints);

    return
        foundation::dot(m_refine_space_geo_normal, direction) > 0.0
            ? m_refine_space_front_point
            : m_refine_space_back_point;
}

inline const foundation::Vector3d& ShadingPoint::get_dpdu(const size_t uvset) const
{
    assert(hit_surface());
    assert(uvset == 0);     // todo: support multiple UV sets

    if (!(m_members & HasWorldSpaceDerivatives))
    {
        compute_world_space_partial_derivatives();
        m_members |= HasWorldSpaceDerivatives;
    }

    return m_dpdu;
}

inline const foundation::Vector3d& ShadingPoint::get_dpdv(const size_t uvset) const
{
    assert(hit_surface());
    assert(uvset == 0);     // todo: support multiple UV sets

    if (!(m_members & HasWorldSpaceDerivatives))
    {
        compute_world_space_partial_derivatives();
        m_members |= HasWorldSpaceDerivatives;
    }

    return m_dpdv;
}

inline const foundation::Vector3d& ShadingPoint::get_dndu(const size_t uvset) const
{
    assert(hit_surface());
    assert(uvset == 0);     // todo: support multiple UV sets

    if (!(m_members & HasWorldSpaceDerivatives))
    {
        compute_world_space_partial_derivatives();
        m_members |= HasWorldSpaceDerivatives;
    }

    return m_dndu;
}

inline const foundation::Vector3d& ShadingPoint::get_dndv(const size_t uvset) const
{
    assert(hit_surface());
    assert(uvset == 0);     // todo: support multiple UV sets

    if (!(m_members & HasWorldSpaceDerivatives))
    {
        compute_world_space_partial_derivatives();
        m_members |= HasWorldSpaceDerivatives;
    }

    return m_dndv;
}

inline const foundation::Vector3d& ShadingPoint::get_dpdx() const
{
    assert(hit_surface());

    if (!(m_members & HasScreenSpaceDerivatives))
    {
        compute_screen_space_partial_derivatives();
        m_members |= HasScreenSpaceDerivatives;
    }

    return m_dpdx;
}

inline const foundation::Vector3d& ShadingPoint::get_dpdy() const
{
    assert(hit_surface());

    if (!(m_members & HasScreenSpaceDerivatives))
    {
        compute_screen_space_partial_derivatives();
        m_members |= HasScreenSpaceDerivatives;
    }

    return m_dpdy;
}

inline const foundation::Vector3d& ShadingPoint::get_geometric_normal() const
{
    assert(hit_surface());

    if (!(m_members & HasGeometricNormal))
    {
        compute_normals();
        m_members |= HasGeometricNormal | HasOriginalShadingNormal;
    }

    return m_geometric_normal;
}

inline const foundation::Vector3d& ShadingPoint::get_original_shading_normal() const
{
    assert(hit_surface());

    if (!(m_members & HasOriginalShadingNormal))
    {
        compute_normals();
        m_members |= HasGeometricNormal | HasOriginalShadingNormal;
    }

    return m_original_shading_normal;
}

inline const foundation::Vector3d& ShadingPoint::get_shading_normal() const
{
    return get_shading_basis().get_normal();
}

inline void ShadingPoint::set_shading_basis(const foundation::Basis3d& basis) const
{
    assert(hit_surface());
    m_shading_basis = basis;
    m_members |= HasShadingBasis;
    m_members &= ~HasScreenSpaceDerivatives;
}

inline const foundation::Basis3d& ShadingPoint::get_shading_basis() const
{
    assert(hit_surface());

    if (!(m_members & HasShadingBasis))
    {
        compute_shading_basis();
        m_members |= HasShadingBasis;
    }

    return m_shading_basis;
}

inline ObjectInstance::Side ShadingPoint::get_side() const
{
    assert(hit_surface());

    if (!(m_members & HasGeometricNormal))
    {
        compute_normals();
        m_members |= HasGeometricNormal | HasOriginalShadingNormal;
    }

    return m_side;
}

inline bool ShadingPoint::is_entering() const
{
    return get_side() == ObjectInstance::FrontSide;
}

inline bool ShadingPoint::is_leaving() const
{
    return get_side() == ObjectInstance::BackSide;
}

inline const foundation::Vector3d& ShadingPoint::get_vertex(const size_t i) const
{
    assert(hit_surface());
    assert(m_primitive_type == PrimitiveTriangle);
    assert(i < 3);

    if (!(m_members & HasWorldSpaceTriangleVertices))
    {
        compute_world_space_triangle_vertices();
        m_members |= HasWorldSpaceTriangleVertices;
    }

    return (&m_v0_w)[i];
}

inline const foundation::Vector3d& ShadingPoint::get_world_space_point_velocity() const
{
    assert(hit_surface());

    if (!(m_members & HasWorldSpacePointVelocity))
    {
        compute_world_space_point_velocity();
        m_members |= HasWorldSpacePointVelocity;
    }

    return m_point_velocity;
}

inline const Material* ShadingPoint::get_material() const
{
    assert(is_valid());

    if (!(m_members & HasMaterials))
    {
        if (hit_volume())
        {
            m_material = m_ray.get_current_medium()->m_material;
            m_opposite_material = m_ray.get_current_medium()->m_material;
        }
        else
            fetch_materials();

        // The material at the intersection point is now available.
        m_members |= HasMaterials;
    }

    return m_material;
}

inline const Material* ShadingPoint::get_opposite_material() const
{
    assert(is_valid());

    if (!(m_members & HasMaterials))
    {
        if (hit_volume())
        {
            m_material = m_ray.get_current_medium()->m_material;
            m_opposite_material = m_ray.get_current_medium()->m_material;
        }
        else
            fetch_materials();

        // The material at the intersection point is now available.
        m_members |= HasMaterials;
    }

    return m_opposite_material;
}

inline const AssemblyInstance& ShadingPoint::get_assembly_instance() const
{
    assert(hit_surface());
    return *m_assembly_instance;
}

inline const foundation::Transformd& ShadingPoint::get_assembly_instance_transform() const
{
    assert(hit_surface());
    return m_assembly_instance_transform;
}

inline const Assembly& ShadingPoint::get_assembly() const
{
    assert(hit_surface());
    cache_source_geometry();
    return *m_assembly;
}

inline ObjectInstance& ShadingPoint::get_object_instance() const
{
    assert(hit_surface());
    cache_source_geometry();
    return *m_object_instance;
}

inline const Object& ShadingPoint::get_object() const
{
    assert(hit_surface());
    cache_source_geometry();
    return *m_object;
}

inline size_t ShadingPoint::get_object_instance_index() const
{
    assert(hit_surface());
    return m_object_instance_index;
}

inline size_t ShadingPoint::get_primitive_index() const
{
    assert(hit_surface());
    return m_primitive_index;
}

inline size_t ShadingPoint::get_primitive_attribute_index() const
{
    assert(hit_surface());
    cache_source_geometry();
    return static_cast<size_t>(m_primitive_pa);
}

inline const Alpha& ShadingPoint::get_alpha() const
{
    if (!(m_members & HasAlpha))
    {
        compute_alpha();
        m_members |= HasAlpha;
    }

    return m_alpha;
}

inline const foundation::Color3f& ShadingPoint::get_per_vertex_color() const
{
    if (!(m_members & HasPerVertexColor))
    {
        compute_per_vertex_color();
        m_members |= HasPerVertexColor;
    }

    return m_color;
}

inline OSL::ShaderGlobals& ShadingPoint::get_osl_shader_globals() const
{
    assert(hit_surface());
    assert(m_members & HasOSLShaderGlobals);
    return m_shader_globals;
}

inline void ShadingPoint::cache_source_geometry() const
{
    if (!(m_members & HasSourceGeometry))
    {
        fetch_source_geometry();
        m_members |= HasSourceGeometry;
    }
}

inline void ShadingPoint::fetch_materials() const
{
    cache_source_geometry();

    m_material = nullptr;
    m_opposite_material = nullptr;

    // Proceed with retrieving the material only if the hit primitive has one.
    if (m_primitive_pa != Triangle::None)
    {
        // Retrieve material indices from the object instance.
        const MaterialArray* materials;
        const MaterialArray* opposite_materials;
        if (get_side() == ObjectInstance::FrontSide)
        {
            materials = &m_object_instance->get_front_materials();
            opposite_materials = &m_object_instance->get_back_materials();
        }
        else
        {
            materials = &m_object_instance->get_back_materials();
            opposite_materials = &m_object_instance->get_front_materials();
        }

        // Fetch the materials.
        const size_t material_index = static_cast<size_t>(m_primitive_pa);
        if (material_index < materials->size())
            m_material = (*materials)[material_index];
        if (material_index < opposite_materials->size())
            m_opposite_material = (*opposite_materials)[material_index];
    }
}

}   // namespace renderer

namespace foundation
{
    template <>
    class PoisonImpl<renderer::ShadingPoint>
    {
      public:
        static void do_poison(renderer::ShadingPoint& point);
    };
}
