
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
#include "renderer/modeling/object/triangle.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/memory/poolallocator.h"
#include "foundation/utility/attributeset.h"
#include "foundation/utility/lazy.h"
#include "foundation/utility/numerictype.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <vector>

namespace renderer
{

//
// A tessellation as a collection of polygonal primitives.
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

    // Primary features.
    VectorArray                 m_vertices;
    VectorArray                 m_vertex_normals;
    PrimitiveArray              m_primitives;

    // Additional attributes.
    // todo: we could live with a single attribute set with multiple channels.
    foundation::AttributeSet    m_tessellation_attributes;
    foundation::AttributeSet    m_vertex_attributes;
    foundation::AttributeSet    m_vertex_normal_attributes;
    foundation::AttributeSet    m_vertex_tangent_attributes;
    foundation::AttributeSet    m_vertex_tangent_poses;
    foundation::AttributeSet    m_primitive_attributes;

    // Constructor.
    StaticTessellation();

    // Insert and access texture coordinates.
    void reserve_tex_coords(const size_t count);
    size_t push_tex_coords(const GVector2& uv);
    size_t get_tex_coords_count() const;
    GVector2 get_tex_coords(const size_t index) const;

    // Insert and access vertex tangents.
    void reserve_vertex_tangents(const size_t count);
    size_t push_vertex_tangent(const GVector3& tangent);    // the tangent must be unit-length
    size_t get_vertex_tangent_count() const;
    GVector3 get_vertex_tangent(const size_t index) const;

    // Set/get the number of motion segments (the number of motion vectors per vertex).
    void set_motion_segment_count(const size_t count);
    size_t get_motion_segment_count() const;

    // Set/get a vertex position for a given motion segment.
    // All vertices must have been inserted before vertex poses can be set.
    // Conversely, no vertex can be inserted after vertex poses have been set.
    void set_vertex_pose(
        const size_t    vertex_index,
        const size_t    motion_segment_index,
        const GVector3& vertex);
    GVector3 get_vertex_pose(
        const size_t    vertex_index,
        const size_t    motion_segment_index) const;

    // Remove all vertex poses.
    void clear_vertex_poses();

    // Set/get a vertex normal for a given motion segment.
    // All vertex normals must have been inserted before vertex normal poses can be set.
    // Conversely, no vertex normal can be inserted after vertex normal poses have been set.
    void set_vertex_normal_pose(
        const size_t    normal_index,
        const size_t    motion_segment_index,
        const GVector3& normal);
    GVector3 get_vertex_normal_pose(
        const size_t    normal_index,
        const size_t    motion_segment_index) const;

    // Remove all vertex normal poses.
    void clear_vertex_normal_poses();

    // Set/get a vertex tangent for a given motion segment.
    // All vertex tangents must have been inserted before vertex tangent poses can be set.
    // Conversely, no vertex tangent can be inserted after vertex tangent poses have been set.
    void set_vertex_tangent_pose(
        const size_t    tangent_index,
        const size_t    motion_segment_index,
        const GVector3& tangent);
    GVector3 get_vertex_tangent_pose(
        const size_t    tangent_index,
        const size_t    motion_segment_index) const;

    // Remove all vertex tangent poses.
    void clear_vertex_tangent_poses();

    // Compute the local space bounding box of the tessellation over the shutter interval.
    GAABB3 compute_local_bbox() const;

  private:
    foundation::AttributeSet::ChannelID m_uv_0_cid;         // UV coordinates set #0
    foundation::AttributeSet::ChannelID m_tangents_cid;     // per-vertex tangent vectors
    foundation::AttributeSet::ChannelID m_ms_count_cid;     // motion segment count
    foundation::AttributeSet::ChannelID m_vp_cid;           // vertex poses
    foundation::AttributeSet::ChannelID m_vnp_cid;          // vertex normal poses
    foundation::AttributeSet::ChannelID m_vtp_cid;          // vertex tangent poses

    void create_uv_0_attribute();
    void create_tangents_attribute();
};

// Specialization of the StaticTessellation class for triangles.
typedef StaticTessellation<Triangle> StaticTriangleTess;


//
// Static triangle tessellation access cache.
//

typedef foundation::AccessCache<
    StaticTriangleTess,
    16,
    1,
    foundation::PoolAllocator<void, 16>
> StaticTriangleTessAccessCache;


//
// StaticTessellation class implementation.
//

template <typename Primitive>
inline StaticTessellation<Primitive>::StaticTessellation()
  : m_uv_0_cid(foundation::AttributeSet::InvalidChannelID)
  , m_tangents_cid(foundation::AttributeSet::InvalidChannelID)
  , m_ms_count_cid(foundation::AttributeSet::InvalidChannelID)
  , m_vp_cid(foundation::AttributeSet::InvalidChannelID)
  , m_vnp_cid(foundation::AttributeSet::InvalidChannelID)
  , m_vtp_cid(foundation::AttributeSet::InvalidChannelID)
{
}

template <typename Primitive>
inline void StaticTessellation<Primitive>::reserve_tex_coords(const size_t count)
{
    if (m_uv_0_cid == foundation::AttributeSet::InvalidChannelID)
        create_uv_0_attribute();

    m_vertex_attributes.reserve_attributes(m_uv_0_cid, count);
}

template <typename Primitive>
inline size_t StaticTessellation<Primitive>::push_tex_coords(const GVector2& uv)
{
    if (m_uv_0_cid == foundation::AttributeSet::InvalidChannelID)
        create_uv_0_attribute();

    return m_vertex_attributes.push_attribute(m_uv_0_cid, uv);
}

template <typename Primitive>
inline size_t StaticTessellation<Primitive>::get_tex_coords_count() const
{
    if (m_uv_0_cid == foundation::AttributeSet::InvalidChannelID)
        return 0;

    return m_vertex_attributes.get_attribute_count(m_uv_0_cid);
}

template <typename Primitive>
inline GVector2 StaticTessellation<Primitive>::get_tex_coords(const size_t index) const
{
    assert(m_uv_0_cid != foundation::AttributeSet::InvalidChannelID);

    GVector2 uv;
    m_vertex_attributes.get_attribute(m_uv_0_cid, index, &uv);

    return uv;
}

template <typename Primitive>
inline void StaticTessellation<Primitive>::reserve_vertex_tangents(const size_t count)
{
    if (m_tangents_cid == foundation::AttributeSet::InvalidChannelID)
        create_tangents_attribute();

    m_vertex_attributes.reserve_attributes(m_tangents_cid, count);
}

template <typename Primitive>
inline size_t StaticTessellation<Primitive>::push_vertex_tangent(const GVector3& tangent)
{
    if (m_tangents_cid == foundation::AttributeSet::InvalidChannelID)
        create_tangents_attribute();

    return m_vertex_attributes.push_attribute(m_tangents_cid, tangent);
}

template <typename Primitive>
inline size_t StaticTessellation<Primitive>::get_vertex_tangent_count() const
{
    if (m_tangents_cid == foundation::AttributeSet::InvalidChannelID)
        return 0;

    return m_vertex_attributes.get_attribute_count(m_tangents_cid);
}

template <typename Primitive>
inline GVector3 StaticTessellation<Primitive>::get_vertex_tangent(const size_t index) const
{
    assert(m_tangents_cid != foundation::AttributeSet::InvalidChannelID);

    GVector3 tangent;
    m_vertex_attributes.get_attribute(m_tangents_cid, index, &tangent);

    return tangent;
}

template <typename Primitive>
inline void StaticTessellation<Primitive>::set_motion_segment_count(const size_t count)
{
    if (m_ms_count_cid == foundation::AttributeSet::InvalidChannelID)
    {
        m_ms_count_cid =
            m_tessellation_attributes.create_channel(
                "motion_segment_count",
                foundation::NumericTypeUInt32,
                1);
    }

    m_tessellation_attributes.set_attribute(m_ms_count_cid, 0, static_cast<std::uint32_t>(count));
}

template <typename Primitive>
inline size_t StaticTessellation<Primitive>::get_motion_segment_count() const
{
    if (m_ms_count_cid == foundation::AttributeSet::InvalidChannelID)
        return 0;

    std::uint32_t count;
    m_tessellation_attributes.get_attribute(m_ms_count_cid, 0, &count);

    return count;
}

template <typename Primitive>
inline void StaticTessellation<Primitive>::set_vertex_pose(
    const size_t    vertex_index,
    const size_t    motion_segment_index,
    const GVector3& vertex)
{
    assert(vertex_index < m_vertices.size());

    const size_t motion_segment_count = get_motion_segment_count();
    assert(motion_segment_index < motion_segment_count);

    if (m_vp_cid == foundation::AttributeSet::InvalidChannelID)
    {
        m_vp_cid =
            m_vertex_attributes.create_channel(
                "vertex_poses",
                foundation::NumericType::id<GVector3::ValueType>(),
                3);
    }

    m_vertex_attributes.set_attribute(
        m_vp_cid,
        vertex_index * motion_segment_count + motion_segment_index,
        vertex);
}

template <typename Primitive>
inline GVector3 StaticTessellation<Primitive>::get_vertex_pose(
    const size_t    vertex_index,
    const size_t    motion_segment_index) const
{
    assert(m_vp_cid != foundation::AttributeSet::InvalidChannelID);
    assert(vertex_index < m_vertices.size());

    const size_t motion_segment_count = get_motion_segment_count();
    assert(motion_segment_index < motion_segment_count);

    GVector3 vertex;
    m_vertex_attributes.get_attribute(
        m_vp_cid,
        vertex_index * motion_segment_count + motion_segment_index,
        &vertex);

    return vertex;
}

template <typename Primitive>
void StaticTessellation<Primitive>::clear_vertex_poses()
{
    if (m_vp_cid != foundation::AttributeSet::InvalidChannelID)
    {
        m_vertex_attributes.delete_channel(m_vp_cid);
        m_vp_cid = foundation::AttributeSet::InvalidChannelID;
    }
}

template <typename Primitive>
inline void StaticTessellation<Primitive>::set_vertex_normal_pose(
    const size_t    normal_index,
    const size_t    motion_segment_index,
    const GVector3& normal)
{
    assert(normal_index < m_vertex_normals.size());

    const size_t motion_segment_count = get_motion_segment_count();
    assert(motion_segment_index < motion_segment_count);

    if (m_vnp_cid == foundation::AttributeSet::InvalidChannelID)
    {
        m_vnp_cid =
            m_vertex_normal_attributes.create_channel(
                "vertex_normal_poses",
                foundation::NumericType::id<GVector3::ValueType>(),
                3);
    }

    m_vertex_normal_attributes.set_attribute(
        m_vnp_cid,
        normal_index * motion_segment_count + motion_segment_index,
        normal);
}

template <typename Primitive>
inline GVector3 StaticTessellation<Primitive>::get_vertex_normal_pose(
    const size_t    normal_index,
    const size_t    motion_segment_index) const
{
    assert(m_vnp_cid != foundation::AttributeSet::InvalidChannelID);
    assert(normal_index < m_vertex_normals.size());

    const size_t motion_segment_count = get_motion_segment_count();
    assert(motion_segment_index < motion_segment_count);

    GVector3 normal;
    m_vertex_normal_attributes.get_attribute(
        m_vnp_cid,
        normal_index * motion_segment_count + motion_segment_index,
        &normal);

    return normal;
}

template <typename Primitive>
void StaticTessellation<Primitive>::clear_vertex_normal_poses()
{
    if (m_vnp_cid != foundation::AttributeSet::InvalidChannelID)
    {
        m_vertex_normal_attributes.delete_channel(m_vnp_cid);
        m_vnp_cid = foundation::AttributeSet::InvalidChannelID;
    }
}

template <typename Primitive>
inline void StaticTessellation<Primitive>::set_vertex_tangent_pose(
    const size_t    tangent_index,
    const size_t    motion_segment_index,
    const GVector3& tangent)
{
    assert(tangent_index < get_vertex_tangent_count());

    const size_t motion_segment_count = get_motion_segment_count();
    assert(motion_segment_index < motion_segment_count);

    if (m_vtp_cid == foundation::AttributeSet::InvalidChannelID)
    {
        m_vtp_cid =
            m_vertex_tangent_poses.create_channel(
                "vertex_tangent_poses",
                foundation::NumericType::id<GVector3::ValueType>(),
                3);
    }

    m_vertex_tangent_poses.set_attribute(
        m_vtp_cid,
        tangent_index * motion_segment_count + motion_segment_index,
        tangent);
}

template <typename Primitive>
inline GVector3 StaticTessellation<Primitive>::get_vertex_tangent_pose(
    const size_t    tangent_index,
    const size_t    motion_segment_index) const
{
    assert(m_vtp_cid != foundation::AttributeSet::InvalidChannelID);
    assert(tangent_index < m_vertex_normals.size());

    const size_t motion_segment_count = get_motion_segment_count();
    assert(motion_segment_index < motion_segment_count);

    GVector3 tangent;
    m_vertex_tangent_poses.get_attribute(
        m_vtp_cid,
        tangent_index * motion_segment_count + motion_segment_index,
        &tangent);

    return tangent;
}

template <typename Primitive>
void StaticTessellation<Primitive>::clear_vertex_tangent_poses()
{
    if (m_vtp_cid != foundation::AttributeSet::InvalidChannelID)
    {
        m_vertex_tangent_poses.delete_channel(m_vtp_cid);
        m_vtp_cid = foundation::AttributeSet::InvalidChannelID;
    }
}

template <typename Primitive>
GAABB3 StaticTessellation<Primitive>::compute_local_bbox() const
{
    GAABB3 bbox;
    bbox.invalidate();

    const size_t vertex_count = m_vertices.size();
    const size_t motion_segment_count = get_motion_segment_count();

    for (size_t i = 0; i < vertex_count; ++i)
    {
        bbox.insert(m_vertices[i]);

        for (size_t j = 0; j < motion_segment_count; ++j)
            bbox.insert(get_vertex_pose(i, j));
    }

    return bbox;
}

template <typename Primitive>
void StaticTessellation<Primitive>::create_uv_0_attribute()
{
    m_uv_0_cid =
        m_vertex_attributes.create_channel(
            "uv_0",
            foundation::NumericType::id<GVector2::ValueType>(),
            2);
}

template <typename Primitive>
void StaticTessellation<Primitive>::create_tangents_attribute()
{
    m_tangents_cid =
        m_vertex_attributes.create_channel(
            "tangents",
            foundation::NumericType::id<GVector3::ValueType>(),
            3);
}

}   // namespace renderer
