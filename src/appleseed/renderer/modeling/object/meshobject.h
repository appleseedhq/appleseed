
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_OBJECT_MESHOBJECT_H
#define APPLESEED_RENDERER_MODELING_OBJECT_MESHOBJECT_H

// appleseed.renderer headers.
#include "renderer/global/globaltypes.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/object/regionkit.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/lazy.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace foundation    { class StringArray; }
namespace foundation    { class StringDictionary; }
namespace renderer      { class BaseGroup; }
namespace renderer      { class OnFrameBeginRecorder; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }
namespace renderer      { class Source; }
namespace renderer      { class Triangle; }

namespace renderer
{

//
// Mesh object (source geometry).
//
// todo: add support for arbitrary polygonal faces.
//

class APPLESEED_DLLSYMBOL MeshObject
  : public Object
{
  public:
    // Delete this instance.
    virtual void release() APPLESEED_OVERRIDE;

    // Return a string identifying the model of this object.
    virtual const char* get_model() const APPLESEED_OVERRIDE;

    // This method is called once before rendering each frame.
    // Returns true on success, false otherwise.
    virtual bool on_frame_begin(
        const Project&              project,
        const BaseGroup*            parent,
        OnFrameBeginRecorder&       recorder,
        foundation::IAbortSwitch*   abort_switch = 0) APPLESEED_OVERRIDE;

    // This method is called once after rendering each frame (only if on_frame_begin() was called).
    virtual void on_frame_end(
        const Project&              project,
        const BaseGroup*            parent) APPLESEED_OVERRIDE;

    // Return true if this object has an alpha map.
    virtual bool has_alpha_map() const APPLESEED_OVERRIDE;

    // Return the source bound to the alpha map input, or 0 if the object doesn't have an alpha map.
    virtual const Source* get_uncached_alpha_map() const APPLESEED_OVERRIDE;

    // Compute the local space bounding box of the object over the shutter interval.
    virtual GAABB3 compute_local_bbox() const APPLESEED_OVERRIDE;

    // Return the region kit of the object.
    virtual foundation::Lazy<RegionKit>& get_region_kit() APPLESEED_OVERRIDE;

    // Insert and access vertices.
    void reserve_vertices(const size_t count);
    size_t push_vertex(const GVector3& vertex);
    size_t get_vertex_count() const;
    const GVector3& get_vertex(const size_t index) const;

    // Insert and access vertex normals.
    void reserve_vertex_normals(const size_t count);
    size_t push_vertex_normal(const GVector3& normal);      // the normal must be unit-length
    size_t get_vertex_normal_count() const;
    const GVector3& get_vertex_normal(const size_t index) const;
    void clear_vertex_normals();

    // Insert and access vertex tangents.
    void reserve_vertex_tangents(const size_t count);
    size_t push_vertex_tangent(const GVector3& tangent);    // the tangent must be unit-length
    size_t get_vertex_tangent_count() const;
    GVector3 get_vertex_tangent(const size_t index) const;

    // Insert and access texture coordinates.
    void reserve_tex_coords(const size_t count);
    size_t push_tex_coords(const GVector2& tex_coords);
    size_t get_tex_coords_count() const;
    GVector2 get_tex_coords(const size_t index) const;

    // Insert and access triangles.
    void reserve_triangles(const size_t count);
    size_t push_triangle(const Triangle& triangle);
    size_t get_triangle_count() const;
    const Triangle& get_triangle(const size_t index) const;
    Triangle& get_triangle(const size_t index);
    void clear_triangles();

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

    // Insert and access material slots.
    void reserve_material_slots(const size_t count);
    size_t push_material_slot(const char* name);
    virtual size_t get_material_slot_count() const APPLESEED_OVERRIDE;
    virtual const char* get_material_slot(const size_t index) const APPLESEED_OVERRIDE;

    // Expose asset file paths referenced by this entity to the outside.
    virtual void collect_asset_paths(foundation::StringArray& paths) const APPLESEED_OVERRIDE;
    virtual void update_asset_paths(const foundation::StringDictionary& mappings) APPLESEED_OVERRIDE;

  private:
    friend class MeshObjectFactory;

    struct Impl;
    Impl* impl;

    // Constructor.
    MeshObject(
        const char*         name,
        const ParamArray&   params);

    // Destructor.
    ~MeshObject();
};


//
// Mesh object factory.
//

class APPLESEED_DLLSYMBOL MeshObjectFactory
{
  public:
    // Return a string identifying this object model.
    static const char* get_model();

    // Create a new mesh object.
    static foundation::auto_release_ptr<MeshObject> create(
        const char*         name,
        const ParamArray&   params);
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_OBJECT_MESHOBJECT_H
