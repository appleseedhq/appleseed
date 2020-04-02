
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
#include "renderer/kernel/tessellation/statictessellation.h"
#include "renderer/modeling/object/iobjectfactory.h"
#include "renderer/modeling/object/object.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/compiler.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace foundation    { class Dictionary; }
namespace foundation    { class DictionaryArray; }
namespace foundation    { class SearchPaths; }
namespace foundation    { class StringArray; }
namespace foundation    { class StringDictionary; }
namespace renderer      { class ObjectRasterizer; }
namespace renderer      { class ParamArray; }
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
    void release() override;

    // Return a string identifying the model of this object.
    const char* get_model() const override;

    // Return the source bound to the alpha map input, or 0 if the object doesn't have an alpha map.
    const Source* get_uncached_alpha_map() const override;

    // Compute the local space bounding box of the object over the shutter interval.
    GAABB3 compute_local_bbox() const override;

    // Return the static triangle tessellation of the object.
    const StaticTriangleTess& get_static_triangle_tess() const;

    // Send this object to an object rasterizer.
    void rasterize(ObjectRasterizer& drawer) const override;

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
    size_t get_material_slot_count() const override;
    const char* get_material_slot(const size_t index) const override;

    // Expose asset file paths referenced by this entity to the outside.
    void collect_asset_paths(foundation::StringArray& paths) const override;
    void update_asset_paths(const foundation::StringDictionary& mappings) override;

  private:
    friend class MeshObjectFactory;

    struct Impl;
    Impl* impl;

    // Constructor.
    MeshObject(
        const char*         name,
        const ParamArray&   params);

    // Destructor.
    ~MeshObject() override;
};


//
// Mesh object factory.
//

class APPLESEED_DLLSYMBOL MeshObjectFactory
  : public IObjectFactory
{
  public:
    // Delete this instance.
    void release() override;

    // Return a string identifying this object model.
    const char* get_model() const override;

    // Return metadata for this object model.
    foundation::Dictionary get_model_metadata() const override;

    // Return metadata for the inputs of this object model.
    foundation::DictionaryArray get_input_metadata() const override;

    // Create a new single empty object.
    foundation::auto_release_ptr<Object> create(
        const char*                     name,
        const ParamArray&               params) const override;

    // Create objects, potentially from external assets.
    bool create(
        const char*                     name,
        const ParamArray&               params,
        const foundation::SearchPaths&  search_paths,
        const bool                      omit_loading_assets,
        ObjectArray&                    objects) const override;
};

}   // namespace renderer
