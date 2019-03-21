
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Francois Beaune, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/utility/api/apiarray.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// Forward declarations.
namespace renderer  { class Entity; }
namespace renderer  { class LightPathStream; }
namespace renderer  { class Project; }

namespace renderer
{

//
// Objects returned by the light paths query API.
//

struct LightPath
{
    size_t          m_pixel_coords[2];
    float           m_sample_position[2];
    size_t          m_vertex_begin_index;       // index of the first vertex in m_vertices
    size_t          m_vertex_end_index;         // index of one vertex past the last one in m_vertices

    bool operator==(const LightPath& rhs) const;
    bool operator!=(const LightPath& rhs) const;
};

APPLESEED_DECLARE_APIARRAY(LightPathArray, LightPath);

struct LightPathVertex
{
    const Entity*   m_entity;
    float           m_position[3];
    float           m_radiance[3];
};


//
// This class allows to
//   - create per-thread streams to collect light paths in memory
//   - query and retrieve light paths
//   - write light paths to disk using an efficient binary format
//

class APPLESEED_DLLSYMBOL LightPathRecorder
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    explicit LightPathRecorder(const Project& project);

    // Destructor.
    ~LightPathRecorder();

    //
    // Recording API.
    //

    // Clear all streams (but don't discard the streams themselves).
    void clear();

    // Create a new stream.
    // Thread-safe. Returns a non-owning pointer.
    LightPathStream* create_stream();

    // Merge all streams into one and build the index.
    void finalize(
        const size_t        render_width,
        const size_t        render_height);

    //
    // Query API.
    //

    // Return the number of stored light paths.
    size_t get_light_path_count() const;

    // Return the total number of stored vertices in all light paths. `finalize()` must have been called.
    size_t get_vertex_count() const;

    // Retrieve all light paths falling into a region of the render.
    // All bounds are inclusive. `finalize()` must have been called.
    void query(
        const size_t        x0,
        const size_t        y0,
        const size_t        x1,
        const size_t        y1,
        LightPathArray&     result) const;

    // Retrieve a given light path vertex. `finalize()` must have been called.
    void get_light_path_vertex(
        const size_t        index,
        LightPathVertex&    result) const;

    // Write light paths to disk. `finalize()` must have been called.
    // Return true if successful, false otherwise.
    bool write(const char* filename) const;

  private:
    struct Impl;
    Impl* impl;

    // Merge `source` into `dest` and clear `source`.
    // Being a static method of `LightPathRecorder` grants it access to the internals of `LightPathStream`.
    static void merge_streams(
        LightPathStream&    dest,
        LightPathStream&    source);
};


//
// LightPath class implementation.
//

inline bool LightPath::operator==(const LightPath& rhs) const
{
    return
        m_pixel_coords[0] == rhs.m_pixel_coords[0] &&
        m_pixel_coords[1] == rhs.m_pixel_coords[1] &&
        m_sample_position[0] == rhs.m_sample_position[0] &&
        m_sample_position[1] == rhs.m_sample_position[1] &&
        m_vertex_begin_index == rhs.m_vertex_begin_index &&
        m_vertex_end_index == rhs.m_vertex_end_index;
}

inline bool LightPath::operator!=(const LightPath& rhs) const
{
    return !(*this == rhs);
}

}   // namespace renderer
