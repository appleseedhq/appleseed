
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

// Interface header.
#include "lightpathrecorder.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/lighting/lightpathstream.h"
#include "renderer/kernel/lighting/pathvertex.h"
#include "renderer/modeling/color/colorspace.h"
#include "renderer/modeling/entity/entity.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exceptionioerror.h"
#include "foundation/memory/memory.h"
#include "foundation/platform/defaulttimers.h"
#include "foundation/platform/thread.h"
#include "foundation/string/string.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/bufferedfile.h"
#include "foundation/utility/stopwatch.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

using namespace foundation;

namespace renderer
{

APPLESEED_DEFINE_APIARRAY(LightPathArray);

struct LightPathRecorder::Impl
{
    const Project&                                m_project;

    boost::mutex                                  m_mutex;
    std::vector<std::unique_ptr<LightPathStream>> m_streams;

    // One entry in the index = one pixel in the frame.
    struct IndexEntry
    {
        size_t  m_begin_path;           // index of the first path
        size_t  m_end_path;             // index of one path past the last one
    };

    size_t                                   m_render_width;
    size_t                                   m_render_height;
    std::vector<IndexEntry>                  m_index;

    explicit Impl(const Project& project)
      : m_project(project)
    {
    }
};

LightPathRecorder::LightPathRecorder(const Project& project)
  : impl(new Impl(project))
{
}

LightPathRecorder::~LightPathRecorder()
{
    delete impl;
}

void LightPathRecorder::clear()
{
    for (auto& stream : impl->m_streams)
        stream->clear();

    clear_release_memory(impl->m_index);
}

size_t LightPathRecorder::get_light_path_count() const
{
    size_t count = 0;

    for (const auto& stream : impl->m_streams)
        count += stream->m_paths.size();

    return count;
}

size_t LightPathRecorder::get_vertex_count() const
{
    assert(impl->m_streams.size() == 1);
    const LightPathStream* stream = impl->m_streams[0].get();

    return stream->m_vertices.size();
}

LightPathStream* LightPathRecorder::create_stream()
{
    boost::mutex::scoped_lock lock(impl->m_mutex);

    auto stream = new LightPathStream(impl->m_project);
    impl->m_streams.push_back(std::unique_ptr<LightPathStream>(stream));

    return stream;
}

void LightPathRecorder::finalize(
    const size_t        render_width,
    const size_t        render_height)
{
    impl->m_render_width = render_width;
    impl->m_render_height = render_height;

    if (impl->m_streams.empty())
        return;

    // Merge all streams into the first one.
    if (impl->m_streams.size() > 1)
    {
        const size_t light_path_count = get_light_path_count();

        RENDERER_LOG_INFO("merging %s light path streams (%s light path%s)...",
            pretty_uint(impl->m_streams.size()).c_str(),
            pretty_uint(light_path_count).c_str(),
            light_path_count > 1 ? "s" : "");

        for (size_t i = 1, e = impl->m_streams.size(); i < e; ++i)
        {
            merge_streams(
                *impl->m_streams[0],
                *impl->m_streams[i]);
        }

        impl->m_streams.resize(1);
    }

    // Retrieve the first and only stream.
    LightPathStream* stream = impl->m_streams[0].get();

    // Remove paths that end outside of the frame.
    RENDERER_LOG_INFO("filtering light path%s...", stream->m_paths.size() > 1 ? "s" : "");
    stream->m_paths.erase(
        remove_if(
            stream->m_paths.begin(),
            stream->m_paths.end(),
            [render_width, render_height](const LightPathStream::StoredPath& p)
            {
                return
                    p.m_pixel_coords.x >= render_width ||
                    p.m_pixel_coords.y >= render_height;
            }),
        stream->m_paths.end());
    const auto light_path_count = stream->m_paths.size();

    // Sort paths by pixel coordinates.
    RENDERER_LOG_INFO("sorting light path%s...", light_path_count > 1 ? "s" : "");
    sort(
        stream->m_paths.begin(),
        stream->m_paths.end(),
        [](const LightPathStream::StoredPath& lhs,
           const LightPathStream::StoredPath& rhs)
        {
            return lhs.m_pixel_coords.y < rhs.m_pixel_coords.y ? true :
                   lhs.m_pixel_coords.y > rhs.m_pixel_coords.y ? false :
                   lhs.m_pixel_coords.x < rhs.m_pixel_coords.x;
        });

    // Build index.
    RENDERER_LOG_INFO("indexing light path%s...", light_path_count > 1 ? "s" : "");
    impl->m_index.resize(render_width * render_height);
    for (auto& index_entry : impl->m_index)
    {
        index_entry.m_begin_path = ~size_t(0);
        index_entry.m_end_path = ~size_t(0);
    }
    for (size_t i = 0, e = stream->m_paths.size(); i < e; ++i)
    {
        // Retrieve index entry.
        const auto& path = stream->m_paths[i];
        const auto x = path.m_pixel_coords.x;
        const auto y = path.m_pixel_coords.y;
        auto& index_entry = impl->m_index[y * render_width + x];

        // Initialize index entry if this is the first path for that pixel.
        if (index_entry.m_begin_path == ~std::uint64_t(0))
        {
            index_entry.m_begin_path = i;
            index_entry.m_end_path = i;
        }

        // One more path for that pixel.
        ++index_entry.m_end_path;
    }
}

void LightPathRecorder::query(
    const size_t        x0,
    const size_t        y0,
    const size_t        x1,
    const size_t        y1,
    LightPathArray&     result) const
{
    assert(impl->m_streams.size() == 1);
    const LightPathStream* stream = impl->m_streams[0].get();

    for (size_t y = y0; y <= y1; ++y)
    {
        for (size_t x = x0; x <= x1; ++x)
        {
            const auto& index_entry = impl->m_index[y * impl->m_render_width + x];

            for (size_t p = index_entry.m_begin_path; p < index_entry.m_end_path; ++p)
            {
                const auto& source_path = stream->m_paths[p];

                LightPath path;
                path.m_pixel_coords[0] = source_path.m_pixel_coords[0];
                path.m_pixel_coords[1] = source_path.m_pixel_coords[1];
                path.m_sample_position[0] = source_path.m_sample_position[0];
                path.m_sample_position[1] = source_path.m_sample_position[1];
                path.m_vertex_begin_index = source_path.m_vertex_begin_index;
                path.m_vertex_end_index = source_path.m_vertex_end_index;

                result.push_back(path);
            }
        }
    }
}

void LightPathRecorder::get_light_path_vertex(
    const size_t        index,
    LightPathVertex&    result) const
{
    assert(impl->m_streams.size() == 1);
    const LightPathStream* stream = impl->m_streams[0].get();

    assert(index < stream->m_vertices.size());
    const auto& source_vertex = stream->m_vertices[index];

    result.m_entity = source_vertex.m_entity;

    result.m_position[0] = source_vertex.m_position[0];
    result.m_position[1] = source_vertex.m_position[1];
    result.m_position[2] = source_vertex.m_position[2];

    result.m_radiance[0] = source_vertex.m_radiance[0];
    result.m_radiance[1] = source_vertex.m_radiance[1];
    result.m_radiance[2] = source_vertex.m_radiance[2];
}

bool LightPathRecorder::write(const char* filename) const
{
    //
    // The format of Light Paths files (*.aspaths) is documented at
    //
    //   https://github.com/appleseedhq/appleseed/wiki/Light-Paths-File-Format
    //

    Stopwatch<DefaultWallclockTimer> stopwatch;
    stopwatch.start();

    // Retrieve the first and only stream.
    assert(impl->m_streams.size() == 1);
    const LightPathStream* stream = impl->m_streams[0].get();
    const size_t light_path_count = stream->m_paths.size();

    try
    {
        RENDERER_LOG_INFO(
            "writing path%s to %s...",
            light_path_count > 1 ? "s" : "",
            filename);

        // Open file.
        BufferedFile file;
        if (!file.open(filename, BufferedFile::BinaryType, BufferedFile::WriteMode))
        {
            RENDERER_LOG_ERROR("failed to open %s for writing.", filename);
            return false;
        }

        // Signature.
        static const char Signature[7] = { 'A', 'S', 'P', 'A', 'T', 'H', 'S' };
        checked_write(file, Signature, sizeof(Signature));

        // Version.
        const std::uint16_t Version = 1;
        checked_write(file, Version);

        // Number of paths.
        assert(light_path_count < 4294967296ULL);
        checked_write(file, static_cast<std::uint32_t>(light_path_count));

        // On-disk variant of Impl::IndexEntry.
        struct StoredIndexEntry
        {
            std::uint64_t  m_start_offset;     // byte offset in the file of the first path
            std::uint16_t  m_path_count;       // number of paths for that pixel
        };

        // Initialize index.
        std::vector<StoredIndexEntry> stored_index(impl->m_render_width * impl->m_render_height);
        for (auto& index_entry : stored_index)
        {
            index_entry.m_start_offset = ~std::uint64_t(0);
            index_entry.m_path_count = 0;
        }

        // Write index dimensions.
        checked_write(file, static_cast<std::uint16_t>(impl->m_render_width));
        checked_write(file, static_cast<std::uint16_t>(impl->m_render_height));

        // Write index placeholder.
        const auto index_location = file.tell();
        for (const auto& index_entry : stored_index)
        {
            checked_write(file, index_entry.m_start_offset);
            checked_write(file, index_entry.m_path_count);
        }

        // Collect entity names and build (entity name -> name index) dictionary.
        std::vector<std::string> entity_names;
        std::map<const Entity*, std::uint16_t> entity_name_to_index;
        for (const auto& vertex : stream->m_vertices)
        {
            if (entity_name_to_index.find(vertex.m_entity) == entity_name_to_index.end())
            {
                // Insert a new (entity name -> name index) entry into the dictionary.
                assert(entity_names.size() < 65536);
                entity_name_to_index.insert(
                    std::make_pair(
                        vertex.m_entity,
                        static_cast<std::uint16_t>(entity_names.size())));

                // Insert the entity name into the vector.
                entity_names.push_back(
                    to_string(vertex.m_entity->get_path()));
            }
        }

        // Write entity names.
        assert(entity_names.size() < 65536);
        checked_write(file, static_cast<std::uint16_t>(entity_names.size()));
        for (const auto& name : entity_names)
        {
            assert(name.size() < 65536);
            checked_write(file, static_cast<std::uint16_t>(name.size()));
            checked_write(file, name.c_str(), name.size());
        }

        // Write paths.
        for (const auto& path : stream->m_paths)
        {
            // Retrieve index entry.
            const auto x = path.m_pixel_coords.x;
            const auto y = path.m_pixel_coords.y;
            auto& index_entry = stored_index[y * impl->m_render_width + x];

            // Initialize index entry if this is the first path for that pixel.
            if (index_entry.m_start_offset == ~std::uint64_t(0))
                index_entry.m_start_offset = static_cast<std::uint64_t>(file.tell());

            // One more path for that pixel.
            assert(index_entry.m_path_count < 65535);
            ++index_entry.m_path_count;

            // Write path info.
            checked_write(file, path.m_sample_position[0]);
            checked_write(file, path.m_sample_position[1]);

            // Write number of vertices for this path.
            const auto vertex_count = path.m_vertex_end_index - path.m_vertex_begin_index;
            assert(vertex_count < 65536);
            checked_write(file, static_cast<std::uint16_t>(vertex_count));

            // Write path vertices.
            for (auto i = path.m_vertex_begin_index; i < path.m_vertex_end_index; ++i)
            {
                const auto& vertex = stream->m_vertices[i];

                // Entity name index.
                const auto it = entity_name_to_index.find(vertex.m_entity);
                assert(it != entity_name_to_index.end());
                checked_write(file, it->second);

                // Write world space position of this vertex.
                checked_write(file, vertex.m_position[0]);
                checked_write(file, vertex.m_position[1]);
                checked_write(file, vertex.m_position[2]);

                // Write radiance at this vertex.
                checked_write(file, vertex.m_radiance[0]);
                checked_write(file, vertex.m_radiance[1]);
                checked_write(file, vertex.m_radiance[2]);
            }
        }

        // Go back and write final index.
        file.seek(index_location, BufferedFile::SeekFromBeginning);
        for (const auto& index_entry : stored_index)
        {
            checked_write(file, index_entry.m_start_offset);
            checked_write(file, index_entry.m_path_count);
        }

        // Close file.
        file.close();

        stopwatch.measure();

        RENDERER_LOG_INFO(
            "wrote %s path%s to %s in %s.",
            pretty_uint(light_path_count).c_str(),
            light_path_count > 1 ? "s" : "",
            filename,
            pretty_time(stopwatch.get_seconds()).c_str());

        return true;
    }
    catch (const ExceptionIOError& e)
    {
        RENDERER_LOG_ERROR("failed to write paths to %s: %s", filename, e.what());
        return false;
    }
}

void LightPathRecorder::merge_streams(
    LightPathStream&    dest,
    LightPathStream&    source)
{
    const size_t old_size = dest.m_paths.size();

    dest.m_paths.insert(
        dest.m_paths.end(),
        source.m_paths.begin(),
        source.m_paths.end());

    clear_release_memory(source.m_paths);

    const auto vertex_index_shift = static_cast<std::uint32_t>(dest.m_vertices.size());

    for (size_t i = old_size, e = dest.m_paths.size(); i < e; ++i)
    {
        dest.m_paths[i].m_vertex_begin_index += vertex_index_shift;
        dest.m_paths[i].m_vertex_end_index += vertex_index_shift;
    }

    dest.m_vertices.insert(
        dest.m_vertices.end(),
        source.m_vertices.begin(),
        source.m_vertices.end());

    clear_release_memory(source.m_vertices);
}

}   // namespace renderer
