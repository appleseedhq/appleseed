
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016 Francois Beaune, The appleseedhq Organization
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
#include "assethandler.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/frame/frame.h"
#include "renderer/modeling/project/configuration.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/project/renderlayerrule.h"
#include "renderer/modeling/scene/scene.h"

// appleseed.foundation headers.
#include "foundation/platform/path.h"
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/otherwise.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/string.h"

// Boost headers.
#include "boost/filesystem/operations.hpp"

// Standard headers.
#include <cstddef>
#include <vector>

using namespace boost;
using namespace foundation;
using namespace std;

namespace renderer
{

//
// AssetHandler class implementation.
//

namespace
{
    template <typename EntityCollection>
    void do_collect_asset_paths(
        StringArray&            paths,
        const EntityCollection& entities)
    {
        for (const_each<EntityCollection> i = entities; i; ++i)
            i->collect_asset_paths(paths);
    }

    template <typename EntityCollection>
    void do_update_asset_paths(
        const StringDictionary& mappings,
        EntityCollection&       entities)
    {
        for (each<EntityCollection> i = entities; i; ++i)
            i->update_asset_paths(mappings);
    }

    string convert_to_posix(string path)
    {
        replace(path.begin(), path.end(), '\\', '/');
        return path;
    }

    bool safe_copy_file(
        const filesystem::path& source_path,
        const filesystem::path& dest_path)
    {
        try
        {
            filesystem::create_directories(dest_path.parent_path());
            filesystem::copy_file(source_path, dest_path, filesystem::copy_option::overwrite_if_exists);
            return true;
        }
        catch (const std::exception& e)
        {
            RENDERER_LOG_ERROR(
                "failed to copy %s to %s: %s.",
                source_path.string().c_str(),
                dest_path.string().c_str(),
                e.what());
            return false;
        }
    }
}

AssetHandler::AssetHandler(
    const Project&      project,
    const char*         filepath,
    const Mode          mode)
  : m_project(project)
  , m_project_old_root_path(filesystem::absolute(project.get_path()))
  , m_project_new_root_path(filesystem::absolute(filepath))
  , m_project_old_root_dir(m_project_old_root_path.parent_path())
  , m_project_new_root_dir(m_project_new_root_path.parent_path())
  , m_mode(mode)
{
}

bool AssetHandler::handle_assets() const
{
    StringArray paths;

    Scene* scene = m_project.get_scene();
    if (scene)
        scene->collect_asset_paths(paths);

    Frame* frame = m_project.get_frame();
    if (frame)
        frame->collect_asset_paths(paths);

    do_collect_asset_paths(paths, m_project.render_layer_rules());
    do_collect_asset_paths(paths, m_project.configurations());

    vector<string> unique_paths = array_vector<vector<string> >(paths);
    sort(unique_paths.begin(), unique_paths.end());
    unique_paths.erase(
        unique(unique_paths.begin(), unique_paths.end()),
        unique_paths.end());

    StringDictionary mappings;
    bool success = true;

    for (size_t i = 0, e = unique_paths.size(); i < e; ++i)
    {
        string asset_path = unique_paths[i];

        if (!handle_asset(asset_path))
            success = false;

        mappings.insert(unique_paths[i], asset_path);
    }

    if (!success)
        return false;

    if (scene)
        scene->update_asset_paths(mappings);

    if (frame)
        frame->update_asset_paths(mappings);

    do_update_asset_paths(mappings, m_project.render_layer_rules());
    do_update_asset_paths(mappings, m_project.configurations());

    return true;
}

bool AssetHandler::handle_asset(string& asset_path_str) const
{
    if (filesystem::path(asset_path_str).is_absolute())
        return handle_absolute_asset(asset_path_str);

    const filesystem::path qualified_asset_path =
        filesystem::canonical(
            m_project.search_paths().qualify(asset_path_str));

    filesystem::path common_path, relative_project_path, relative_asset_path;
    split_paths(
        m_project_old_root_path,        // input path 1
        qualified_asset_path,           // input path 2
        common_path,                    // common part
        relative_project_path,          // remaining part of path 1
        relative_asset_path);           // remaining part of path 2

    if (relative_project_path.has_parent_path())
        return handle_absolute_asset(asset_path_str);

    return copy_relative_asset(asset_path_str, relative_asset_path);
}

bool AssetHandler::handle_absolute_asset(string& asset_path_str) const
{
    switch (m_mode)
    {
      case CopyRelativeAssetsOnly:
        return cleanup_absolute_asset_path(asset_path_str);

      case CopyAllAssets:
        return copy_absolute_asset(asset_path_str);

      assert_otherwise_and_return(false);
    }
}

bool AssetHandler::cleanup_absolute_asset_path(string& asset_path_str) const
{
    // Make sure the asset path is qualified and canonized.
    filesystem::path qualified_asset_path =
        filesystem::canonical(
            m_project.search_paths().qualify(asset_path_str));

    // Make sure absolute paths use native separators.
    qualified_asset_path.make_preferred();
    asset_path_str = qualified_asset_path.string();

    return true;
}

bool AssetHandler::copy_absolute_asset(string& asset_path_str) const
{
    const filesystem::path old_absolute_asset_path(asset_path_str);
    const filesystem::path asset_filename = old_absolute_asset_path.filename();
    const filesystem::path new_relative_asset_path = "assets" / asset_filename;
    const filesystem::path new_absolute_asset_path = m_project_new_root_dir / new_relative_asset_path;

    if (!safe_copy_file(
            m_project.search_paths().qualify(asset_path_str),
            new_absolute_asset_path))
        return false;

    // Make sure relative paths use POSIX separators.
    asset_path_str = new_relative_asset_path.string();
    asset_path_str = convert_to_posix(asset_path_str);

    return true;
}

bool AssetHandler::copy_relative_asset(string& asset_path_str, const filesystem::path& relative_asset_path) const
{
    // Copy the asset file only if the destination differs from the source.
    if (m_project_old_root_dir != m_project_new_root_dir)
    {
        if (!safe_copy_file(
                m_project.search_paths().qualify(asset_path_str),
                m_project_new_root_dir / relative_asset_path))
            return false;
    }

    // Make sure relative paths use POSIX separators.
    asset_path_str = relative_asset_path.string();
    asset_path_str = convert_to_posix(asset_path_str);

    return true;
}

}   // namespace renderer
