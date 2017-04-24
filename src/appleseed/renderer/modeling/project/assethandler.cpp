
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2017 Francois Beaune, The appleseedhq Organization
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
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"
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
using namespace boost::filesystem;
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
        const path& source_path,
        const path& dest_path)
    {
        try
        {
            create_directories(dest_path.parent_path());
            copy_file(source_path, dest_path, copy_option::overwrite_if_exists);
            return true;
        }
        catch (const std::exception& e)     // needs namespace qualification
        {
            RENDERER_LOG_ERROR(
                "failed to copy %s to %s: %s.",
                source_path.string().c_str(),
                dest_path.string().c_str(),
                e.what());
            return false;
        }
    }

    // Return true if p1 is an extension of p2.
    // The resources pointed to by p1 and p2 must exist on the filesystem.
    bool is_extension_of(const path& p1, const path& p2)
    {
        const string r1 = canonical(p1).string();
        const string r2 = canonical(p2).string();
        return starts_with(r1, r2);
    }

    void remove_absolute_search_paths(SearchPaths& search_paths)
    {
        for (size_t i = 0; i < search_paths.size(); ++i)    // must evaluate size() at each iteration
        {
            if (path(search_paths[i]).is_absolute())
                search_paths.remove(i);
        }
    }
}

AssetHandler::AssetHandler(
    const Project&      project,
    const char*         filepath,
    const Mode          mode)
  : m_project(project)
  , m_project_old_root_path(absolute(project.get_path()))
  , m_project_new_root_path(absolute(filepath))
  , m_project_old_root_dir(canonical(m_project_old_root_path.parent_path()))
  , m_project_new_root_dir(canonical(m_project_new_root_path.parent_path()))
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

    // If we copied all assets, we no longer need the absolute search paths, so remove them.
    if (m_mode == CopyAllAssets)
        remove_absolute_search_paths(m_project.search_paths());

    return true;
}

bool AssetHandler::handle_asset(string& asset_path) const
{
    // Let's first get rid of the case where the asset path is absolute.
    if (path(asset_path).is_absolute())
        return handle_absolute_asset(asset_path);

    // Otherwise, let's check if the asset is found in a search path.
    APIString qualified_asset_path;
    APIString search_path;
    m_project.search_paths().qualify(asset_path, &qualified_asset_path, &search_path);

    // Relative asset path, that does not exist. Handling is skipped.
    // Means that this file is in unprovided search path (therefore should be provided by parent project)
    if (!exists(qualified_asset_path.c_str()))
    {
        return true;
    }

    if (search_path.empty() || path(search_path.c_str()).is_relative())
    {
        // Relative asset path, found in a relative search path or in the root directory.
        if (m_project_old_root_dir == m_project_new_root_dir ||
            is_extension_of(qualified_asset_path.c_str(), m_project.search_paths().get_root_path().c_str()))
        {
            // Relative asset/search path, below the project's root path.
            return copy_relative_asset(asset_path);
        }
        else
        {
            // Relative asset/search path, above the project's root path -> consider it absolute.
            return handle_absolute_asset(asset_path);
        }
    }
    else
    {
        // Relative asset path, found in an absolute search path.
        switch (m_mode)
        {
          case CopyRelativeAssetsOnly:
            // Make sure relative paths use POSIX separators.
            asset_path = convert_to_posix(asset_path);
            return true;
    
          case CopyAllAssets:
            return copy_absolute_asset(asset_path);
    
          assert_otherwise_and_return(false);
        }
    }
}

bool AssetHandler::handle_absolute_asset(string& asset_path) const
{
    switch (m_mode)
    {
      case CopyRelativeAssetsOnly:
        return make_absolute_asset_path(asset_path);

      case CopyAllAssets:
        return copy_absolute_asset(asset_path);

      assert_otherwise_and_return(false);
    }
}

bool AssetHandler::make_absolute_asset_path(string& asset_path) const
{
    // Make sure the asset path is qualified and canonized.
    path absolute_asset_path =
        canonical(m_project.search_paths().qualify(asset_path).c_str());

    // Make sure absolute paths use native separators.
    absolute_asset_path.make_preferred();
    asset_path = absolute_asset_path.string();

    return true;
}

bool AssetHandler::copy_absolute_asset(string& asset_path) const
{
    const path old_absolute_asset_path(asset_path);
    const path asset_filename = old_absolute_asset_path.filename();
    const path new_relative_asset_path = "assets" / asset_filename;
    const path new_absolute_asset_path = m_project_new_root_dir / new_relative_asset_path;

    // Copy the asset file.
    if (!safe_copy_file(
            m_project.search_paths().qualify(asset_path).c_str(),
            new_absolute_asset_path))
        return false;

    // The asset path is now relative to the project.
    asset_path = new_relative_asset_path.string();

    // Make sure relative paths use POSIX separators.
    asset_path = convert_to_posix(asset_path);

    return true;
}

bool AssetHandler::copy_relative_asset(string& asset_path) const
{
    // Copy the asset file only if the destination differs from the source.
    if (m_project_old_root_dir != m_project_new_root_dir)
    {
        // Copy the asset file.
        if (!safe_copy_file(
                m_project.search_paths().qualify(asset_path).c_str(),
                m_project_new_root_dir / asset_path))
            return false;
    }

    // Make sure relative paths use POSIX separators.
    asset_path = convert_to_posix(asset_path);

    return true;
}

}   // namespace renderer
