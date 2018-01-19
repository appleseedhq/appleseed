
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_ENTITY_REGISTERENTITYFACTORIES_H
#define APPLESEED_RENDERER_MODELING_ENTITY_REGISTERENTITYFACTORIES_H

// Interface header.
#include "entityfactoryregistrar.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/utility/plugin.h"

// appleseed.foundation headers.
#include "foundation/platform/sharedlibrary.h"
#include "foundation/utility/searchpaths.h"
#include "foundation/utility/string.h"

// Boost headers.
#include "boost/filesystem.hpp"

// Standard headers.
#include <string>

namespace renderer
{

//
// EntityFactoryRegistrar::register_factories_from_plugins class implementation.
//

template <typename Entity>
void EntityFactoryRegistrar::register_factories_from_plugins(
    const foundation::SearchPaths&          search_paths,
    const std::function<void (void*)>&      register_factory)
{
    namespace bf = boost::filesystem;

    const std::string entry_point_name =
        foundation::format("appleseed_create_{0}_factory", EntityTraits<Entity>::get_entity_type_name());

    // Iterate over all search paths.
    for (size_t i = 0, e = search_paths.get_path_count(); i < e; ++i)
    {
        bf::path search_path(search_paths.get_path(i));

        // Make the search path absolute if it isn't already.
        if (!search_path.is_absolute() && search_paths.has_root_path())
            search_path = search_paths.get_root_path().c_str() / search_path;

        // Only consider directories.
        if (!bf::exists(search_path) || !bf::is_directory(search_path))
            continue;

        const std::string entity_type_name =
            foundation::lower_case(EntityTraits<Entity>::get_human_readable_entity_type_name());

        RENDERER_LOG_DEBUG("scanning %s in search of %s plugins...",
            search_path.string().c_str(),
            entity_type_name.c_str());

        // Iterate over all files in this directory.
        for (bf::directory_iterator j(search_path), f; j != f; ++j)
        {
            // Only consider shared library files.
            if (!bf::is_regular_file(*j) ||
                j->path().extension() != foundation::SharedLibrary::get_default_file_extension())
                continue;

            const std::string plugin_path = j->path().string();

            // Only consider libraries that can be loaded and define the right magic symbol.
            try
            {
                foundation::SharedLibrary library(plugin_path.c_str());
                library.get_symbol(entry_point_name.c_str(), false);
            }
            catch (const foundation::ExceptionCannotLoadSharedLib& e)
            {
                RENDERER_LOG_DEBUG("could not open shared library %s: %s.",
                    plugin_path.c_str(), e.what());
                continue;
            }
            catch (const foundation::ExceptionSharedLibCannotGetSymbol&)
            {
                RENDERER_LOG_DEBUG("shared library %s is not an appleseed %s plugin because it does not export a %s() function.",
                    plugin_path.c_str(), entity_type_name.c_str(), entry_point_name.c_str());
                continue;
            }

            // Load the plugin into the cache and retrieve its entry point.
            foundation::auto_release_ptr<Plugin> plugin(PluginCache::load(plugin_path.c_str()));
            void* plugin_entry_point = plugin->get_symbol(entry_point_name.c_str());
            if (plugin_entry_point == nullptr)
                continue;

            // Store the plugin to keep it alive.
            store_plugin(plugin.release());

            // Let the caller handle the discovered plugin.
            RENDERER_LOG_INFO("registering %s plugin %s...", entity_type_name.c_str(), plugin_path.c_str());
            register_factory(plugin_entry_point);
        }
    }
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_ENTITY_REGISTERENTITYFACTORIES_H
