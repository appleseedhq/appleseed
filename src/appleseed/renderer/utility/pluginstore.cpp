
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
#include "pluginstore.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/utility/plugin.h"

// appleseed.foundation headers.
#include "foundation/platform/path.h"
#include "foundation/platform/sharedlibrary.h"
#include "foundation/utility/searchpaths.h"

// Boost headers.
#include "boost/filesystem.hpp"
#include "boost/range/iterator_range.hpp"
#include "boost/thread/locks.hpp"
#include "boost/thread/mutex.hpp"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <map>
#include <memory>
#include <string>

using namespace foundation;

namespace bf = boost::filesystem;

namespace renderer
{

struct PluginStore::Impl
{
    typedef PluginStore::PluginHandlerType PluginHandlerType;
    typedef std::map<std::string, PluginHandlerType> PluginHandlerMap;

    struct PluginDeleter;
    typedef std::unique_ptr<Plugin, PluginDeleter> PluginUniquePtr;

    typedef std::map<std::string, PluginUniquePtr> PluginMap;
    typedef std::map<Plugin*, PluginMap::const_iterator> PluginInverseMap;

    struct PluginDeleter
    {
        void operator()(Plugin* plugin)
        {
            RENDERER_LOG_INFO("unloading plugin %s...", plugin->get_filepath());

            // Try to call the plugin's uninitialization function if defined.
            const auto uninit_fn = reinterpret_cast<Plugin::UnInitPluginFnType>(plugin->get_symbol("uninitialize_plugin"));
            if (uninit_fn != nullptr)
                uninit_fn();

            // Delete the plugin.
            delete plugin;
        }
    };

    boost::mutex        m_store_mutex;
    PluginHandlerMap    m_plugin_handlers;
    PluginMap           m_plugin_map;
    PluginInverseMap    m_plugin_inverse_map;

    Plugin* load_plugin_no_lock(const char* filepath)
    {
        auto plugin_map_it = m_plugin_map.find(filepath);

        if (plugin_map_it == m_plugin_map.end())
        {
            RENDERER_LOG_INFO("loading plugin %s...", filepath);

            // Load the plugin.
            PluginUniquePtr plugin(new Plugin(filepath));

            // Try to call the plugin's initialization function if defined.
            const auto init_fn = reinterpret_cast<Plugin::InitPluginFnType>(plugin->get_symbol("initialize_plugin"));
            if (init_fn != nullptr)
            {
                if (!init_fn())
                {
                    RENDERER_LOG_WARNING("plugin %s failed to initialize itself.", filepath);
                    return nullptr;
                }
            }

            // Insert the plugin into the map.
            const auto insert_result =
                m_plugin_map.insert(PluginMap::value_type(filepath, std::move(plugin)));
            assert(insert_result.second);
            plugin_map_it = insert_result.first;

            // Insert the corresponding entry into the inverse map.
#ifndef NDEBUG
            const auto inverse_insert_result =
#endif
                m_plugin_inverse_map.insert(PluginInverseMap::value_type(plugin_map_it->second.get(), plugin_map_it));
            assert(inverse_insert_result.second);

            RENDERER_LOG_DEBUG("plugin %s successfully loaded and initialized.", filepath);
        }
        else
        {
            RENDERER_LOG_DEBUG("plugin %s already loaded.", filepath);
        }

        return plugin_map_it->second.get();
    }

    Plugin* load_plugin_and_invoke_handlers_no_lock(const char* filepath)
    {
        // Load the plugin.
        Plugin* plugin = load_plugin_no_lock(filepath);

        if (plugin != nullptr)
        {
            // Invoke plugin handlers.
            for (const auto& plugin_handler_item : m_plugin_handlers)
            {
                const std::string& entry_point_name = plugin_handler_item.first;
                const PluginHandlerType& plugin_handler = plugin_handler_item.second;

                // If the plugin exposes the expected entry point then pass it to the plugin handler.
                void* plugin_entry_point = plugin->get_symbol(entry_point_name.c_str());
                if (plugin_entry_point != nullptr)
                    plugin_handler(plugin, plugin_entry_point);
            }
        }

        return plugin;
    }

    void load_all_plugins_from_path_no_lock(bf::path path)
    {
        path = safe_weakly_canonical(path);

        // Only consider directories.
        if (!bf::exists(path) || !bf::is_directory(path))
        {
            RENDERER_LOG_DEBUG("not scanning %s for plugins since it doesn't exist or it isn't a directory.",
                path.string().c_str());
            return;
        }

        RENDERER_LOG_INFO("scanning %s for plugins...", path.string().c_str());

        // Iterate over all files in this directory.
        for (const bf::path& entry_path : boost::make_iterator_range(bf::directory_iterator(path)))
        {
            // Only consider files.
            if (!bf::is_regular_file(entry_path))
                continue;

            // Only consider shared libraries.
            if (lower_case(entry_path.extension().string()) != SharedLibrary::get_default_file_extension())
                continue;

            // Load the plugin and invoke plugin handlers.
            load_plugin_and_invoke_handlers_no_lock(entry_path.string().c_str());
        }
    }
};

PluginStore::PluginStore()
  : impl(new Impl())
{
}

PluginStore::~PluginStore()
{
    unload_all_plugins();
    delete impl;
}

void PluginStore::register_plugin_handler(
    const char*                 entry_point_name,
    const PluginHandlerType&    plugin_handler)
{
    boost::lock_guard<boost::mutex> lock(impl->m_store_mutex);
    impl->m_plugin_handlers.insert(
        Impl::PluginHandlerMap::value_type(entry_point_name, plugin_handler));
}

void PluginStore::unload_all_plugins()
{
    boost::lock_guard<boost::mutex> lock(impl->m_store_mutex);

    RENDERER_LOG_INFO("unloading all plugins...");

    impl->m_plugin_inverse_map.clear();
    impl->m_plugin_map.clear();
}

Plugin* PluginStore::load_plugin(const char* filepath)
{
    boost::lock_guard<boost::mutex> lock(impl->m_store_mutex);
    return impl->load_plugin_and_invoke_handlers_no_lock(filepath);
}

void PluginStore::unload_plugin(Plugin* plugin)
{
    boost::lock_guard<boost::mutex> lock(impl->m_store_mutex);

    const auto plugin_map_inverse_it = impl->m_plugin_inverse_map.find(plugin);
    assert(plugin_map_inverse_it != impl->m_plugin_inverse_map.end());

    impl->m_plugin_map.erase(plugin_map_inverse_it->second);
    impl->m_plugin_inverse_map.erase(plugin_map_inverse_it);
}

void PluginStore::load_all_plugins_from_paths(const SearchPaths& search_paths)
{
    boost::lock_guard<boost::mutex> lock(impl->m_store_mutex);

    for (size_t i = 0, e = search_paths.get_path_count(); i < e; ++i)
    {
        bf::path search_path(search_paths.get_path(i));

        // Make the search path absolute if it isn't already.
        if (!search_path.is_absolute() && search_paths.has_root_path())
            search_path = search_paths.get_root_path().c_str() / search_path;

        // Load all plugins from this search path.
        impl->load_all_plugins_from_path_no_lock(search_path);
    }
}

void PluginStore::load_all_plugins_from_path(const char* path)
{
    boost::lock_guard<boost::mutex> lock(impl->m_store_mutex);
    impl->load_all_plugins_from_path_no_lock(path);
}

}   // namespace renderer
