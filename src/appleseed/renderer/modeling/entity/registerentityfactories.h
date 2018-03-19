
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
    const std::function<void (void*)>&      register_factory)
{
    namespace bf = boost::filesystem;

    const std::string entry_point_name =
        foundation::format("appleseed_create_{0}_factory", EntityTraits<Entity>::get_entity_type_name());

    const std::string entity_type_name =
            foundation::lower_case(EntityTraits<Entity>::get_human_readable_entity_type_name());

    for (int i = 0; i < loaded_libraries.size(); i++)
    {

        // Only consider libraries that can be loaded and define the right magic symbol.
        try
        {
            loaded_libraries[i].first->get_symbol(entry_point_name.c_str(), false);
        }
        catch (const foundation::ExceptionSharedLibCannotGetSymbol&)
        {
            RENDERER_LOG_DEBUG("shared library %s is not an appleseed %s plugin because it does not export a %s() function.",
                loaded_libraries[i].second.c_str(), entity_type_name.c_str(), entry_point_name.c_str());
            continue;
        }

        // Load the plugin into the cache and retrieve its entry point
        foundation::auto_release_ptr<Plugin> plugin(PluginCache::load(loaded_libraries[i].second.c_str()));
        void* plugin_entry_point = plugin->get_symbol(entry_point_name.c_str());
        if (plugin_entry_point == nullptr)
            continue;

        // Store the plugin to keep it alive.
        store_plugin(plugin.release());

        // Let the caller handle the discovered plugin.
        RENDERER_LOG_INFO("registering %s plugin %s...", entity_type_name.c_str(), loaded_libraries[i].second.c_str());
        register_factory(plugin_entry_point);        
    }
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_ENTITY_REGISTERENTITYFACTORIES_H
