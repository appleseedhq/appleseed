
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

// Interface header.
#include "entityfactoryregistrar.h"

// appleseed.renderer headers.
#include "renderer/utility/plugin.h"

// appleseed.foundation headers.
#include "foundation/utility/searchpaths.h"

// Standard headers.
#include <vector>

using namespace renderer;
using namespace std;

namespace renderer
{

struct EntityFactoryRegistrar::Impl
{
    vector<Plugin*> m_plugins;

    ~Impl()
    {
        clear();
    }

    void clear()
    {
        for (Plugin* plugin : m_plugins)
            plugin->release();

        m_plugins.clear();
    }
};

EntityFactoryRegistrar::EntityFactoryRegistrar()
  : impl(new Impl())
{
}

EntityFactoryRegistrar::~EntityFactoryRegistrar()
{
    delete impl;
}

void EntityFactoryRegistrar::discover_plugins(
    const foundation::SearchPaths&          search_paths){
    namespace bf = boost::filesystem;

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



        // Iterate over all files in this directory.
        for (bf::directory_iterator j(search_path), f; j != f; ++j)
        {

            //iterate over entities;

            // Only consider shared library files.
            if (!bf::is_regular_file(*j) ||
                j->path().extension() != foundation::SharedLibrary::get_default_file_extension())
                continue;

            const std::string plugin_path = j->path().string();
            
            //load plugin into memory 
            //foundation::SharedLibrary s(plugin_path.c_str());
            try
            {
                unique_ptr<foundation::SharedLibrary> library(new foundation::SharedLibrary(plugin_path.c_str()));
                loaded_libraries.push_back(std::make_pair(std::move(library),plugin_path));
            }
            catch (const foundation::ExceptionCannotLoadSharedLib& e)
            {
                RENDERER_LOG_DEBUG("could not open shared library %s: %s.",
                    plugin_path.c_str(), e.what());
                continue;
            }
            //iterate over all  plugins  ( because now we are in the path )
          
        }
    }
}

void EntityFactoryRegistrar::unload_all_plugins()
{
    impl->clear();
}

void EntityFactoryRegistrar::store_plugin(Plugin* plugin)
{
    impl->m_plugins.push_back(plugin);
}

std::vector<std::pair<std::unique_ptr<foundation::SharedLibrary>, std::string>> EntityFactoryRegistrar::loaded_libraries;
}   // namespace renderer
