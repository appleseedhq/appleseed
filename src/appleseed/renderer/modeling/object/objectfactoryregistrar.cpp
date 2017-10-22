
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
#include "objectfactoryregistrar.h"

// appleseed.renderer headers.
#include "renderer/modeling/object/curveobject.h"
#include "renderer/modeling/object/iobjectfactory.h"
#include "renderer/modeling/object/meshobject.h"
#include "renderer/utility/plugin.h"

// appleseed.foundation headers.
#include "foundation/platform/sharedlibrary.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/registrar.h"
#include "foundation/utility/searchpaths.h"

// Boost headers.
#include "boost/filesystem.hpp"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <string>
#include <utility>
#include <vector>

namespace bf = boost::filesystem;
using namespace foundation;
using namespace std;

namespace renderer
{

APPLESEED_DEFINE_APIARRAY(ObjectFactoryArray);

struct ObjectFactoryRegistrar::Impl
{
    Registrar<IObjectFactory>   m_registrar;
    vector<Plugin*>             m_plugins;

    ~Impl()
    {
        for (Plugin* plugin : m_plugins)
            plugin->release();
    }
};

ObjectFactoryRegistrar::ObjectFactoryRegistrar(const SearchPaths& search_paths)
  : impl(new Impl())
{
    register_factory(auto_release_ptr<FactoryType>(new CurveObjectFactory()));
    register_factory(auto_release_ptr<FactoryType>(new MeshObjectFactory()));

    load_plugins(search_paths);
}

ObjectFactoryRegistrar::~ObjectFactoryRegistrar()
{
    delete impl;
}

void ObjectFactoryRegistrar::register_factory(auto_release_ptr<FactoryType> factory)
{
    const string model = factory->get_model();
    impl->m_registrar.insert(model, move(factory));
}

ObjectFactoryArray ObjectFactoryRegistrar::get_factories() const
{
    FactoryArrayType factories;

    for (const_each<Registrar<FactoryType>::Items> i = impl->m_registrar.items(); i; ++i)
        factories.push_back(i->second);

    return factories;
}

const ObjectFactoryRegistrar::FactoryType* ObjectFactoryRegistrar::lookup(const char* name) const
{
    assert(name);

    return impl->m_registrar.lookup(name);
}

void ObjectFactoryRegistrar::load_plugins(const SearchPaths& search_paths)
{
    // Iterate over all known search paths.
    for (size_t i = 0, e = search_paths.size(); i < e; ++i)
    {
        bf::path search_path(search_paths[i]);

        // Make the search path absolute if it isn't already.
        if (!search_path.is_absolute() && search_paths.has_root_path())
            search_path = search_paths.get_root_path().c_str() / search_path;

        // Only consider directories.
        if (!bf::exists(search_path) || !bf::is_directory(search_path))
            continue;

        // Iterate over all files in this directory.
        for (bf::directory_iterator i(search_path), e; i != e; ++i)
        {
            const bf::path lib_path(*i);

            // Only consider shared library files.
            if (!bf::is_regular_file(lib_path) ||
                lib_path.extension() != SharedLibrary::get_default_file_extension())
                continue;

            // Only consider valid plugins.
            if (SharedLibrary(lib_path.string().c_str()).get_symbol("appleseed_create_object_factory") == nullptr)
                continue;

            // Load the plugin into the cache.
            auto_release_ptr<Plugin> plugin(PluginCache::load(lib_path.string().c_str()));

            // Retrieve the plugin's entry point.
            typedef IObjectFactory* (*CreateFnType)();
            CreateFnType create_fn =
                reinterpret_cast<CreateFnType>(plugin->get_symbol("appleseed_create_object_factory"));
            if (create_fn == nullptr)
                continue;

            // Register the factory provided by the plugin.
            register_factory(auto_release_ptr<IObjectFactory>(create_fn()));

            // todo: temporarily keep plugins alive.
            impl->m_plugins.push_back(plugin.release());
        }
    }
}

}   // namespace renderer
