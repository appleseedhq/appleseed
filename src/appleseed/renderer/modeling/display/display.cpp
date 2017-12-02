
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "display.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/rendering/itilecallback.h"
#include "renderer/modeling/project/project.h"
#include "renderer/utility/plugin.h"

// appleseed.foundation headers.
#include "foundation/platform/sharedlibrary.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/searchpaths.h"

// Standard headers.
#include <memory>
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// Display class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

UniqueID Display::get_class_uid()
{
    return g_class_uid;
}

struct Display::Impl
{
    Impl(
        const char*         plugin_path,
        const ParamArray&   params)
      : m_plugin(PluginCache::load(plugin_path))
    {
        typedef ITileCallbackFactory* (*CreateFnType)(const ParamArray*);

        CreateFnType create_fn =
            reinterpret_cast<CreateFnType>(m_plugin->get_symbol("create_tile_callback_factory", false));

        m_tile_callback_factory.reset(create_fn(&params));
    }

    auto_release_ptr<Plugin>                m_plugin;
    auto_release_ptr<ITileCallbackFactory>  m_tile_callback_factory;
};

Display::Display(
    const char*         name,
    const ParamArray&   params)
  : Entity(g_class_uid, params)
  , impl(nullptr)
{
    set_name(name);
}

Display::~Display()
{
    close();
}

void Display::release()
{
    delete this;
}

bool Display::open(const Project& project)
{
    string plugin_path;

    try
    {
        // Qualify the plugin path.
        plugin_path = m_params.get("plugin_name");
        plugin_path += SharedLibrary::get_default_file_extension();
        plugin_path = to_string(project.search_paths().qualify(plugin_path));
    }
    catch (const ExceptionDictionaryKeyNotFound&)
    {
        RENDERER_LOG_ERROR("cannot open display: missing \"plugin_name\" parameter.");
        return false;
    }

    try
    {
        impl = new Impl(plugin_path.c_str(), m_params);
    }
    catch (const ExceptionCannotLoadSharedLib& e)
    {
        RENDERER_LOG_ERROR("cannot open display: %s", e.what());
        return false;
    }
    catch (const ExceptionPluginInitializationFailed&)
    {
        RENDERER_LOG_ERROR("initialization of display plugin %s failed", plugin_path.c_str());
        return false;
    }
    catch (const ExceptionSharedLibCannotGetSymbol& e)
    {
        RENDERER_LOG_ERROR("cannot load symbol %s from display plugin", e.what());
        return false;
    }

    return true;
}

void Display::close()
{
    delete impl;
    impl = nullptr;
}

ITileCallbackFactory* Display::get_tile_callback_factory() const
{
    if (impl)
        return impl->m_tile_callback_factory.get();

    return nullptr;
}


//
// DisplayFactory class implementation.
//

auto_release_ptr<Display> DisplayFactory::create(
    const char*         name,
    const ParamArray&   params)
{
    return auto_release_ptr<Display>(new Display(name, params));
}

}   // namespace renderer
