
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/modeling/project/project.h"

// appleseed.foundation headers.
#include "foundation/platform/sharedlibrary.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/searchpaths.h"

// Standard headers.
#include <string>

using namespace std;
using namespace foundation;

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
  : public NonCopyable
{
    Impl(
        const char*         plugin_path,
        const ParamArray&   params)
      : m_plugin(new SharedLibrary(plugin_path))
    {
        typedef ITileCallbackFactory*(*CreateFnType)(const ParamArray&);

        CreateFnType create_fn =
            reinterpret_cast<CreateFnType>(m_plugin->get_symbol("create_tile_callback_factory"), false);

        m_tile_callback_factory.reset(create_fn(params));
    }

    auto_release_ptr<SharedLibrary>         m_plugin;
    auto_release_ptr<ITileCallbackFactory>  m_tile_callback_factory;
};

Display::Display(
    const char*         name,
    const ParamArray&   params)
  : Entity(g_class_uid, params)
  , impl(0)
{
    set_name(name);
}

Display::~Display()
{
    delete impl;
}

void Display::release()
{
    delete this;
}

void Display::open(const Project& project) const
{
    string plugin;

    try
    {
        plugin = get_parameters().get("plugin_name");
        // add platform dependent extension here...
#ifdef _WIN32
        plugin += ".dll";
#else
        plugin += ".so";
#endif

        plugin = project.search_paths().qualify(plugin);
    }
    catch(const ExceptionDictionaryItemNotFound&)
    {
        RENDERER_LOG_ERROR("%s", "Cannot open display. Bad params.");
        return;
    }

    try
    {
        impl = new Impl(plugin.c_str(), get_parameters());
    }
    catch(const ExceptionCannotLoadSharedLib& e)
    {
        RENDERER_LOG_ERROR("Cannot open display.%s", e.what());
    }
    catch(const ExceptionSharedLibCannotGetSymbol& e)
    {
        RENDERER_LOG_ERROR("Cannot open display.%s", e.what());
    }
}

void Display::close() const
{
    delete impl;
}

ITileCallbackFactory* Display::get_tile_callback_factory() const
{
    if (impl)
        return impl->m_tile_callback_factory.get();

    return 0;
}

//
// DisplayFactory class implementation.
//

auto_release_ptr<Display> DisplayFactory::create(
        const char*             name,
        const ParamArray&       params)
{
    return auto_release_ptr<Display>(new Display(name, params));
}

}   // namespace renderer
