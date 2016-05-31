
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2016 Esteban Tovagliari, The appleseedhq Organization
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
#include "plugin.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"

// appleseed.foundation headers.
#include "foundation/platform/sharedlibrary.h"

// Boost headers.
#include <boost/shared_ptr.hpp>
#include <boost/thread/locks.hpp>
#include <boost/thread/mutex.hpp>
#include <boost/weak_ptr.hpp>

// Standard headers.
#include <algorithm>
#include <map>
#include <memory>
#include <string>

using namespace foundation;
using namespace boost;
using namespace std;

namespace renderer
{

//
// ExceptionPluginInitializationFailed class implementation.
//

ExceptionPluginInitializationFailed::ExceptionPluginInitializationFailed()
{
    set_what("Plugin initialization failed");
}


//
// Plugin class implementation.
//

struct Plugin::Impl
{
    explicit Impl(boost::shared_ptr<SharedLibrary> p)
      : m_library(p)
    {
        assert(m_library);
    }

    boost::shared_ptr<SharedLibrary> m_library;
};

Plugin::Plugin(Impl* impl)
{
    assert(impl);
    this->impl = impl;
}

Plugin::~Plugin()
{
    delete impl;
}

void Plugin::release()
{
    delete this;
}

void* Plugin::get_symbol(const char* name, const bool no_throw) const
{
    return impl->m_library->get_symbol(name, no_throw);
}

const char* Plugin::get_default_file_extension()
{
#ifdef _WIN32
    return ".dll";
#else
    return ".so";
#endif
}


//
// PluginCache class implementation.
//

namespace
{
    typedef map<string, boost::weak_ptr<SharedLibrary> > PluginCacheType;

    PluginCacheType g_plugin_cache;
    boost::mutex g_plugin_cache_mutex;

    struct PluginDeleter
    {
        void operator()(SharedLibrary* p)
        {
            boost::lock_guard<boost::mutex> lock(g_plugin_cache_mutex);

            // Try to call the plugin uninitialize function if defined.
            Plugin::UnInitPluginFnType uninit_fn =
                reinterpret_cast<Plugin::UnInitPluginFnType>(
                    p->get_symbol("uninitialize_plugin"));
            if (uninit_fn)
                uninit_fn();

            delete p;
        }
    };
}

auto_release_ptr<Plugin> PluginCache::load(const char* path)
{
    boost::lock_guard<boost::mutex> lock(g_plugin_cache_mutex);

    // Check if we loaded this plugin before.
    PluginCacheType::iterator it = g_plugin_cache.find(path);
    if (it != g_plugin_cache.end())
    {
        if (boost::shared_ptr<SharedLibrary> lib = it->second.lock())
        {
            Plugin::Impl* impl = new Plugin::Impl(lib);
            return auto_release_ptr<Plugin>(new Plugin(impl));
        }
    }

    // If this plugin is not in the cache, load the shared lib.
    boost::shared_ptr<SharedLibrary> lib(new SharedLibrary(path), PluginDeleter());

    // Try to call the initialization function if defined.
    Plugin::InitPluginFnType init_fn =
        reinterpret_cast<Plugin::InitPluginFnType>(
            lib->get_symbol("initialize_plugin"));
    if (init_fn)
    {
        if (!init_fn())
            throw ExceptionPluginInitializationFailed();
    }

    Plugin::Impl* impl = new Plugin::Impl(lib);
    g_plugin_cache[path] = boost::weak_ptr<SharedLibrary>(lib);
    return auto_release_ptr<Plugin>(new Plugin(impl));
}

}   // namespace renderer
