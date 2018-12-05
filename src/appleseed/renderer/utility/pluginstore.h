
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

#pragma once

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"

// Standard headers.
#include <functional>

// Forward declarations.
namespace foundation    { class SearchPaths; }
namespace renderer      { class Plugin; }

namespace renderer
{

//
// All methods of this class are thread-safe.
//

class PluginStore
  : public foundation::NonCopyable
{
  public:
    typedef std::function<void (Plugin*, void*)> PluginHandlerType;

    // Constructor.
    PluginStore();

    // Destructor.
    ~PluginStore();

    // Register a plugin handler for a given entry point name.
    // The plugin handler will be invoked whenever a plugin that defines this entry point is loaded.
    void register_plugin_handler(
        const char*                 entry_point_name,
        const PluginHandlerType&    plugin_handler);

    // Unload all plugins.
    void unload_all_plugins();

    // Load a plugin if it isn't already loaded.
    Plugin* load_plugin(const char* filepath);

    // Unload a plugin. The plugin must have been previously loaded.
    void unload_plugin(Plugin* plugin);

    // Load all plugins present inside a given directory.
    void load_all_plugins_from_path(const char* path);

    // Load all plugins present inside a collection of search paths.
    void load_all_plugins_from_paths(const foundation::SearchPaths& search_paths);

  private:
    struct Impl;
    Impl* impl;
};

}   // namespace renderer
