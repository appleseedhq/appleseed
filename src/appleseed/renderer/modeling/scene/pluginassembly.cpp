
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "pluginassembly.h"

// appleseed.renderer headers.
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/utility/paramarray.h"
#include "renderer/utility/plugin.h"

// appleseed.foundation headers.
#include "foundation/platform/sharedlibrary.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/job/abortswitch.h"
#include "foundation/utility/searchpaths.h"

// Standard headers.
#include <memory>
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// PluginAssembly class implementation.
//

namespace
{
    const char* Model = "plugin_assembly";
}

PluginAssembly::PluginAssembly(
    const char*         name,
    const ParamArray&   params)
  : ProceduralAssembly(name, params)
  , m_plugin_assembly(nullptr)
{
}

PluginAssembly::~PluginAssembly()
{
    if (m_plugin_assembly)
        m_plugin_assembly->release();
}

void PluginAssembly::release()
{
    delete this;
}

const char* PluginAssembly::get_model() const
{
    return Model;
}

bool PluginAssembly::expand_contents(
    const Project&      project,
    const Assembly*     parent,
    IAbortSwitch*       abort_switch)
{
    // Skip already expanded procedurals.
    if (m_plugin_assembly)
        return true;

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
        RENDERER_LOG_ERROR("cannot open assembly plugin: missing \"plugin_name\" parameter.");
        return false;
    }

    auto_release_ptr<Assembly> plugin_assembly;

    try
    {
        // Load the plugin.
        auto_release_ptr<Plugin> plugin(PluginCache::load(plugin_path.c_str()));

        // Create the plugin assembly factory.
        typedef IAssemblyFactory*(*CreateFnType)();
        CreateFnType create_fn =
            reinterpret_cast<CreateFnType>(plugin->get_symbol("create_assembly_factory", false));

        auto_release_ptr<IAssemblyFactory> assembly_factory(create_fn());

        // Create the plugin assembly.
        plugin_assembly =
            assembly_factory->create(
                get_name(),
                get_parameters().child("parameters"));
    }
    catch (const ExceptionCannotLoadSharedLib& e)
    {
        RENDERER_LOG_ERROR(
            "cannot open %s assembly plugin, error: %s",
            plugin_path.c_str(),
            e.what());
        return false;
    }
    catch (const ExceptionPluginInitializationFailed&)
    {
        RENDERER_LOG_ERROR("initialization of %s assembly plugin failed", plugin_path.c_str());
        return false;
    }
    catch (const ExceptionSharedLibCannotGetSymbol& e)
    {
        RENDERER_LOG_ERROR("cannot load symbol from %s assembly plugin, error: %s",
            plugin_path.c_str(),
            e.what());
        return false;
    }

    // Expand contents of the plugin assembly.
    const bool success =
        static_cast<PluginAssembly*>(plugin_assembly.get())->expand_contents(
            project,
            parent,
            abort_switch);

    if (success)
    {
        // Move the contents of the plugin assembly into this assembly.
        swap_contents(*plugin_assembly);

        // Keep the plugin assembly alive, in case it contains state
        // that could be needed by children procedural assemblies.
        m_plugin_assembly = plugin_assembly.release();
    }

    return success;
}


//
// PluginAssemblyFactory class implementation.
//

void PluginAssemblyFactory::release()
{
    delete this;
}

const char* PluginAssemblyFactory::get_model() const
{
    return Model;
}

auto_release_ptr<Assembly> PluginAssemblyFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return auto_release_ptr<Assembly>(new PluginAssembly(name, params));
}

}   // namespace renderer
