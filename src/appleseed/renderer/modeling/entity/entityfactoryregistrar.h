
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

#ifndef APPLESEED_RENDERER_MODELING_ENTITY_ENTITYFACTORYREGISTRAR_H
#define APPLESEED_RENDERER_MODELING_ENTITY_ENTITYFACTORYREGISTRAR_H
//boost function
#include <boost/function.hpp>

// appleseed.renderer headers.
#include "renderer/modeling/entity/entitytraits.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/utility/autoreleaseptr.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>
#include <vector>
#include <utility>

// Forward declarations.
namespace foundation    { class SearchPaths; }
namespace renderer      { class Plugin; }

namespace renderer
{

//
// Base class for factory registrars.
//

class APPLESEED_DLLSYMBOL EntityFactoryRegistrar
  : public foundation::NonCopyable
{
public:
    //register_factories_from_plugins is kept public to allow  calling from project.cpp ( as it is a prototype for a different plugins loading architecture/technique)
    // Register factories from plugins found in search paths.
    template <typename T>
    void register_factories_from_plugins(
            const foundation::SearchPaths&      search_paths);
    void clear_plugins_data();
  protected:
    // Constructor.
    EntityFactoryRegistrar();

    // Destructor.
    virtual ~EntityFactoryRegistrar();

    //collect plugins(register_factory) and entry_name for a later search in paths for each specific plugin type
    template <typename Entity>
    void collect_plugins(
            const std::function<void (void*)>&  register_factory);


    // Unload all plugins loaded by `register_factories_from_plugins()`.
    void unload_all_plugins();

  private:
    struct Impl;
    Impl* impl;
    static std::vector<std::pair<std::pair<std::string,std::string>,boost::function<void(void*)>>> plugins_data;


    // Store a plugin in order to keep it alive while the registrar exists.
    void store_plugin(renderer::Plugin* plugin);

};


}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_ENTITY_ENTITYFACTORYREGISTRAR_H
