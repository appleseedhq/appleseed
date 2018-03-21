
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

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/entity/entitytraits.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/platform/sharedlibrary.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Boost headers.
#include "boost/filesystem.hpp"
#include "boost/make_shared.hpp"

// Standard headers.
#include <cstddef>
#include <string>
#include <vector>
#include <functional>
#include <memory>

// Forward declarations.
namespace foundation    { class SearchPaths; }
namespace renderer      { class Plugin; }

namespace renderer
{

//define loaded libraries container structure
typedef  std::vector<std::pair<std::unique_ptr<foundation::SharedLibrary>, std::string> >  loaded_libs_container;

//
// Base class for factory registrars.
//

class APPLESEED_DLLSYMBOL EntityFactoryRegistrar
  : public foundation::NonCopyable
{
  public:
    //search for plugins and load them
    static boost::shared_ptr<loaded_libs_container> discover_plugins(
        const foundation::SearchPaths&          search_paths);
  
  protected:
    // Constructor.
    EntityFactoryRegistrar();

    // Destructor.
    virtual ~EntityFactoryRegistrar();

    // Register factories from plugins found in search paths.
    template <typename Entity>
    void register_factories_from_plugins(
        const boost::shared_ptr<loaded_libs_container>  loaded_libraries,
        const std::function<void (void*)>&        register_factory);

    // Unload all plugins loaded by `register_factories_from_plugins()`.
    void unload_all_plugins();

  private:
    struct Impl;
    Impl* impl;

    // Store a plugin in order to keep it alive while the registrar exists.
    void store_plugin(renderer::Plugin* plugin);
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_ENTITY_ENTITYFACTORYREGISTRAR_H
