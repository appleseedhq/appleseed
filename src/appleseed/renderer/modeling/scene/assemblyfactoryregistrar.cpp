
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "assemblyfactoryregistrar.h"

// appleseed.renderer headers.
#include "renderer/modeling/entity/entityfactoryregistrar.h"
#include "renderer/modeling/scene/archiveassembly.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblytraits.h"
#include "renderer/modeling/scene/iassemblyfactory.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"

using namespace foundation;

namespace renderer
{

APPLESEED_DEFINE_APIARRAY(AssemblyFactoryArray);

struct AssemblyFactoryRegistrar::Impl
  : public EntityFactoryRegistrarImpl<
        AssemblyFactoryRegistrar::EntityType,
        AssemblyFactoryRegistrar::FactoryType,
        AssemblyFactoryRegistrar::FactoryArrayType
    >
{
};

AssemblyFactoryRegistrar::AssemblyFactoryRegistrar(const SearchPaths& search_paths)
  : impl(new Impl())
{
    // Register built-in factories.
    impl->register_factory(auto_release_ptr<FactoryType>(new ArchiveAssemblyFactory()));
    impl->register_factory(auto_release_ptr<FactoryType>(new AssemblyFactory()));
}

AssemblyFactoryRegistrar::~AssemblyFactoryRegistrar()
{
    delete impl;
}

void AssemblyFactoryRegistrar::register_factory_plugin(Plugin* plugin, void* plugin_entry_point)
{
    impl->register_factory_plugin(plugin, plugin_entry_point);
}

AssemblyFactoryArray AssemblyFactoryRegistrar::get_factories() const
{
    return impl->get_factories();
}

const AssemblyFactoryRegistrar::FactoryType* AssemblyFactoryRegistrar::lookup(const char* name) const
{
    return impl->lookup(name);
}

}   // namespace renderer
