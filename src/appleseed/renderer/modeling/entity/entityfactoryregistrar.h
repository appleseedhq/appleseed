
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2017-2018 Francois Beaune, The appleseedhq Organization
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
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/registrar.h"

// Standard headers.
#include <cassert>
#include <string>
#include <utility>

// Forward declarations.
namespace renderer  { class Plugin; }

namespace renderer
{

//
// Base class for factory registrar implementations.
//

template <typename EntityType, typename FactoryType, typename FactoryArrayType>
class EntityFactoryRegistrarImpl
  : public foundation::NonCopyable
{
  public:
    // Register a factory.
    void register_factory(foundation::auto_release_ptr<FactoryType> factory);

    // Register a factory defined in a plugin.
    void register_factory_plugin(Plugin* plugin, void* plugin_entry_point);

    // Retrieve registered factories.
    FactoryArrayType get_factories() const;

    // Lookup a factory by name.
    const FactoryType* lookup(const char* name) const;

  private:
    foundation::Registrar<FactoryType> m_registrar;
};


//
// EntityFactoryRegistrar class implementation.
//

template <typename EntityType, typename FactoryType, typename FactoryArrayType>
void EntityFactoryRegistrarImpl<EntityType, FactoryType, FactoryArrayType>::register_factory(foundation::auto_release_ptr<FactoryType> factory)
{
    const std::string model = factory->get_model();
    m_registrar.insert(model, std::move(factory));
}

template <typename EntityType, typename FactoryType, typename FactoryArrayType>
void EntityFactoryRegistrarImpl<EntityType, FactoryType, FactoryArrayType>::register_factory_plugin(Plugin* plugin, void* plugin_entry_point)
{
    const auto create_factory = reinterpret_cast<FactoryType* (*)()>(plugin_entry_point);
    register_factory(foundation::auto_release_ptr<FactoryType>(create_factory()));
}

template <typename EntityType, typename FactoryType, typename FactoryArrayType>
FactoryArrayType EntityFactoryRegistrarImpl<EntityType, FactoryType, FactoryArrayType>::get_factories() const
{
    FactoryArrayType factories;

    for (const auto& item : m_registrar.items())
        factories.push_back(item.second);

    return factories;
}

template <typename EntityType, typename FactoryType, typename FactoryArrayType>
const FactoryType* EntityFactoryRegistrarImpl<EntityType, FactoryType, FactoryArrayType>::lookup(const char* name) const
{
    assert(name);
    return m_registrar.lookup(name);
}

}   // namespace renderer
