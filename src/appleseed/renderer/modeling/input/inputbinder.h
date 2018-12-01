
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2018 Francois Beaune, The appleseedhq Organization
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

// appleseed.renderer headers.
#include "renderer/modeling/entity/entity.h"
#include "renderer/modeling/entity/entitytraits.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/input/symbol.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/containers.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <cassert>
#include <cstddef>
#include <map>
#include <string>
#include <vector>

// Forward declarations.
namespace renderer  { class ConnectableEntity; }
namespace renderer  { class Scene; }

namespace renderer
{

//
// Input binder.
//

class InputBinder
  : public foundation::NonCopyable
{
  public:
    // Constructor.
    explicit InputBinder(const Scene& scene);

    // Bind all inputs of all entities in a scene.
    void bind();

    // Return the number of reported binding errors.
    size_t get_error_count() const;

    struct ReferencedEntity
    {
        Entity*         m_entity;
        EntityVector*   m_vector;       // either this is nullptr
        EntityMap*      m_map;          // or this is, but never both

        ReferencedEntity();
        ReferencedEntity(EntityVector& vector, Entity* entity);
        ReferencedEntity(EntityMap& map, Entity* entity);
        ReferencedEntity(EntityVector& vector, const char* name);
        ReferencedEntity(EntityMap& map, const char* name);
    };

    // Find an entity with a given name using the input binding logic.
    template <typename EntityType>
    static EntityType* static_find_entity(
        const char*                     name,
        const Entity*                   parent);
    ReferencedEntity find_entity(
        const char*                     name,
        const Entity*                   parent) const;

    // Find the entity referenced by another entity's input.
    ReferencedEntity find_referenced_entity(
        const ConnectableEntity&        entity,
        const InputArray::iterator&     input) const;

  private:
    struct AssemblyInfo
    {
        const Assembly*     m_assembly;
        const SymbolTable*  m_assembly_symbols;
    };

    typedef std::vector<AssemblyInfo> AssemblyInfoVector;
    typedef AssemblyInfoVector::const_reverse_iterator AssemblyInfoIt;

    const Scene&            m_scene;
    size_t                  m_error_count;
    SymbolTable             m_scene_symbols;
    std::map<const Assembly*, SymbolTable> m_assembly_symbols;
    AssemblyInfoVector      m_assembly_info;

    // Build the symbol table for a given scene.
    void build_scene_symbol_table();

    // Build the symbol table for a given assembly.
    void build_assembly_symbol_table(
        const Assembly&                 assembly,
        SymbolTable&                    symbols);

    void collect_assembly_symbols(
        const Assembly&                 assembly);

    // Bind all inputs of all entities of a given scene.
    void bind_scene_entities_inputs();

    // Bind all inputs of all entities of a given assembly.
    void bind_assembly_entities_inputs(
        const Assembly&                 assembly);

    // Bind all inputs of a given entity.
    void bind_entity_inputs(
        const char*                     entity_type,
        ConnectableEntity&              entity);

    // Try to bind a scene entity to a given input.
    bool try_bind_scene_entity_to_input(
        const char*                     entity_type,
        const char*                     entity_name,
        const char*                     param_value,
        InputArray::iterator&           input);

    // Try to bind an entity from any parent assembly to a given input.
    bool try_bind_assembly_entity_to_input(
        const char*                     entity_type,
        const char*                     entity_name,
        const char*                     param_value,
        InputArray::iterator&           input);

    // Try to bind an entity from a given assembly to a given input.
    bool try_bind_assembly_entity_to_input(
        const Assembly&                 assembly,
        const SymbolTable&              assembly_symbols,
        const char*                     entity_type,
        const char*                     entity_name,
        const char*                     param_value,
        InputArray::iterator&           input);

    // Try to bind a scalar to a given input.
    bool try_bind_scalar_to_input(
        const std::string&              param_value,
        InputArray::iterator&           input) const;

    // Bind a color to a given input.
    void bind_color_to_input(
        const ColorContainer&           colors,
        const char*                     param_value,
        InputArray::iterator&           input);

    // Bind a texture instance to a given input.
    void bind_texture_instance_to_input(
        const TextureInstanceContainer& texture_instances,
        const foundation::UniqueID      assembly_uid,           // unique ID of parent assembly, or ~0 for scene
        const char*                     entity_type,
        const char*                     entity_name,
        const char*                     param_value,
        InputArray::iterator&           input);

    ReferencedEntity find_entity_in_assembly(
        const Assembly&                 assembly,
        const char*                     name) const;

    ReferencedEntity find_entity_in_scene(
        const char*                     name) const;
};


//
// InputBinder class implementation.
//

template <typename EntityType>
EntityType* InputBinder::static_find_entity(
    const char*                         name,
    const Entity*                       parent)
{
    assert(name);

    while (parent)
    {
        const Assembly* assembly = dynamic_cast<const Assembly*>(parent);

        if (assembly == nullptr)
            break;

        auto& container = EntityTraits<EntityType>::get_entity_container(*assembly);
        EntityType* entity = container.get_by_name(name);

        if (entity)
            return entity;

        parent = parent->get_parent();
    }

    // todo: bug, we should also look in the scene.

    return nullptr;
}

}   // namespace renderer
