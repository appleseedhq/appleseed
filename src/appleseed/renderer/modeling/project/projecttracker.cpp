
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

// Interface header.
#include "projecttracker.h"

// appleseed.renderer headers.
#include "renderer/modeling/color/colorentity.h"
#include "renderer/modeling/entity/connectableentity.h"
#include "renderer/modeling/entity/entity.h"
#include "renderer/modeling/entity/entitymap.h"
#include "renderer/modeling/entity/entityvector.h"
#include "renderer/modeling/input/inputbinder.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/project/project.h"
#include "renderer/modeling/scene/assembly.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/scene/textureinstance.h"
#include "renderer/modeling/shadergroup/shadergroup.h"
#include "renderer/modeling/texture/texture.h"

// appleseed.foundation headers.
#include "foundation/log/log.h"
#include "foundation/platform/types.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/uid.h"

// Standard headers.
#include <algorithm>
#include <cassert>
#include <cstring>
#include <map>
#include <set>
#include <vector>

using namespace foundation;

namespace renderer
{

//
// ProjectTracker class implementation.
//
// Terminology:
//   - Incoming references are references *to* a given entity from other ("referencing") entities
//   - Outgoing references are references *from* a given entity to other ("referenced") entities
//
// Example:
//
// Suppose we have four entities: BSDF1, BSDF2, Color1 and Color2.
//
// BSDF1 has two references:
//   input1 --> references Color1
//   input2 --> references Color2
//
// BSDF2 has a single reference:
//   input3 --> references Color1
//
// Incoming references (m_incoming_refs) are then as follow:
//
//   { BSDF1,  [] }
//   { BSDF2,  [] }
//       "No entity is referencing BSDF1 nor BSDF2"
//
//   { Color1, [ (BSDF1, input1), (BSDF2, input3) ] }
//       "Color1 is referenced by BSDF1 via input1 and BSDF2 via input3"
//
//   { Color2, [ (BSDF1, input2) ] }
//       "Color2 is referenced by BSDF1 via input2"
//
// And outgoing references (m_outgoing_refs) are as follow:
//
//   { BSDF1,  [ Color1, Color2 ] }
//       "BSDF1 references Color1 and Color2"
//
//   { BSDF2,  [ Color1 ] }
//       "BSDF2 references Color1"
//
//   { Color1, [] }
//   { Color2, [] }
//       "Color1 and Color2 don't reference any entities"
//

#define FMT_ENTITY "\"%s\" (#" FMT_UNIQUE_ID ")"

struct ProjectTracker::Impl
{
    struct IncomingRef
    {
        Entity*         m_entity;           // the referencing entity
        const char*     m_input_name;       // may be nullptr
    };

    struct OutgoingRef
    {
        Entity*         m_entity;           // the referenced entity
        EntityVector*   m_vector;           // either this is nullptr
        EntityMap*      m_map;              // or this is, but never both
    };

    typedef std::vector<IncomingRef> IncomingRefVec;
    typedef std::vector<OutgoingRef> OutgoingRefVec;

    typedef std::map<Entity*, IncomingRefVec> IncomingRefs;
    typedef std::map<Entity*, OutgoingRefVec> OutgoingRefs;

    Project&            m_project;
    InputBinder         m_input_binder;

    IncomingRefs        m_incoming_refs;
    OutgoingRefs        m_outgoing_refs;

    explicit Impl(Project& project)
      : m_project(project)
      , m_input_binder(*m_project.get_scene())
    {
        Scene& scene = *m_project.get_scene();
        collect_relations_from_scene(scene);
    }

    //
    // Collects relations between entities.
    //

    void collect_relations_from_scene(Scene& scene)
    {
        collect_relations_from(scene.colors());
        collect_relations_from(scene.textures());
        collect_relations_from(scene.texture_instances());
        collect_relations_from(scene.shader_groups());
        collect_relations_from(scene.assembly_instances());

        collect_relations_from(scene.cameras());

        Environment* environment = scene.get_environment();
        if (environment != nullptr)
            collect_relations_from_entity(*environment);

        collect_relations_from(scene.environment_edfs());
        collect_relations_from(scene.environment_shaders());

        for (auto& assembly : scene.assemblies())
            collect_relations_from_assembly(assembly);

        // Top-level assembly instances are implicitly referenced by the scene.
        for (auto& assembly_instance : scene.assembly_instances())
        {
            const InputBinder::ReferencedEntity referenced_entity(scene.assembly_instances(), &assembly_instance);
            insert_relation(referenced_entity, scene);
        }
    }

    void collect_relations_from_assembly(Assembly& assembly)
    {
        collect_relations_from(assembly.colors());
        collect_relations_from(assembly.textures());
        collect_relations_from(assembly.texture_instances());
        collect_relations_from(assembly.shader_groups());
        collect_relations_from(assembly.assembly_instances());

        collect_relations_from(assembly.bsdfs());
        collect_relations_from(assembly.bssrdfs());
        collect_relations_from(assembly.edfs());
        collect_relations_from(assembly.surface_shaders());
        collect_relations_from(assembly.materials());
        collect_relations_from(assembly.lights());
        collect_relations_from(assembly.objects());
        collect_relations_from(assembly.object_instances());
        collect_relations_from(assembly.volumes());

        for (auto& child_assembly : assembly.assemblies())
            collect_relations_from_assembly(child_assembly);

        // Lights are implicitly referenced by their parent assembly.
        for (auto& light : assembly.lights())
        {
            const InputBinder::ReferencedEntity referenced_entity(assembly.lights(), &light);
            insert_relation(referenced_entity, assembly);
        }

        // Object instances are implicitly referenced by their parent assembly.
        for (auto& object_instance : assembly.object_instances())
        {
            const InputBinder::ReferencedEntity referenced_entity(assembly.object_instances(), &object_instance);
            insert_relation(referenced_entity, assembly);
        }

        // Assembly instances are implicitly referenced by their parent assembly.
        for (auto& assembly_instance : assembly.assembly_instances())
        {
            const InputBinder::ReferencedEntity referenced_entity(assembly.assembly_instances(), &assembly_instance);
            insert_relation(referenced_entity, assembly);
        }
    }

    template <typename EntityCollection>
    void collect_relations_from(EntityCollection& entities)
    {
        for (auto& entity : entities)
            collect_relations_from_entity(entity);
    }

    void collect_relations_from_entity(AssemblyInstance& assembly_instance)
    {
        const auto referenced_entity =
            m_input_binder.find_entity(
                assembly_instance.get_assembly_name(),
                assembly_instance.get_parent());

        if (referenced_entity.m_entity != nullptr)
            insert_relation(referenced_entity, assembly_instance);
    }

    void collect_relations_from_entity(ObjectInstance& object_instance)
    {
        std::set<std::string> material_names;

        for (const auto& kv : object_instance.get_front_material_mappings())
            material_names.insert(kv.value());

        for (const auto& kv : object_instance.get_back_material_mappings())
            material_names.insert(kv.value());

        for (const auto& material_name : material_names)
        {
            const auto referenced_entity =
                m_input_binder.find_entity(
                    material_name.c_str(),
                    object_instance.get_parent());

            if (referenced_entity.m_entity != nullptr)
                insert_relation(referenced_entity, object_instance);
        }

        {
            const auto referenced_entity =
                m_input_binder.find_entity(
                    object_instance.get_object_name(),
                    object_instance.get_parent());

            if (referenced_entity.m_entity != nullptr)
                insert_relation(referenced_entity, object_instance);
        }
    }

    void collect_relations_from_entity(TextureInstance& texture_instance)
    {
        const auto referenced_entity =
            m_input_binder.find_entity(
                texture_instance.get_texture_name(),
                texture_instance.get_parent());

        if (referenced_entity.m_entity != nullptr)
            insert_relation(referenced_entity, texture_instance);
    }

    void collect_relations_from_entity(ConnectableEntity& entity)
    {
        for (const auto& input : entity.get_inputs())
        {
            const auto referenced_entity =
                m_input_binder.find_referenced_entity(entity, input);

            if (referenced_entity.m_entity != nullptr)
                insert_relation(referenced_entity, entity, input.name());
        }
    }

    void collect_relations_from_entity(Entity& entity)
    {
        // Nothing to do for other non-connectable entities.
    }

    void insert_relation(
        const InputBinder::ReferencedEntity&    referenced_entity,
        Entity&                                 referencing_entity,
        const char*                             referencing_input_name = nullptr)
    {
        IncomingRef incoming_ref;
        incoming_ref.m_entity = &referencing_entity;
        incoming_ref.m_input_name = referencing_input_name;
        m_incoming_refs[referenced_entity.m_entity].push_back(incoming_ref);

        OutgoingRef outgoing_ref;
        outgoing_ref.m_entity = referenced_entity.m_entity;
        outgoing_ref.m_vector = referenced_entity.m_vector;
        outgoing_ref.m_map = referenced_entity.m_map;
        m_outgoing_refs[&referencing_entity].push_back(outgoing_ref);
    }

    //
    // Printing dependencies between entities.
    //

    void print_dependencies(Logger& logger) const
    {
        print_incoming_references(logger);
        print_outgoing_references(logger);
    }

    void print_incoming_references(Logger& logger) const
    {
        typedef std::pair<Entity*, IncomingRefVec> IncomingRefItem;

        std::vector<IncomingRefItem> sorted_incoming_refs;
        sorted_incoming_refs.reserve(m_incoming_refs.size());

        for (const auto& kv : m_incoming_refs)
            sorted_incoming_refs.emplace_back(kv.first, kv.second);

        std::sort(
            sorted_incoming_refs.begin(),
            sorted_incoming_refs.end(),
            [](const IncomingRefItem& lhs, const IncomingRefItem& rhs)
            {
                return strcmp(lhs.first->get_path().c_str(), rhs.first->get_path().c_str()) < 0;
            });

        LOG_INFO(logger, "--- " FMT_SIZE_T " incoming reference%s ---",
            sorted_incoming_refs.size(),
            sorted_incoming_refs.size() > 1 ? "s" : "");

        for (const auto& kv : sorted_incoming_refs)
        {
            LOG_INFO(logger, FMT_ENTITY " is referenced by:",
                kv.first->get_path().c_str(),
                kv.first->get_uid());

            for (const auto& incoming_ref : kv.second)
            {
                if (incoming_ref.m_input_name == nullptr)
                {
                    LOG_INFO(logger, "    " FMT_ENTITY,
                        incoming_ref.m_entity->get_path().c_str(),
                        incoming_ref.m_entity->get_uid());
                }
                else
                {
                    LOG_INFO(logger, "    " FMT_ENTITY " via its input \"%s\"",
                        incoming_ref.m_entity->get_path().c_str(),
                        incoming_ref.m_entity->get_uid(),
                        incoming_ref.m_input_name);
                }
            }
        }
    }

    void print_outgoing_references(Logger& logger) const
    {
        typedef std::pair<Entity*, OutgoingRefVec> OutgoingRefItem;

        std::vector<OutgoingRefItem> sorted_outgoing_refs;
        sorted_outgoing_refs.reserve(m_outgoing_refs.size());

        for (const auto& kv : m_outgoing_refs)
            sorted_outgoing_refs.emplace_back(kv.first, kv.second);

        std::sort(
            sorted_outgoing_refs.begin(),
            sorted_outgoing_refs.end(),
            [](const OutgoingRefItem& lhs, const OutgoingRefItem& rhs)
            {
                return strcmp(lhs.first->get_path().c_str(), rhs.first->get_path().c_str()) < 0;
            });

        LOG_INFO(logger, "--- " FMT_SIZE_T " outgoing reference%s ---",
            sorted_outgoing_refs.size(),
            sorted_outgoing_refs.size() > 1 ? "s" : "");

        for (const auto& kv : sorted_outgoing_refs)
        {
            LOG_INFO(logger, FMT_ENTITY " has references to:",
                kv.first->get_path().c_str(),
                kv.first->get_uid());

            for (const auto& outgoing_ref : kv.second)
            {
                LOG_INFO(logger, "    " FMT_ENTITY,
                    outgoing_ref.m_entity->get_path().c_str(),
                    outgoing_ref.m_entity->get_uid());
            }
        }
    }

    //
    // Renaming entities.
    //

    void rename(Entity& entity, const char* new_name)
    {
        for (const auto& ref : m_incoming_refs[&entity])
        {
            if (ref.m_input_name != nullptr)
            {
                ParamArray& params = ref.m_entity->get_parameters();
                assert(strcmp(params.get(ref.m_input_name), entity.get_name()) == 0);

                params.set(ref.m_input_name, new_name);
            }
        }

        entity.set_name(new_name);
    }

    //
    // Removing unused entities.
    //

    void remove_unused_entities()
    {
        Scene& scene = *m_project.get_scene();

        remove_unused_entities_from(scene.colors());
        remove_unused_entities_from(scene.textures());
        remove_unused_entities_from(scene.texture_instances());
        remove_unused_entities_from(scene.shader_groups());
        remove_unused_entities_from(scene.assembly_instances());

        remove_unused_entities_from(scene.environment_edfs());
        remove_unused_entities_from(scene.environment_shaders());

        for (auto& assembly : scene.assemblies())
            remove_unused_entities_from_assembly(assembly);
    }

    void remove_unused_entities_from_assembly(Assembly& assembly)
    {
        remove_unused_entities_from(assembly.colors());
        remove_unused_entities_from(assembly.textures());
        remove_unused_entities_from(assembly.texture_instances());
        remove_unused_entities_from(assembly.shader_groups());
        remove_unused_entities_from(assembly.assembly_instances());

        remove_unused_entities_from(assembly.bsdfs());
        remove_unused_entities_from(assembly.bssrdfs());
        remove_unused_entities_from(assembly.edfs());
        remove_unused_entities_from(assembly.surface_shaders());
        remove_unused_entities_from(assembly.materials());
        remove_unused_entities_from(assembly.lights());
        remove_unused_entities_from(assembly.objects());
        remove_unused_entities_from(assembly.object_instances());
        remove_unused_entities_from(assembly.volumes());

        for (auto& child_assembly : assembly.assemblies())
            remove_unused_entities_from_assembly(child_assembly);
    }

    template <typename EntityCollection>
    void remove_unused_entities_from(EntityCollection& entities)
    {
        typedef typename EntityCollection::value_type EntityType;

        std::vector<EntityType*> to_remove;

        for (auto& entity : entities)
        {
            if (!is_referenced(entity))
            {
                RENDERER_LOG_DEBUG("entity " FMT_ENTITY " is not referenced and will be removed.",
                    entity.get_path().c_str(),
                    entity.get_uid());

                to_remove.push_back(&entity);
                remove_unreferenced_entity(entity);
            }
        }

        for (EntityType* entity : to_remove)
        {
            RENDERER_LOG_DEBUG("removing entity " FMT_ENTITY "...",
                entity->get_path().c_str(),
                entity->get_uid());

            entities.remove(entity);
        }
    }

    void remove_unreferenced_entity(Entity& entity)
    {
        assert(!is_referenced(entity));

        for (auto& referenced_entity : m_outgoing_refs[&entity])
        {
            // Remove references from referenced entities toward this entity.
            IncomingRefVec& incoming_refs = m_incoming_refs[referenced_entity.m_entity];
            erase_if(
                incoming_refs,
                [&entity](const IncomingRef& ref) { return ref.m_entity == &entity; });
            if (incoming_refs.empty())
                m_incoming_refs.erase(referenced_entity.m_entity);

            if (!is_referenced(*referenced_entity.m_entity))
            {
                RENDERER_LOG_DEBUG("entity " FMT_ENTITY " is not referenced and will be removed.",
                    referenced_entity.m_entity->get_path().c_str(),
                    referenced_entity.m_entity->get_uid());

                remove_unreferenced_entity(*referenced_entity.m_entity);

                RENDERER_LOG_DEBUG("removing entity " FMT_ENTITY "...",
                    referenced_entity.m_entity->get_path().c_str(),
                    referenced_entity.m_entity->get_uid());

                assert(referenced_entity.m_map || referenced_entity.m_vector);
                if (referenced_entity.m_map != nullptr)
                    referenced_entity.m_map->remove(referenced_entity.m_entity);
                else referenced_entity.m_vector->remove(referenced_entity.m_entity);
            }
        }

        // Remove references from this entity toward other entities.
        m_outgoing_refs.erase(&entity);
    }

    template <typename VectorType, typename Pred>
    void erase_if(VectorType& vec, Pred pred)
    {
        const auto new_end = remove_if(vec.begin(), vec.end(), pred);
        if (new_end != vec.end())
            vec.erase(new_end);
    }

    bool is_referenced(Entity& entity) const
    {
        const auto it = m_incoming_refs.find(&entity);
        return it != m_incoming_refs.end() && !it->second.empty();
    }
};

#undef FMT_ENTITY

ProjectTracker::ProjectTracker(Project& project)
  : impl(new Impl(project))
{
}

ProjectTracker::~ProjectTracker()
{
    delete impl;
}

void ProjectTracker::print_dependencies(Logger& logger) const
{
    impl->print_dependencies(logger);
}

void ProjectTracker::rename(Entity& entity, const char* new_name)
{
    impl->rename(entity, new_name);
}

void ProjectTracker::remove_unused_entities()
{
    impl->remove_unused_entities();
}

}   // namespace renderer
