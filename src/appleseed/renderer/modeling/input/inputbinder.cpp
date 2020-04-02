
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

// Interface header.
#include "inputbinder.h"

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/modeling/bsdf/bsdf.h"
#include "renderer/modeling/bssrdf/bssrdf.h"
#include "renderer/modeling/camera/camera.h"
#include "renderer/modeling/color/colorentity.h"
#include "renderer/modeling/edf/edf.h"
#include "renderer/modeling/entity/connectableentity.h"
#include "renderer/modeling/entity/entitymap.h"
#include "renderer/modeling/entity/entityvector.h"
#include "renderer/modeling/environment/environment.h"
#include "renderer/modeling/environmentedf/environmentedf.h"
#include "renderer/modeling/environmentshader/environmentshader.h"
#include "renderer/modeling/input/colorsource.h"
#include "renderer/modeling/input/scalarsource.h"
#include "renderer/modeling/input/texturesource.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/scene/textureinstance.h"
#include "renderer/modeling/shadergroup/shadergroup.h"
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/modeling/texture/texture.h"
#include "renderer/modeling/volume/volume.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/string/string.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/otherwise.h"

// Standard headers.
#include <exception>
#include <utility>

using namespace foundation;

namespace renderer
{

//
// InputBinder::ReferencedEntity class implementation.
//

InputBinder::ReferencedEntity::ReferencedEntity()
  : m_entity(nullptr)
  , m_vector(nullptr)
  , m_map(nullptr)
{
}

InputBinder::ReferencedEntity::ReferencedEntity(EntityVector& vector, Entity* entity)
  : m_entity(entity)
  , m_vector(&vector)
  , m_map(nullptr)
{
}

InputBinder::ReferencedEntity::ReferencedEntity(EntityMap& map, Entity* entity)
  : m_entity(entity)
  , m_vector(nullptr)
  , m_map(&map)
{
}

InputBinder::ReferencedEntity::ReferencedEntity(EntityVector& vector, const char* name)
  : m_entity(vector.get_by_name(name))
  , m_vector(&vector)
  , m_map(nullptr)
{
}

InputBinder::ReferencedEntity::ReferencedEntity(EntityMap& map, const char* name)
  : m_entity(map.get_by_name(name))
  , m_vector(nullptr)
  , m_map(&map)
{
}


//
// InputBinder class implementation.
//

InputBinder::InputBinder(const Scene& scene)
  : m_scene(scene)
  , m_error_count(0)
{
    // Build the symbol table of the scene.
    build_scene_symbol_table();

    // Collect symbol tables for all assemblies.
    for (const auto& assembly : m_scene.assemblies())
        collect_assembly_symbols(assembly);
}

void InputBinder::bind()
{
    try
    {
        // Bind all inputs of all entities in the scene.
        bind_scene_entities_inputs();

        // Bind all inputs of all entities in all assemblies.
        for (const auto& assembly : m_scene.assemblies())
        {
            assert(m_assembly_info.empty());
            bind_assembly_entities_inputs(assembly);
        }
    }
    catch (const ExceptionUnknownEntity& e)
    {
        RENDERER_LOG_ERROR(
            "while binding inputs of \"%s\": could not locate entity \"%s\".",
            e.get_context_path().c_str(),
            e.string());
        ++m_error_count;
    }
}

size_t InputBinder::get_error_count() const
{
    return m_error_count;
}

InputBinder::ReferencedEntity InputBinder::find_entity(
    const char*                     name,
    const Entity*                   parent) const
{
    assert(name);
    assert(parent);

    while (parent)
    {
        const Assembly* assembly = dynamic_cast<const Assembly*>(parent);

        if (assembly == nullptr)
            break;

        const auto referenced_entity = find_entity_in_assembly(*assembly, name);

        if (referenced_entity.m_entity != nullptr)
            return referenced_entity;

        parent = parent->get_parent();
    }

    return find_entity_in_scene(name);
}

InputBinder::ReferencedEntity InputBinder::find_referenced_entity(
    const ConnectableEntity&        entity,
    const InputArray::iterator&     input) const
{
    const ParamArray& entity_params = entity.get_parameters();

    std::string param_value;

    if (entity_params.strings().exist(input.name()))
    {
        // A value is assigned to this input, retrieve it.
        param_value = entity_params.get<std::string>(input.name());
    }
    else if (input.type() == InputTypeOptional)
    {
        // This input is optional, use its default value.
        param_value = input.default_value();
    }

    if (param_value.empty())
        return ReferencedEntity();

    return find_entity(param_value.c_str(), entity.get_parent());
}

namespace
{
    template <typename EntityContainer>
    void insert_entities(
        SymbolTable&                symbols,
        const EntityContainer&      entities,
        const SymbolTable::SymbolID symbol_id)
    {
        for (const auto& entity : entities)
            symbols.insert(entity.get_name(), symbol_id);
    }
}

void InputBinder::build_scene_symbol_table()
{
    try
    {
        insert_entities(m_scene_symbols, m_scene.cameras(), SymbolTable::SymbolCamera);
        insert_entities(m_scene_symbols, m_scene.colors(), SymbolTable::SymbolColor);
        insert_entities(m_scene_symbols, m_scene.textures(), SymbolTable::SymbolTexture);
        insert_entities(m_scene_symbols, m_scene.texture_instances(), SymbolTable::SymbolTextureInstance);
        insert_entities(m_scene_symbols, m_scene.environment_edfs(), SymbolTable::SymbolEnvironmentEDF);
        insert_entities(m_scene_symbols, m_scene.environment_shaders(), SymbolTable::SymbolEnvironmentShader);
        insert_entities(m_scene_symbols, m_scene.shader_groups(), SymbolTable::SymbolShaderGroup);

        if (m_scene.get_environment())
            m_scene_symbols.insert(m_scene.get_environment()->get_name(), SymbolTable::SymbolEnvironment);

        insert_entities(m_scene_symbols, m_scene.assemblies(), SymbolTable::SymbolAssembly);
        insert_entities(m_scene_symbols, m_scene.assembly_instances(), SymbolTable::SymbolAssemblyInstance);
    }
    catch (const SymbolTable::ExceptionDuplicateSymbol& e)
    {
        RENDERER_LOG_ERROR("duplicate entity \"%s\".", e.string());
        ++m_error_count;
    }
}

void InputBinder::build_assembly_symbol_table(
    const Assembly&                 assembly,
    SymbolTable&                    symbols)
{
    try
    {
        insert_entities(symbols, assembly.colors(), SymbolTable::SymbolColor);
        insert_entities(symbols, assembly.textures(), SymbolTable::SymbolTexture);
        insert_entities(symbols, assembly.texture_instances(), SymbolTable::SymbolTextureInstance);
        insert_entities(symbols, assembly.bsdfs(), SymbolTable::SymbolBSDF);
        insert_entities(symbols, assembly.bssrdfs(), SymbolTable::SymbolBSSRDF);
        insert_entities(symbols, assembly.edfs(), SymbolTable::SymbolEDF);
        insert_entities(symbols, assembly.shader_groups(), SymbolTable::SymbolShaderGroup);
        insert_entities(symbols, assembly.surface_shaders(), SymbolTable::SymbolSurfaceShader);
        insert_entities(symbols, assembly.materials(), SymbolTable::SymbolMaterial);
        insert_entities(symbols, assembly.lights(), SymbolTable::SymbolLight);
        insert_entities(symbols, assembly.objects(), SymbolTable::SymbolObject);
        insert_entities(symbols, assembly.object_instances(), SymbolTable::SymbolObjectInstance);
        insert_entities(symbols, assembly.volumes(), SymbolTable::SymbolVolume);
    }
    catch (const SymbolTable::ExceptionDuplicateSymbol& e)
    {
        RENDERER_LOG_ERROR("duplicate entity \"%s\".", e.string());
        ++m_error_count;
    }
}

void InputBinder::collect_assembly_symbols(
    const Assembly&                 assembly)
{
    // Build the symbol table of the assembly.
    SymbolTable symbols;
    build_assembly_symbol_table(assembly, symbols);

    // Store the symbol table of the assembly.
    m_assembly_symbols.insert(std::make_pair(&assembly, symbols));

    // Recurse into child assemblies.
    for (const auto& child_assembly : assembly.assemblies())
        collect_assembly_symbols(child_assembly);
}

void InputBinder::bind_scene_entities_inputs()
{
    // Bind textures to texture instances.
    // Other entities might need to access the textures bound to texture instances,
    // so binding of textures to texture instances must come first.
    for (auto& texture_instance : m_scene.texture_instances())
    {
        texture_instance.unbind_texture();
        texture_instance.bind_texture(m_scene.textures());
        texture_instance.check_texture();
    }

    // Bind the inputs of the default surface shader.
    bind_entity_inputs(
        SymbolTable::symbol_name(SymbolTable::SymbolSurfaceShader),
        *m_scene.get_default_surface_shader());

    // Bind camera inputs.
    for (auto& camera : m_scene.cameras())
    {
        bind_entity_inputs(
            SymbolTable::symbol_name(SymbolTable::SymbolCamera),
            camera);
    }

    // Bind environment EDFs inputs.
    for (auto& environment_edf : m_scene.environment_edfs())
    {
        bind_entity_inputs(
            SymbolTable::symbol_name(SymbolTable::SymbolEnvironmentEDF),
            environment_edf);
    }

    // Bind environment shaders inputs.
    for (auto& environment_shader : m_scene.environment_shaders())
    {
        bind_entity_inputs(
            SymbolTable::symbol_name(SymbolTable::SymbolEnvironmentShader),
            environment_shader);
    }

    // Bind environment inputs.
    if (m_scene.get_environment())
    {
        bind_entity_inputs(
            SymbolTable::symbol_name(SymbolTable::SymbolEnvironment),
            *m_scene.get_environment());
    }

    // Bind assemblies to assembly instances.
    for (auto& assembly_instance : m_scene.assembly_instances())
    {
        assembly_instance.unbind_assembly();
        assembly_instance.bind_assembly(m_scene.assemblies());
        assembly_instance.check_assembly();
    }
}

void InputBinder::bind_assembly_entities_inputs(
    const Assembly&                 assembly)
{
    // Push the assembly and its symbol table to the stack.
    AssemblyInfo info;
    info.m_assembly = &assembly;
    info.m_assembly_symbols = &m_assembly_symbols.find(&assembly)->second;
    m_assembly_info.push_back(info);

    // Bind textures to texture instances.
    // Other entities might need to access the textures bound to texture instances,
    // so binding of textures to texture instances must come first.
    for (auto& texture_instance : assembly.texture_instances())
    {
        texture_instance.unbind_texture();

        for (auto j = m_assembly_info.rbegin(); j != m_assembly_info.rend(); ++j)
            texture_instance.bind_texture(j->m_assembly->textures());

        texture_instance.bind_texture(m_scene.textures());

        texture_instance.check_texture();
    }

    // Bind BSDFs inputs.
    for (auto& bsdf : assembly.bsdfs())
    {
        bind_entity_inputs(
            SymbolTable::symbol_name(SymbolTable::SymbolBSDF),
            bsdf);
    }

    // Bind BSSRDFs inputs.
    for (auto& bssrdf : assembly.bssrdfs())
    {
        bind_entity_inputs(
            SymbolTable::symbol_name(SymbolTable::SymbolBSSRDF),
            bssrdf);
    }

    // Bind EDFs inputs.
    for (auto& edf : assembly.edfs())
    {
        bind_entity_inputs(
            SymbolTable::symbol_name(SymbolTable::SymbolEDF),
            edf);
    }

    // Bind volumes inputs.
    for (auto& volume : assembly.volumes())
    {
        bind_entity_inputs(
            SymbolTable::symbol_name(SymbolTable::SymbolVolume),
            volume);
    }

    // Bind ShaderGroups inputs.
    for (auto& shader_group : assembly.shader_groups())
    {
        bind_entity_inputs(
            SymbolTable::symbol_name(SymbolTable::SymbolShaderGroup),
            shader_group);
    }

    // Bind surface shaders inputs.
    for (auto& surface_shader : assembly.surface_shaders())
    {
        bind_entity_inputs(
            SymbolTable::symbol_name(SymbolTable::SymbolSurfaceShader),
            surface_shader);
    }

    // Bind materials inputs.
    for (auto& material : assembly.materials())
    {
        bind_entity_inputs(
            SymbolTable::symbol_name(SymbolTable::SymbolMaterial),
            material);
    }

    // Bind lights inputs.
    for (auto& light : assembly.lights())
    {
        bind_entity_inputs(
            SymbolTable::symbol_name(SymbolTable::SymbolLight),
            light);
    }

    // Bind objects inputs.
    for (auto& object : assembly.objects())
    {
        bind_entity_inputs(
            SymbolTable::symbol_name(SymbolTable::SymbolObject),
            object);
    }

    // Bind objects to object instances. This must be done before binding materials.
    for (auto& object_instance : assembly.object_instances())
    {
        object_instance.unbind_object();

        for (auto j = m_assembly_info.rbegin(); j != m_assembly_info.rend(); ++j)
            object_instance.bind_object(j->m_assembly->objects());

        object_instance.check_object();
    }

    // Bind materials to object instances.
    for (auto& object_instance : assembly.object_instances())
    {
        object_instance.unbind_materials();

        for (auto j = m_assembly_info.rbegin(); j != m_assembly_info.rend(); ++j)
            object_instance.bind_materials(j->m_assembly->materials());

        object_instance.check_materials();
    }

    // Bind assemblies to assembly instances.
    for (auto& assembly_instance : assembly.assembly_instances())
    {
        assembly_instance.unbind_assembly();

        for (auto j = m_assembly_info.rbegin(); j != m_assembly_info.rend(); ++j)
            assembly_instance.bind_assembly(j->m_assembly->assemblies());

        assembly_instance.bind_assembly(m_scene.assemblies());

        assembly_instance.check_assembly();
    }

    // Recurse into child assemblies.
    for (const auto& child_assembly : assembly.assemblies())
        bind_assembly_entities_inputs(child_assembly);

    // Pop the information about this assembly from the stack.
    m_assembly_info.pop_back();
}

void InputBinder::bind_entity_inputs(
    const char*                     entity_type,
    ConnectableEntity&              entity)
{
    const std::string entity_path(entity.get_path().c_str());
    const ParamArray& entity_params = entity.get_parameters();

    for (auto& input : entity.get_inputs())
    {
        std::string param_value;

        if (entity_params.strings().exist(input.name()))
        {
            // A value is assigned to this input, retrieve it.
            param_value = entity_params.get<std::string>(input.name());
        }
        else if (input.type() == InputTypeOptional)
        {
            // This input is optional, use its default value.
            param_value = input.default_value();
            if (param_value.empty())
                continue;
        }
        else
        {
            // This input is required but has no value, this is an error.
            RENDERER_LOG_ERROR(
                "while binding inputs of %s \"%s\": required parameter \"%s\" missing.",
                entity_type,
                entity_path.c_str(),
                input.name());
            ++m_error_count;
            continue;
        }

        if (try_bind_assembly_entity_to_input(
                entity_type,
                entity_path.c_str(),
                param_value.c_str(),
                input))
            continue;

        if (try_bind_scene_entity_to_input(
                entity_type,
                entity_path.c_str(),
                param_value.c_str(),
                input))
            continue;

        if (try_bind_scalar_to_input(param_value, input))
            continue;

        RENDERER_LOG_ERROR(
            "while binding inputs of %s \"%s\": cannot bind \"%s\" to parameter \"%s\".",
            entity_type,
            entity_path.c_str(),
            param_value.c_str(),
            input.name());

        ++m_error_count;
    }
}

bool InputBinder::try_bind_scene_entity_to_input(
    const char*                     entity_type,
    const char*                     entity_name,
    const char*                     param_value,
    InputArray::iterator&           input)
{
    if (input.format() == InputFormatEntity)
    {
        #define BIND(symbol, collection)                        \
            case symbol:                                        \
              input.bind(collection.get_by_name(param_value));  \
              return true

        switch (m_scene_symbols.lookup(param_value))
        {
          BIND(SymbolTable::SymbolColor, m_scene.colors());
          BIND(SymbolTable::SymbolTexture, m_scene.textures());
          BIND(SymbolTable::SymbolTextureInstance, m_scene.texture_instances());
          BIND(SymbolTable::SymbolShaderGroup, m_scene.shader_groups());
          BIND(SymbolTable::SymbolEnvironmentEDF, m_scene.environment_edfs());
          BIND(SymbolTable::SymbolEnvironmentShader, m_scene.environment_shaders());
          case SymbolTable::SymbolNotFound: break;
          assert_otherwise;
        }

        #undef BIND
    }
    else
    {
        switch (m_scene_symbols.lookup(param_value))
        {
          case SymbolTable::SymbolColor:
            bind_color_to_input(
                m_scene.colors(),
                param_value,
                input);
            return true;

          case SymbolTable::SymbolTextureInstance:
            bind_texture_instance_to_input(
                m_scene.texture_instances(),
                ~UniqueID(0),       // the parent is the scene, not an assembly
                entity_type,
                entity_name,
                param_value,
                input);
            return true;

          case SymbolTable::SymbolNotFound:
            break;

          default: break;           // might be a scalar
        }
    }

    return false;
}

bool InputBinder::try_bind_assembly_entity_to_input(
    const char*                     entity_type,
    const char*                     entity_name,
    const char*                     param_value,
    InputArray::iterator&           input)
{
    for (auto i = m_assembly_info.rbegin(); i != m_assembly_info.rend(); ++i)
    {
        if (try_bind_assembly_entity_to_input(
                *i->m_assembly,
                *i->m_assembly_symbols,
                entity_type,
                entity_name,
                param_value,
                input))
            return true;
    }

    return false;
}

bool InputBinder::try_bind_assembly_entity_to_input(
    const Assembly&                 assembly,
    const SymbolTable&              assembly_symbols,
    const char*                     entity_type,
    const char*                     entity_name,
    const char*                     param_value,
    InputArray::iterator&           input)
{
    if (input.format() == InputFormatEntity)
    {
        #define BIND(symbol, collection)                        \
            case symbol:                                        \
              input.bind(collection.get_by_name(param_value));  \
              return true

        switch (assembly_symbols.lookup(param_value))
        {
          BIND(SymbolTable::SymbolColor, assembly.colors());
          BIND(SymbolTable::SymbolTexture, assembly.textures());
          BIND(SymbolTable::SymbolTextureInstance, assembly.texture_instances());
          BIND(SymbolTable::SymbolShaderGroup, assembly.shader_groups());
          BIND(SymbolTable::SymbolBSDF, assembly.bsdfs());
          BIND(SymbolTable::SymbolBSSRDF, assembly.bssrdfs());
          BIND(SymbolTable::SymbolEDF, assembly.edfs());
          BIND(SymbolTable::SymbolSurfaceShader, assembly.surface_shaders());
          BIND(SymbolTable::SymbolMaterial, assembly.materials());
          BIND(SymbolTable::SymbolLight, assembly.lights());
          BIND(SymbolTable::SymbolObject, assembly.objects());
          BIND(SymbolTable::SymbolObjectInstance, assembly.object_instances());
          BIND(SymbolTable::SymbolVolume, assembly.volumes());
          case SymbolTable::SymbolNotFound: break;
          default: break;           // might be a scene entity (e.g. an environment EDF)
        }

        #undef BIND
    }
    else
    {
        switch (assembly_symbols.lookup(param_value))
        {
          case SymbolTable::SymbolColor:
            bind_color_to_input(
                assembly.colors(),
                param_value,
                input);
            return true;

          case SymbolTable::SymbolTextureInstance:
            bind_texture_instance_to_input(
                assembly.texture_instances(),
                assembly.get_uid(),
                entity_type,
                entity_name,
                param_value,
                input);
            return true;

          case SymbolTable::SymbolNotFound:
            break;

          default: break;           // might be a scalar
        }
    }

    return false;
}

bool InputBinder::try_bind_scalar_to_input(
    const std::string&              param_value,
    InputArray::iterator&           input) const
{
    try
    {
        const float value = from_string<float>(param_value);
        input.bind(new ScalarSource(value));
        return true;
    }
    catch (const ExceptionStringConversionError&)
    {
        return false;
    }
}

void InputBinder::bind_color_to_input(
    const ColorContainer&           colors,
    const char*                     param_value,
    InputArray::iterator&           input)
{
    const ColorEntity* color_entity = colors.get_by_name(param_value);
    assert(color_entity);

    input.bind(new ColorSource(*color_entity));
}

void InputBinder::bind_texture_instance_to_input(
    const TextureInstanceContainer& texture_instances,
    const UniqueID                  assembly_uid,
    const char*                     entity_type,
    const char*                     entity_name,
    const char*                     param_value,
    InputArray::iterator&           input)
{
    const TextureInstance* texture_instance = texture_instances.get_by_name(param_value);
    assert(texture_instance);

    try
    {
        input.bind(
            texture_instance->get_texture().create_source(
                assembly_uid,
                *texture_instance));
    }
    catch (const std::exception& e)
    {
        RENDERER_LOG_ERROR(
            "while binding inputs of %s \"%s\", failed to bind \"%s\" to input \"%s\" (%s).",
            entity_type,
            entity_name,
            param_value,
            input.name(),
            e.what());

        ++m_error_count;
    }
}

InputBinder::ReferencedEntity InputBinder::find_entity_in_assembly(
    const Assembly&                 assembly,
    const char*                     name) const
{
    const SymbolTable& assembly_symbols = m_assembly_symbols.find(&assembly)->second;

    switch (assembly_symbols.lookup(name))
    {
      case SymbolTable::SymbolColor: return ReferencedEntity(assembly.colors(), name);
      case SymbolTable::SymbolTexture: return ReferencedEntity(assembly.textures(), name);
      case SymbolTable::SymbolTextureInstance: return ReferencedEntity(assembly.texture_instances(), name);
      case SymbolTable::SymbolShaderGroup: return ReferencedEntity(assembly.shader_groups(), name);
      case SymbolTable::SymbolBSDF: return ReferencedEntity(assembly.bsdfs(), name);
      case SymbolTable::SymbolBSSRDF: return ReferencedEntity(assembly.bssrdfs(), name);
      case SymbolTable::SymbolEDF: return ReferencedEntity(assembly.edfs(), name);
      case SymbolTable::SymbolSurfaceShader: return ReferencedEntity(assembly.surface_shaders(), name);
      case SymbolTable::SymbolMaterial: return ReferencedEntity(assembly.materials(), name);
      case SymbolTable::SymbolLight: return ReferencedEntity(assembly.lights(), name);
      case SymbolTable::SymbolObject: return ReferencedEntity(assembly.objects(), name);
      case SymbolTable::SymbolObjectInstance: return ReferencedEntity(assembly.object_instances(), name);
      case SymbolTable::SymbolVolume: return ReferencedEntity(assembly.volumes(), name);
      case SymbolTable::SymbolNotFound: break;
      default: break;               // might be a scalar
    }

    return ReferencedEntity();
}

InputBinder::ReferencedEntity InputBinder::find_entity_in_scene(
    const char*                     name) const
{
    switch (m_scene_symbols.lookup(name))
    {
      case SymbolTable::SymbolColor: return ReferencedEntity(m_scene.colors(), name);
      case SymbolTable::SymbolTexture: return ReferencedEntity(m_scene.textures(), name);
      case SymbolTable::SymbolTextureInstance: return ReferencedEntity(m_scene.texture_instances(), name);
      case SymbolTable::SymbolShaderGroup: return ReferencedEntity(m_scene.shader_groups(), name);
      case SymbolTable::SymbolEnvironmentEDF: return ReferencedEntity(m_scene.environment_edfs(), name);
      case SymbolTable::SymbolEnvironmentShader: return ReferencedEntity(m_scene.environment_shaders(), name);
      case SymbolTable::SymbolNotFound: break;
      default: break;               // might be a scalar
    }

    return ReferencedEntity();
}

}   // namespace renderer
