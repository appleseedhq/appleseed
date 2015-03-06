
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014-2015 Francois Beaune, The appleseedhq Organization
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
#include "renderer/modeling/input/inputformat.h"
#include "renderer/modeling/input/scalarsource.h"
#include "renderer/modeling/input/symbol.h"
#include "renderer/modeling/input/texturesource.h"
#include "renderer/modeling/light/light.h"
#include "renderer/modeling/material/material.h"
#include "renderer/modeling/object/object.h"
#include "renderer/modeling/scene/assemblyinstance.h"
#include "renderer/modeling/scene/objectinstance.h"
#include "renderer/modeling/scene/scene.h"
#include "renderer/modeling/scene/textureinstance.h"
#ifdef APPLESEED_WITH_OSL
#include "renderer/modeling/shadergroup/shadergroup.h"
#endif
#include "renderer/modeling/surfaceshader/surfaceshader.h"
#include "renderer/modeling/texture/texture.h"
#include "renderer/utility/paramarray.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <exception>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// InputBinder class implementation.
//

InputBinder::InputBinder()
  : m_error_count(0)
{
}

void InputBinder::bind(const Scene& scene)
{
    try
    {
        // Build the symbol table of the scene.
        SymbolTable scene_symbols;
        build_scene_symbol_table(scene, scene_symbols);

        // Bind all inputs of all entities in the scene.
        bind_scene_entities_inputs(scene, scene_symbols);

        // Bind all inputs of all entities in all assemblies.
        for (const_each<AssemblyContainer> i = scene.assemblies(); i; ++i)
        {
            assert(m_assembly_info.empty());
            bind_assembly_entities_inputs(scene, scene_symbols, *i);
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

namespace
{
    template <typename EntityContainer>
    void insert_entities(
        SymbolTable&                symbols,
        const EntityContainer&      entities,
        const SymbolTable::SymbolID symbol_id)
    {
        for (const_each<EntityContainer> i = entities; i; ++i)
            symbols.insert(i->get_name(), symbol_id);
    }
}

void InputBinder::build_scene_symbol_table(
    const Scene&                    scene,
    SymbolTable&                    symbols)
{
    try
    {
        if (scene.get_camera())
            symbols.insert(scene.get_camera()->get_name(), SymbolTable::SymbolCamera);

        insert_entities(symbols, scene.colors(), SymbolTable::SymbolColor);
        insert_entities(symbols, scene.textures(), SymbolTable::SymbolTexture);
        insert_entities(symbols, scene.texture_instances(), SymbolTable::SymbolTextureInstance);
        insert_entities(symbols, scene.environment_edfs(), SymbolTable::SymbolEnvironmentEDF);
        insert_entities(symbols, scene.environment_shaders(), SymbolTable::SymbolEnvironmentShader);

#ifdef APPLESEED_WITH_OSL
        insert_entities(symbols, scene.shader_groups(), SymbolTable::SymbolShaderGroup);
#endif
        if (scene.get_environment())
            symbols.insert(scene.get_environment()->get_name(), SymbolTable::SymbolEnvironment);

        insert_entities(symbols, scene.assemblies(), SymbolTable::SymbolAssembly);
        insert_entities(symbols, scene.assembly_instances(), SymbolTable::SymbolAssemblyInstance);
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
        insert_entities(symbols, assembly.edfs(), SymbolTable::SymbolEDF);
#ifdef APPLESEED_WITH_OSL
        insert_entities(symbols, assembly.shader_groups(), SymbolTable::SymbolShaderGroup);
#endif
        insert_entities(symbols, assembly.surface_shaders(), SymbolTable::SymbolSurfaceShader);
        insert_entities(symbols, assembly.materials(), SymbolTable::SymbolMaterial);
        insert_entities(symbols, assembly.lights(), SymbolTable::SymbolLight);
        insert_entities(symbols, assembly.objects(), SymbolTable::SymbolObject);
        insert_entities(symbols, assembly.object_instances(), SymbolTable::SymbolObjectInstance);
    }
    catch (const SymbolTable::ExceptionDuplicateSymbol& e)
    {
        RENDERER_LOG_ERROR("duplicate entity \"%s\".", e.string());
        ++m_error_count;
    }
}

void InputBinder::bind_scene_entities_inputs(
    const Scene&                    scene,
    const SymbolTable&              scene_symbols)
{
    // Bind textures to texture instances.
    // Other entities might need to access the textures bound to texture instances,
    // so binding of textures to texture instances must come first.
    for (each<TextureInstanceContainer> i = scene.texture_instances(); i; ++i)
    {
        i->unbind_texture();
        i->bind_texture(scene.textures());
        i->check_texture();
    }

    // Bind the inputs of the default surface shader.
    bind_scene_entity_inputs(
        scene,
        scene_symbols,
        SymbolTable::symbol_name(SymbolTable::SymbolSurfaceShader),
        *scene.get_default_surface_shader());

    // Bind camera inputs.
    if (scene.get_camera())
    {
        bind_scene_entity_inputs(
            scene,
            scene_symbols,
            SymbolTable::symbol_name(SymbolTable::SymbolCamera),
            *scene.get_camera());
    }

    // Bind environment EDFs inputs.
    for (each<EnvironmentEDFContainer> i = scene.environment_edfs(); i; ++i)
    {
        bind_scene_entity_inputs(
            scene,
            scene_symbols,
            SymbolTable::symbol_name(SymbolTable::SymbolEnvironmentEDF),
            *i);
    }

    // Bind environment shaders inputs.
    for (each<EnvironmentShaderContainer> i = scene.environment_shaders(); i; ++i)
    {
        bind_scene_entity_inputs(
            scene,
            scene_symbols,
            SymbolTable::symbol_name(SymbolTable::SymbolEnvironmentShader),
            *i);
    }

    // Bind environment inputs.
    if (scene.get_environment())
    {
        bind_scene_entity_inputs(
            scene,
            scene_symbols,
            SymbolTable::symbol_name(SymbolTable::SymbolEnvironment),
            *scene.get_environment());
    }

    // Bind assemblies to assembly instances.
    for (each<AssemblyInstanceContainer> i = scene.assembly_instances(); i; ++i)
    {
        i->unbind_assembly();
        i->bind_assembly(scene.assemblies());
        i->check_assembly();
    }
}

void InputBinder::bind_scene_entity_inputs(
    const Scene&                    scene,
    const SymbolTable&              scene_symbols,
    const char*                     entity_type,
    ConnectableEntity&              entity)
{
    const string entity_path = entity.get_path();
    const ParamArray& entity_params = entity.get_parameters();

    for (each<InputArray> i = entity.get_inputs(); i; ++i)
    {
        InputArray::iterator& input = *i;
        string param_value;

        if (entity_params.strings().exist(input.name()))
        {
            // A value is assigned to this input, retrieve it.
            param_value = entity_params.get<string>(input.name());
        }
        else if (input.default_value())
        {
            // A default value is assigned to this input, use it.
            param_value = input.default_value();
            if (param_value.empty())
                continue;
        }
        else
        {
            // No value or default value, this is an error.
            RENDERER_LOG_ERROR(
                "while defining %s \"%s\": required parameter \"%s\" missing.",
                entity_type,
                entity_path.c_str(),
                input.name());
            ++m_error_count;
            continue;
        }

        if (try_bind_scene_entity_to_input(
                scene,
                scene_symbols,
                entity_type,
                entity_path.c_str(),
                param_value.c_str(),
                input))
            continue;

        if (try_bind_scalar_to_input(param_value, input))
            continue;

        RENDERER_LOG_ERROR(
            "while defining %s \"%s\": cannot bind \"%s\" to parameter \"%s\".",
            entity_type,
            entity_path.c_str(),
            param_value.c_str(),
            input.name());

        ++m_error_count;
    }
}

void InputBinder::bind_assembly_entities_inputs(
    const Scene&                    scene,
    const SymbolTable&              scene_symbols,
    const Assembly&                 assembly)
{
    // Build the symbol table of the assembly.
    SymbolTable assembly_symbols;
    build_assembly_symbol_table(assembly, assembly_symbols);

    // Push the assembly and its symbol table to the stack.
    AssemblyInfo info;
    info.m_assembly = &assembly;
    info.m_assembly_symbols = &assembly_symbols;
    m_assembly_info.push_back(info);

    // Bind textures to texture instances.
    // Other entities might need to access the textures bound to texture instances,
    // so binding of textures to texture instances must come first.
    for (each<TextureInstanceContainer> i = assembly.texture_instances(); i; ++i)
    {
        i->unbind_texture();

        for (AssemblyInfoIt j = m_assembly_info.rbegin(); j != m_assembly_info.rend(); ++j)
            i->bind_texture(j->m_assembly->textures());

        i->bind_texture(scene.textures());

        i->check_texture();
    }

    // Bind BSDFs inputs.
    for (each<BSDFContainer> i = assembly.bsdfs(); i; ++i)
    {
        bind_assembly_entity_inputs(
            scene,
            scene_symbols,
            SymbolTable::symbol_name(SymbolTable::SymbolBSDF),
            *i);
    }

    // Bind EDFs inputs.
    for (each<EDFContainer> i = assembly.edfs(); i; ++i)
    {
        bind_assembly_entity_inputs(
            scene,
            scene_symbols,
            SymbolTable::symbol_name(SymbolTable::SymbolEDF),
            *i);
    }

#ifdef APPLESEED_WITH_OSL
    // Bind ShaderGroups inputs.
    for (each<ShaderGroupContainer> i = assembly.shader_groups(); i; ++i)
    {
        bind_assembly_entity_inputs(
            scene,
            scene_symbols,
            SymbolTable::symbol_name(SymbolTable::SymbolShaderGroup),
            *i);
    }
#endif

    // Bind surface shaders inputs.
    for (each<SurfaceShaderContainer> i = assembly.surface_shaders(); i; ++i)
    {
        bind_assembly_entity_inputs(
            scene,
            scene_symbols,
            SymbolTable::symbol_name(SymbolTable::SymbolSurfaceShader),
            *i);
    }

    // Bind materials inputs.
    for (each<MaterialContainer> i = assembly.materials(); i; ++i)
    {
        bind_assembly_entity_inputs(
            scene,
            scene_symbols,
            SymbolTable::symbol_name(SymbolTable::SymbolMaterial),
            *i);
    }

    // Bind lights inputs.
    for (each<LightContainer> i = assembly.lights(); i; ++i)
    {
        bind_assembly_entity_inputs(
            scene,
            scene_symbols,
            SymbolTable::symbol_name(SymbolTable::SymbolLight),
            *i);
    }

    // Bind objects inputs.
    for (each<ObjectContainer> i = assembly.objects(); i; ++i)
    {
        bind_assembly_entity_inputs(
            scene,
            scene_symbols,
            SymbolTable::symbol_name(SymbolTable::SymbolObject),
            *i);
    }

    // Bind objects to object instances. This must be done before binding materials.
    for (each<ObjectInstanceContainer> i = assembly.object_instances(); i; ++i)
    {
        i->unbind_object();

        for (AssemblyInfoIt j = m_assembly_info.rbegin(); j != m_assembly_info.rend(); ++j)
            i->bind_object(j->m_assembly->objects());

        i->check_object();
    }

    // Bind materials to object instances.
    for (each<ObjectInstanceContainer> i = assembly.object_instances(); i; ++i)
    {
        i->unbind_materials();

        for (AssemblyInfoIt j = m_assembly_info.rbegin(); j != m_assembly_info.rend(); ++j)
            i->bind_materials(j->m_assembly->materials());

        i->check_materials();
    }

    // Bind assemblies to assembly instances.
    for (each<AssemblyInstanceContainer> i = assembly.assembly_instances(); i; ++i)
    {
        i->unbind_assembly();

        for (AssemblyInfoIt j = m_assembly_info.rbegin(); j != m_assembly_info.rend(); ++j)
            i->bind_assembly(j->m_assembly->assemblies());

        i->bind_assembly(scene.assemblies());

        i->check_assembly();
    }

    // Recurse into child assemblies.
    for (const_each<AssemblyContainer> i = assembly.assemblies(); i; ++i)
        bind_assembly_entities_inputs(scene, scene_symbols, *i);

    // Pop the information about this assembly from the stack.
    m_assembly_info.pop_back();
}

void InputBinder::bind_assembly_entity_inputs(
    const Scene&                    scene,
    const SymbolTable&              scene_symbols,
    const char*                     entity_type,
    ConnectableEntity&              entity)
{
    const string entity_path = entity.get_path();
    const ParamArray& entity_params = entity.get_parameters();

    for (each<InputArray> i = entity.get_inputs(); i; ++i)
    {
        InputArray::iterator& input = *i;
        string param_value;

        if (entity_params.strings().exist(input.name()))
        {
            // A value is assigned to this input, retrieve it.
            param_value = entity_params.get<string>(input.name());
        }
        else if (input.default_value())
        {
            // A default value is assigned to this input, use it.
            param_value = input.default_value();
            if (param_value.empty())
                continue;
        }
        else
        {
            // No value or default value, this is an error.
            RENDERER_LOG_ERROR(
                "while defining %s \"%s\": required parameter \"%s\" missing.",
                entity_type,
                entity_path.c_str(),
                input.name());
            ++m_error_count;
            continue;
        }

        if (try_bind_assembly_entity_to_input(
                scene,
                scene_symbols,
                entity_type,
                entity_path.c_str(),
                param_value.c_str(),
                input))
            continue;

        if (try_bind_scene_entity_to_input(
                scene,
                scene_symbols,
                entity_type,
                entity_path.c_str(),
                param_value.c_str(),
                input))
            continue;

        if (try_bind_scalar_to_input(param_value, input))
            continue;

        RENDERER_LOG_ERROR(
            "while defining %s \"%s\": cannot bind \"%s\" to parameter \"%s\".",
            entity_type,
            entity_path.c_str(),
            param_value.c_str(),
            input.name());

        ++m_error_count;
    }
}

bool InputBinder::try_bind_scene_entity_to_input(
    const Scene&                    scene,
    const SymbolTable&              scene_symbols,
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

        switch (scene_symbols.lookup(param_value))
        {
          BIND(SymbolTable::SymbolColor, scene.colors());
          BIND(SymbolTable::SymbolTexture, scene.textures());
          BIND(SymbolTable::SymbolTextureInstance, scene.texture_instances());
#ifdef APPLESEED_WITH_OSL
          BIND(SymbolTable::SymbolShaderGroup, scene.shader_groups());
#endif
          BIND(SymbolTable::SymbolEnvironmentEDF, scene.environment_edfs());
          BIND(SymbolTable::SymbolEnvironmentShader, scene.environment_shaders());
        }

        #undef BIND
    }
    else
    {
        switch (scene_symbols.lookup(param_value))
        {
          case SymbolTable::SymbolColor:
            bind_color_to_input(
                scene.colors(),
                param_value,
                input);
            return true;

          case SymbolTable::SymbolTextureInstance:
            bind_texture_instance_to_input(
                scene.texture_instances(),
                ~0,                 // the parent is the scene, not an assembly
                entity_type,
                entity_name,
                param_value,
                input);
            return true;
        }
    }

    return false;
}

bool InputBinder::try_bind_assembly_entity_to_input(
    const Scene&                    scene,
    const SymbolTable&              scene_symbols,
    const char*                     entity_type,
    const char*                     entity_name,
    const char*                     param_value,
    InputArray::iterator&           input)
{
    for (AssemblyInfoIt i = m_assembly_info.rbegin(); i != m_assembly_info.rend(); ++i)
    {
        if (try_bind_assembly_entity_to_input(
                scene,
                scene_symbols,
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
    const Scene&                    scene,
    const SymbolTable&              scene_symbols,
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
          BIND(SymbolTable::SymbolBSDF, assembly.bsdfs());
          BIND(SymbolTable::SymbolEDF, assembly.edfs());
#ifdef APPLESEED_WITH_OSL
          BIND(SymbolTable::SymbolShaderGroup, assembly.shader_groups());
#endif
          BIND(SymbolTable::SymbolSurfaceShader, assembly.surface_shaders());
          BIND(SymbolTable::SymbolMaterial, assembly.materials());
          BIND(SymbolTable::SymbolLight, assembly.lights());
          BIND(SymbolTable::SymbolObject, assembly.objects());
          BIND(SymbolTable::SymbolObjectInstance, assembly.object_instances());
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
        }
    }

    return false;
}

bool InputBinder::try_bind_scalar_to_input(
    const string&                   param_value,
    InputArray::iterator&           input) const
{
    try
    {
        const double value = from_string<double>(param_value);
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
            new TextureSource(
                assembly_uid,
                *texture_instance));
    }
    catch (const exception& e)
    {
        RENDERER_LOG_ERROR(
            "while defining %s \"%s\", failed to bind \"%s\" to input \"%s\" (%s).",
            entity_type,
            entity_name,
            param_value,
            input.name(),
            e.what());

        ++m_error_count;
    }
}

}   // namespace renderer
