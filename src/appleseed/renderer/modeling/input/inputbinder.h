
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2011 Francois Beaune
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

#ifndef APPLESEED_RENDERER_MODELING_INPUT_INPUTBINDER_H
#define APPLESEED_RENDERER_MODELING_INPUT_INPUTBINDER_H

// appleseed.renderer headers.
#include "renderer/global/global.h"
#include "renderer/modeling/input/inputarray.h"
#include "renderer/modeling/scene/containers.h"

// Forward declarations.
namespace renderer      { class Assembly; }
namespace renderer      { class Scene; }
namespace renderer      { class SymbolTable; }

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
    InputBinder();

    // Bind all inputs of all entities in a scene.
    void bind(const Scene& scene);

    // Return the number of reported binding errors.
    size_t get_error_count() const;

  private:
    size_t  m_error_count;

    // Build the symbol table for a given scene.
    void build_scene_symbol_table(
        const Scene&                    scene,
        SymbolTable&                    symbols);

    // Build the symbol table for a given assembly.
    void build_assembly_symbol_table(
        const Assembly&                 assembly,
        SymbolTable&                    symbols);

    // Bind all inputs of all entities of a given scene.
    void bind_scene_entities_inputs(
        const Scene&                    scene,
        const SymbolTable&              scene_symbols);

    // Bind all inputs of all entities of a given assembly.
    void bind_assembly_entities_inputs(
        const Scene&                    scene,
        const SymbolTable&              scene_symbols,
        const Assembly&                 assembly,
        const SymbolTable&              assembly_symbols);

    // Bind all inputs of a given entity of a given scene.
    void bind_scene_entity_inputs(
        const Scene&                    scene,
        const SymbolTable&              scene_symbols,
        const char*                     entity_type,
        const char*                     entity_name,
        const ParamArray&               entity_params,
        InputArray&                     entity_inputs);

    // Bind all inputs of a given entity of a given assembly.
    void bind_assembly_entity_inputs(
        const Scene&                    scene,
        const SymbolTable&              scene_symbols,
        const Assembly&                 assembly,
        const SymbolTable&              assembly_symbols,
        const char*                     entity_type,
        const char*                     entity_name,
        const ParamArray&               entity_params,
        InputArray&                     entity_inputs);

    // Bind a given scene entity to a given input.
    void bind_scene_entity_to_input(
        const Scene&                    scene,
        const SymbolTable&              scene_symbols,
        const char*                     entity_type,
        const char*                     entity_name,
        const char*                     param_value,
        InputArray::iterator&           input);

    // Bind a given assembly entity to a given input.
    void bind_assembly_entity_to_input(
        const Scene&                    scene,
        const SymbolTable&              scene_symbols,
        const Assembly&                 assembly,
        const SymbolTable&              assembly_symbols,
        const char*                     entity_type,
        const char*                     entity_name,
        const char*                     param_value,
        InputArray::iterator&           input);

    // Try binding a scalar to a given input.
    // Return true on success (the value was indeed a scalar), false otherwise.
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
        const TextureContainer&         textures,
        const TextureInstanceContainer& texture_instances,
        const foundation::UniqueID      assembly_uid,           // unique ID of parent assembly, or ~0 for scene
        const char*                     entity_type,
        const char*                     entity_name,
        const char*                     param_value,
        InputArray::iterator&           input);
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_INPUT_INPUTBINDER_H
