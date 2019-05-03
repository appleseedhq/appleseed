
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
#include "renderer/modeling/scene/containers.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace foundation    { class StringArray; }
namespace foundation    { class StringDictionary; }
namespace renderer      { class Entity; }
namespace renderer      { class OnFrameBeginRecorder; }
namespace renderer      { class OnRenderBeginRecorder; }
namespace renderer      { class OSLShadingSystem; }
namespace renderer      { class Project; }
namespace renderer      { class ShaderCompiler; }

namespace renderer
{

//
// Base class for renderer::Scene and renderer::Assembly.
//

class APPLESEED_DLLSYMBOL BaseGroup
{
  public:
    // Access the colors.
    ColorContainer& colors() const;

    // Access the textures.
    TextureContainer& textures() const;

    // Access the texture instances.
    TextureInstanceContainer& texture_instances() const;

    // Access the OSL shader groups.
    ShaderGroupContainer& shader_groups() const;

    // Clear the base group contents.
    void clear();

    // Create OSL shader groups and optimize them.
    bool create_optimized_osl_shader_groups(
        OSLShadingSystem&           shading_system,
        const ShaderCompiler*       shader_compiler,
        foundation::IAbortSwitch*   abort_switch = nullptr);

    // Release internal OSL shader groups.
    void release_optimized_osl_shader_groups();

    // Access the assemblies.
    AssemblyContainer& assemblies() const;

    // Access the assembly instances.
    AssemblyInstanceContainer& assembly_instances() const;

    // Expose asset file paths referenced by this entity to the outside.
    void collect_asset_paths(foundation::StringArray& paths) const;
    void update_asset_paths(const foundation::StringDictionary& mappings);

    // Please refer to the documentation of Entity::on_render_begin().
    bool on_render_begin(
        const Project&              project,
        const BaseGroup*            parent,
        OnRenderBeginRecorder&      recorder,
        foundation::IAbortSwitch*   abort_switch = nullptr);

    // Please refer to the documentation of Entity::on_frame_begin().
    bool on_frame_begin(
        const Project&              project,
        const BaseGroup*            parent,
        OnFrameBeginRecorder&       recorder,
        foundation::IAbortSwitch*   abort_switch = nullptr);

  protected:
    // Constructor.
    explicit BaseGroup(Entity* parent = nullptr);

    // Destructor.
    ~BaseGroup();

  private:
    struct Impl;
    Impl* impl;
};

}   // namespace renderer
