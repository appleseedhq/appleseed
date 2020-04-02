
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/modeling/entity/connectableentity.h"
#include "renderer/modeling/scene/containers.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/api/specializedapiarrays.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstdint>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace foundation    { class DictionaryArray; }
namespace renderer      { class AssemblyInstance; }
namespace renderer      { class OSLShadingSystem; }
namespace renderer      { class ObjectInstance; }
namespace renderer      { class ParamArray; }
namespace renderer      { class ShaderCompiler; }

namespace renderer
{

//
// OSL shader group.
//

class APPLESEED_DLLSYMBOL ShaderGroup
  : public ConnectableEntity
{
  public:
    // Delete this instance.
    void release() override;

    // Return a string identifying the model of this shader group.
    const char* get_model() const;

    // Remove all shaders and connections.
    void clear();

    // Add a new shader to the group.
    void add_shader(
        const char*                 type,
        const char*                 name,
        const char*                 layer,
        const ParamArray&           params);

    // Add a new shader to the group.
    void add_source_shader(
        const char*                 type,
        const char*                 name,
        const char*                 layer,
        const char*                 source,
        const ParamArray&           params);

    // Add a connection between two parameters of two shaders.
    void add_connection(
        const char*                 src_layer,
        const char*                 src_param,
        const char*                 dst_layer,
        const char*                 dst_param);

    // Create internal OSL shader group.
    bool create_optimized_osl_shader_group(
        OSLShadingSystem&           shading_system,
        const ShaderCompiler*       shader_compiler,
        foundation::IAbortSwitch*   abort_switch = nullptr);

    // Release internal OSL shader group.
    void release_optimized_osl_shader_group();

    // Access the shaders.
    const ShaderContainer& shaders() const;

    // Access the shader connections.
    const ShaderConnectionContainer& shader_connections() const;

    // Return true if the shader group was setup correctly.
    bool is_valid() const;

    // Return true if the shader group contains at least one BSDF closure.
    bool has_bsdfs() const;

    // Return true if the shader group contains at least one emission closure.
    bool has_emission() const;

    // Return true if the shader group contains at least one transparency closure.
    bool has_transparency() const;

    // Return true if the shader group contains at least one subsurface closure.
    bool has_subsurface() const;

    // Return true if the shader group contains at least one debug closure.
    bool has_debug() const;

    // Return true if the shader group contains at least one NPR closure.
    bool has_npr() const;

    // Return true if the shader group contains at least one matte or holdout closure.
    bool has_matte() const;

    // Return true if the shader group uses the dPdtime global.
    bool uses_dPdtime() const;

    // Return the surface area of an object.
    // Can only be called if the shader group has emission closures.
    float get_surface_area(
        const AssemblyInstance* assembly_instance,
        const ObjectInstance*   object_instance) const;

    // Return an opaque pointer to the internal OSL shader group.
    void* osl_shader_group() const;

  private:
    friend class LightSamplerBase;
    friend class ShaderGroupFactory;

    struct Impl;
    Impl* impl;

    enum Flags
    {
        // Closures.
        HasBSDFs        = 1u << 0,
        HasEmission     = 1u << 1,
        HasTransparency = 1u << 2,
        HasSubsurface   = 1u << 3,
        HasDebug        = 1u << 4,
        HasNPR          = 1u << 5,
        HasMatte        = 1u << 6,
        HasAllClosures  =
            HasBSDFs        |
            HasEmission     |
            HasTransparency |
            HasSubsurface   |
            HasDebug        |
            HasNPR          |
            HasMatte,

        // Globals.
        UsesdPdTime     = 1u << 7,
        UsesAllGlobals  = UsesdPdTime
    };

    std::uint32_t m_flags;

    // Constructor.
    explicit ShaderGroup(const char* name);

    // Destructor.
    ~ShaderGroup() override;

    bool compile_source_shaders(const ShaderCompiler* compiler);

    void get_shadergroup_closures_info(OSLShadingSystem& shading_system);
    void report_has_closure(const char* closure_name, const Flags flag) const;

    void get_shadergroup_globals_info(OSLShadingSystem& shading_system);
    void report_uses_global(const char* global_name, const Flags flag) const;

    void set_surface_area(
        const AssemblyInstance* assembly_instance,
        const ObjectInstance*   object_instance,
        const float             area) const;
};


//
// ShaderGroup factory.
//

class APPLESEED_DLLSYMBOL ShaderGroupFactory
{
  public:
    // Return a string identifying this shader group model.
    static const char* get_model();

    // Return a set of input metadata for this shader group model.
    static foundation::DictionaryArray get_input_metadata();

    // Create a new shader group.
    static foundation::auto_release_ptr<ShaderGroup> create(const char* name);

    // Create a new shader group.
    static foundation::auto_release_ptr<ShaderGroup> create(
        const char*         name,
        const ParamArray&   params);
};


//
// ShaderGroup class implementation.
//

inline bool ShaderGroup::has_bsdfs() const
{
    return (m_flags & HasBSDFs) != 0;
}

inline bool ShaderGroup::has_emission() const
{
    return (m_flags & HasEmission) != 0;
}

inline bool ShaderGroup::has_transparency() const
{
    return (m_flags & HasTransparency) != 0;
}

inline bool ShaderGroup::has_subsurface() const
{
    return (m_flags & HasSubsurface) != 0;
}

inline bool ShaderGroup::has_debug() const
{
    return (m_flags & HasDebug) != 0;
}

inline bool ShaderGroup::has_npr() const
{
    return (m_flags & HasNPR) != 0;
}

inline bool ShaderGroup::has_matte() const
{
    return (m_flags & HasMatte) != 0;
}

inline bool ShaderGroup::uses_dPdtime() const
{
    return (m_flags & UsesdPdTime) != 0;
}

}   // namespace renderer
