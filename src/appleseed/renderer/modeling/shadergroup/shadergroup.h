
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2016 Esteban Tovagliari, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_MODELING_SHADERGROUP_SHADERGROUP_H
#define APPLESEED_RENDERER_MODELING_SHADERGROUP_SHADERGROUP_H

// appleseed.renderer headers.
#include "renderer/modeling/entity/connectableentity.h"
#include "renderer/modeling/scene/containers.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/autoreleaseptr.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// OSL headers.
#include "foundation/platform/oslheaderguards.h"
BEGIN_OSL_INCLUDES
#include "OSL/oslexec.h"
END_OSL_INCLUDES

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class AssemblyInstance; }
namespace renderer      { class LightSamper; }
namespace renderer      { class ParamArray; }
namespace renderer      { class ObjectInstance; }
namespace renderer      { class Project; }

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
    virtual void release() APPLESEED_OVERRIDE;

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

    // Add a connection between two parameters of two shaders.
    void add_connection(
        const char*                 src_layer,
        const char*                 src_param,
        const char*                 dst_layer,
        const char*                 dst_param);

    // Create OSL shader group.
    bool create_optimized_osl_shader_group(
        OSL::ShadingSystem&         shading_system,
        foundation::IAbortSwitch*   abort_switch = 0);

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

    // Return true if the shader group contains at least one dielectric closure.
    bool has_refraction() const;

    // Return true if the shader group contains at least one holdout closure.
    bool has_holdout() const;

    // Return true if the shader group contains at least one debug closure.
    bool has_debug() const;

    // Return true if the shader group uses the dPdtime global.
    bool uses_dPdtime() const;

    // Return the surface area of an object.
    // Can only be called if the shader group has emission closures.
    float get_surface_area(const AssemblyInstance* ass, const ObjectInstance* obj) const;

    // Return a reference-counted (but opaque) reference to the internal OSL shader group.
    OSL::ShaderGroupRef& shader_group_ref() const;

  private:
    friend class LightSampler;
    friend class ShaderGroupFactory;

    struct Impl;
    Impl* impl;

    enum Flags
    {
        // Closures.
        HasBSDFs        = 1 << 0,
        HasEmission     = 1 << 1,
        HasTransparency = 1 << 2,
        HasSubsurface   = 1 << 3,
        HasRefraction   = 1 << 4,
        HasHoldout      = 1 << 5,
        HasDebug        = 1 << 6,
        HasAllClosures  =
            HasBSDFs        |
            HasEmission     |
            HasTransparency |
            HasSubsurface   |
            HasRefraction   |
            HasHoldout      |
            HasDebug,

        // Globals.
        UsesdPdTime     = 1 << 7,
        UsesAllGlobals  = UsesdPdTime
    };
    foundation::uint32 m_flags;

    // Constructor.
    explicit ShaderGroup(const char* name);

    // Destructor.
    ~ShaderGroup();

    void get_shadergroup_closures_info(OSL::ShadingSystem& shading_system);
    void report_has_closure(const char* closure_name, const Flags flag) const;

    void get_shadergroup_globals_info(OSL::ShadingSystem& shading_system);
    void report_uses_global(const char* global_name, const Flags flag) const;

    void set_surface_area(
        const AssemblyInstance* ass,
        const ObjectInstance*   obj,
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

    // Create a new shader group.
    static foundation::auto_release_ptr<ShaderGroup> create(const char* name);
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

inline bool ShaderGroup::has_refraction() const
{
    return (m_flags & HasRefraction) != 0;
}

inline bool ShaderGroup::has_holdout() const
{
    return (m_flags & HasHoldout) != 0;
}

inline bool ShaderGroup::has_debug() const
{
    return (m_flags & HasDebug) != 0;
}

inline bool ShaderGroup::uses_dPdtime() const
{
    return (m_flags & UsesdPdTime) != 0;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_SHADERGROUP_SHADERGROUP_H
