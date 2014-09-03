
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014 Esteban Tovagliari, The appleseedhq Organization
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
namespace foundation    { class AbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }

namespace renderer
{

//
// OSL Shadergroup.
//

class DLLSYMBOL ShaderGroup
  : public ConnectableEntity
{
  public:
    // Delete this instance.
    virtual void release() OVERRIDE;

    // Return a string identifying the model of this shader group.
    const char* get_model() const;

    // Adds a new shader to the group.
    void add_shader(
        const char*                     type,
        const char*                     name,
        const char*                     layer,
        const ParamArray&               params);

    // Adds a connection between two parameters of two shaders.
    void add_connection(
        const char*                     src_layer,
        const char*                     src_param,
        const char*                     dst_layer,
        const char*                     dst_param);

    // Create OSL shadergroup.
    bool create_osl_shader_group(
        OSL::ShadingSystem&             shading_system,
        foundation::AbortSwitch*        abort_switch = 0);

    // Release internal OSL shadergroup.
    void release_osl_shader_group();

    // Access the shaders.
    const ShaderContainer& shaders() const;

    // Access the shader connections.
    const ShaderConnectionContainer& shader_connections() const;

    // Returns true if the shader group was setup correctly.
    bool valid() const;

    // Returns true if the shader group contains at least one emission closure.
    bool has_emission() const;

    // Returns true if the shader group contains at least one transparency closure.
    bool has_transparency() const;

    // Returns true if the shader group contains at least one holdout closure.
    bool has_holdout() const;

    // Returns true if the shader group contains at least one debug closure.
    bool has_debug() const;

    // Return a reference-counted (but opaque) reference to the OSL shader.
    OSL::ShaderGroupRef& shader_group_ref() const;

  private:
    friend class ShaderGroupFactory;

    struct Impl;
    Impl* impl;

    bool    m_has_emission;
    bool    m_has_transparency;
    bool    m_has_holdout;
    bool    m_has_debug;

    // Constructor.
    explicit ShaderGroup(const char* name);

    // Destructor.
    ~ShaderGroup();

    void report_has_closures(const char* closure_name, bool has_closures) const;
    void get_shader_group_info(OSL::ShadingSystem& shading_system);
};


//
// ShaderGroup factory.
//

class DLLSYMBOL ShaderGroupFactory
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

inline bool ShaderGroup::has_emission() const
{
    return m_has_emission;
}

inline bool ShaderGroup::has_transparency() const
{
    return m_has_transparency;
}

inline bool ShaderGroup::has_holdout() const
{
    return m_has_holdout;
}

inline bool ShaderGroup::has_debug() const
{
    return m_has_debug;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_SHADERGROUP_SHADERGROUP_H
