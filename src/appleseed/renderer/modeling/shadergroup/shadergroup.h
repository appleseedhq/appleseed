
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
#include "renderer/modeling/shadergroup/shader.h"
#include "renderer/modeling/shadergroup/shaderconnection.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/autoreleaseptr.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cstddef>

// OSL headers.
#include <OSL/oslexec.h>

// Forward declarations.
namespace foundation    { class AbortSwitch; }
namespace foundation    { class SearchPaths; }
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
        const char*                 type,
        const char*                 name,
        const char*                 layer,
        const ParamArray&           params);

    // Adds a connection between two parameters of two shaders.
    void add_connection(
        const char*                 src_layer,
        const char*                 src_param,
        const char*                 dst_layer,
        const char*                 dst_param);

    // This method is called once before rendering each frame.
    // Returns true on success, false otherwise.
    bool on_frame_begin(
        const Project&              project,
        const Assembly&             assembly,
        OSL::ShadingSystem&         shading_system,
        foundation::AbortSwitch*    abort_switch = 0);

    // This method is called once after rendering each frame.
    void on_frame_end(
        const Project&              project,
        const Assembly&             assembly);

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

    // Return a reference-counted (but opaque) reference to the OSL shader.
    OSL::ShaderGroupRef& shadergroup_ref() const;

  private:
    friend class ShaderGroupFactory;

    ShaderContainer                 m_shaders;
    ShaderConnectionContainer       m_connections;
    mutable OSL::ShaderGroupRef     m_shadergroup_ref;
    const foundation::SearchPaths&  m_search_paths;
    bool                            m_has_emission;
    bool                            m_has_transparency;

    // Constructor.
    ShaderGroup(const char* name, const foundation::SearchPaths& searchpaths);
};


//
// ShaderGroup factory.
//

class DLLSYMBOL ShaderGroupFactory
{
  public:
    // Return a string identifying this ShaderGroup model.
    static const char* get_model();

    // Create a new ShaderGroup.
    static foundation::auto_release_ptr<ShaderGroup> create(
        const char* name,
        const foundation::SearchPaths& searchpaths);
};


//
// ShaderGroup class implementation.
//

inline const ShaderContainer& ShaderGroup::shaders() const
{
    return m_shaders;
}

inline const ShaderConnectionContainer& ShaderGroup::shader_connections() const
{
    return m_connections;
}

inline OSL::ShaderGroupRef& ShaderGroup::shadergroup_ref() const
{
    return m_shadergroup_ref;
}

inline bool ShaderGroup::valid() const
{
    return m_shadergroup_ref.get() != 0;
}

inline bool ShaderGroup::has_emission() const
{
    return m_has_emission;
}

inline bool ShaderGroup::has_transparency() const
{
    return m_has_transparency;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_SHADERGROUP_SHADERGROUP_H
