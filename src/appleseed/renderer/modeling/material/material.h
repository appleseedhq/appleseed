
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
#include "renderer/modeling/entity/connectableentity.h"

// appleseed.foundation headers.
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Standard headers.
#include <cassert>

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class BaseGroup; }
namespace renderer      { class BSDF; }
namespace renderer      { class BSSRDF; }
namespace renderer      { class EDF; }
namespace renderer      { class IBasisModifier; }
namespace renderer      { class MessageContext; }
namespace renderer      { class OnFrameBeginRecorder; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }
namespace renderer      { class ShaderGroup; }
namespace renderer      { class Source; }
namespace renderer      { class SurfaceShader; }
namespace renderer      { class Volume; }

namespace renderer
{

//
// Base class for materials.
//

class APPLESEED_DLLSYMBOL Material
  : public ConnectableEntity
{
  public:
    // Return the unique ID of this class of entities.
    static foundation::UniqueID get_class_uid();

    // Return a string identifying the model of this material.
    virtual const char* get_model() const = 0;

    // Return the name the surface shader bound to this material, or nullptr if the material doesn't have one.
    const char* get_surface_shader_name() const;

    // Return the name the BSDF bound to this material, or nullptr if the material doesn't have one.
    const char* get_bsdf_name() const;

    // Return the name the BSSRDF bound to this material, or nullptr if the material doesn't have one.
    const char* get_bssrdf_name() const;

    // Return the name the volume bound to this material, or nullptr if the material doesn't have one.
    const char* get_volume_name() const;

    // Return the name the EDF bound to this material, or nullptr if the material doesn't have one.
    const char* get_edf_name() const;

    // Return the surface shader of the material, or nullptr if the material doesn't have one.
    const SurfaceShader* get_uncached_surface_shader() const;

    // Return the BSDF of the material, or nullptr if the material doesn't have one.
    const BSDF* get_uncached_bsdf() const;

    // Return the BSSRDF of the material, or nullptr if the material doesn't have one.
    const BSSRDF* get_uncached_bssrdf() const;

    // Return the EDF of the material, or nullptr if the material doesn't have one.
    const EDF* get_uncached_edf() const;

    // Return the volume of the material, or nullptr if the material doesn't have one.
    const Volume* get_uncached_volume() const;

    // Return the source bound to the alpha map input, or nullptr if the material doesn't have an alpha map.
    const Source* get_uncached_alpha_map() const;

    // Return the OSL surface shader of the material, or nullptr if the material doesn't have one.
    virtual const ShaderGroup* get_uncached_osl_surface() const;

    // Return true if this material has an alpha map.
    bool has_alpha_map() const;

    // Return true if this material has an uniform alpha value equals to 1.0f.
    bool has_opaque_uniform_alpha_map() const;

    // Return true if the material emits light.
    virtual bool has_emission() const;

    bool on_frame_begin(
        const Project&              project,
        const BaseGroup*            parent,
        OnFrameBeginRecorder&       recorder,
        foundation::IAbortSwitch*   abort_switch = nullptr) override;

    void on_frame_end(
        const Project&              project,
        const BaseGroup*            parent) override;

    struct APPLESEED_DLLSYMBOL RenderData
    {
        enum class DefaultTangentMode
        {
            UV,
            LocalX,
            LocalY,
            LocalZ,
            Radial
        };

        const SurfaceShader*        m_surface_shader;
        const BSDF*                 m_bsdf;
        const BSSRDF*               m_bssrdf;
        const EDF*                  m_edf;
        const Volume*               m_volume;
        const Source*               m_alpha_map;
        const ShaderGroup*          m_shader_group;
        const IBasisModifier*       m_basis_modifier;           // owned by RenderData
        DefaultTangentMode          m_default_tangent_mode;

        RenderData();

        void clear();
    };

    // Return render-time data of this entity.
    // Render-time data are available between on_frame_begin() and on_frame_end() calls.
    const RenderData& get_render_data() const;

  protected:
    RenderData m_render_data;

    // Constructor.
    Material(
        const char*                 name,
        const ParamArray&           params);

    const char* get_non_empty(const ParamArray& params, const char* name) const;

    IBasisModifier* create_basis_modifier(const MessageContext& context) const;

  private:
    RenderData::DefaultTangentMode get_default_tangent_mode() const;
};


//
// Material class implementation.
//

inline const Material::RenderData& Material::get_render_data() const
{
    return m_render_data;
}

}   // namespace renderer
