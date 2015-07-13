
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

#ifndef APPLESEED_RENDERER_MODELING_MATERIAL_MATERIAL_H
#define APPLESEED_RENDERER_MODELING_MATERIAL_MATERIAL_H

// appleseed.renderer headers.
#include "renderer/modeling/entity/connectableentity.h"

// appleseed.foundation headers.
#include "foundation/platform/compiler.h"
#include "foundation/utility/autoreleaseptr.h"
#include "foundation/utility/uid.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class IAbortSwitch; }
namespace renderer      { class Assembly; }
namespace renderer      { class BSDF; }
namespace renderer      { class BSSRDF; }
namespace renderer      { class EDF; }
namespace renderer      { class IBasisModifier; }
namespace renderer      { class MessageContext; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }
#ifdef APPLESEED_WITH_OSL
namespace renderer      { class ShaderGroup; }
#endif
namespace renderer      { class Source; }
namespace renderer      { class SurfaceShader; }

namespace renderer
{

//
// Material.
//

class APPLESEED_DLLSYMBOL Material
  : public ConnectableEntity
{
  public:
    // Return the unique ID of this class of entities.
    static foundation::UniqueID get_class_uid();

    // Return a string identifying the model of this material.
    virtual const char* get_model() const = 0;

    // Return true if this material has an alpha map.
    bool has_alpha_map() const;

    // Return the name the surface shader bound to this material, or 0 if the material doesn't have one.
    const char* get_surface_shader_name() const;

    // Return the name the BSDF bound to this material, or 0 if the material doesn't have one.
    const char* get_bsdf_name() const;

    // Return the name the BSSRDF bound to this material, or 0 if the material doesn't have one.
    const char* get_bssrdf_name() const;

    // Return the name the EDF bound to this material, or 0 if the material doesn't have one.
    const char* get_edf_name() const;

    // This method is called once before rendering each frame.
    // Returns true on success, false otherwise.
    virtual bool on_frame_begin(
        const Project&              project,
        const Assembly&             assembly,
        foundation::IAbortSwitch*   abort_switch = 0);

    // This method is called once after rendering each frame.
    virtual void on_frame_end(
        const Project&              project,
        const Assembly&             assembly);

    // Return whether surface shaders should be invoked for fully transparent shading points.
    bool shade_alpha_cutouts() const;

    //
    // The get_*() methods below retrieve entities that were cached by on_frame_begin().
    // To retrieve the entities before on_frame_begin() or after on_frame_end() is called,
    // use the get_uncached_*() variants.
    //

    // Return the surface shader of the material, or 0 if the material doesn't have one.
    const SurfaceShader* get_surface_shader() const;
    const SurfaceShader* get_uncached_surface_shader() const;

    // Return the BSDF of the material, or 0 if the material doesn't have one.
    const BSDF* get_bsdf() const;
    const BSDF* get_uncached_bsdf() const;

    // Return the BSSRDF of the material, or 0 if the material doesn't have one.
    const BSSRDF* get_bssrdf() const;
    const BSSRDF* get_uncached_bssrdf() const;

    // Return the EDF of the material, or 0 if the material doesn't have one.
    const EDF* get_edf() const;
    const EDF* get_uncached_edf() const;

    // Return the source bound to the alpha map input, or 0 if the material doesn't have an alpha map.
    const Source* get_alpha_map() const;
    const Source* get_uncached_alpha_map() const;

    // Return the basis modifier of the material, or 0 if the material doesn't have one.
    const IBasisModifier* get_basis_modifier() const;

    // Return true if the material emits light.
    virtual bool has_emission() const;

#ifdef APPLESEED_WITH_OSL
    virtual bool has_osl_surface() const;
    const ShaderGroup* get_osl_surface() const;
    virtual const ShaderGroup* get_uncached_osl_surface() const;
#endif

  protected:
    bool                            m_shade_alpha_cutouts;
    const SurfaceShader*            m_surface_shader;
    const BSDF*                     m_bsdf;
    const BSSRDF*                   m_bssrdf;
    const EDF*                      m_edf;
    const Source*                   m_alpha_map;
    const IBasisModifier*           m_basis_modifier;
#ifdef APPLESEED_WITH_OSL
    const ShaderGroup*              m_shader_group;
#endif

    // Constructor.
    Material(
        const char*                 name,
        const ParamArray&           params);

    const char* get_non_empty(const ParamArray& params, const char* name) const;

    bool create_basis_modifier(const MessageContext& context);
};


//
// Material class implementation.
//

inline bool Material::shade_alpha_cutouts() const
{
    return m_shade_alpha_cutouts;
}

inline const SurfaceShader* Material::get_surface_shader() const
{
    return m_surface_shader;
}

inline const BSDF* Material::get_bsdf() const
{
    return m_bsdf;
}

inline const BSSRDF* Material::get_bssrdf() const
{
    return m_bssrdf;
}

inline const EDF* Material::get_edf() const
{
    return m_edf;
}

inline const Source* Material::get_alpha_map() const
{
    return m_alpha_map;
}

inline const IBasisModifier* Material::get_basis_modifier() const
{
    return m_basis_modifier;
}

#ifdef APPLESEED_WITH_OSL

inline const ShaderGroup* Material::get_osl_surface() const
{
    return m_shader_group;
}

#endif

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_MATERIAL_MATERIAL_H
