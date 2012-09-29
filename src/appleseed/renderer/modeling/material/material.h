
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz Limited
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

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class DictionaryArray; }
namespace renderer      { class Assembly; }
namespace renderer      { class BSDF; }
namespace renderer      { class EDF; }
namespace renderer      { class INormalModifier; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }
namespace renderer      { class Source; }
namespace renderer      { class SurfaceShader; }
namespace renderer      { class TextureCache; }

namespace renderer
{

//
// Material.
//

class DLLSYMBOL Material
  : public ConnectableEntity
{
  public:
    // Delete this instance.
    virtual void release() override;

    // Return a string identifying the model of this material.
    const char* get_model() const;

    // Return true if this material has an alpha map.
    bool has_alpha_map() const;

    // This method is called once before rendering each frame.
    // Returns true on success, false otherwise.
    bool on_frame_begin(
        const Project&      project,
        const Assembly&     assembly);

    // This method is called once after rendering each frame.
    void on_frame_end(
        const Project&      project,
        const Assembly&     assembly);

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

    // Return the EDF of the material, or 0 if the material doesn't have one.
    const EDF* get_edf() const;
    const EDF* get_uncached_edf() const;

    // Return the source bound to the alpha map input, or 0 if the material doesn't have an alpha map.
    const Source* get_alpha_map() const;
    const Source* get_uncached_alpha_map() const;

    // Return the normal modifier of the material, or 0 if the material doesn't have one.
    const INormalModifier* get_normal_modifier() const;

  private:
    friend class MaterialFactory;

    const SurfaceShader*    m_surface_shader;
    const BSDF*             m_bsdf;
    const EDF*              m_edf;
    const Source*           m_alpha_map;
    const INormalModifier*  m_normal_modifier;

    // Constructor.
    Material(
        const char*         name,
        const ParamArray&   params);
};


//
// Material factory.
//

class RENDERERDLL MaterialFactory
{
  public:
    // Return a string identifying this material model.
    static const char* get_model();

    // Return a set of widget definitions for this material model.
    static foundation::DictionaryArray get_widget_definitions();

    // Create a new material.
    static foundation::auto_release_ptr<Material> create(
        const char*         name,
        const ParamArray&   params);
};


//
// Material class implementation.
//

inline const SurfaceShader* Material::get_surface_shader() const
{
    return m_surface_shader;
}

inline const BSDF* Material::get_bsdf() const
{
    return m_bsdf;
}

inline const EDF* Material::get_edf() const
{
    return m_edf;
}

inline const Source* Material::get_alpha_map() const
{
    return m_alpha_map;
}

inline const INormalModifier* Material::get_normal_modifier() const
{
    return m_normal_modifier;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_MATERIAL_MATERIAL_H
