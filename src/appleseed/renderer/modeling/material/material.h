
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
#include "renderer/modeling/scene/containers.h"

// appleseed.foundation headers.
#include "foundation/utility/autoreleaseptr.h"

// appleseed.main headers.
#include "main/dllsymbol.h"

// Forward declarations.
namespace foundation    { class DictionaryArray; }
namespace renderer      { class Assembly; }
namespace renderer      { class BSDF; }
namespace renderer      { class EDF; }
namespace renderer      { class ParamArray; }
namespace renderer      { class Project; }
namespace renderer      { class Source; }
namespace renderer      { class SurfaceShader; }

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
    virtual void release();

    // Return a string identifying the model of this material.
    const char* get_model() const;

    // Return true if this material has an alpha map.
    bool has_alpha_map() const;

    // Perform entity binding.
    void bind_entities(
        const SurfaceShaderContainer&   surface_shaders,
        const BSDFContainer&            bsdfs,
        const EDFContainer&             edfs);

    // This method is called once before rendering each frame.
    void on_frame_begin(
        const Project&                  project,
        const Assembly&                 assembly);

    // This method is called once after rendering each frame.
    void on_frame_end(
        const Project&                  project,
        const Assembly&                 assembly);

    // Return the surface shader of the material, or 0 if the material doesn't have one.
    const SurfaceShader* get_surface_shader() const;

    // Return the BSDF of the material, or 0 if the material doesn't have one.
    const BSDF* get_bsdf() const;

    // Return the EDF of the material, or 0 if the material doesn't have one.
    const EDF* get_edf() const;

    // Return the source bound to the alpha map input, or 0 if the material doesn't have an alpha map.
    const Source* get_alpha_map() const;

    // Return the source bound to the normal map input, or 0 if the material doesn't have a normal map.
    const Source* get_normal_map() const;

  private:
    friend class MaterialFactory;

    const SurfaceShader*    m_surface_shader;
    const BSDF*             m_bsdf;
    const EDF*              m_edf;
    const Source*           m_alpha_map;
    const Source*           m_normal_map;

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

inline const Source* Material::get_normal_map() const
{
    return m_normal_map;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_MATERIAL_MATERIAL_H
