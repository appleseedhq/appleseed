
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010 Francois Beaune
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
#include "renderer/global/global.h"
#include "renderer/modeling/entity/entity.h"
#include "renderer/modeling/scene/containers.h"

// Forward declarations.
namespace renderer      { class BSDF; }
namespace renderer      { class EDF; }
namespace renderer      { class SurfaceShader; }

namespace renderer
{

//
// Material.
//

class RENDERERDLL Material
  : public Entity
{
  public:
    // Delete this instance.
    virtual void release();

    // Return the name of this material.
    virtual const char* get_name() const;

    // Return a string identifying the model of this material.
    const char* get_model() const;

    // Return the surface shader of this material.
    const SurfaceShader& get_surface_shader() const;

    // Return the BSDF of this material, or 0 if the material doesn't have one.
    const BSDF* get_bsdf() const;

    // Return the EDF of this material, or 0 if the material doesn't have one.
    const EDF* get_edf() const;

  private:
    friend class MaterialFactory;

    // Private implementation.
    struct Impl;
    Impl* impl;

    // Derogate to the private implementation rule, for performance reasons.
    const SurfaceShader*    m_surface_shader;
    const BSDF*             m_bsdf;
    const EDF*              m_edf;

    // Constructors.
    Material(
        const char*                     name,
        const SurfaceShader*            surface_shader);
    Material(
        const char*                     name,
        const SurfaceShader*            surface_shader,
        const BSDF*                     bsdf);
    Material(
        const char*                     name,
        const SurfaceShader*            surface_shader,
        const BSDF*                     bsdf,
        const EDF*                      edf);
    Material(
        const char*                     name,
        const ParamArray&               params,
        const SurfaceShaderContainer&   surface_shaders,
        const BSDFContainer&            bsdfs,
        const EDFContainer&             edfs);

    // Destructor.
    ~Material();
};


//
// Material factory.
//

class RENDERERDLL MaterialFactory
{
  public:
    // Return a string identifying this material model.
    static const char* get_model();

    // Create a new material.
    static foundation::auto_release_ptr<Material> create(
        const char*                     name,
        const SurfaceShader*            surface_shader);
    static foundation::auto_release_ptr<Material> create(
        const char*                     name,
        const SurfaceShader*            surface_shader,
        const BSDF*                     bsdf);
    static foundation::auto_release_ptr<Material> create(
        const char*                     name,
        const SurfaceShader*            surface_shader,
        const BSDF*                     bsdf,
        const EDF*                      edf);
    static foundation::auto_release_ptr<Material> create(
        const char*                     name,
        const ParamArray&               params,
        const SurfaceShaderContainer&   surface_shaders,
        const BSDFContainer&            bsdfs,
        const EDFContainer&             edfs);
};


//
// Material class implementation.
//

// Return the surface shader of this material.
inline const SurfaceShader& Material::get_surface_shader() const
{
    assert(m_surface_shader);
    return *m_surface_shader;
}

// Return the BSDF of this material, or 0 if the material doesn't have one.
inline const BSDF* Material::get_bsdf() const
{
    return m_bsdf;
}

// Return the EDF of this material, or 0 if the material doesn't have one.
inline const EDF* Material::get_edf() const
{
    return m_edf;
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_MATERIAL_MATERIAL_H
