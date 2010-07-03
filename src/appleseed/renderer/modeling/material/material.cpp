
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

// Interface header.
#include "material.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// Material class implementation.
//

struct Material::Impl
{
    string  m_name;
};

// Constructors.
Material::Material(
    const char*                     name,
    const SurfaceShader*            surface_shader)
  : impl(new Impl())
{
    assert(name);
    assert(surface_shader);

    impl->m_name = name;

    m_surface_shader = surface_shader;
    m_bsdf = 0;
    m_edf = 0;
}
Material::Material(
    const char*                     name,
    const SurfaceShader*            surface_shader,
    const BSDF*                     bsdf)
  : impl(new Impl())
{
    assert(name);
    assert(surface_shader);

    impl->m_name = name;

    m_surface_shader = surface_shader;
    m_bsdf = bsdf;
    m_edf = 0;
}
Material::Material(
    const char*                     name,
    const SurfaceShader*            surface_shader,
    const BSDF*                     bsdf,
    const EDF*                      edf)
  : impl(new Impl())
{
    assert(name);
    assert(surface_shader);

    impl->m_name = name;

    m_surface_shader = surface_shader;
    m_bsdf = bsdf;
    m_edf = edf;
}
Material::Material(
    const char*                     name,
    const ParamArray&               params,
    const SurfaceShaderContainer&   surface_shaders,
    const BSDFContainer&            bsdfs,
    const EDFContainer&             edfs)
  : Entity(params)
  , impl(new Impl())
{
    assert(name);

    impl->m_name = name;

    m_surface_shader =
        get_required_entity<SurfaceShader>(
            surface_shaders,
            params,
            "surface_shader");

    m_bsdf = get_optional_entity<BSDF>(bsdfs, params, "bsdf");
    m_edf = get_optional_entity<EDF>(edfs, params, "edf");
}

// Destructor.
Material::~Material()
{
    delete impl;
}

// Delete this instance.
void Material::release()
{
    delete this;
}

// Return the name of this material.
const char* Material::get_name() const
{
    return impl->m_name.c_str();
}

// Return a string identifying the model of this material.
const char* Material::get_model() const
{
    return MaterialFactory::get_model();
}


//
// MaterialFactory class implementation.
//

// Return a string identifying this material model.
const char* MaterialFactory::get_model()
{
    return "generic_material";
}

// Create a new material.
auto_release_ptr<Material> MaterialFactory::create(
    const char*                     name,
    const SurfaceShader*            surface_shader)
{
    return
        auto_release_ptr<Material>(
            new Material(
                name,
                surface_shader));
}
auto_release_ptr<Material> MaterialFactory::create(
    const char*                     name,
    const SurfaceShader*            surface_shader,
    const BSDF*                     bsdf)
{
    return
        auto_release_ptr<Material>(
            new Material(
                name,
                surface_shader,
                bsdf));
}
auto_release_ptr<Material> MaterialFactory::create(
    const char*                     name,
    const SurfaceShader*            surface_shader,
    const BSDF*                     bsdf,
    const EDF*                      edf)
{
    return
        auto_release_ptr<Material>(
            new Material(
                name,
                surface_shader,
                bsdf,
                edf));
}
auto_release_ptr<Material> MaterialFactory::create(
    const char*                     name,
    const ParamArray&               params,
    const SurfaceShaderContainer&   surface_shaders,
    const BSDFContainer&            bsdfs,
    const EDFContainer&             edfs)
{
    return
        auto_release_ptr<Material>(
            new Material(
                name,
                params,
                surface_shaders,
                bsdfs,
                edfs));
}

}   // namespace renderer
