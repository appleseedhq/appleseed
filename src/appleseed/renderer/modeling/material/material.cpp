
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2012 Francois Beaune, Jupiter Jazz
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

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

using namespace foundation;
using namespace std;

namespace renderer
{

//
// Material class implementation.
//

namespace
{
    const UniqueID g_class_uid = new_guid();
}

Material::Material(
    const char*                     name,
    const ParamArray&               params)
  : ConnectableEntity(g_class_uid, params)
  , m_surface_shader(0)
  , m_bsdf(0)
  , m_edf(0)
{
    set_name(name);
}

void Material::release()
{
    delete this;
}

const char* Material::get_model() const
{
    return MaterialFactory::get_model();
}

void Material::bind_entities(
    const SurfaceShaderContainer&   surface_shaders,
    const BSDFContainer&            bsdfs,
    const EDFContainer&             edfs)
{
    m_surface_shader =
        get_required_entity<SurfaceShader>(
            surface_shaders,
            m_params,
            "surface_shader");

    m_bsdf = get_optional_entity<BSDF>(bsdfs, m_params, "bsdf");

    m_edf = get_optional_entity<EDF>(edfs, m_params, "edf");
}

void Material::on_frame_begin(
    const Project&                  project,
    const Assembly&                 assembly)
{
}

void Material::on_frame_end(
    const Project&                  project,
    const Assembly&                 assembly)
{
}


//
// MaterialFactory class implementation.
//

const char* MaterialFactory::get_model()
{
    return "generic_material";
}

DictionaryArray MaterialFactory::get_widget_definitions()
{
    DictionaryArray definitions;

    definitions.push_back(
        Dictionary()
            .insert("name", "bsdf")
            .insert("label", "BSDF")
            .insert("widget", "entity_picker")
            .insert("entity_types", Dictionary().insert("bsdf", "BSDF"))
            .insert("use", "optional"));

    definitions.push_back(
        Dictionary()
            .insert("name", "edf")
            .insert("label", "EDF")
            .insert("widget", "entity_picker")
            .insert("entity_types", Dictionary().insert("edf", "EDF"))
            .insert("use", "optional"));

    definitions.push_back(
        Dictionary()
            .insert("name", "surface_shader")
            .insert("label", "Surface Shader")
            .insert("widget", "entity_picker")
            .insert("entity_types",
                Dictionary().insert("surface_shader", "Surface Shaders"))
            .insert("use", "required"));

    return definitions;
}

auto_release_ptr<Material> MaterialFactory::create(
    const char*                     name,
    const ParamArray&               params)
{
    return auto_release_ptr<Material>(new Material(name, params));
}

}   // namespace renderer
