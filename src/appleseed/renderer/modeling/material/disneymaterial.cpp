
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

// Interface header.
#include "disneymaterial.h"

// appleseed.renderer headers.
#include "renderer/modeling/bsdf/disneybrdf.h"
#include "renderer/modeling/input/expression.h"

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

// Standard headers.
#include <algorithm>
#include <vector>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// DisneyMaterialLayer class implementation.
//

struct DisneyMaterialLayer::Impl
{
    Impl(
        const char*         name,
        const Dictionary&   params)
      : m_name(name)
      , m_layer_number(params.get<size_t>("layer_number"))
    {
        m_layer_number = params.get<size_t>("layer_number");
        m_mask.set_expression(params.get<string>("mask").c_str(), false);
        m_base_color.set_expression(params.get<string>("base_color").c_str());
        m_subsurface.set_expression(params.get<string>("subsurface").c_str(), false);
        m_metallic.set_expression(params.get<string>("metallic").c_str(), false);
        m_specular.set_expression(params.get<string>("specular").c_str(), false);
        m_specular_tint.set_expression(params.get<string>("specular_tint").c_str(), false);
        m_anisotropic.set_expression(params.get<string>("anisotropic").c_str(), false);
        m_roughness.set_expression(params.get<string>("roughness").c_str(), false);
        m_sheen.set_expression(params.get<string>("sheen").c_str(), false);
        m_sheen_tint.set_expression(params.get<string>("sheen_tint").c_str(), false);
        m_clearcoat.set_expression(params.get<string>("clearcoat").c_str(), false);
        m_clearcoat_gloss.set_expression(params.get<string>("clearcoat_gloss").c_str(), false);
    }

    string      m_name;
    size_t      m_layer_number;
    Expression  m_mask;
    Expression  m_base_color;
    Expression  m_subsurface;
    Expression  m_metallic;
    Expression  m_specular;
    Expression  m_specular_tint;
    Expression  m_anisotropic;
    Expression  m_roughness;
    Expression  m_sheen;
    Expression  m_sheen_tint;
    Expression  m_clearcoat;
    Expression  m_clearcoat_gloss;
};

DisneyMaterialLayer::DisneyMaterialLayer(
    const char*         name,
    const Dictionary&   params)
  : impl(new Impl(name, params))
{
}

DisneyMaterialLayer::~DisneyMaterialLayer()
{
    delete impl;
}

DisneyMaterialLayer::DisneyMaterialLayer(const DisneyMaterialLayer& other)
  : impl(new Impl(*other.impl))
{
}

DisneyMaterialLayer& DisneyMaterialLayer::operator=(const DisneyMaterialLayer& other)
{
    DisneyMaterialLayer tmp(other);
    swap(tmp);
    return *this;
}

void DisneyMaterialLayer::swap(DisneyMaterialLayer& other)
{
    std::swap(impl, other.impl);
}

bool DisneyMaterialLayer::operator<(const DisneyMaterialLayer& other) const
{
    return impl->m_layer_number < other.impl->m_layer_number;
}

bool DisneyMaterialLayer::check_expressions_syntax() const
{
    return  impl->m_mask.syntax_ok() &&
            impl->m_base_color.syntax_ok() &&
            impl->m_subsurface.syntax_ok() &&
            impl->m_metallic.syntax_ok() &&
            impl->m_specular.syntax_ok() &&
            impl->m_specular_tint.syntax_ok() &&
            impl->m_anisotropic.syntax_ok() &&
            impl->m_roughness.syntax_ok() &&
            impl->m_sheen.syntax_ok() &&
            impl->m_sheen_tint.syntax_ok() &&
            impl->m_clearcoat.syntax_ok() &&
            impl->m_clearcoat_gloss.syntax_ok();            
}

DictionaryArray DisneyMaterialLayer::get_input_metadata()
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "layer_name")
            .insert("label", "Layer name")
            .insert("type", "text")
            .insert("default", "layer1"));

    metadata.push_back(
        Dictionary()
            .insert("name", "mask")
            .insert("label", "Mask")
            .insert("type", "colormap")
            .insert("default", "0"));

    metadata.append(DisneyBRDFFactory().get_input_metadata());

    // Modify base_color default value.
    for (size_t i = 0; i < metadata.size(); ++i)
    {
        const string name = metadata[i].get<string>("name");
        if (name == "base_color")
            metadata[i].insert("default", "[0.5, 0.5, 0.5]");
    }

    return metadata;
}

Dictionary DisneyMaterialLayer::get_default_values()
{
    DictionaryArray metadata = get_input_metadata();

    Dictionary layer_params;
    for (size_t i = 0; i < metadata.size(); ++i)
    {
        const Dictionary& parameter = metadata[i];
        const string name = parameter.get<string>("name");
        const string default_value = parameter.get<string>("default");

        layer_params.insert(name, default_value);
    }

    layer_params.insert("layer_number", 1);

    Dictionary values;
    values.insert("layer1", layer_params);
    return values;
}

//
// DisneyMaterial class implementation.
//

namespace
{
    const char* Model = "disney_material";
}

struct DisneyMaterial::Impl
{
    vector<DisneyMaterialLayer> m_layers;
};

DisneyMaterial::DisneyMaterial(
  const char*         name,
  const ParamArray&   params)
  : Material(name, params)
  , impl(new Impl())
{
}

DisneyMaterial::~DisneyMaterial()
{
    delete impl;
}

void DisneyMaterial::release()
{
    delete this;
}

const char* DisneyMaterial::get_model() const
{
    return Model;
}

bool DisneyMaterial::on_frame_begin(
    const Project&  project,
    const Assembly& assembly,
    AbortSwitch*    abort_switch)
{
    if (!Material::on_frame_begin(project, assembly, abort_switch))
        return false;

    try
    {
        for (const_each<DictionaryDictionary> it = m_params.dictionaries(); it; ++it)
            impl->m_layers.push_back(DisneyMaterialLayer(it->name(), it->value()));
    }
    // TODO: be more specific about what we catch here,
    // once we know what can be thrown. (est.)
    catch(...)
    {
        return false;
    }

    sort(impl->m_layers.begin(), impl->m_layers.end());

    for (const_each<vector<DisneyMaterialLayer> > it = impl->m_layers; it ; ++it)
    {
        if (!it->check_expressions_syntax())
            return false;
    }

    return true;
}

// This method is called once after rendering each frame.
void DisneyMaterial::on_frame_end(
    const Project&  project,
    const Assembly& assembly) OVERRIDE
{
    impl->m_layers.clear();    
    Material::on_frame_end(project, assembly);
}

std::size_t DisneyMaterial::get_layer_count() const
{
    return impl->m_layers.size();
}

const DisneyMaterialLayer& DisneyMaterial::get_layer(const size_t index) const
{
    assert(index < get_layer_count());

    return impl->m_layers[index];
}

//
// DisneyMaterialFactory class implementation.
//

const char* DisneyMaterialFactory::get_model() const
{
    return Model;
}

const char* DisneyMaterialFactory::get_human_readable_model() const
{
    return "Disney Material";
}

DictionaryArray DisneyMaterialFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    add_common_input_metadata(metadata);

    metadata.push_back(
        Dictionary()
            .insert("name", "alpha_mask")
            .insert("label", "Alpha Mask")
            .insert("type", "colormap")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "edf")
            .insert("label", "EDF")
            .insert("type", "entity")
            .insert("entity_types", Dictionary().insert("edf", "EDF"))
            .insert("use", "optional"));

    metadata.push_back(
        Dictionary()
            .insert("name", "alpha_map")
            .insert("label", "Alpha Map")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "optional"));

    metadata.push_back(
        Dictionary()
            .insert("name", "displacement_map")
            .insert("label", "Displacement Map")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional"));

    metadata.push_back(
        Dictionary()
            .insert("name", "displacement_method")
            .insert("label", "Displacement Method")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("Bump Mapping", "bump")
                    .insert("Normal Mapping", "normal"))
            .insert("use", "required")
            .insert("default", "bump"));

    metadata.push_back(
        Dictionary()
            .insert("name", "bump_amplitude")
            .insert("label", "Bump Amplitude")
            .insert("type", "text")
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "normal_map_up")
            .insert("label", "Normal Map Up Vector")
            .insert("type", "enumeration")
            .insert("items",
                Dictionary()
                    .insert("Green Channel (Y)", "y")
                    .insert("Blue Channel (Z)", "z"))
            .insert("use", "optional")
            .insert("default", "z"));

    return metadata;
}

auto_release_ptr<Material> DisneyMaterialFactory::create(
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<Material>(
            new DisneyMaterial(name, params));
}

}   // namespace renderer
