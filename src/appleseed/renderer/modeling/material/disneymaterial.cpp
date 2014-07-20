
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

// appleseed.foundation headers.
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

// standard headers
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
        const char* name,
        const Dictionary& params)
      : m_name(name)
    {
        m_layer_number = params.get<size_t>("layer_number");
    }

    string      m_name;
    size_t      m_layer_number;
};

DisneyMaterialLayer::DisneyMaterialLayer(
    const char* name,
    const Dictionary& params)
  : impl(new Impl(name, params))
{
}

DisneyMaterialLayer::~DisneyMaterialLayer()
{
    delete impl;
}

DisneyMaterialLayer::DisneyMaterialLayer(const DisneyMaterialLayer& other)
{
    impl = new Impl(*other.impl);
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
    return false;         
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

    metadata.push_back(
        Dictionary()
            .insert("name", "base_color")
            .insert("label", "Base Color")
            .insert("type", "color")
            .insert("default", "[0.0, 0.0, 0.0]"));

   metadata.push_back(
        Dictionary()
            .insert("name", "subsurface")
            .insert("label", "Subsurface")
            .insert("type", "colormap")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "metallic")
            .insert("label", "Metallic")
            .insert("type", "colormap")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "specular")
            .insert("label", "Specular")
            .insert("type", "colormap")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "specular_tint")
            .insert("label", "Specular tint")
            .insert("type", "colormap")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "anisotropic")
            .insert("label", "Anisotropic")
            .insert("type", "colormap")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "roughness")
            .insert("label", "Roughness")
            .insert("type", "colormap")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "sheen")
            .insert("label", "Sheen")
            .insert("type", "colormap")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "shin_tint")
            .insert("label", "Shin tint")
            .insert("type", "colormap")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "clearcoat")
            .insert("label", "Clearcoat")
            .insert("type", "colormap")
            .insert("default", "0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "clearcoat_gloss")
            .insert("label", "Clearcoat gloss")
            .insert("type", "colormap")
            .insert("default", "0"));
    return metadata;
}

Dictionary DisneyMaterialLayer::get_default_values()
{
    DictionaryArray metadata = get_input_metadata();

    Dictionary layer_params;
    for (size_t i = 0; i < metadata.size(); ++i)
    {
        Dictionary parameter = metadata[i];
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
    vector<DisneyMaterialLayer>                     m_layers;
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
    const Project&              project,
    const Assembly&             assembly,
    AbortSwitch*                abort_switch)
{
    if (!Material::on_frame_begin(project, assembly, abort_switch))
        return false;

    try
    {
        for (const_each<DictionaryDictionary> it = m_params.dictionaries(); it; ++it)
            impl->m_layers.push_back(DisneyMaterialLayer(it->name(), it->value()));
    }
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
    const Project&              project,
    const Assembly&             assembly) OVERRIDE
{
    impl->m_layers.clear();    
    Material::on_frame_end(project, assembly);
}

std::size_t DisneyMaterial::num_layers() const
{
    return impl->m_layers.size();
}

const DisneyMaterialLayer& DisneyMaterial::get_layer(std::size_t index) const
{
    assert(index < num_layers());

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
