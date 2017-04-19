
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2017 Esteban Tovagliari, The appleseedhq Organization
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
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/disneybrdf.h"
#include "renderer/modeling/bsdf/disneylayeredbrdf.h"
#include "renderer/modeling/scene/containers.h"
#include "renderer/utility/seexpr.h"

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/math/scalar.h"
#include "foundation/utility/api/specializedapiarrays.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/seexpr.h"
#include "foundation/utility/tls.h"

// Boost headers.
#include "boost/algorithm/string.hpp"
#include "boost/algorithm/string/split.hpp"

// Standard headers.
#include <algorithm>
#include <map>
#include <memory>
#include <string>
#include <vector>

using namespace foundation;
using namespace boost;
using namespace std;

namespace renderer
{

namespace
{
    //
    // The DisneyLayerParam class wraps an SeAppleseedExpr object to add basic optimizations
    // for straightforward expressions such as a single scalar or a simple texture lookup.
    //

    class DisneyLayerParam
    {
      public:
        DisneyLayerParam(
            const char*             name,
            const Dictionary&       params,
            const bool              is_vector)
          : m_param_name(name)
          , m_expr(params.get<string>(name))
          , m_is_vector(is_vector)
          , m_is_constant(false)
          , m_texture_is_srgb(true)
        {
        }

        DisneyLayerParam(const DisneyLayerParam& other)
          : m_param_name(other.m_param_name)
          , m_expr(other.m_expr)
          , m_is_vector(other.m_is_vector)
          , m_is_constant(other.m_is_constant)
          , m_constant_value(other.m_constant_value)
          , m_texture_filename(other.m_texture_filename)
          , m_texture_options(other.m_texture_options)
          , m_texture_is_srgb(other.m_texture_is_srgb)
        {
        }

        string& expression()
        {
            return m_expr;
        }

        const string& expression() const
        {
            return m_expr;
        }

        bool prepare()
        {
            m_expression.setWantVec(m_is_vector);
            m_expression.set_expr(m_expr);

            if (!m_expression.isValid())
            {
                RENDERER_LOG_ERROR("expression error for \"%s\" parameter: %s",
                    m_param_name, m_expression.parseError().c_str());
                return false;
            }

            // Case of a simple constant.
            m_is_constant = m_expression.isConstant();
            if (m_is_constant)
            {
                const SeVec3d result = m_expression.evaluate();
                m_constant_value = Color3d(result[0], result[1], result[2]);
                return true;
            }

            // Case of a simple texture lookup of the form texture("path/to/texture", $u, $v).
            {
                const string expression = trim_both(m_expression.getExpr(), " \r\n");
                vector<string> tokens;
                tokenize(expression, "()", tokens);

                if (tokens.size() != 2)
                    return true;

                if (trim_both(tokens[0]) != "texture")
                    return true;

                const string inner_content = tokens[1];
                tokens.clear();
                tokenize(inner_content, ",", tokens);

                if (tokens.size() != 3)
                    return true;

                if (trim_both(tokens[1]) != "$u")
                    return true;

                if (trim_both(tokens[2]) != "$v")
                    return true;

                m_texture_filename = OIIO::ustring(trim_both(tokens[0], " \""));
                m_texture_is_srgb = texture_is_srgb(m_texture_filename);
                m_texture_options.swrap = OIIO::TextureOpt::WrapPeriodic;
                m_texture_options.rwrap = OIIO::TextureOpt::WrapPeriodic;
            }

            return true;
        }

        Color3f evaluate(
            const ShadingPoint&     shading_point,
            OIIO::TextureSystem&    texture_system) const
        {
            if (m_is_constant)
                return m_constant_value;

            if (!m_texture_filename.empty())
            {
                const Vector2f& uv = shading_point.get_uv(0);

                Color3f color;
                if (!texture_system.texture(
                        m_texture_filename,
                        m_texture_options,
                        uv[0],
                        uv[1],
                        0.0f,
                        0.0f,
                        0.0f,
                        0.0f,
                        3,
                        &color[0]))
                {
                    // Failed to find or open the texture.
                    const string message = texture_system.geterror();
                    if (!message.empty())
                    {
                        const string modified_message = prefix_all_lines(trim_both(message), "oiio: ");
                        RENDERER_LOG_ERROR("%s", modified_message.c_str());
                    }
                    return Color3f(1.0f, 0.0f, 1.0f);
                }

                // Colors in SeExpr are always in the sRGB color space.
                if (!m_texture_is_srgb)
                    color = linear_rgb_to_srgb(color);

                return color;
            }

            return
                m_expression.update_and_evaluate(
                    shading_point,
                    texture_system);
        }

      private:
        const char*                 m_param_name;
        string                      m_expr;
        bool                        m_is_vector;
        bool                        m_is_constant;
        Color3d                     m_constant_value;
        OIIO::ustring               m_texture_filename;
        mutable OIIO::TextureOpt    m_texture_options;
        bool                        m_texture_is_srgb;
        mutable SeAppleseedExpr     m_expression;
    };
}


//
// DisneyMaterialLayer class implementation.
//

namespace
{
    const UniqueID g_disney_material_layer_class_uid = new_guid();
}

struct DisneyMaterialLayer::Impl
{
    Impl(
        const char*         name,
        const Dictionary&   params)
      : m_name(name)
      , m_layer_number(params.get<int>("layer_number"))
      , m_mask("mask", params, false)
      , m_base_color("base_color", params, true)
      , m_subsurface("subsurface", params, false)
      , m_metallic("metallic", params, false)
      , m_specular("specular", params, false)
      , m_specular_tint("specular_tint", params, false)
      , m_anisotropic("anisotropic", params, false)
      , m_roughness("roughness", params, false)
      , m_sheen("sheen", params, false)
      , m_sheen_tint("sheen_tint", params, false)
      , m_clearcoat("clearcoat", params, false)
      , m_clearcoat_gloss("clearcoat_gloss", params, false)
    {
    }

    const string            m_name;
    const int               m_layer_number;
    DisneyLayerParam        m_mask;
    DisneyLayerParam        m_base_color;
    DisneyLayerParam        m_subsurface;
    DisneyLayerParam        m_metallic;
    DisneyLayerParam        m_specular;
    DisneyLayerParam        m_specular_tint;
    DisneyLayerParam        m_anisotropic;
    DisneyLayerParam        m_roughness;
    DisneyLayerParam        m_sheen;
    DisneyLayerParam        m_sheen_tint;
    DisneyLayerParam        m_clearcoat;
    DisneyLayerParam        m_clearcoat_gloss;
};

DisneyMaterialLayer::DisneyMaterialLayer(
    const char*             name,
    const Dictionary&       params)
  : Entity(g_disney_material_layer_class_uid, params)
  , impl(new Impl(name, params))
{
}

DisneyMaterialLayer::DisneyMaterialLayer(const DisneyMaterialLayer& other)
  : Entity(other.m_class_uid, other.m_params)
  , impl(new Impl(*other.impl))
{
}

DisneyMaterialLayer::~DisneyMaterialLayer()
{
    delete impl;
}

void DisneyMaterialLayer::release()
{
    delete this;
}

DisneyMaterialLayer& DisneyMaterialLayer::operator=(const DisneyMaterialLayer& other)
{
    DisneyMaterialLayer tmp(other);
    std::swap(impl, tmp.impl);
    return *this;
}

bool DisneyMaterialLayer::operator<(const DisneyMaterialLayer& other) const
{
    return impl->m_layer_number < other.impl->m_layer_number;
}

int DisneyMaterialLayer::get_layer_number() const
{
    return impl->m_layer_number;
}

bool DisneyMaterialLayer::prepare_expressions() const
{
    return
        impl->m_mask.prepare() &&
        impl->m_base_color.prepare() &&
        impl->m_subsurface.prepare() &&
        impl->m_metallic.prepare() &&
        impl->m_specular.prepare() &&
        impl->m_specular_tint.prepare() &&
        impl->m_anisotropic.prepare() &&
        impl->m_roughness.prepare() &&
        impl->m_sheen.prepare() &&
        impl->m_sheen_tint.prepare() &&
        impl->m_clearcoat.prepare() &&
        impl->m_clearcoat_gloss.prepare();
}

void DisneyMaterialLayer::evaluate_expressions(
    const ShadingPoint&     shading_point,
    OIIO::TextureSystem&    texture_system,
    Color3f&                base_color,
    DisneyBRDFInputValues&  values) const
{
    const float mask = saturate(impl->m_mask.evaluate(shading_point, texture_system)[0]);

    if (mask == 0.0f)
        return;

    base_color =
        lerp(
            base_color,
            impl->m_base_color.evaluate(shading_point, texture_system),
            mask);

    values.m_subsurface =
        lerp(
            values.m_subsurface,
            saturate(impl->m_subsurface.evaluate(shading_point, texture_system)[0]),
            mask);

    values.m_metallic =
        lerp(
            values.m_metallic,
            saturate(impl->m_metallic.evaluate(shading_point, texture_system)[0]),
            mask);

    values.m_specular =
        lerp(
            values.m_specular,
            max(impl->m_specular.evaluate(shading_point, texture_system)[0], 0.0f),
            mask);

    values.m_specular_tint =
        lerp(
            values.m_specular_tint,
            saturate(impl->m_specular_tint.evaluate(shading_point, texture_system)[0]),
            mask);

    values.m_anisotropic =
        lerp(
            values.m_anisotropic,
            clamp(impl->m_anisotropic.evaluate(shading_point, texture_system)[0], -1.0f, 1.0f),
            mask);

    values.m_roughness =
        lerp(
            values.m_roughness,
            clamp(impl->m_roughness.evaluate(shading_point, texture_system)[0], 0.001f, 1.0f),
            mask);

    values.m_sheen =
        lerp(
            values.m_sheen,
            impl->m_sheen.evaluate(shading_point, texture_system)[0],
            mask);

    values.m_sheen_tint =
        lerp(
            values.m_sheen_tint,
            saturate(impl->m_sheen_tint.evaluate(shading_point, texture_system)[0]),
            mask);

    values.m_clearcoat =
        lerp(
            values.m_clearcoat,
            impl->m_clearcoat.evaluate(shading_point, texture_system)[0],
            mask);

    values.m_clearcoat_gloss =
        lerp(
            values.m_clearcoat_gloss,
            saturate(impl->m_clearcoat_gloss.evaluate(shading_point, texture_system)[0]),
            mask);
}

DictionaryArray DisneyMaterialLayer::get_input_metadata()
{
    DictionaryArray metadata;

    metadata.push_back(
        Dictionary()
            .insert("name", "layer_name")
            .insert("label", "Layer Name")
            .insert("type", "text")
            .insert("use", "required"));

    metadata.push_back(
        Dictionary()
            .insert("name", "mask")
            .insert("label", "Mask")
            .insert("type", "colormap")
            .insert("use", "optional")
            .insert("default", "1.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "base_color")
            .insert("label", "Base Color")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary()
                    .insert("color", "Colors")
                    .insert("texture_instance", "Textures"))
            .insert("use", "required")
            .insert("default", "[0.9, 0.9, 0.9]"));

    metadata.push_back(
        Dictionary()
            .insert("name", "subsurface")
            .insert("label", "Subsurface")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "metallic")
            .insert("label", "Metallic")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "specular")
            .insert("label", "Specular")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "specular_tint")
            .insert("label", "Specular Tint")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "anisotropic")
            .insert("label", "Anisotropic")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "roughness")
            .insert("label", "Roughness")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "0.1"));

    metadata.push_back(
        Dictionary()
            .insert("name", "sheen")
            .insert("label", "Sheen")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "sheen_tint")
            .insert("label", "Sheen Tint")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "clearcoat")
            .insert("label", "Clearcoat")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "0.0"));

    metadata.push_back(
        Dictionary()
            .insert("name", "clearcoat_gloss")
            .insert("label", "Clearcoat Gloss")
            .insert("type", "colormap")
            .insert("entity_types",
                Dictionary().insert("texture_instance", "Textures"))
            .insert("use", "optional")
            .insert("default", "1.0"));

    return metadata;
}

// todo: this is actually completely generic, could be moved somewhere more central.
Dictionary DisneyMaterialLayer::get_default_values()
{
    Dictionary values;

    const DictionaryArray input_metadata = get_input_metadata();

    for (size_t i = 0; i < input_metadata.size(); ++i)
    {
        const Dictionary& im = input_metadata[i];
        const string input_name = im.get<string>("name");

        if (im.strings().exist("default"))
        {
            const string input_value = im.get<string>("default");
            values.insert(input_name, input_value);
        }
    }

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
    typedef vector<DisneyMaterialLayer> DisneyMaterialLayerContainer;

    static const size_t MaxThreadCount = 256;

    DisneyMaterialLayerContainer                m_layers;
    auto_ptr<DisneyLayeredBRDF>                 m_brdf;
    mutable TLS<DisneyMaterialLayerContainer*>  m_per_thread_layers;

    explicit Impl(const DisneyMaterial* parent)
      : m_brdf(new DisneyLayeredBRDF(parent))
      , m_per_thread_layers(MaxThreadCount)
    {
        for (size_t i = 0; i < MaxThreadCount; ++i)
            m_per_thread_layers[i] = 0;
    }

    ~Impl()
    {
        for (size_t i = 0; i < MaxThreadCount; ++i)
            assert(m_per_thread_layers[i] == 0);
    }

    void clear_per_thread_layers()
    {
        for (size_t i = 0; i < MaxThreadCount; ++i)
        {
            if (m_per_thread_layers[i])
            {
                delete m_per_thread_layers[i];
                m_per_thread_layers[i] = 0;
            }
        }

    }
};

DisneyMaterial::DisneyMaterial(
    const char*             name,
    const ParamArray&       params)
  : Material(name, params)
  , impl(new Impl(this))
{
    m_inputs.declare("alpha_map", InputFormatFloat, "");
    m_inputs.declare("displacement_map", InputFormatSpectralReflectance, "");
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

void DisneyMaterial::collect_asset_paths(StringArray& paths) const
{
    SeExprFilePathExtractor extractor;
    SeExprFilePathExtractor::PathCollection paths_;

    for (const_each<DictionaryDictionary> layer_it = m_params.dictionaries(); layer_it; ++layer_it)
    {
        const StringDictionary& layer_params = layer_it->value().strings();
        for (const_each<StringDictionary> param_it = layer_params; param_it; ++param_it)
            extractor.extract_paths(param_it->value(), paths_);
    }

    for (const_each<SeExprFilePathExtractor::PathCollection> i = paths_; i; ++i)
        paths.push_back(i->c_str());
}

void DisneyMaterial::update_asset_paths(const StringDictionary& mappings)
{
    SeExprFilePathExtractor extractor;
    SeExprFilePathExtractor::MappingCollection mappings_;

    for (const_each<StringDictionary> i = mappings; i; ++i)
        mappings_.insert(make_pair(i->key(), i->value()));

    for (each<DictionaryDictionary> layer_it = m_params.dictionaries(); layer_it; ++layer_it)
    {
        StringDictionary& layer_params = layer_it->value().strings();
        for (const_each<StringDictionary> param_it = layer_params; param_it; ++param_it)
        {
            layer_params.set(
                param_it->key(),
                extractor.replace_paths(param_it->value(), mappings_));
        }
    }
}

bool DisneyMaterial::on_frame_begin(
    const Project&          project,
    const BaseGroup*        parent,
    OnFrameBeginRecorder&   recorder,
    IAbortSwitch*           abort_switch)
{
    if (!Material::on_frame_begin(project, parent, recorder, abort_switch))
        return false;

    if (!impl->m_brdf->on_frame_begin(project, parent, recorder, abort_switch))
        return false;

    const EntityDefMessageContext context("material", this);

    m_render_data.m_bsdf = impl->m_brdf.get();
    m_render_data.m_basis_modifier = create_basis_modifier(context);

    return prepare_layers(context);
}

void DisneyMaterial::on_frame_end(
    const Project&          project,
    const BaseGroup*        parent)
{
    impl->clear_per_thread_layers();
    impl->m_layers.clear();

    Material::on_frame_end(project, parent);
}

void DisneyMaterial::add_layer(Dictionary layer_values)
{
    // Assign a name to the layer if there isn't one already.
    if (!layer_values.strings().exist("layer_name"))
    {
        const string layer_name = make_unique_name("layer", impl->m_layers);
        layer_values.insert("layer_name", layer_name);
    }

    // Assign a number to the layer if there isn't one already.
    if (!layer_values.strings().exist("layer_number"))
    {
        int layer_number = 0;
        for (const_each<Impl::DisneyMaterialLayerContainer> i = impl->m_layers; i; ++i)
            layer_number = max(layer_number, i->get_layer_number());
        layer_values.insert("layer_number", layer_number);
    }

    // Insert the layer into the material.
    const string& layer_name = layer_values.get<string>("layer_name");
    m_params.insert(layer_name, layer_values);
}

void DisneyMaterial::add_new_default_layer()
{
    add_layer(DisneyMaterialLayer::get_default_values());
}

size_t DisneyMaterial::get_layer_count() const
{
    return impl->m_layers.size();
}

const DisneyMaterialLayer& DisneyMaterial::get_layer(
    const size_t            index,
    const size_t            thread_index) const
{
    assert(index < get_layer_count());

    if (thread_index == size_t(~0))
        return impl->m_layers[index];

    assert(thread_index < Impl::MaxThreadCount);

    vector<DisneyMaterialLayer>* layers =
        impl->m_per_thread_layers[thread_index];

    if (layers == 0)
    {
        layers = new vector<DisneyMaterialLayer>(impl->m_layers);

        for (const_each<vector<DisneyMaterialLayer> > it = *layers; it; ++it)
        {
            APPLESEED_UNUSED const bool ok = it->prepare_expressions();
            assert(ok);
        }

        impl->m_per_thread_layers[thread_index] = layers;
    }

    return (*layers)[index];
}

bool DisneyMaterial::prepare_layers(const MessageContext& context)
{
    assert(impl->m_layers.empty());

    try
    {
        for (const_each<DictionaryDictionary> it = m_params.dictionaries(); it; ++it)
        {
            DisneyMaterialLayer layer(it->key(), it->value());

            if (!layer.prepare_expressions())
                return false;

            impl->m_layers.push_back(layer);
        }
    }
    catch (const Exception& e)
    {
        RENDERER_LOG_ERROR("%s: %s.", context.get(), e.what());
        return false;
    }

    sort(impl->m_layers.begin(), impl->m_layers.end());

    return true;
}


//
// DisneyMaterialFactory class implementation.
//

const char* DisneyMaterialFactory::get_model() const
{
    return Model;
}

Dictionary DisneyMaterialFactory::get_model_metadata() const
{
    return
        Dictionary()
            .insert("name", Model)
            .insert("label", "Disney Material")
            .insert("default_model", "true");
}

DictionaryArray DisneyMaterialFactory::get_input_metadata() const
{
    DictionaryArray metadata;

    add_surface_shader_metadata(metadata);

    metadata.push_back(
        Dictionary()
            .insert("name", "edf")
            .insert("label", "EDF")
            .insert("type", "entity")
            .insert("entity_types", Dictionary().insert("edf", "EDF"))
            .insert("use", "optional"));

    add_alpha_map_metadata(metadata);
    add_displacement_metadata(metadata);

    return metadata;
}

auto_release_ptr<Material> DisneyMaterialFactory::create(
    const char*             name,
    const ParamArray&       params) const
{
    return auto_release_ptr<Material>(new DisneyMaterial(name, params));
}

auto_release_ptr<Material> DisneyMaterialFactory::static_create(
    const char*             name,
    const ParamArray&       params)
{
    return auto_release_ptr<Material>(new DisneyMaterial(name, params));
}

}   // namespace renderer
