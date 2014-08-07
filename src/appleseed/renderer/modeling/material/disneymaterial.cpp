
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
#include "renderer/kernel/shading/shadingpoint.h"
#include "renderer/modeling/bsdf/disneybrdf.h"
#include "renderer/modeling/bsdf/disneylayeredbrdf.h"

// appleseed.foundation headers.
#include "foundation/math/scalar.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"

// SeExpr headers
#include "SeExpression.h"
#include "SeExprFunc.h"
#include "SeExprNode.h"

// Boost headers.
#include "boost/algorithm/string.hpp"
#include "boost/algorithm/string/split.hpp"
#include "boost/ptr_container/ptr_vector.hpp"
#include "boost/thread/locks.hpp"
#include "boost/thread/mutex.hpp"

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
    class TextureSeExprFunc
      : public SeExprFuncX
    {
      public:
        TextureSeExprFunc()
          : SeExprFuncX(true)
          , m_texture_system(0)
        {
            m_texture_options.nchannels = 3;
        }

        void set_texture_system(OIIO::TextureSystem*  texture_system)
        {
            m_texture_system = texture_system;
        }

        virtual bool prep(SeExprFuncNode* node, bool /*wantVec*/) OVERRIDE
        {
            if (node->nargs() != 3)
            {
                node->addError("3 arguments expected.");
                return false;
            }

            if (!node->isStrArg(0))
            {
                node->addError("First argument must be a texture filename.");
                return false;
            }

            if (node->getStrArg(0).empty())
            {
                node->addError("Filepath to texture is empty.");
                return false;
            }

            if (!node->child(1)->prep(0) || !node->child(2)->prep(0))
                return false;

            m_texture_filename = OIIO::ustring(node->getStrArg(0), 0);

            return true;
        }

        virtual void eval(const SeExprFuncNode* node, SeVec3d& result) const OVERRIDE
        {
            SeVec3d u, v;
            node->child(1)->eval(u);
            node->child(2)->eval(v);

            float color[3];
            m_texture_system->texture(
                m_texture_filename,
                m_texture_options,
                static_cast<float>(u[0]),
                static_cast<float>(v[0]),
                0.0f,
                0.0f,
                0.0f,
                0.0f,
                color);

            result = SeVec3d(color[0], color[1], color[2]);
        }

      private:
        OIIO::TextureSystem*        m_texture_system;
        OIIO::ustring               m_texture_filename;
        mutable OIIO::TextureOpt    m_texture_options;
    };

    class SeAppleseedExpr
      : public SeExpression
    {
      public:
        struct Var
          : public SeExprScalarVarRef
        {
            double m_val;

            Var() {}

            explicit Var(const double val)
              : m_val(val)
            {
            }

            virtual void eval(const SeExprVarNode* /*node*/, SeVec3d& result) OVERRIDE
            {
                result[0] = m_val;
            }
        };

        SeAppleseedExpr()
          : SeExpression()
        {
        }

        SeAppleseedExpr(const string& expr)
          : SeExpression(expr)
        {
            m_vars["u"] = Var(0.0);
            m_vars["v"] = Var(0.0);
        }

        void set_expr(const string& e)
        {
            SeExpression::setExpr(e);
            m_vars["u"] = Var(0.0);
            m_vars["v"] = Var(0.0);
        }

        bool prep_texture_expr(OIIO::ustring& filename)
        {
            filename = "";
            string expression = trim_both(getExpr(), " \r\n");
            vector<string> tokens;
            tokenize(expression, "()", tokens);
            if (tokens.size() != 2)
                return false;
            if (trim_both(tokens[0]) != "texture")
                return false;
            string inner_content = tokens[1];
            tokens.clear();
            tokenize(inner_content, " ,", tokens);
            if (tokens.size() != 3)
                return false;
            if (trim_both(tokens[1]) != "$u" && trim_both(tokens[2]) != "$v")
                return false;
            filename = OIIO::ustring(trim_both(tokens[0], " \""), 0);
            return true;
        }

        SeExprVarRef* resolveVar(const string& name) const OVERRIDE
        {
            const map<string, Var>::iterator i = m_vars.find(name);

            if (i != m_vars.end())
                return &i->second;

            return 0;
        }

        SeExprFunc* resolveFunc(const string& name) const OVERRIDE
        {
            if (name == "texture")
            {
                TextureSeExprFunc* texture_function_x = new TextureSeExprFunc();
                SeExprFunc* texture_function = new SeExprFunc(*texture_function_x, 3, 3);
                m_functions_x.push_back(texture_function_x);
                m_functions.push_back(texture_function);
                return texture_function;
            }
            return SeExpression::resolveFunc(name);
        }

        Color3d update_and_evaluate(
            const ShadingPoint&     shading_point,
            OIIO::TextureSystem&    texture_system)
        {
            for (each<ptr_vector<TextureSeExprFunc> > it = m_functions_x; it; ++it)
            {
                it->set_texture_system(&texture_system);
            }
            m_vars["u"] = SeAppleseedExpr::Var(shading_point.get_uv(0)[0]);
            m_vars["v"] = SeAppleseedExpr::Var(shading_point.get_uv(0)[1]);

            SeVec3d result = evaluate();
            return Color3d(result[0], result[1], result[2]);
        }

      private:
        mutable map<string, Var>                m_vars;
        mutable ptr_vector<TextureSeExprFunc>   m_functions_x;
        mutable ptr_vector<SeExprFunc>          m_functions;
    };

    void report_expression_error(
        const char*             message1,
        const char*             message2,
        const SeAppleseedExpr&  expr)
    {
        if (message2)
            RENDERER_LOG_ERROR("%s%s", message1, message2);
        else
            RENDERER_LOG_ERROR("%s:", message1);

        const string error = expr.parseError();

        vector<string> errors;
        split(errors, error, is_any_of("\n"));

        for (const_each<vector<string> > e = errors; e; ++e)
        {
            if (!e->empty())
                RENDERER_LOG_ERROR("%s", e->c_str());
        }
    }
}


//
// DisneyParamExpression class implementation.
//

struct DisneyParamExpression::Impl
  : public NonCopyable
{
    SeAppleseedExpr m_expr;

    explicit Impl(const char *expr)
      : m_expr(expr)
    {
    }
};

DisneyParamExpression::DisneyParamExpression(const char* expr)
  : impl(new Impl(expr))
{
}

DisneyParamExpression::~DisneyParamExpression()
{
    delete impl;
}

bool DisneyParamExpression::is_valid() const
{
    return impl->m_expr.isValid();
}

const char* DisneyParamExpression::parse_error() const
{
    return impl->m_expr.parseError().c_str();
}

void DisneyParamExpression::report_error(const char* message) const
{
    report_expression_error(message, 0, impl->m_expr);
}

bool DisneyParamExpression::is_constant() const
{
    return impl->m_expr.isConstant();
}


//
// DisneyLayerParam class implementation.
//

class DisneyLayerParam
{
  public:
    DisneyLayerParam(
        const char*         name,
        const Dictionary&   params,
        const bool          is_vector)
      : m_param_name(name)
      , m_expr(params.get<string>(m_param_name))
      , m_is_vector(is_vector)
      , m_is_constant(false)
    {
    }

    DisneyLayerParam(const DisneyLayerParam& other)
      : m_param_name(other.m_param_name)
      , m_expr(other.m_expr)
      , m_is_vector(other.m_is_vector)
      , m_is_constant(false)
    {
    }

    void swap(DisneyLayerParam& other)
    {
        std::swap(m_param_name, other.m_param_name);
        std::swap(m_expr, other.m_expr);
        std::swap(m_is_vector, other.m_is_vector);
    }

    DisneyLayerParam& operator=(const DisneyLayerParam& other)
    {
        DisneyLayerParam tmp(other);
        swap(tmp);
        return *this;
    }

    bool prepare()
    {
        m_expression.setWantVec(m_is_vector);
        m_expression.set_expr(m_expr);

        if (!m_expression.isValid())
        {
            report_expression_error("Expression error: ", m_param_name, m_expression);
            return false;
        }

        m_is_constant = m_expression.isConstant();

        if (m_is_constant)
        {
            SeVec3d result = m_expression.evaluate();
            m_constant_value = Color3d(result[0], result[1], result[2]);
        }

        m_expression.prep_texture_expr(m_texture_filename);
        m_texture_options.nchannels = 3;

        return true;
    }

    Color3d evaluate(
        const ShadingPoint&     shading_point,
        OIIO::TextureSystem&    texture_system) const
    {
        if (m_is_constant)
            return m_constant_value;

        if (!m_texture_filename.empty())
        {
            float val[3];
            texture_system.texture(
                m_texture_filename,
                m_texture_options,
                static_cast<float>(shading_point.get_uv(0)[0]),
                static_cast<float>(shading_point.get_uv(0)[1]),
                0.0f,
                0.0f,
                0.0f,
                0.0f,
                val);

            return Color3d(val[0], val[1], val[2]);
        }

        lock_guard<mutex> lock(m_mutex);

        return m_expression.update_and_evaluate(shading_point, texture_system);
    }

  private:
    const char*                 m_param_name;
    string                      m_expr;
    bool                        m_is_vector;

    bool                        m_is_constant;
    Color3d                     m_constant_value;

    mutable OIIO::TextureOpt    m_texture_options;
    OIIO::ustring               m_texture_filename;
    mutable SeAppleseedExpr     m_expression;

    // TODO: this is horrible. Remove it ASAP.
    mutable mutex               m_mutex;
};


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

    const string        m_name;
    const size_t        m_layer_number;
    DisneyLayerParam    m_mask;
    DisneyLayerParam    m_base_color;
    DisneyLayerParam    m_subsurface;
    DisneyLayerParam    m_metallic;
    DisneyLayerParam    m_specular;
    DisneyLayerParam    m_specular_tint;
    DisneyLayerParam    m_anisotropic;
    DisneyLayerParam    m_roughness;
    DisneyLayerParam    m_sheen;
    DisneyLayerParam    m_sheen_tint;
    DisneyLayerParam    m_clearcoat;
    DisneyLayerParam    m_clearcoat_gloss;
};

DisneyMaterialLayer::DisneyMaterialLayer(
    const char*             name,
    const Dictionary&       params)
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

bool DisneyMaterialLayer::prepare_expressions() const
{
    return  impl->m_mask.prepare() &&
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
    Color3d&                base_color,
    DisneyBRDFInputValues&  values) const
{
    const double mask = saturate(impl->m_mask.evaluate(shading_point, texture_system)[0]);

    if (mask == 0.0)
        return;

    base_color = lerp(base_color, impl->m_base_color.evaluate(shading_point, texture_system), mask);

    values.m_subsurface = lerp(
        values.m_subsurface,
        saturate(impl->m_subsurface.evaluate(shading_point, texture_system)[0]),
        mask);

    values.m_metallic = lerp(
        values.m_metallic,
        saturate(impl->m_metallic.evaluate(shading_point, texture_system)[0]),
        mask);

    values.m_specular = lerp(
        values.m_specular,
        max(impl->m_specular.evaluate(shading_point, texture_system)[0], 0.0),
        mask);

    values.m_specular_tint = lerp(
        values.m_specular_tint,
        saturate(impl->m_specular_tint.evaluate(shading_point, texture_system)[0]),
        mask);

    values.m_anisotropic = lerp(
        values.m_anisotropic,
        saturate(impl->m_anisotropic.evaluate(shading_point, texture_system)[0]),
        mask);

    values.m_roughness = lerp(
        values.m_roughness,
        clamp(impl->m_roughness.evaluate(shading_point, texture_system)[0], 0.001, 1.0),
        mask);

    values.m_sheen = lerp(
        values.m_sheen,
        impl->m_sheen.evaluate(shading_point, texture_system)[0],
        mask);

    values.m_sheen_tint = lerp(
        values.m_sheen_tint,
        saturate(impl->m_sheen_tint.evaluate(shading_point, texture_system)[0]),
        mask);

    values.m_clearcoat = lerp(
        values.m_clearcoat,
        impl->m_clearcoat.evaluate(shading_point, texture_system)[0],
        mask);

    values.m_clearcoat_gloss = lerp(
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
            .insert("label", "Layer name")
            .insert("type", "text")
            .insert("default", "layer1"));

    metadata.push_back(
        Dictionary()
            .insert("name", "mask")
            .insert("label", "Mask")
            .insert("type", "colormap")
            .insert("default", "1"));

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
    Dictionary layer_params;

    DictionaryArray metadata = get_input_metadata();

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
    explicit Impl(const DisneyMaterial* parent)
      : m_brdf(new DisneyLayeredBRDF(parent))
    {
    }

    vector<DisneyMaterialLayer> m_layers;
    auto_ptr<DisneyLayeredBRDF> m_brdf;
};

DisneyMaterial::DisneyMaterial(
    const char*             name,
    const ParamArray&       params)
  : Material(name, params)
  , impl(new Impl(this))
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
    const Project&          project,
    const Assembly&         assembly,
    AbortSwitch*            abort_switch)
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
    catch (...)
    {
        return false;
    }

    sort(impl->m_layers.begin(), impl->m_layers.end());

    for (const_each<vector<DisneyMaterialLayer> > it = impl->m_layers; it ; ++it)
    {
        if (!it->prepare_expressions())
            return false;
    }

    m_bsdf = impl->m_brdf.get();
    return true;
}

void DisneyMaterial::on_frame_end(
    const Project&          project,
    const Assembly&         assembly) OVERRIDE
{
    impl->m_layers.clear();
    Material::on_frame_end(project, assembly);
}

size_t DisneyMaterial::get_layer_count() const
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
    const char*             name,
    const ParamArray&       params) const
{
    return
        auto_release_ptr<Material>(
            new DisneyMaterial(name, params));
}

}   // namespace renderer
