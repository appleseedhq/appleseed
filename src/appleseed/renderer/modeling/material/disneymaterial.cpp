
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

// Boost headers.
#include "boost/thread/locks.hpp"
#include "boost/thread/mutex.hpp"

// Standard headers.
#include <algorithm>
#include <iostream>
#include <memory>
#include <vector>

using namespace foundation;
using namespace boost;
using namespace std;

namespace renderer
{

class SeExpr : public SeExpression
{
  public:
    struct Var : public SeExprScalarVarRef
    {
        Var() {}

        explicit Var(const double val)
          : m_val(val)
        {
        }

        virtual void eval(const SeExprVarNode* /*node*/,SeVec3d& result) OVERRIDE
        {
            result[0] = m_val;
        }
        
        double m_val;            
    };
    
    SeExpr() : SeExpression()
    {
    }

    SeExpr(const std::string& expr) : SeExpression(expr)
    {
        m_vars["u"] = Var(0.0);
        m_vars["v"] = Var(0.0);
    }

    void setExpr(const std::string& e)
    {
        SeExpression::setExpr(e);
        m_vars["u"] = Var(0.0);
        m_vars["v"] = Var(0.0);
    }

    SeExprVarRef* resolveVar(const std::string& name) const OVERRIDE
    {
        map<string,Var>::iterator i = m_vars.find(name);
    
        if (i != m_vars.end())
            return &i->second;
    
        return 0;
    }

    mutable std::map<std::string,Var> m_vars;
};

//
// DisneyParamExpression class implementation.
//

struct DisneyParamExpression::Impl : NonCopyable
{
    explicit Impl(const char *expr)
      : m_expr(expr)
    {
    }
    
    SeExpr m_expr;
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
        m_expression.setExpr(m_expr);
        m_expression.m_vars["u"] = SeExpr::Var(0.0);
        m_expression.m_vars["v"] = SeExpr::Var(0.0);
        
        if (!m_expression.isValid())
        {
            std::cout << "SeExpr Error: " << m_expression.parseError() << std::endl;
            return false;
        }

        m_is_constant = m_expression.isConstant();
        
        if (m_is_constant)
        {
            SeVec3d result = m_expression.evaluate();
            m_constant_value = Color3d(result[0], result[1], result[2]);
        }

        // Check for texture lookup here...

        return true;
    }
    
    Color3d evaluate(const ShadingPoint& shading_point) const
    {
        if (m_is_constant)
            return m_constant_value;

        lock_guard<mutex> lock(m_mutex);

        m_expression.m_vars["u"] = SeExpr::Var(shading_point.get_uv(0)[0]);
        m_expression.m_vars["v"] = SeExpr::Var(shading_point.get_uv(0)[1]);
        SeVec3d result = m_expression.evaluate();
        return Color3d(result[0], result[1], result[2]);       
    }

  private:
    const char*     m_param_name;
    string          m_expr;
    bool            m_is_vector;

    bool            m_is_constant;
    Color3d         m_constant_value;
    
    mutable SeExpr  m_expression;
    // TODO: this is horrible. Remove it ASAP.
    mutable mutex   m_mutex;
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
    Color3d&                base_color,        
    DisneyBRDFInputValues&  values) const
{
    const double mask = clamp(impl->m_mask.evaluate(shading_point)[0], 0.0, 1.0);

    if (mask == 0)
        return;
    
    base_color = mix(base_color, impl->m_base_color.evaluate(shading_point), mask);
    
    values.m_subsurface = mix(
        values.m_subsurface,
        clamp(impl->m_subsurface.evaluate(shading_point)[0], 0.0, 1.0),
        mask);

    values.m_metallic = mix(
        values.m_metallic,
        clamp(impl->m_metallic.evaluate(shading_point)[0], 0.0, 1.0),
        mask);
    
    values.m_specular = mix(
        values.m_specular, 
        max(impl->m_specular.evaluate(shading_point)[0], 0.0),
        mask);
    
    values.m_specular_tint = mix(
        values.m_specular_tint, 
        clamp(impl->m_specular_tint.evaluate(shading_point)[0], 0.0, 1.0),
        mask);
    
    values.m_anisotropic = mix(
        values.m_anisotropic, 
        clamp(impl->m_anisotropic.evaluate(shading_point)[0], 0.0, 1.0),
        mask);
    
    values.m_roughness = mix(
        values.m_roughness,
        clamp(impl->m_roughness.evaluate(shading_point)[0], 0.001, 1.0),
        mask);

    values.m_sheen = mix(
        values.m_sheen, 
        impl->m_sheen.evaluate(shading_point)[0],
        mask);

    values.m_sheen_tint = mix(
        values.m_sheen_tint, 
        clamp(impl->m_sheen_tint.evaluate(shading_point)[0], 0.0, 1.0),
        mask);
    
    values.m_clearcoat = mix(
        values.m_clearcoat, 
        impl->m_clearcoat.evaluate(shading_point)[0],
        mask);
    
    values.m_clearcoat_gloss = mix(
        values.m_clearcoat_gloss, 
        clamp(impl->m_clearcoat_gloss.evaluate(shading_point)[0], 0.0, 1.0),
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
    explicit Impl(const DisneyMaterial* parent)
      : m_brdf(new DisneyLayeredBRDF(parent))
    {
    }

    vector<DisneyMaterialLayer> m_layers;
    auto_ptr<DisneyLayeredBRDF> m_brdf;
};

DisneyMaterial::DisneyMaterial(
  const char*         name,
  const ParamArray&   params)
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
        if (!it->prepare_expressions())
            return false;
    }

    m_bsdf = impl->m_brdf.get();
    return true;
}

void DisneyMaterial::on_frame_end(
    const Project&  project,
    const Assembly& assembly) OVERRIDE
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
    const char*         name,
    const ParamArray&   params) const
{
    return
        auto_release_ptr<Material>(
            new DisneyMaterial(name, params));
}

}   // namespace renderer
