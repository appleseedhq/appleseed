
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2015 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/image/colorspace.h"
#include "foundation/math/scalar.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/tls.h"

// SeExpr headers.
#pragma warning (push)
#pragma warning (disable : 4267)    // conversion from 'size_t' to 'int', possible loss of data
#include "SeExpression.h"
#include "SeExprFunc.h"
#include "SeExprNode.h"
#pragma warning (pop)

// Boost headers.
#include "boost/algorithm/string.hpp"
#include "boost/algorithm/string/split.hpp"
#include "boost/ptr_container/ptr_vector.hpp"

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

//
// DisneyParamExpression class implementation.
//

namespace
{
    bool texture_is_srgb(const OIIO::ustring& filename)
    {
        if (filename.rfind(".exr") == filename.length() - 4)
            return false;

        return true;
    }

    class TextureSeExprFunc
      : public SeExprFuncX
    {
      public:
        TextureSeExprFunc()
          : SeExprFuncX(true)   // true = thread-safe
          , m_texture_system(0)
          , m_texture_is_srgb(true)
        {
#if OIIO_VERSION <= 10504
            m_texture_options.nchannels = 3;
#endif
            m_texture_options.swrap = OIIO::TextureOpt::WrapPeriodic;
            m_texture_options.twrap = OIIO::TextureOpt::WrapPeriodic;
        }

        void set_texture_system(OIIO::TextureSystem* texture_system)
        {
            m_texture_system = texture_system;
        }

        virtual bool prep(SeExprFuncNode* node, bool /*wantVec*/) APPLESEED_OVERRIDE
        {
            if (node->nargs() != 3)
            {
                node->addError("3 arguments expected.");
                return false;
            }

            if (!node->isStrArg(0))
            {
                node->addError("First argument must be a texture file path.");
                return false;
            }

            if (node->getStrArg(0).empty())
            {
                node->addError("Path to texture file is empty.");
                return false;
            }

            if (!node->child(1)->prep(0) || !node->child(2)->prep(0))
                return false;

            m_texture_filename = OIIO::ustring(node->getStrArg(0), 0);
            m_texture_is_srgb = texture_is_srgb(m_texture_filename);

            return true;
        }

        virtual void eval(const SeExprFuncNode* node, SeVec3d& result) const APPLESEED_OVERRIDE
        {
            SeVec3d u, v;
            node->child(1)->eval(u);
            node->child(2)->eval(v);

            Color3f color;
            if (!m_texture_system->texture(
                    m_texture_filename,
                    m_texture_options,
                    static_cast<float>(u[0]),
                    static_cast<float>(1.0 - v[0]),
                    0.0f,
                    0.0f,
                    0.0f,
                    0.0f,
#if OIIO_VERSION > 10504
                    3,
#endif
                    &color[0]))
            {
                // Failed to find or open the texture.
                // todo: issue an error message (once).
                result = SeVec3d(1.0, 0.0, 1.0);
                return;
            }

            // Colors in SeExpr are always in the sRGB color space.
            if (!m_texture_is_srgb)
                color = linear_rgb_to_srgb(color);

            result = SeVec3d(color[0], color[1], color[2]);
        }

      private:
        OIIO::TextureSystem*        m_texture_system;
        OIIO::ustring               m_texture_filename;
        mutable OIIO::TextureOpt    m_texture_options;
        bool                        m_texture_is_srgb;
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

            virtual void eval(const SeExprVarNode* /*node*/, SeVec3d& result) APPLESEED_OVERRIDE
            {
                result[0] = m_val;
            }
        };

        SeAppleseedExpr()
        {
        }

        SeAppleseedExpr(const string& expr)
          : SeExpression(expr)
        {
            m_vars["u"] = Var(0.0);
            m_vars["v"] = Var(0.0);
        }

        void set_expr(const string& expr)
        {
            SeExpression::setExpr(expr);
            m_vars["u"] = Var(0.0);
            m_vars["v"] = Var(0.0);
        }

        // Called during preparation.
        SeExprVarRef* resolveVar(const string& name) const APPLESEED_OVERRIDE
        {
            const map<string, Var>::iterator i = m_vars.find(name);
            return i != m_vars.end() ? &i->second : 0;
        }

        // Called during preparation.
        SeExprFunc* resolveFunc(const string& name) const APPLESEED_OVERRIDE
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
            for (each<ptr_vector<TextureSeExprFunc> > i = m_functions_x; i; ++i)
                i->set_texture_system(&texture_system);

            const Vector2d& uv = shading_point.get_uv(0);
            m_vars["u"] = Var(uv[0]);
            m_vars["v"] = Var(uv[1]);

            const SeVec3d result = evaluate();

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

struct DisneyParamExpression::Impl
{
    SeAppleseedExpr m_expr;

    explicit Impl(const char* expr)
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
// DisneyMaterialLayer class implementation.
//

namespace
{
    class DisneyLayerParam
    {
      public:
        DisneyLayerParam(
            const char*             name,
            const Dictionary&       params,
            const bool              is_vector)
          : m_param_name(name)
          , m_expr(params.get<string>(m_param_name))
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

        void swap(DisneyLayerParam& other)
        {
            std::swap(m_param_name, other.m_param_name);
            std::swap(m_expr, other.m_expr);
            std::swap(m_is_vector, other.m_is_vector);
            std::swap(m_is_constant, other.m_is_constant);
            std::swap(m_constant_value, other.m_constant_value);
            std::swap(m_texture_filename, other.m_texture_filename);
            std::swap(m_texture_options, other.m_texture_options);
            std::swap(m_texture_is_srgb, other.m_texture_is_srgb);
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

            // Case of a simple constant.
            m_is_constant = m_expression.isConstant();
            if (m_is_constant)
            {
                const SeVec3d result = m_expression.evaluate();
                m_constant_value = Color3d(result[0], result[1], result[2]);
                return true;
            }

            // Case of a simple texture lookup.
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
#if OIIO_VERSION <= 10504
                m_texture_options.nchannels = 3;
#endif
            }

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
                const Vector2d& uv = shading_point.get_uv(0);

                Color3f color;
                if (!texture_system.texture(
                        m_texture_filename,
                        m_texture_options,
                        static_cast<float>(uv[0]),
                        static_cast<float>(1.0 - uv[1]),
                        0.0f,
                        0.0f,
                        0.0f,
                        0.0f,
#if OIIO_VERSION > 10504
                        3,
#endif
                        &color[0]))
                {
                    // Failed to find or open the texture.
                    // todo: issue an error message (once).
                    return Color3d(1.0, 0.0, 1.0);
                }

                // Colors in SeExpr are always in the sRGB color space.
                if (!m_texture_is_srgb)
                    color = linear_rgb_to_srgb(color);

                return Color3d(color);
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
    Color3d&                base_color,
    DisneyBRDFInputValues&  values) const
{
    const double mask = saturate(impl->m_mask.evaluate(shading_point, texture_system)[0]);

    if (mask == 0.0)
        return;

    base_color = lerp(base_color, impl->m_base_color.evaluate(shading_point, texture_system), mask);

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
            max(impl->m_specular.evaluate(shading_point, texture_system)[0], 0.0),
            mask);

    values.m_specular_tint =
        lerp(
            values.m_specular_tint,
            saturate(impl->m_specular_tint.evaluate(shading_point, texture_system)[0]),
            mask);

    values.m_anisotropic =
        lerp(
            values.m_anisotropic,
            saturate(impl->m_anisotropic.evaluate(shading_point, texture_system)[0]),
            mask);

    values.m_roughness =
        lerp(
            values.m_roughness,
            clamp(impl->m_roughness.evaluate(shading_point, texture_system)[0], 0.001, 1.0),
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
            .insert("default", "0.5"));

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
            .insert("default", "0.5"));

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
    m_inputs.declare("alpha_map", InputFormatScalar, "");
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

bool DisneyMaterial::on_frame_begin(
    const Project&          project,
    const Assembly&         assembly,
    IAbortSwitch*           abort_switch)
{
    if (!Material::on_frame_begin(project, assembly, abort_switch))
        return false;

    const EntityDefMessageContext context("material", this);

    m_bsdf = impl->m_brdf.get();

    if (!create_basis_modifier(context))
        return false;

    if (!prepare_layers(context))
        return false;

    return true;
}

void DisneyMaterial::on_frame_end(
    const Project&          project,
    const Assembly&         assembly)
{
    impl->clear_per_thread_layers();
    impl->m_layers.clear();

    Material::on_frame_end(project, assembly);
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

    if (layers)
        return (*layers)[index];

    layers = new vector<DisneyMaterialLayer>(impl->m_layers);

    for (const_each<vector<DisneyMaterialLayer> > it = *layers; it; ++it)
    {
        const bool ok = it->prepare_expressions();
        assert(ok);
    }

    impl->m_per_thread_layers[thread_index] = layers;

    return (*layers)[index];
}

Dictionary DisneyMaterial::get_new_layer_values() const
{
    int layer_number = 0;

    for (const_each<Impl::DisneyMaterialLayerContainer> i = impl->m_layers; i; ++i)
        layer_number = max(layer_number, i->get_layer_number());

    return
        DisneyMaterialLayer::get_default_values()
            .insert("layer_name", make_unique_name("layer", impl->m_layers))
            .insert("layer_number", layer_number);
}

bool DisneyMaterial::prepare_layers(const MessageContext& context)
{
    assert(impl->m_layers.empty());

    try
    {
        for (const_each<DictionaryDictionary> it = m_params.dictionaries(); it; ++it)
        {
            DisneyMaterialLayer layer(it->name(), it->value());

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

    add_common_input_metadata(metadata);

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
