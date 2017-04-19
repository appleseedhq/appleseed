
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2016-2017 Francois Beaune, The appleseedhq Organization
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

#ifndef APPLESEED_RENDERER_UTILITY_SEEXPR_H
#define APPLESEED_RENDERER_UTILITY_SEEXPR_H

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"
#include "renderer/kernel/shading/shadingpoint.h"

// appleseed.foundation headers.
#include "foundation/image/color.h"
#include "foundation/image/colorspace.h"
#include "foundation/math/vector.h"
#include "foundation/platform/compiler.h"
#include "foundation/utility/foreach.h"
#include "foundation/utility/string.h"

// SeExpr headers.
#pragma warning (push)
#pragma warning (disable : 4267)    // conversion from 'size_t' to 'int', possible loss of data
#include "SeExpression.h"
#include "SeExprFunc.h"
#include "SeExprNode.h"
#pragma warning (pop)

// OpenImageIO headers.
#include "foundation/platform/oiioheaderguards.h"
BEGIN_OIIO_INCLUDES
#include "OpenImageIO/texture.h"
#include "OpenImageIO/ustring.h"
END_OIIO_INCLUDES

// Boost headers.
#include "boost/ptr_container/ptr_vector.hpp"

// Standard headers.
#include <cassert>
#include <string>

namespace renderer
{

inline bool texture_is_srgb(const OIIO::ustring& filename)
{
   return filename.rfind(".exr") != filename.length() - 4;
}


//
// TextureSeExprFunc class.
//

class TextureSeExprFunc
  : public SeExprFuncX
{
  public:
    TextureSeExprFunc()
      : SeExprFuncX(true)   // true = thread-safe
      , m_texture_system(0)
      , m_texture_is_srgb(true)
    {
        m_texture_options.swrap = OIIO::TextureOpt::WrapPeriodic;
        m_texture_options.twrap = OIIO::TextureOpt::WrapPeriodic;
    }

    void set_texture_system(OIIO::TextureSystem* texture_system)
    {
        m_texture_system = texture_system;
    }

    virtual bool prep(
        SeExprFuncNode*         node,
        bool                    /*wantVec*/) APPLESEED_OVERRIDE
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

    virtual void eval(
        const SeExprFuncNode*   node,
        SeVec3d&                result) const APPLESEED_OVERRIDE
    {
        SeVec3d u, v;
        node->child(1)->eval(u);
        node->child(2)->eval(v);

        foundation::Color3f color;
        if (!m_texture_system->texture(
                m_texture_filename,
                m_texture_options,
                static_cast<float>(u[0]),
                static_cast<float>(v[0]),
                0.0f,
                0.0f,
                0.0f,
                0.0f,
                3,
                &color[0]))
        {
            // Failed to find or open the texture.
            const std::string message = m_texture_system->geterror();
            if (!message.empty())
            {
                const std::string modified_message =
                    foundation::prefix_all_lines(foundation::trim_both(message), "oiio: ");
                RENDERER_LOG_ERROR("%s", modified_message.c_str());
            }
            result = SeVec3d(1.0, 0.0, 1.0);
            return;
        }

        // Colors in SeExpr are always in the sRGB color space.
        if (!m_texture_is_srgb)
            color = foundation::linear_rgb_to_srgb(color);

        result = SeVec3d(color[0], color[1], color[2]);
    }

  private:
    OIIO::TextureSystem*        m_texture_system;
    OIIO::ustring               m_texture_filename;
    mutable OIIO::TextureOpt    m_texture_options;
    bool                        m_texture_is_srgb;
};


//
// SeAppleseedExpr class.
//

class SeAppleseedExpr
  : public SeExpression
{
  public:
    SeAppleseedExpr()
    {
    }

    SeAppleseedExpr(const std::string& expr)
      : SeExpression(expr)
    {
        reset_vars();
    }

    void set_expr(const std::string& expr)
    {
        SeExpression::setExpr(expr);
        reset_vars();
    }

    // Called during preparation.
    SeExprVarRef* resolveVar(const std::string& name) const APPLESEED_OVERRIDE
    {
        assert(name.length() >= 1);

        if (name[0] == 'u')
            return &m_u_var;
        else if (name[0] == 'v')
            return &m_v_var;
        else return SeExpression::resolveVar(name);
    }

    // Called during preparation.
    SeExprFunc* resolveFunc(const std::string& name) const APPLESEED_OVERRIDE
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

    foundation::Color3d update_and_evaluate(
        const ShadingPoint&     shading_point,
        OIIO::TextureSystem&    texture_system)
    {
        for (foundation::each<boost::ptr_vector<TextureSeExprFunc> > i = m_functions_x; i; ++i)
            i->set_texture_system(&texture_system);

        const foundation::Vector2f& uv = shading_point.get_uv(0);
        m_u_var.m_val = uv[0];
        m_v_var.m_val = uv[1];

        const SeVec3d result = evaluate();
        return foundation::Color3d(result[0], result[1], result[2]);
    }

  private:
    void reset_vars() const
    {
        m_u_var.m_val = 0.0;
        m_v_var.m_val = 0.0;
    }

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

    mutable Var                                     m_u_var;
    mutable Var                                     m_v_var;
    mutable boost::ptr_vector<TextureSeExprFunc>    m_functions_x;
    mutable boost::ptr_vector<SeExprFunc>           m_functions;
};

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_UTILITY_SEEXPR_H
