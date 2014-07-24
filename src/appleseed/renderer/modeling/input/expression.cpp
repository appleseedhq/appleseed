
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2010-2013 Francois Beaune, Jupiter Jazz Limited
// Copyright (c) 2014 Francois Beaune, The appleseedhq Organization
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
#include "expression.h"

// Boost headers.
#include "boost/thread/tss.hpp"

// Standard headers.
#include <algorithm>
#include <string>

using namespace boost;
using namespace foundation;
using namespace std;

namespace renderer
{

//
// Expression class implementation.
//

class Expression::Impl
{
  public:
    
    class SeExpr : public SeExpression
    {
      public:
        SeExpr() : SeExpression()
        {
        }

        SeExpr( const string& expr, bool is_vector)
          : SeExpression(expr, is_vector)
        {
        }
        
      private:
    };
    
    Impl()
      : m_is_vector(false)
    {
    }

    explicit Impl(const char* expr, bool is_vector = true)
    {
        set_expression(expr, is_vector);
    }

    void set_expression(const char* expr, bool is_vector = true)
    {
        m_expr = expr;
        m_is_vector = is_vector;        
    }
    
    bool syntax_ok() const
    {
        SeExpr x(m_expr, m_is_vector);
        return x.syntaxOK();
    }

    string                              m_expr;
    bool                                m_is_vector;
    mutable thread_specific_ptr<SeExpr> m_seexprs;
    
    SeExpr& get_expression() const
    {
        SeExpr* e = m_seexprs.get();
        
        if (!e)
        {
            e = new SeExpr(m_expr, m_is_vector);
            m_seexprs.reset(e);
        }

        return *e;
    }
};

Expression::Expression()
  : impl(new Impl())
{
}

Expression::Expression(const char* expr, bool is_vector)
  : impl(new Impl(expr, is_vector))
{
}

Expression::~Expression()
{
    delete impl;
}

Expression::Expression(const Expression& other)
  : impl(new Impl(other.impl->m_expr.c_str(), other.impl->m_is_vector))
{
}

Expression& Expression::operator=(const Expression& other)
{
    Expression tmp(other);
    swap(tmp);
    return *this;
}

void Expression::swap(Expression& other)
{
    std::swap(impl, other.impl);
}

void Expression::set_expression(const char* expr, bool is_vector)
{
    impl->set_expression(expr, is_vector);
}

bool Expression::syntax_ok() const
{
    return impl->syntax_ok();
}

Color3d Expression::evaluate(const ShadingPoint& shading_point) const
{
    return Color3d(0.0);
}

}   // namespace renderer
