
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

// appleseed.renderer headers.

// appleseed.foundation headers.

// Standard headers.
#include <algorithm>
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// Expression class implementation.
//

struct Expression::Impl : public SeExpression
{
    Impl() : SeExpression()
    {
    }

    explicit Impl(const string& expr, bool is_vector = true)
      : SeExpression(expr, is_vector)
    {
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
  : impl(new Impl(other.impl->getExpr(), other.impl->wantVec()))
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
    impl->setExpr(expr);
    impl->setWantVec(is_vector);
}

bool Expression::syntax_ok() const
{
    return impl->syntaxOK();
}

}   // namespace renderer
