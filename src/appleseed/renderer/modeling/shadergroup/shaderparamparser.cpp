

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
#include "shaderparamparser.h"

// appleseed.renderer headers.

// appleseed.foundation headers.

// Standard headers.

using namespace foundation;
using namespace std;

namespace renderer
{

//
// ShaderParamParser class implementation.
//

ShaderParamParser::ShaderParamParser(const std::string& string)
{
    tokenize(string, Blanks, m_tokens);
    m_tok_it = m_tokens.begin();
    m_tok_end = m_tokens.end();
    
    const std::string tok(*m_tok_it);

    if (tok == "color")
        m_param_type = OSLParamTypeColor;
    else if (tok == "float")
        m_param_type = OSLParamTypeFloat;
    else if (tok == "int")
        m_param_type = OSLParamTypeInt;
    else if (tok == "normal")
        m_param_type = OSLParamTypeNormal;
    else if (tok == "point")
        m_param_type = OSLParamTypePoint;
    else if (tok == "string")
        m_param_type = OSLParamTypeString;
    else if (tok == "vector")
        m_param_type = OSLParamTypeVector;
    else
        throw ExceptionOSLParamParseError();

    ++m_tok_it;
}

template<class T>
T ShaderParamParser::parse_one_value(bool expect_end)
{
    if (m_tok_it == m_tok_end)
        throw ExceptionOSLParamParseError();

    string s(*m_tok_it);
    s = foundation::trim_both(s);

    if (s[0] == '\0')
        s.erase(0, 1);

    T value = convert_from_string<T>(s);
    ++m_tok_it;
    
    if (expect_end)
    {
        if (m_tok_it != m_tok_end)
            throw ExceptionOSLParamParseError();
    }

    return value;
}

template<class T>
void ShaderParamParser::parse_three_values(
    T& a, 
    T& b,
    T& c,
    bool parse_as_color)
{
    a = parse_one_value<T>();

    if (m_tok_it == m_tok_end)
    {
        if (parse_as_color)
        {
            b = c = a;
            return;
        }

        throw ExceptionOSLParamParseError();
    }

    b = parse_one_value<T>();

    if (m_tok_it == m_tok_end)
        throw ExceptionOSLParamParseError();

    c = parse_one_value<T>();

    if (m_tok_it != m_tok_end)
        throw ExceptionOSLParamParseError();
}

float ShaderParamParser::float_value()
{
    assert(param_type() == OSLParamTypeFloat);
    return parse_one_value<float>(true);
}

int ShaderParamParser::int_value()
{
    assert(param_type() == OSLParamTypeInt);
    return parse_one_value<int>(true);
}

void ShaderParamParser::color_value(float& r, float& g, float& b)
{
    assert(param_type() == OSLParamTypeColor);
    parse_three_values(r, g, b, true);
}

}   // namespace renderer
