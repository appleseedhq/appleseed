
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2014-2016 Esteban Tovagliari, The appleseedhq Organization
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

// Standard headers.
#include <cassert>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// ShaderParamParser class implementation.
//

ShaderParamParser::ShaderParamParser(const string& s)
  : m_original_string(s)
{
    tokenize(s, Blanks, m_tokens);

    m_tok_it = m_tokens.begin();
    m_tok_end = m_tokens.end();

    const string tok(*m_tok_it);

    if (tok == "color")
        m_param_type = OSLParamTypeColor;
    else if (tok == "color[]")
        m_param_type = OSLParamTypeColorArray;
    else if (tok == "float")
        m_param_type = OSLParamTypeFloat;
    else if (tok == "float[]")
        m_param_type = OSLParamTypeFloatArray;
    else if (tok == "int")
        m_param_type = OSLParamTypeInt;
    else if (tok == "int[]")
        m_param_type = OSLParamTypeIntArray;
    else if (tok == "matrix")
        m_param_type = OSLParamTypeMatrix;
    else if (tok == "matrix[]")
        m_param_type = OSLParamTypeMatrixArray;
    else if (tok == "normal")
        m_param_type = OSLParamTypeNormal;
    else if (tok == "normal[]")
        m_param_type = OSLParamTypeNormalArray;
    else if (tok == "point")
        m_param_type = OSLParamTypePoint;
    else if (tok == "point[]")
        m_param_type = OSLParamTypePointArray;
    else if (tok == "string")
        m_param_type = OSLParamTypeString;
    else if (tok == "vector")
        m_param_type = OSLParamTypeVector;
    else if (tok == "vector[]")
        m_param_type = OSLParamTypeVectorArray;
    else
        throw ExceptionOSLParamParseError();

    ++m_tok_it;
}

void ShaderParamParser::parse_int_array(std::vector<int>& values)
{
    values.clear();

    while (m_tok_it != m_tok_end)
        values.push_back(parse_one_value<int>(false));

    if (values.empty())
        throw ExceptionOSLParamParseError();
}

void ShaderParamParser::parse_float_array(std::vector<float>& values)
{
    values.clear();

    while (m_tok_it != m_tok_end)
        values.push_back(parse_one_value<float>(false));

    if (values.empty())
        throw ExceptionOSLParamParseError();
}

void ShaderParamParser::parse_float3_array(std::vector<float>& values)
{
    parse_float_array(values);

    if (values.size() % 3 != 0)
        throw ExceptionOSLParamParseError();
}

void ShaderParamParser::parse_matrix_array(std::vector<float>& values)
{
    parse_float_array(values);

    if (values.size() % 16 != 0)
        throw ExceptionOSLParamParseError();
}

string ShaderParamParser::parse_string_value()
{
    assert(param_type() == OSLParamTypeString);

    // Remove the string prefix and trim whitespace.
    string val(m_original_string, 6, string::npos);
    return trim_both(val);
}

}   // namespace renderer
