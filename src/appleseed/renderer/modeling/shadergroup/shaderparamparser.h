
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

#ifndef APPLESEED_RENDERER_MODELING_SHADERGROUP_SHADERPARAMPARSER_H
#define APPLESEED_RENDERER_MODELING_SHADERGROUP_SHADERPARAMPARSER_H

// appleseed.renderer headers.
#include "renderer/modeling/shadergroup/shaderparam.h"

// appleseed.foundation headers.
#include "foundation/core/exceptions/exception.h"
#include "foundation/utility/string.h"

// Standard headers.
#include <cstddef>
#include <string>
#include <vector>

namespace renderer
{

//
// Exception class thrown in case of an OSL param string parse error.
//

struct ExceptionOSLParamParseError
  : public foundation::Exception
{
};


//
// An utility class to parse OSL shader parameter value strings.
//

class ShaderParamParser
{
  public:
    explicit ShaderParamParser(const std::string& s);

    OSLParamType param_type() const;

    template <typename T>
    T parse_one_value(const bool expect_end = true);

    template <typename T>
    void parse_three_values(
        T&          a,
        T&          b,
        T&          c,
        const bool  parse_as_color = false);

    template <typename T>
    void parse_n_values(size_t n, T* values);

    void parse_int_array(std::vector<int>& values);

    void parse_float_array(std::vector<float>& values);
    void parse_float3_array(std::vector<float>& values);

    void parse_matrix_array(std::vector<float>& values);

    std::string parse_string_value();

  private:
    std::string                                 m_original_string;
    std::vector<std::string>                    m_tokens;
    OSLParamType                                m_param_type;
    std::vector<std::string>::const_iterator    m_tok_it;
    std::vector<std::string>::const_iterator    m_tok_end;

    template <typename T>
    T convert_from_string(const std::string& s) const;
};


//
// ShaderParamParser implementation.
//

inline OSLParamType ShaderParamParser::param_type() const
{
    return m_param_type;
}

template <typename T>
T ShaderParamParser::convert_from_string(const std::string& s) const
{
    try
    {
        return foundation::from_string<T>(s);
    }
    catch (const foundation::ExceptionStringConversionError&)
    {
        throw ExceptionOSLParamParseError();
    }
}

template <typename T>
T ShaderParamParser::parse_one_value(const bool expect_end)
{
    if (m_tok_it == m_tok_end)
        throw ExceptionOSLParamParseError();

    std::string s(*m_tok_it);
    s = foundation::trim_both(s);

    if (s[0] == '\0')
        s.erase(0, 1);

    const T value = convert_from_string<T>(s);
    ++m_tok_it;

    if (expect_end)
    {
        if (m_tok_it != m_tok_end)
            throw ExceptionOSLParamParseError();
    }

    return value;
}

template <typename T>
void ShaderParamParser::parse_three_values(
    T&          a,
    T&          b,
    T&          c,
    const bool  parse_as_color)
{
    a = parse_one_value<T>(false);

    if (m_tok_it == m_tok_end)
    {
        if (parse_as_color)
        {
            b = c = a;
            return;
        }

        throw ExceptionOSLParamParseError();
    }

    b = parse_one_value<T>(false);

    if (m_tok_it == m_tok_end)
        throw ExceptionOSLParamParseError();

    c = parse_one_value<T>(true);
}

template <typename T>
void ShaderParamParser::parse_n_values(size_t n, T* values)
{
    for (size_t i = 0; i < n; ++i)
        values[i] = parse_one_value<T>((i == n - 1) ? true : false);
}

}       // namespace renderer

#endif  // !APPLESEED_RENDERER_MODELING_SHADERGROUP_SHADERPARAMPARSER_H
