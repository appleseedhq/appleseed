
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015-2018 Esteban Tovagliari, The appleseedhq Organization
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
#include "shaderquery.h"

// appleseed.foundation headers.
#include "foundation/containers/dictionary.h"
#include "foundation/math/vector.h"
#include "foundation/utility/api/apistring.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/searchpaths.h"

// OSL headers.
#include "foundation/platform/_beginoslheaders.h"
#include "OSL/oslquery.h"
#include "foundation/platform/_endoslheaders.h"

// Boost headers.
#include "boost/optional.hpp"

// Standard headers.
#include <string>
#include <vector>

using namespace foundation;

namespace renderer
{

//
// ShaderQuery class implementation.
//

struct ShaderQuery::Impl
{
    typedef boost::optional<Dictionary> OptionalDictionary;

    std::string                              m_search_path;
    OSL::OSLQuery                            m_query;
    mutable OptionalDictionary               m_metadata;
    mutable std::vector<OptionalDictionary>  m_param_info;

    static bool copy_value_to_dict(
        const OSL::OSLQuery::Parameter& param,
        const char*                     value_key,
        Dictionary&                     dictionary)
    {
        if (param.type == OSL::TypeDesc::TypeInt)
            dictionary.insert(value_key, param.idefault[0]);
        else if (param.type == OSL::TypeDesc::TypeFloat)
            dictionary.insert(value_key, param.fdefault[0]);
        else if (param.type == OSL::TypeDesc::TypeString)
            dictionary.insert(value_key, param.sdefault[0].c_str());
        else if (
            param.type == OSL::TypeDesc::TypeColor  ||
            param.type == OSL::TypeDesc::TypeNormal ||
            param.type == OSL::TypeDesc::TypePoint  ||
            param.type == OSL::TypeDesc::TypeVector)
        {
            dictionary.insert(
                value_key,
                Vector3f(
                    param.fdefault[0],
                    param.fdefault[1],
                    param.fdefault[2]));
        }
        else return false;

        return true;
   }

    static Dictionary metadata_param_to_dict(const OSL::OSLQuery::Parameter& param)
    {
        Dictionary dictionary;
        dictionary.insert("name", param.name.c_str());
        dictionary.insert("type", param.type.c_str());
        copy_value_to_dict(param, "value", dictionary);
        return dictionary;
    }

    static Dictionary param_to_dict(const OSL::OSLQuery::Parameter& param)
    {
        Dictionary dictionary;
        dictionary.insert("name", param.name.c_str());
        dictionary.insert("type", param.type.c_str());

        dictionary.insert("validdefault", param.validdefault);
        dictionary.insert("isoutput", param.isoutput);
        dictionary.insert("isclosure", param.isclosure);

        dictionary.insert("isstruct", param.isstruct);
        if (param.isstruct)
            dictionary.insert("structname", param.structname);

        const bool is_array = param.type.arraylen != 0;
        dictionary.insert("isarray", is_array);
        if (is_array)
        {
            // Special float[2] cases (maya, UVs, etc).
            if (param.type.arraylen == 2 && param.type.elementtype() == OSL::TypeDesc::TypeFloat)
            {
                dictionary.insert(
                    "default",
                    Vector2f(param.fdefault[0], param.fdefault[1]));
            }

            dictionary.insert("arraylen", param.type.arraylen);
        }

        if (param.validdefault && !is_array)
            copy_value_to_dict(param, "default", dictionary);

        if (!param.metadata.empty())
        {
            Dictionary metadata;
            for (size_t i = 0, e = param.metadata.size(); i < e; ++i)
            {
                metadata.dictionaries().insert(
                    param.metadata[i].name.c_str(),
                    metadata_param_to_dict(param.metadata[i]));
            }

            dictionary.insert("metadata", metadata);
        }

        return dictionary;
    }

    bool open(const char* shader_name)
    {
        init();

        const bool ok = m_query.open(shader_name, m_search_path);

        if (ok)
            m_param_info.assign(m_query.nparams(), OptionalDictionary());

        return ok;
    }

    bool open_bytecode(const char* shader_code)
    {
        init();

        const bool ok = m_query.open_bytecode(shader_code);

        if (ok)
            m_param_info.assign(m_query.nparams(), OptionalDictionary());

        return ok;
    }

    void init()
    {
        m_metadata = OptionalDictionary();
        m_param_info.clear();

        m_query = OSL::OSLQuery();
    }
};

ShaderQuery::ShaderQuery()
  : impl(new Impl())
{
}

ShaderQuery::ShaderQuery(const char* search_path)
  : impl(new Impl())
{
    assert(search_path);

    impl->m_search_path = search_path;
}

ShaderQuery::ShaderQuery(const SearchPaths& search_paths)
  : impl(new Impl())
{
    impl->m_search_path =
        to_string(search_paths.to_string_reversed(SearchPaths::osl_path_separator()));
}

ShaderQuery::~ShaderQuery()
{
    delete impl;
}

void ShaderQuery::release()
{
    delete this;
}

bool ShaderQuery::open(const char* shader_name)
{
    return impl->open(shader_name);
}

bool ShaderQuery::open_bytecode(const char* shader_code)
{
    return impl->open_bytecode(shader_code);
}

const char* ShaderQuery::get_shader_name() const
{
    return impl->m_query.shadername().c_str();
}

const char* ShaderQuery::get_shader_type() const
{
    return impl->m_query.shadertype().c_str();
}

size_t ShaderQuery::get_param_count() const
{
    return impl->m_query.nparams();
}

const Dictionary& ShaderQuery::get_param_info(const size_t param_index) const
{
    assert(param_index < get_param_count());

    if (!impl->m_param_info[param_index])
    {
        impl->m_param_info[param_index] =
            Impl::param_to_dict(*impl->m_query.getparam(param_index));
    }

    return impl->m_param_info[param_index].get();
}

const Dictionary& ShaderQuery::get_metadata() const
{
    if (!impl->m_metadata)
    {
        Dictionary metadata;

        for (size_t i = 0, e = impl->m_query.metadata().size(); i < e; ++i)
        {
            metadata.insert(
                impl->m_query.metadata()[i].name.c_str(),
                Impl::metadata_param_to_dict(impl->m_query.metadata()[i]));
        }

        impl->m_metadata = metadata;
    }

    return impl->m_metadata.get();
}


//
// ShaderQueryFactory class implementation.
//

auto_release_ptr<ShaderQuery> ShaderQueryFactory::create()
{
    return auto_release_ptr<ShaderQuery>(new ShaderQuery());
}

auto_release_ptr<ShaderQuery> ShaderQueryFactory::create(const char* search_path)
{
    return auto_release_ptr<ShaderQuery>(new ShaderQuery(search_path));
}

auto_release_ptr<ShaderQuery> ShaderQueryFactory::create(
    const SearchPaths& search_paths)
{
    return auto_release_ptr<ShaderQuery>(new ShaderQuery(search_paths));
}

}   // namespace renderer
