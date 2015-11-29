
//
// This source file is part of appleseed.
// Visit http://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2015 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.renderer headers.
#include "renderer/global/globallogger.h"

// appleseed.foundation headers.
#include "foundation/math/vector.h"
#include "foundation/utility/containers/dictionary.h"
#include "foundation/utility/containers/specializedarrays.h"
#include "foundation/utility/iostreamop.h"
#include "foundation/utility/searchpaths.h"

// OSL headers.
#include "foundation/platform/oslheaderguards.h"
BEGIN_OSL_INCLUDES
#include "OSL/oslquery.h"
END_OSL_INCLUDES

// Standard headers.
#include <string>

using namespace foundation;
using namespace std;

namespace renderer
{

//
// ShaderQuery class implementation.
//

struct ShaderQuery::Impl
{
    std::string m_search_path;
    OSL::OSLQuery m_query;

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
        else
            return false;

        return true;
   }

    static Dictionary metadata_param_to_dict(const OSL::OSLQuery::Parameter& param)
    {
        Dictionary dictionary;
        dictionary.insert("name", param.name.c_str());
        dictionary.insert("type", param.type.c_str());

        if (!copy_value_to_dict(param, "value", dictionary))
        {
            RENDERER_LOG_WARNING(
                "skipping metadata value for entry %s of type %s.",
                param.name.c_str(),
                param.type.c_str());
        }

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

        const bool is_array = (param.type.arraylen != 0);
        dictionary.insert("isarray", is_array);
        if (is_array)
            dictionary.insert("arraylen", param.type.arraylen);

        if (param.validdefault)
        {
            if (is_array)
            {
                RENDERER_LOG_WARNING(
                    "skipping default value for param %s of type %s array.",
                    param.name.c_str(),
                    param.type.c_str());
            }
            else
            {
                if (!copy_value_to_dict(param, "default", dictionary))
                {
                    RENDERER_LOG_WARNING(
                        "skipping default value for param %s of type %s.",
                        param.name.c_str(),
                        param.type.c_str());
                }
            }
        }

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
};

ShaderQuery::ShaderQuery(const SearchPaths& search_paths)
  : impl(new Impl())
{
    impl->m_search_path = search_paths.to_string();
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
    return impl->m_query.open(shader_name, impl->m_search_path);
}

const char* ShaderQuery::get_shader_name() const
{
    return impl->m_query.shadername().c_str();
}

const char* ShaderQuery::get_shader_type() const
{
    return impl->m_query.shadertype().c_str();
}

size_t ShaderQuery::get_num_params() const
{
    return impl->m_query.nparams();
}

Dictionary ShaderQuery::get_param_info(const size_t param_index) const
{
    assert(param_index < get_num_params());

    return Impl::param_to_dict(
                *impl->m_query.getparam(param_index));
}

DictionaryArray ShaderQuery::get_metadata() const
{
    DictionaryArray metadata;

    for (size_t i = 0, e = impl->m_query.metadata().size(); i < e; ++i)
    {
        metadata.push_back(
            Impl::metadata_param_to_dict(impl->m_query.metadata()[i]));
    }

    return metadata;
}


//
// ShaderQueryFactory class implementation.
//

auto_release_ptr<ShaderQuery> ShaderQueryFactory::create(
    const SearchPaths& search_paths)
{
    return auto_release_ptr<ShaderQuery>(new ShaderQuery(search_paths));
}

}   // namespace renderer
