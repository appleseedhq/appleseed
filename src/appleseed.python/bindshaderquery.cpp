
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Esteban Tovagliari, The appleseedhq Organization
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

// appleseed.python headers.
#include "dict2dict.h"

// appleseed.renderer headers.
#include "renderer/api/shadergroup.h"

// appleseed.foundation headers.
#include "foundation/core/concepts/noncopyable.h"
#include "foundation/memory/autoreleaseptr.h"
#include "foundation/platform/python.h"
#include "foundation/utility/searchpaths.h"

// Standard headers.
#include <cstddef>
#include <string>

namespace bpy = boost::python;
using namespace foundation;
using namespace renderer;

namespace
{
    // A wrapper class that contains a ShaderQuery and a SearchPaths.
    class ShaderQueryWrapper
      : public NonCopyable
    {
      public:
        ShaderQueryWrapper()
        {
            m_shader_query = ShaderQueryFactory::create();
        }

        explicit ShaderQueryWrapper(const char* search_path)
        {
            m_shader_query = ShaderQueryFactory::create(search_path);
        }

        explicit ShaderQueryWrapper(const bpy::list& search_paths)
        {
            for (bpy::ssize_t i = 0, e = bpy::len(search_paths); i < e; ++i)
            {
                const bpy::extract<const char*> extractor(search_paths[i]);
                if (extractor.check())
                    m_search_paths.push_back_explicit_path(extractor());
                else
                {
                    PyErr_SetString(PyExc_TypeError, "Incompatible type. Only strings accepted.");
                    bpy::throw_error_already_set();
                }
            }

            m_shader_query = ShaderQueryFactory::create(m_search_paths);
        }

        bool open(const char* shader_name) const
        {
            return m_shader_query->open(shader_name);
        }

        bool open_bytecode(const char* shader_code) const
        {
            return m_shader_query->open_bytecode(shader_code);
        }

        std::string get_shader_name() const
        {
            return m_shader_query->get_shader_name();
        }

        std::string get_shader_type() const
        {
            return m_shader_query->get_shader_type();
        }

        size_t get_param_count() const
        {
            return m_shader_query->get_param_count();
        }

        bpy::dict get_param_info(const size_t param_index) const
        {
            return dictionary_to_bpy_dict(
                m_shader_query->get_param_info(param_index));
        }

        bpy::dict get_metadata() const
        {
            return dictionary_to_bpy_dict(
                m_shader_query->get_metadata());
        }

      private:
        SearchPaths                     m_search_paths;
        auto_release_ptr<ShaderQuery>   m_shader_query;
    };
}

void bind_shader_query()
{
    bpy::class_<ShaderQueryWrapper, boost::noncopyable>("ShaderQuery")
        .def(bpy::init<const char*>())
        .def(bpy::init<bpy::list>())
        .def("open", &ShaderQueryWrapper::open)
        .def("open_bytecode", &ShaderQueryWrapper::open_bytecode)
        .def("get_shader_name", &ShaderQueryWrapper::get_shader_name)
        .def("get_shader_type", &ShaderQueryWrapper::get_shader_type)
        .def("get_num_params", &ShaderQueryWrapper::get_param_count)
        .def("get_param_info", &ShaderQueryWrapper::get_param_info)
        .def("get_metadata", &ShaderQueryWrapper::get_metadata);
}
