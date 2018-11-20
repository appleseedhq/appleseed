
//
// This source file is part of appleseed.
// Visit https://appleseedhq.net/ for additional information and resources.
//
// This software is released under the MIT license.
//
// Copyright (c) 2018 Jonathan Dent, the appleseedhq Organization
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

// appleseed.foundation headers.
#include "foundation/core/appleseed.h"
#include "foundation/core/thirdparties.h"
#include "foundation/platform/python.h"

namespace bpy = boost::python;
using namespace foundation;

namespace
{
    // Assembles version information on all third party dependencies
    bpy::dict get_deps_lib_versions()
    {
        bpy::dict output_dict;
        const LibraryVersionArray& info_array = ThirdParties::get_versions();
        for (size_t index = 0; index < info_array.size(); index++)
        {
            const APIStringPair& result = info_array[index];
            output_dict[result.m_first.c_str()] = result.m_second.c_str();
        }

        return output_dict;
    }

    bpy::str get_synthetic_version_string()
    {
        return Appleseed::get_synthetic_version_string();
    }

    bpy::str get_lib_name()
    {
        return Appleseed::get_lib_name();
    }

    bpy::str get_lib_version()
    {
        return Appleseed::get_lib_version();
    }

    bpy::str get_lib_variant()
    {
        return Appleseed::get_lib_variant();
    }

    bpy::str get_lib_configuration()
    {
        return Appleseed::get_lib_configuration();
    }

    bpy::str get_lib_compilation_date()
    {
        return Appleseed::get_lib_compilation_date();
    }

    bpy::str get_lib_compilation_time()
    {
        return Appleseed::get_lib_compilation_time();
    }
}

void bind_appleseed_info()
{
    bpy::def("get_deps_lib_versions", &get_deps_lib_versions);
    bpy::def("get_synthetic_version_string", &get_synthetic_version_string);
    bpy::def("get_lib_name", &get_lib_name);
    bpy::def("get_lib_version", &get_lib_version);
    bpy::def("get_lib_variant", &get_lib_variant);
    bpy::def("get_lib_configuration", &get_lib_configuration);
    bpy::def("get_lib_compilation_date", &get_lib_compilation_date);
    bpy::def("get_lib_compilation_time", &get_lib_compilation_time);
}
