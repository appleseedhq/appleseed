
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

// Standard headers.
#include <cstddef>

namespace bpy = boost::python;
using namespace foundation;

namespace
{
    bpy::str get_lib_name()
    {
        return Appleseed::get_lib_name();
    }

    bpy::str get_lib_version()
    {
        return Appleseed::get_lib_version();
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

    bpy::str get_lib_cpu_features()
    {
        return Appleseed::get_lib_cpu_features();
    }

    bpy::str get_synthetic_version_string()
    {
        return Appleseed::get_synthetic_version_string();
    }

    // Assembles version information on all third party dependencies.
    bpy::dict get_third_parties_versions()
    {
        bpy::dict output_dict;

        const LibraryVersionArray version_array = ThirdParties::get_versions();
        for (size_t i = 0; i < version_array.size(); ++i)
        {
            const APIStringPair version = version_array[i];
            output_dict[version.m_first.c_str()] = version.m_second.c_str();
        }

        return output_dict;
    }
}

void bind_appleseed()
{
    bpy::def("get_lib_name", &get_lib_name);
    bpy::def("get_lib_version", &get_lib_version);
    bpy::def("get_lib_configuration", &get_lib_configuration);
    bpy::def("get_lib_compilation_date", &get_lib_compilation_date);
    bpy::def("get_lib_compilation_time", &get_lib_compilation_time);
    bpy::def("get_lib_cpu_features", &get_lib_cpu_features);
    bpy::def("get_synthetic_version_string", &get_synthetic_version_string);
    bpy::def("get_third_parties_versions", &get_third_parties_versions);
}
